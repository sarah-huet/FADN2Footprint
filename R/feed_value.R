#' Function to estimate feed value
#' `f_feed_value` Estimate the feed value based on EUROSTAT feed stuff prices
#'
#' @param my_object a FADN2Footprint object
#' @returns a list of two tables
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_feed_value(object = fadn_fict_obj)
#'
#' @concept practice-herding
#' @export
#' @keywords internal

#' @import dplyr
#' @import tidyr


f_feed_value <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # EUROSTAT feed stuff prices ------------------------------------------------------

  EUROSTAT_ref_value <- data_extra$crops |>
    dplyr::select(FADN_code_letter,EUROSTAT_feedstuff) |>
    tidyr::separate_longer_delim(EUROSTAT_feedstuff,";") |>
    dplyr::rename(FADN_code_feed = FADN_code_letter) |>
    dplyr::left_join(
      data_extra$Sailley_2021_feed_flows |>
        dplyr::select(Sailley_feed,EUROSTAT_feedstuff) |>
        tidyr::separate_longer_delim(EUROSTAT_feedstuff,";"),
      by = c('EUROSTAT_feedstuff'),
      relationship = "many-to-many"
    ) |>
    # add feed value
    dplyr::left_join(EUROSTAT_input_price,
                     by = c('EUROSTAT_feedstuff'),
                     relationship = "many-to-many") |>
    dplyr::filter(!is.na(EUROSTAT_feedstuff)) |>
    tidyr::pivot_longer(cols = as.character(2014:2024),
                        names_to = "year",
                        values_to = "euros_t") |>
    # wrangle variables
    dplyr::mutate(
      YEAR = as.character(year)
    )

  # Check missing values ----

  # Missing countries
  # add reference value for missing countries as the average of other countries' values for each year
  missing_country = setdiff(unique(object@farm$Country_ISO_3166_1_A3 ),
                            unique(EUROSTAT_ref_value$Country_ISO_3166_1_A3 ))

  feed_price_country_year <- EUROSTAT_ref_value |>
    dplyr::bind_rows(
      dplyr::bind_rows(lapply(missing_country,function(x) cbind(
        tibble(Country_ISO_3166_1_A3  = x),
        EUROSTAT_ref_value |>
          dplyr::select(!Country_ISO_3166_1_A3 ) |>
          dplyr::summarise(euros_t = mean(euros_t,na.rm = T),
                    .by = c(YEAR,FADN_code_feed,Sailley_feed,EUROSTAT_feedstuff))
      )))
    )

  # Missing feed stuff
  # compute average price per crop & year across countries to handle missing crop-country reference prices by falling back to a cross-country average for that crop.
  feed_price_year <- EUROSTAT_ref_value |>
    dplyr::summarise(euros_t = mean(euros_t,na.rm = T),
              .by = c(YEAR,FADN_code_feed,Sailley_feed,EUROSTAT_feedstuff))



  return(list(
    feed_price_country_year = feed_price_country_year,
    feed_price_year = feed_price_year
  ))

}
