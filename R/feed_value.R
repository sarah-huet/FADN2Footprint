#' Function to estimate feed value
#' `f_feed_value` Estimate the feed value based on EUROSTAT feed stuff prices
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#'
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

  EUROSTAT_ref_value <- data_extra$Sailley_2021_feed_flows |>
    dplyr::select(Sailley_feed, feed_type, EUROSTAT_feedstuff) |>
    tidyr::separate_longer_delim(EUROSTAT_feedstuff,";") |>
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
                           .by = c(YEAR, Sailley_feed, EUROSTAT_feedstuff))
      )))
    )# |>
    #dplyr::summarise(euros_t = mean(euros_t, na.rm = T),
                     #.by = c(YEAR, Country_ISO_3166_1_A3, feed_type))

  # Missing feed stuff
  # compute average price per crop & year across countries to handle missing crop-country reference prices by falling back to a cross-country average for that crop.
  feed_price_year <- EUROSTAT_ref_value |>
    dplyr::summarise(euros_t = mean(euros_t, na.rm = T),
                     .by = c(YEAR, Sailley_feed, EUROSTAT_feedstuff))
                     #.by = c(YEAR, feed_type))

  # change cake price ----
  #feed_price_country_year <- feed_price_country_year |>
  #  #dplyr::mutate(euros_t = ifelse(Sailley_feed == "Tourteaux", euros_t*0.01, euros_t))
  #  dplyr::mutate(euros_t = euros_t*0.5)
  ##
  ##
  #feed_price_year <- feed_price_year |>
  #  #dplyr::mutate(euros_t = ifelse(Sailley_feed == "Tourteaux", euros_t*0.1, euros_t))
  #  dplyr::mutate(euros_t = euros_t*0.5)


  return(list(
    feed_price_country_year = feed_price_country_year,
    feed_price_year = feed_price_year
  ))

}
