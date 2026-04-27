#' Intermediate functions n°1 for the estimation of BVI for crops
#' `data_for_BVIAS` Create a tibble with the following variables: "farm_id", "crop",  "land_use_type", "metric_code", "value", "max", "x_norm", "org_farming"
#'
#' @param object a FADN2Footprint object
#' @returns a tibble
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' data_for_BVIAS(object = fadn_fict_obj)
#' @export

#' @import dplyr
#' @import tidyr
#' @import tidyselect
#'
#' @concept footprint-biodiv
#'


# data preparation (first step) -----------------------------------------------------------------------------------------------------
data_for_BVIAS <- function(object,...){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # Parameters of the BVI model (Lindner et al., 2019) ------------------------------------------------------------------------------

  object <- infer_practices(object)

  input_data <- tibble()
  for (i in names(object@practices$crops)[!sapply(object@practices$crops, is.null)]) {
    tmp = merge(
      object@crop |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),
                      dplyr::matches("land_use_type|FADN_code")),
      object@practices$crops[[i]] |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),
                      dplyr::matches("land_use_type|FADN_code"),all_of(i)) |>
        tidyr::pivot_longer(cols = dplyr::all_of(i),names_to = "metric_code",values_to = "value")
    )

    input_data <- input_data |>
      dplyr::bind_rows(tmp)
  }
  # add ORGANIC
  tmp_x <- merge(
    input_data,
    object@farm |>
      dplyr::select(dplyr::all_of(object@traceability$id_cols),dplyr::matches("ORGANIC"))
  )

  # Normalize data ------------------------------------------------------------------------------

  # set 95th percentile as max
  tmp_max <- tmp_x |>
    dplyr::filter(value >0) |>
    dplyr::summarise(max = as.vector(stats::quantile(unique(value),0.95,na.rm = T)),
                     .by = c(metric_code,land_use_type))

  tmp_x_norm <- tmp_x |>
    # add max
    dplyr::left_join(
      tmp_max,
      by = join_by(land_use_type,metric_code)) |>
    ## calculate BV = Normalize data
    dplyr::mutate(
      x_norm =
        dplyr::case_when(
          value <= 0 ~ 0,
          value >= max ~ 1,
          .default =  value / max ))

    # select practices for each land use type
  practice_set = data_extra$BVIAS_var_weight |>
    dplyr::select(land_use_type,metric_code) |>
    dplyr::distinct()

tmp_x_norm <- tmp_x_norm  |>
    dplyr::semi_join(
      practice_set,
      by = c('land_use_type','metric_code')
    )

  return(tmp_x_norm)

}


utils::globalVariables(c('metric_code', 'value'))
# this is to avoid a note in check package (the issue is from the use of dplyr)
