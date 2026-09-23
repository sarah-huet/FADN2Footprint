#' Estimate pseudoherd animals
#'
#' @description
#' f_pseudoherd_animals combines
#' species-specific pseudo-herd estimates for cattle, swine and poultry into
#' a single harmonised table.
#'
#' @details
#' Pseudo-herd animals represent the livestock population that would be
#' required to sustain production, based on recorded animal on the farm.
#' This allows accounting for emissions and
#' resource use associated with purchased or otherwise unaccounted animals
#' under a scope 3 / life cycle perspective.
#'
#' The function proceeds as follows:
#' 1. Species-specific pseudo-herd estimates are computed by calling
#'    f_pseudoherd_cattle, f_pseudoherd_swine and f_pseudoherd_poultry, each
#'    returning a list containing a `pseudoherd` element.
#' 2. The three `pseudoherd` tables are row-bound into a single table.
#' 3. Rows with no meaningful production or observed herd data (i.e., all of
#'    Qobs, Qeq_milk, Qeq_meat and Qeq_eggs are NA or zero) are dropped.
#' 4. Rows are filtered to keep only valid, non-aggregated FADN livestock
#'    codes (as listed in data_extra$livestock$FADN_code_letter), excluding
#'    mixed or composite categories (e.g. "LBOV1_2F_breeders").
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow, providing all data required by f_pseudoherd_cattle,
#'   f_pseudoherd_swine and f_pseudoherd_poultry.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A tibble combining pseudo-herd estimates for cattle, swine and
#'   poultry, with one row per farm × livestock category, including columns
#'   such as:
#' \describe{
#'   \item{FADN_code_letter}{FADN livestock category code.}
#'   \item{Qobs}{Observed number of animals in the farm's own herd.}
#'   \item{Qeq_milk}{Pseudo-herd animal-equivalent derived from milk
#'     production/purchase not attributable to the observed herd.}
#'   \item{Qeq_meat}{Pseudo-herd animal-equivalent derived from meat
#'     production/purchase.}
#'   \item{Qeq_eggs}{Pseudo-herd animal-equivalent derived from egg
#'     production/purchase (poultry only).}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' pseudoherd <- f_pseudoherd_animals(f)
#' head(pseudoherd)
#' }
#'
#' @seealso f_pseudoherd_cattle, f_pseudoherd_swine, f_pseudoherd_poultry,
#'   f_GHGE_herd
#'
#' @concept practice-pseudoherd
#' @export
#' @import dplyr


f_pseudoherd_animals <- function(object,
                                 overwrite = FALSE) {
        if (!inherits(object, "FADN2Footprint")) {
                stop("Input must be a valid 'FADN2Footprint' object.")
        }

        if (!is.null(object@practices$herding$pseudoherd$animals)&& !overwrite) {
                message("Using cached values stored in object@practices$herding$pseudoherd$animals.")
                return(object@practices$herding$pseudoherd$animals)  # use cached value
        }


        id_cols = object@traceability$id_cols
        # retrieve herd activities
        herd_activities <- f_herd_activities(object)

        # Estimate off-farm herd ----
        pseudoherd_cattle = f_pseudoherd_cattle(object)
        #pseudoherd_swine = f_pseudoherd_swine(object)
        #pseudoherd_poultry = f_pseudoherd_poultry(object)



        #pseudoherd_sheep = f_pseudoherd_sheep(object)


       #pseudoherd_animals <- Reduce(bind_rows,
       #                             list(pseudoherd_cattle$pseudoherd,
       #                                  pseudoherd_swine$pseudoherd,
       #                                  pseudoherd_poultry$pseudoherd
       #                                  )) |>
        pseudoherd_animals <- pseudoherd_cattle$pseudoherd |>
                # keep only FADN_code_letter
                ## this remove the code for mixed categories (e.g., "LBOV1_2F_breeders")
                dplyr::filter(FADN_code_letter %in% data_extra$livestock$FADN_code_letter)|>
                # estimate total number of animals, and off-farm animals
                dplyr::mutate(
                        Qeq = dplyr::coalesce(Qeq_milk, 0) + dplyr::coalesce(Qeq_meat, 0),# + dplyr::coalesce(Qeq_eggs, 0),
                        Qeq = round(Qeq, 2),
                        Qofffarm = dplyr::coalesce(Qeq, 0) - dplyr::coalesce(Qobs, 0),
                        Qofffarm = round(Qofffarm, 2)
                ) |>
                # keep row if: Qobs > 0 OR Qeq > 0
                dplyr::filter(
                        dplyr::if_any(
                                dplyr::matches("Qobs|Qeq"),
                                ~ .x > 0
                        )
                )


                return(pseudoherd_animals)

}
