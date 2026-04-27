#' Function to estimate off-farm animals of the cattle herd based on FADN data
#' `f_pseudoherd` Estimate off-farm animals of the cattle herd
#'
#' @param object a FADN2Footprint object
#' @returns
#' A list with, for each livestock category:
#' - Q_obs: the number of animals observed in the farm (in number of heads)
#' - Q_obs_pseudo: the number of animals of the pseudo-herd (in number of heads)
#'
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_pseudoherd(object = fadn_fict_obj)
#'
#' @concept practice-pseudoherd
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_pseudoherd <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # Retrieve on-farm herd ----
  herd <- f_herd_activities(object)

  # Estimate off-farm herd ----
  pseudoherd_cattle = f_pseudoherd_cattle(object)
  pseudoherd_swine = f_pseudoherd_swine(object)
  pseudoherd_poultry = f_pseudoherd_poultry(object)

  pseudoherd <- Reduce(
    function(x,y) full_join(x,y,by = c(object@traceability$id_cols, "FADN_code_letter")),
    list(
      pseudoherd_cattle$pseudoherd_milk,
      Reduce(bind_rows,list(pseudoherd_cattle$pseudoherd_meat,
                            pseudoherd_swine$pseudoherd_meat,
                            pseudoherd_poultry$pseudoherd_meat)
      ),
      pseudoherd_poultry$pseudoherd_eggs,
      # add on-farm quantities
      object@herd |>
        select(all_of(object@traceability$id_cols), FADN_code_letter, Qobs)

    )) |>
    # Calculate difference between on-farm and pseudo herd number of animals (i.e., off-farm animals)
    mutate(
      Qeq = coalesce(Qeq_milk,0) + coalesce(Qeq_meat,0) + coalesce(Qeq_eggs,0),
      Q_off_farm = Qeq - coalesce(Qobs,0)
    ) |>
    filter(Qeq >0)

  # Combine on- and off-farm herds ----
  pseudoherd <- full_join(herd,pseudoherd,
                          by = c(object@traceability$id_cols, "FADN_code_letter", "Qobs"))  |>
    # estimate Qobs off-farm per activity
    mutate(
      Q_off_farm_milk = coalesce(Qeq_milk,0) - coalesce(Qobs_milk,0),
      Q_off_farm_meat = coalesce(Qeq_meat,0) - coalesce(Qobs_meat,0),
      Q_off_farm_eggs = coalesce(Qeq_eggs,0) - coalesce(Qobs_eggs,0)
    )

  return(pseudoherd)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

