# Constructor ----

setClass("FADN2Footprint",
         slots = list(
           farm = "data.frame",
           crop = "data.frame",
           herd = "data.frame",
           input = "data.frame",
           output = "list",
           landscape_metrics = "ANY",
           practices = "list",
           footprints = "list",
           traceability = "list"  # Optional: to store info like original version, mapping version, etc.
         ))

#' Constructor: Build a \code{FADN2Footprint} object from raw FADN data
#'
#' Creates a standardized \linkS4class{FADN2Footprint} object from (EU or national)
#' raw FADN microdata. The constructor (i) optionally removes numeric columns that
#' contain only zeros, (ii) infers the naming/version scheme of the provided FADN
#' variables using \code{var_dict}, (iii) harmonizes column names to the
#' \code{var_common} convention when needed, and (iv) parses the input dataset into
#' internal tables (farm, crop, herd, inputs, outputs). A \code{traceability} slot
#' is populated to keep track of key transformations (e.g., removed columns,
#' mapping decisions).
#'
#' @param df A \code{data.frame} containing raw FADN data at farm-year level
#'   (wide format), including identifiers (see \code{id_cols}) and the variables
#'   required by \pkg{FADN2Footprint} according to \code{var_dict}.
#' @param landscape_metrics Optional. A \code{data.frame} containing landscape
#'   metrics (e.g., hedge density, mean field size, ground cover). If provided, it
#'   is stored in the \code{landscape_metrics} slot and may later be used for
#'   practice indicators and footprint calculations.
#' @param var_dict A \code{data.frame} mapping variable names across FADN versions
#'   to a common naming scheme. Must include at least a \code{var_common} column.
#'   Can additionally include \code{var_pre2014}, \code{var_post2014}, and/or
#'   \code{var_user} (user-defined variables). If \code{NULL}, the internal
#'   dataset \code{dict_FADN} is used.
#' @param id_cols A character vector giving the identifier column names present
#'   in \code{df}. Must include at least \code{"ID"}, \code{"YEAR"}, and
#'   \code{"COUNTRY"}. Defaults to \code{c("ID", "YEAR", "COUNTRY")}.
#' @param ... Reserved for future extensions.
#'
#' @details
#' The returned object is an S4 instance of class \linkS4class{FADN2Footprint}
#' with the following main slots:
#' \itemize{
#'   \item \code{farm}: parsed farm characteristics table.
#'   \item \code{crop}: parsed crop/activity table.
#'   \item \code{herd}: parsed livestock/herd table.
#'   \item \code{input}: parsed input-use table (restricted to supported inputs).
#'   \item \code{output}: a list of parsed output components.
#'   \item \code{landscape_metrics}: user-provided landscape metrics (or \code{NULL}).
#'   \item \code{practices}: initialized list of practice indicators (mostly \code{NULL}).
#'   \item \code{footprints}: initialized list of footprint results (mostly \code{NULL}).
#'   \item \code{traceability}: list capturing version detection and mapping/removal logs.
#' }
#'
#' @return An S4 object of class \linkS4class{FADN2Footprint}.
#'
#' @seealso
#' \linkS4class{FADN2Footprint}
#'
#' @examples
#' data(my_FADN_data)
#' data(dict_FADN)
#'
#' my_object <- data_4FADN2Footprint(
#'   df = my_FADN_data,
#'   var_dict = dict_FADN,
#'   id_cols = c("ID", "YEAR", "COUNTRY")
#' )
#'
#' my_object
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import stringr
#'
#' @concept data-preparation
#' @export

data_4FADN2Footprint <- function(df,
                                 landscape_metrics = NULL,
                                 var_dict = NULL,
                                 id_cols = c("ID","YEAR","COUNTRY"),
                                 ...) {

  message(
    "FADN2Footprint has only been validated for cereals (TF14=15) and dairy farms (TF14=45).\n
    Results for other types may be unreliable. Consider filtering your data first."
  )


  # check formats
  if(is.data.frame(df) == FALSE) stop("You must provide a data frame")
  if (!is.null(landscape_metrics) && !is.data.frame(landscape_metrics)) {
    stop("landscape_metrics must be a data.frame or NULL")
  }

  # Check if user provided data
  if (is.null(var_dict)) {
    # Using '::' ensures we grab the data from YOUR package,
    # preventing conflicts if the user has a variable named 'ref_prices'
    var_dict <- dict_FADN

    message("No dictionary provided. Using default FADN dictionary.")
  }

  # Check that required identifiers are present
  required <- c("ID", "YEAR", "COUNTRY")
  missing <- setdiff(required, id_cols)

  if (length(missing) > 0) {
    stop("The 'id_cols' parameter must include at least: ",
         paste(required, collapse = ", "),
         ". Missing: ", paste(missing, collapse = ", "))
  }

  # Start traceability list
  traceability <- list(id_cols = id_cols,
                       version = NULL,
                       columns = list(
                         raw_colnames = colnames(df),
                         columns_with_zeros = NULL,
                         trace_map = NULL,
                         dropped_common = NULL,
                         unmatched_user_var = NULL
                       ),
                       discarded_farms = NULL)

  # Step 1: Harmonize column names ----

  ### detect columns with zeros
  columns_with_zeros = setdiff(colnames(df),
                               colnames(df %>%
                                          dplyr::select(dplyr::where(function(col) {
                                            if (is.numeric(col)) {
                                              any(col != 0, na.rm = TRUE)
                                            } else {
                                              TRUE
                                            }
                                          }))
                               )
  )
  # keep columns needed for the package
  columns_with_zeros <- setdiff(
    columns_with_zeros,
    var_dict %>%
      dplyr::filter(!is.na(needed_in_FADN2Footprint) & !is.na(var_user)) %>%
      tidyr::separate_longer_delim(var_user,";") %>%
      dplyr::pull(var_user))
  columns_with_zeros <- setdiff(
    columns_with_zeros,
    var_dict %>%
      dplyr::filter(!is.na(needed_in_FADN2Footprint) & !is.na(var_common)) %>%
      dplyr::pull(var_common))
  ## Remove numeric columns with only zeros
  df_harmonized <- df %>%
    dplyr::select(-dplyr::matches(columns_with_zeros))
  message(length(columns_with_zeros)," columns contain only zeros and will be removed.")
  traceability$columns$columns_with_zeros <- columns_with_zeros

  ## Detect FADN version
    # TODO: remove this part as the conversion from national to European FADN is handled beforehands
  version <- f_infer_fadn_version(df, var_dict)
  message("Detected FADN column version: ", version)
  traceability$version <- version
  ## Map the variable names to a common format
  if (version == "var_common") {
    df_harmonized <- df_harmonized %>%
      dplyr::mutate(ID = as.character(ID))
  } else {
    df_harmonized <- f_map_columns_to_common(df_harmonized, id_cols, var_dict, version) %>%
      dplyr::mutate(ID = as.character(ID))

    traceability$columns$trace_map <- attr(df_harmonized,"trace_map")
    traceability$columns$dropped_common <- attr(df_harmonized,"dropped_common")
    traceability$columns$unmatched_user_var <- attr(df_harmonized,"unmatched_user_var")

    # remove attributes
    attributes(df_harmonized) <- attributes(df_harmonized)[c("names", "row.names", "class")]

  }

  # Ensure id cols are characters
  df_harmonized[id_cols] <- lapply(df_harmonized[id_cols], as.character)

  # Step 2: Extract subsets based on pattern ----
  # TODO: check select functions for compatibility with pre2014 and post2014 variable names
  # TODO: control for the presence or absence of variables such as organic or PDO (especially organic which is needed to estimate nitrogen fertilization and pesticides)

  ## FARM CHARACTERISTICS ----
  message("\n~~~ Parsing farm characteristics ~~~\n")

  # Load your internal dataset (assuming it's saved in sysdata.rda or data/)

  farm_data <- .parse_farm_data(
    df_harmonized = df_harmonized,
    id_cols = id_cols
  )

  ## CROPS ----
  message("\n~~~ Parsing crop data ~~~\n")

  crop_data <- .parse_crop_data(
    df_harmonized = df_harmonized,
    farm_data = farm_data,
    id_cols = id_cols
  )

  ## HERD ----
  message("\n~~~ Parsing herd data ~~~\n")

  herd_data <- .parse_herd_data(
    df_harmonized = df_harmonized,
    id_cols = id_cols
  )

  ## INPUTS ----
  message("\n~~~ Parsing input data ~~~\n")
  message("Make sure that your input variables are in the same units as those described in the dictionary (i.e., euros or tonnes).\n")
  message("Only these input variables are considered: IGRFEDCNCTRPUR_V, IGRFEDCRSPUR_V, INUSE_Q, IPROT_V, IFULS_V (see dictionnary)")

  input_data  <- .parse_input_data(df_harmonized = df_harmonized, id_cols = id_cols)

  ## OUTPUTS ----
  message("\n~~~ Parsing output data ~~~\n")

  output_list <- .parse_output_data(df_harmonized = df_harmonized,
                                    crop_data = crop_data,
                                    herd_data = herd_data,
                                    id_cols = id_cols)

  # TODO: Step X: Remove farms with aberrant values (crop production without area, etc)

  # Step 3: Create master object ----
  message("\n~~~ Building FADN2Footprint object ~~~\n")
  message("Data for ",
          nrow(farm_data %>% dplyr::select(dplyr::all_of(id_cols)) %>% distinct()),
          " farms have been gathered into a FADN2Footprint object.")

  obj <- new("FADN2Footprint",
             farm = farm_data,
             crop = crop_data,
             herd = herd_data,
             input = input_data,
             output = output_list,
             landscape_metrics = landscape_metrics,
             practices = list(crops = list(crop_diversity = NULL,
                                           N_ferti = NULL,
                                           share_Nmin_ferti = NULL,
                                           pesticides = NULL,
                                           tillage = NULL,
                                           livestock_density = NULL,
                                           hedge_density = NULL,
                                           mean_field_size = NULL,
                                           ground_cover = NULL),
                              herding = NULL),
             footprints = list(GHGE = NULL,
                               BVIAS = list(model_parameters = list(constants = NULL,
                                                                    weights = NULL),
                                            BVI_crops = NULL,
                                            BVI_milk = NULL,
                                            BVI_feed = NULL),
                               Water_pollution = NULL),
             traceability = traceability)


  return(obj)
}

# FADN version ###############################################################################
#' Infer input data base variable name version
#'
#' @param df A FADN input data frame.
#' @param var_dict The FADN variable dictionary.
#' @return The input data version.
#' @keywords internal
#'
f_infer_fadn_version <- function(df, var_dict) {
  cn <- colnames(df)

  match_counts <- c(
    var_pre2014  = sum(cn %in% var_dict$var_pre2014),
    var_post2014 = sum(cn %in% var_dict$var_post2014),
    var_common   = sum(cn %in% var_dict$var_common),
    var_user   = sum(cn %in% unlist(strsplit(var_dict$var_user,";")))
  )

  best_match <- names(which.max(match_counts))
  return(best_match)
}

# Translation map ###############################################################################
#' Map (and aggregate if needed) input data frame columns to common format
#'
#' This function harmonizes a dataset (FADN or RICA) to a standardized common
#' variable naming scheme. It aggregates multiple source variables into one
#' when required (e.g., RICA's "Spring barley" + "Winter barley" -> FADN "Barley").
#'
#' When multiple common targets exist for the same source variable
#' (e.g., RICA's "eggs" -> FADN "eggs for consumption" + "eggs for hatching"),
#' the first one (in dictionary order) is kept and the others are set to zero or empty.
#'
#'
#' @param df A data.frame or tibble containing raw data (e.g., RICA or FADN).
#' @param var_dict A tibble with columns:
#'   - `var_common`: standardized variable names (targets).
#'   - `var_user`: original variable names (sources).
#' @param id_cols A vector with the column name of the data set identifiers. Default are ID and YEAR.
#' @param version One of "var_pre2014", "var_post2014", "var_user", or "var_common".
#' @param trace Logical; if TRUE, stores mapping info in attributes:
#'   - `attr(df_out, "dropped_common")`: dropped common variables when source variables have matched with more than one common variable, and were stored under the first common variable name in dictionary order.
#'   - `attr(df_out, "trace_map")`: mapping info.
#'
#' @return A tibble with harmonized variables aggregated as needed.
#' @keywords internal
#'
f_map_columns_to_common <- function(df,
                                    id_cols = c("ID","YEAR","COUNTRY"),
                                    var_dict = dict_FADN,
                                    version = "var_user",
                                    trace = TRUE) {

  stopifnot(is.data.frame(df))
  stopifnot(all(c("var_common", version) %in% names(var_dict)))

  df_out <- df
  trace_map <- list()

  # Clean and normalize dictionary
  var_dict_clean <- var_dict %>%
    dplyr::select(var_common,any_of(version)) %>%
    dplyr::rename(var_user = any_of(version)) %>%
    # duplicate rows in case several source variables match one target variable (will be summed afterwards)
    tidyr::separate_longer_delim(var_user,";") %>%
    # keep only variables in user data
    dplyr::filter(!is.na(var_user) & var_user != "" & var_user %in% colnames(df))

  # Handle multiple common targets per source:
  # Keep only the first one encountered and mark others as "ignored"
  var_dict_first <- var_dict_clean %>%
    dplyr::mutate(first_common = dplyr::first(var_common), .by = var_user) %>%
    dplyr::mutate(var_common = first_common) %>%
    dplyr::select(-first_common) %>%
    dplyr::distinct()

  id_cols_raw = var_dict_first$var_user[var_dict_first$var_common %in% id_cols]

  dropped_common <- setdiff(var_dict_clean$var_common,var_dict_first$var_common)
  message(length(dropped_common)," source variables have matched with more than one common variable, and were stored under the first common variable name in dictionary order.")
  if (trace) attr(df_out, "dropped_common") <- dropped_common

  for (common_var in unique(var_dict_first$var_common)) {

    user_vars <- var_dict_first$var_user[var_dict_first$var_common == common_var]
    matched_vars <- intersect(user_vars, colnames(df_out))
    if (length(matched_vars) == 0) next  # no match, skip

    # Aggregate the matched columns
    matched_cols <- df_out[, matched_vars, drop = FALSE]
    ## if all matched variables are numeric, aggregate multiple source variable by summing rows.
    ## Otherwise, paste row elements
    if (all(sapply(matched_cols, is.numeric))) {

      df_out[,common_var] <- if(length(matched_vars)>1) {
        apply(matched_cols,1, function(row) sum(row, na.rm = T))
      } else {
        matched_cols
      }

    } else {

      df_out[,common_var] <- if(length(matched_vars)>1) {
        apply(matched_cols,1, function(row) paste(row, collapse = ";"))
      } else {
        matched_cols
      }
    }

    # Drop the original user columns
    tmp <- setdiff(matched_vars,common_var)
    df_out <- df_out %>% dplyr::select(-dplyr::any_of(tmp))

    # Trace mapping back
    if (trace) trace_map[[common_var]] <- matched_vars

  }

  unmatched_user_var <- intersect(colnames(df),colnames(df_out))
  unmatched_user_var <- setdiff(unmatched_user_var,id_cols)
  message(length(unmatched_user_var)," variables have not matched with any common FADN variable names and have been dropped.")
  if (trace) attr(df_out, "unmatched_user_var") <- unmatched_user_var

  if (trace) attr(df_out, "trace_map") <- trace_map
  return(df_out)
}


