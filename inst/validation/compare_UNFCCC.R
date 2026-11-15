# Report-only helpers; source this file after loading FADN2Footprint.
h_unfccc_sum <- function(x) {
  # Do not let sum() silently return a partial total when one input is missing.
  if (!length(x) || any(!is.finite(x))) NA_real_ else sum(x)
}

h_unfccc_ratio <- function(numerator, denominator) {
  # A non-positive denominator cannot define a meaningful coverage ratio.
  ifelse(
    is.finite(numerator) & is.finite(denominator) & denominator > 0,
    numerator / denominator,
    NA_real_
  )
}

h_unfccc_manure <- function(object) {
  # Only the known missing-EF3 case makes manure outputs unavailable; other
  # errors still stop the comparison so they are not hidden.
  tryCatch(f_GHGE_n2o_manure(object, return_components = TRUE), error = function(error) {
    message <- conditionMessage(error)
    if (!startsWith(message, "No finite EF3 for positive manure allocation:")) {
      stop(error)
    }
    warning("Manure comparisons unavailable: ", message, call. = FALSE)
    list(model_error = message)
  })
}

h_unfccc_catalogue <- function() {
  # Keep the CRT reference structure in one table. Later functions use these
  # identifiers both to extract values and to describe comparisons in reports.
  # Store the animal category, its exact CRT label, and its code suffix
  # together. The suffixes are deliberately keyed by category, not by their
  # position in a vector: in particular, 3.A.4 is Other livestock, while
  # sheep is 3.A.2.
  livestock <- tibble::tribble(
    ~livcat,             ~category_label,    ~code_suffix,
    "cattle",            "Cattle",           ".1",
    "dairy_cattle",      "Dairy cattle",     ".1.a",
    "non_dairy_cattle",  "Non-dairy cattle", ".1.b",
    "sheep",             "Sheep",            ".2",
    "swine",             "Swine",            ".3",
    "poultry",           "Poultry",          ".4.g"
  )
  rows <- list()
  # Add one fully described metric row to the catalogue.
  add <- function(
    id,
    sheet,
    variable,
    code,
    category,
    unit,
    family,
    component,
    note = ""
  ) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      metric_id = id,
      sheet = sheet,
      variable_pattern = variable,
      category_code = code,
      category_label = category,
      unit = unit,
      family = family,
      component = component,
      note = note
    )
  }
  herd <- function(id, sheet, variable, prefix, unit, component) {
    # Build one metric per named category so changing table order cannot
    # silently assign a different CRT category code.
    for (i in seq_len(nrow(livestock))) {
      category <- livestock[i, ]
      add(
        paste0(id, "_", category$livcat),
        sheet,
        variable,
        paste0(prefix, category$code_suffix),
        category$category_label,
        unit,
        "livestock",
        component
      )
    }
  }
  # Livestock totals, population, intake and emission components.
  herd(
    "enteric_total",
    "Table3",
    "^Total GHG emissions",
    "3.A",
    "kt CO2-eq/year",
    "enteric_total"
  )
  herd(
    "manure_total",
    "Table3",
    "^Total GHG emissions",
    "3.B",
    "kt CO2-eq/year",
    "manure_total"
  )
  herd(
    "population",
    "Table3.A",
    " - Population size",
    "3.A",
    "heads",
    "population"
  )
  herd(
    "gross_energy",
    "Table3.A",
    " - Average gross energy intake",
    "3.A",
    "MJ/head/day",
    "gross_energy"
  )
  herd(
    "enteric_ch4",
    "Table3.A",
    "^EMISSIONS.*CH4$",
    "3.A",
    "kt CH4/year",
    "enteric_ch4"
  )
  herd(
    "manure_ch4",
    "Table3.B(a)",
    "^EMISSIONS.*CH4$",
    "3.B",
    "kt CH4/year",
    "manure_ch4"
  )
  herd(
    "n_excreted",
    "Table3.B(b)",
    " - Total N Excreted$",
    "3.B",
    "t N/year",
    "n_excreted"
  )
  herd(
    "manure_direct_n2o",
    "Table3.B(b)",
    "^EMISSIONS.*N2O.*Direct$",
    "3.B",
    "kt N2O/year",
    "manure_direct_n2o"
  )
  # Indirect manure N2O is the sum of the CRT atmospheric deposition and
  # nitrogen leaching/run-off lines (summed in h_unfccc_inventory).
  for (entry in list(
    c("manure_volatilised_n", " - Total N volatilised", "t N/year"),
    c(
      "manure_indirect_n2o",
      "^EMISSIONS.*N2O.*Indirect.*(Atmospheric deposition|Nitrogen leaching and run-off)$",
      "kt N2O/year"
    )
  )) {
    add(
      entry[1],
      "Table3.B(b)",
      entry[2],
      "3.B.5",
      "Indirect N2O emissions",
      entry[3],
      "manure",
      entry[1],
      if (entry[1] == "manure_volatilised_n") {
        "Package volatilisation includes NH3 and NOx, not N2; shown as a proxy, not an equivalent."
      } else {
        ""
      }
    )
  }
  # Soil activity and emissions are kept separate so the report can compare
  # individual pathways as well as the total.
  soils <- c(
    "3.D.1" = "Direct N2O emissions from managed soils",
    "3.D.1.a" = "Inorganic N fertilizers",
    "3.D.1.b" = "Organic N fertilizers",
    "3.D.1.c" = "Urine and dung deposited by grazing animals",
    "3.D.1.d" = "Crop residues",
    "3.D.2" = "Indirect N2O Emissions from managed soils",
    "3.D.2.a" = "Atmospheric deposition",
    "3.D.2.b" = "Nitrogen leaching and run-off"
  )
  components <- c(
    "direct",
    "inorganic",
    "organic",
    "grazing",
    "residues",
    "indirect",
    "deposition",
    "leaching"
  )
  for (i in seq_along(soils)) {
    note <- if (components[i] == "grazing") {
      "Package approximates grazing deposits by organic fertilization of grassland."
    } else if (components[i] == "direct") {
      "Package excludes organic soils and soil organic matter mineralisation."
    } else if (components[i] == "indirect") {
      "Activity combines deposition and leaching pathways, which may overlap; not a unique N input."
    } else {
      ""
    }
    add(
      paste0("soil_activity_", components[i]),
      "Table3.D",
      "^ACTIVITY DATA.* - Value$",
      names(soils)[i],
      soils[i],
      "t N/year",
      "soil",
      paste0("soil_activity_", components[i]),
      note
    )
    add(
      paste0("soil_n2o_", components[i]),
      "Table3.D",
      "^EMISSIONS.*N2O$",
      names(soils)[i],
      soils[i],
      "kt N2O/year",
      "soil",
      paste0("soil_n2o_", components[i]),
      note
    )
  }
  # Table 3 only reports the 3.D total (direct + indirect N2O).
  add(
    "soil_total",
    "Table3",
    "^Total GHG emissions",
    "3.D",
    "Agricultural soils",
    "kt CO2-eq/year",
    "soil",
    "soil_total",
    "Package excludes organic soils and soil organic matter mineralisation."
  )
  for (i in seq_along(c("wheat", "barley", "maize"))) {
    crop <- c("wheat", "barley", "maize")[i]
    add(
      paste0("production_", crop),
      "Table3.F",
      "^Crop production$",
      paste0("3.F.1.", letters[i]),
      tools::toTitleCase(crop),
      "t/year",
      "crop",
      paste0("production_", crop),
      "CRT field-burning table may cover only crops with residue burning; grain maize only."
    )
  }
  # Area coverage is used later to rescale crop and soil estimates.
  add(
    "area_cropland",
    "Table4.B",
    "^ACTIVITY DATA - Total area",
    "4.B",
    "Total cropland",
    "ha",
    "area",
    "area_cropland",
    "FADN crop areas include arable and permanent crops, not all inventory cropland."
  )
  add(
    "area_grassland",
    "Table4.C",
    "^ACTIVITY DATA - Total area",
    "4.C",
    "Total grassland",
    "ha",
    "area",
    "area_grassland",
    "FADN grassland is agricultural grassland, not all inventory grassland."
  )
  dplyr::bind_rows(rows)
}

h_unfccc_inventory <- function(crt, catalogue) {
  # Check the imported CRT table before applying catalogue-specific matching.
  required <- c(
    "iso3",
    "year",
    "sheet",
    "variable",
    "category_code",
    "category",
    "value",
    "flag",
    "unit_expected",
    "unit_found",
    "unit_check"
  )
  if (!all(required %in% names(crt))) {
    stop("CRT extract lacks required columns.")
  }
  # Normalize labels so differences in numbering, punctuation and annotations
  # do not prevent matching the same category.
  label <- function(x) {
    x <- gsub("^\\s*[0-9]+(?:\\.[A-Za-z0-9]+)*\\.?\\s*", "", x, perl = TRUE)
    x <- gsub("\\[[^]]*\\]", "", x)
    x <- gsub("\\([^)]*\\)", "", x)
    sub("^all ", "", tolower(trimws(gsub("\\s+", " ", x))))
  }
  # Extract and validate each catalogue metric independently.
  dplyr::bind_rows(lapply(seq_len(nrow(catalogue)), function(i) {
    m <- catalogue[i, ]
    x <- crt[
      crt$sheet == m$sheet &
        grepl(
          m$variable_pattern,
          crt$variable,
          ignore.case = TRUE,
          perl = TRUE
        ),
    ]
    # A category code by itself is not enough: a valid-looking code can refer
    # to another label, as with 3.A.4 (Other livestock) versus 3.A.2 (Sheep).
    # Require both code and normalized label when a code is present. Retain
    # label-only matching only for CRT rows whose code field is genuinely blank.
    label_matches <- !is.na(x$category) &
      label(x$category) == label(m$category_label)
    code_matches <- !is.na(x$category_code) &
      x$category_code == m$category_code
    code_missing <- is.na(x$category_code) |
      !nzchar(trimws(x$category_code))
    x <- x[
      label_matches & (code_matches | code_missing),
    ]
    if (!nrow(x)) {
      return(NULL)
    }
    x |>
      dplyr::group_by(iso3, year) |>
      dplyr::group_modify(function(data, key) {
        canonical <- data[
          !is.na(data$category_code) & data$category_code == m$category_code,
        ]
        if (nrow(canonical)) {
          data <- canonical
        }
        flags <- paste(
          sort(unique(stats::na.omit(data$flag[data$flag != ""]))),
          collapse = ";"
        )
        normalize_unit <- function(x) {
          tolower(gsub(
            "[()[:space:].]",
            "",
            gsub("\\([0-9]+(?:,[0-9]+)*\\)", "", x, perl = TRUE)
          ))
        }
        units <- normalize_unit(data$unit_expected)
        found <- normalize_unit(data$unit_found)
        # Convert only recognized source units to the catalogue's target unit.
        # Unknown or inconsistent units are retained as a status, not guessed.
        multiplier <- switch(
          m$unit,
          "heads" = ifelse(
            units %in%
              c(
                "1000s",
                "1000head",
                "1000heads",
                "thousandhead",
                "thousandheads"
              ),
            1000,
            ifelse(units == "heads", 1, NA_real_)
          ),
          "ha" = ifelse(
            units %in% c("kha", "1000ha"),
            1000,
            ifelse(units == "ha", 1, NA_real_)
          ),
          "t N/year" = ifelse(
            units %in% c("kgn", "kgn/yr", "kgn/year"),
            0.001,
            ifelse(
              units %in% c("ktn", "ktn/yr", "ktn/year"),
              1000,
              ifelse(units %in% c("tn", "tn/yr", "tn/year"), 1, NA_real_)
            )
          ),
          "MJ/head/day" = ifelse(units == "mj/head/day", 1, NA_real_),
          "t/year" = ifelse(
            units %in% c("kt", "kt/yr", "kt/year"),
            1000,
            ifelse(units %in% c("t", "t/yr", "t/year"), 1, NA_real_)
          ),
          "kt CO2-eq/year" = ifelse(
            units %in%
              c(
                "kt",
                "ktco2eq",
                "ktco2-eq",
                "ktco2equiv",
                "ktco2equivalent",
                "ktco2equivalents",
                "co2eqkt",
                "co2equivkt",
                "co2equivalentkt",
                "co2equivalentskt"
              ),
            1,
            NA_real_
          ),
          ifelse(units %in% c("kt", "ktch4", "ktn2o"), 1, NA_real_)
        )
        bad_unit <- any(!is.finite(multiplier)) ||
          any(
            data$unit_check %in% c("MISMATCH", "UNIT_MISMATCH"),
            na.rm = TRUE
          ) ||
          any(!is.na(found) & nzchar(found) & found != units)
        values <- if (m$metric_id == "manure_indirect_n2o") {
          # Both CRT lines (deposition and leaching) are required for the sum.
          per_line <- tapply(
            data$value * multiplier,
            data$variable,
            function(v) if (length(unique(v)) == 1L) v[1] else NA_real_
          )
          if (length(per_line) == 2L) sum(per_line) else NA_real_
        } else {
          unique(data$value * multiplier)
        }
        # Keep a value only when matching and quality checks leave one
        # unambiguous, usable result.
        status <- if (length(values) > 1L) {
          "ambiguous"
        } else if (nzchar(flags)) {
          "notation_key"
        } else if (bad_unit) {
          "unit_mismatch"
        } else if (any(!is.finite(values))) {
          "missing_value"
        } else {
          "ok"
        }
        tibble::tibble(
          metric_id = m$metric_id,
          crt_value = if (status == "ok") values[1] else NA_real_,
          crt_flag = flags,
          crt_status = status,
          crt_unit_check = paste(sort(unique(data$unit_check)), collapse = ";")
        )
      }) |>
      dplyr::ungroup() |>
      dplyr::rename(YEAR = year) |>
      dplyr::mutate(YEAR = as.character(YEAR))
  }))
}

h_unfccc_fadn <- function(object, catalogue, coverage_only = FALSE) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Expected a FADN2Footprint object.")
  }
  ids <- object@traceability$id_cols
  # Attach country codes and expansion weights once, then reuse them for each
  # FADN output table.
  farm <- object@farm |>
    dplyr::select(dplyr::all_of(unique(c(ids, "COUNTRY", "YEAR", "SYS02")))) |>
    dplyr::left_join(
      data_extra$country_names |>
        dplyr::transmute(
          COUNTRY = country_FADN,
          iso3 = Country_ISO_3166_1_A3
        ) |>
        dplyr::distinct(),
      by = "COUNTRY",
      relationship = "many-to-one"
    )
  if (anyDuplicated(farm[ids])) {
    stop("Farm-year identifiers must be unique.")
  }
  if (anyNA(farm$iso3) || any(!is.finite(farm$SYS02) | farm$SYS02 <= 0)) {
    stop("Every farm needs an ISO3 country and a positive finite SYS02 weight.")
  }
  attach <- function(x) {
    dplyr::left_join(
      x |> dplyr::select(-dplyr::any_of(c("SYS02", "iso3"))),
      farm |> dplyr::select(dplyr::all_of(unique(c(ids, "SYS02", "iso3")))),
      by = ids,
      relationship = "many-to-one"
    )
  }
  livestock <- function(x) {
    # Map FADN animal codes to the CRT categories; also add a cattle aggregate
    # because the CRT reports both cattle overall and cattle subcategories.
    x <- attach(x)
    if (!"species" %in% names(x)) {
      x <- dplyr::left_join(
        x,
        object@herd |>
          dplyr::select(dplyr::all_of(ids), FADN_code_letter, species),
        by = c(ids, "FADN_code_letter"),
        relationship = "many-to-one"
      )
    }
    x <- x |>
      dplyr::mutate(
        livcat = dplyr::case_when(
          FADN_code_letter == "LCOWDAIR" ~ "dairy_cattle",
          species == "cattle" ~ "non_dairy_cattle",
          TRUE ~ species
        )
      )
    dplyr::bind_rows(
      x,
      x |>
        dplyr::filter(species == "cattle") |>
        dplyr::mutate(livcat = "cattle")
    )
  }
  results <- list()
  # Sum farm-level quantities with SYS02 weights, then convert to the unit
  # recorded in the catalogue. For livestock metrics, retain species groups.
  total <- function(
    x,
    column,
    component,
    divisor = 1,
    herd = FALSE,
    status = "ok",
    species_rescale = FALSE
  ) {
    x <- if (herd) livestock(x) else attach(x)
    group <- c("iso3", "YEAR", if (herd) "livcat")
    out <- x |>
      dplyr::summarise(
        f2f_value = h_unfccc_sum(.data[[column]] * SYS02) / divisor,
        n_farms = dplyr::n_distinct(dplyr::pick(dplyr::all_of(setdiff(
          ids,
          group
        )))),
        .by = dplyr::all_of(group)
      ) |>
      dplyr::mutate(
        metric_id = if (herd) paste0(component, "_", livcat) else component,
        f2f_status = dplyr::if_else(
          is.finite(f2f_value), status, "missing_f2f_value"
        ),
        f2f_error = NA_character_
      )
    if (species_rescale) {
      contributions <- x |>
        dplyr::summarise(
          value = h_unfccc_sum(.data[[column]] * SYS02) / divisor,
          .by = c(iso3, YEAR, species)
        ) |>
        dplyr::summarise(
          f2f_species_values = list(stats::setNames(value, species)),
          .by = c(iso3, YEAR)
        )
      out <- dplyr::left_join(
        out,
        contributions,
        by = c("iso3", "YEAR"),
        relationship = "one-to-one"
      )
    }
    results[[length(results) + 1L]] <<- dplyr::select(
      out,
      -dplyr::any_of("livcat")
    )
  }
  # Livestock population and gross energy intake.
  total(object@herd, "Qobs", "population", herd = TRUE)
  for (land in c("cropland", "grassland")) {
    total(
      object@crop |>
        dplyr::filter(
          if (land == "grassland") {
            land_use_type == "grassland"
          } else {
            land_use_type %in% c("arable", "fallow")
          }
        ),
      "area_ha",
      paste0("area_", land)
    )
  }
  if (coverage_only) {
    return(
      dplyr::bind_rows(results) |>
        dplyr::mutate(YEAR = as.character(YEAR)) |>
        dplyr::semi_join(catalogue, by = "metric_id")
    )
  }
  feed <- f_herd_feed(object)$feed_intake$total
  feed <- feed |>
    dplyr::select(-dplyr::matches("^Qobs")) |>
    dplyr::left_join(
      object@herd |>
        dplyr::select(dplyr::all_of(ids), FADN_code_letter, Qobs),
      by = c(ids, "FADN_code_letter"),
      relationship = "many-to-one"
    )
  ge <- livestock(feed) |>
    dplyr::summarise(
      f2f_value = h_unfccc_ratio(
        h_unfccc_sum(GE_MJ_anim * Qobs * SYS02),
        h_unfccc_sum(Qobs * SYS02)
      ) /
        365,
      n_farms = dplyr::n_distinct(dplyr::pick(dplyr::all_of(setdiff(
        ids,
        c("iso3", "YEAR", "livcat")
      )))),
      .by = c(iso3, YEAR, livcat)
    ) |>
    dplyr::mutate(
      metric_id = paste0("gross_energy_", livcat),
      f2f_status = dplyr::if_else(
        is.finite(f2f_value), "ok", "missing_f2f_value"
      )
    ) |>
    dplyr::select(-livcat)
  results[[length(results) + 1L]] <- ge
  # Enteric and manure emissions use the package's existing calculation
  # functions; this script only reshapes their outputs for comparison.
  enteric <- f_GHGE_ch4_enteric(object)
  methane <- f_GHGE_ch4_manure(object)
  manure <- h_unfccc_manure(object)
  total(enteric, "CH4_enteric_kgCO2e_livcat", "enteric_total", 1e6, TRUE)
  total(
    enteric,
    "CH4_enteric_kgCO2e_livcat",
    "enteric_ch4",
    1e6 * GWP[["CH4"]],
    TRUE
  )
  total(methane, "CH4_MM_kgCO2e_livcat", "manure_ch4", 1e6 * GWP[["CH4"]], TRUE)
  if (is.null(manure$model_error)) {
    total(
      manure$emissions,
      "N2O_D_MM_kgCO2e_livcat",
      "manure_direct_n2o",
      1e6 * GWP[["N20"]],
      TRUE
    )
    mm <- dplyr::left_join(
      methane,
      manure$emissions |>
        dplyr::select(
          dplyr::all_of(ids),
          FADN_code_letter,
          dplyr::ends_with("_livcat")
        ),
      by = c(ids, "FADN_code_letter"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      # CRT species-level manure totals exclude the indirect N2O in 3.B.5.
      manure_total = CH4_MM_kgCO2e_livcat +
        N2O_D_MM_kgCO2e_livcat
    )
    total(mm, "manure_total", "manure_total", 1e6, TRUE)
    total(
      manure$volatilisation,
      "N_vol_mms",
      "manure_volatilised_n",
      1000,
      status = "proxy_not_equivalent",
      species_rescale = TRUE
    )
    indirect_mm <- dplyr::bind_rows(
      manure$volatilisation |> dplyr::mutate(n2o_indirect = N2O_G_mm),
      manure$leaching |> dplyr::mutate(n2o_indirect = N2O_L_mm)
    ) |>
      dplyr::select(
        dplyr::all_of(ids),
        dplyr::any_of(c("FADN_code_letter", "species")),
        n2o_indirect
      )
    total(
      indirect_mm,
      "n2o_indirect",
      "manure_indirect_n2o",
      1e6,
      species_rescale = TRUE
    )
  } else {
    # Preserve the expected country-year rows and explicitly mark affected
    # manure metrics unavailable when the model cannot calculate them.
    failed <- catalogue |>
      dplyr::filter(
        component %in%
          c(
            "manure_total",
            "manure_direct_n2o",
            "manure_volatilised_n",
            "manure_indirect_n2o"
          )
      )
    results[[length(results) + 1L]] <- tidyr::crossing(
      farm |> dplyr::select(iso3, YEAR) |> dplyr::distinct(),
      failed |> dplyr::select(metric_id)
    ) |>
      dplyr::mutate(
        f2f_value = NA_real_,
        n_farms = NA_integer_,
        f2f_status = "model_error",
        f2f_error = manure$model_error
      )
  }
  total(
    dplyr::bind_rows(f_n_excr(object)),
    "Nex_kgN_livcat_y",
    "n_excreted",
    1000,
    TRUE
  )
  # The managed-soils output includes activity inputs and component emissions.
  soil <- f_GHGE_n2o_msoils(object)
  activity <- soil |>
    dplyr::mutate(
      inorganic = F_SN,
      organic = F_ON,
      grazing = F_PRP,
      residues = F_CR,
      direct = F_SN + F_ON + F_PRP + F_CR
    )
  indirect <- soil |>
    dplyr::mutate(
      deposition = F_SN * 0.11 + (F_ON + F_PRP) * 0.21,
      leaching = (F_SN + F_ON + F_PRP + F_CR) * 0.24,
      indirect = deposition + leaching
    )
  # Convert package CO2-equivalent component outputs back to N2O for CRT totals.
  direct <- soil |>
    dplyr::mutate(
      inorganic = N2O_d_kgCO2e_SN / GWP[["N20"]],
      organic = N2O_d_kgCO2e_ON / GWP[["N20"]],
      grazing = N2O_d_kgCO2e_PRP / GWP[["N20"]],
      residues = N2O_d_kgCO2e_CR / GWP[["N20"]],
      direct = N2O_d_kgCO2e / GWP[["N20"]]
    )
  for (component in c(
    "direct",
    "inorganic",
    "organic",
    "grazing",
    "residues"
  )) {
    total(activity, component, paste0("soil_activity_", component), 1000)
    total(direct, component, paste0("soil_n2o_", component), 1e6)
  }
  indirect <- indirect |>
    dplyr::mutate(
      n2o_deposition = N2O_ATD_kgCO2e / GWP[["N20"]],
      n2o_leaching = N2O_L_kgCO2e / GWP[["N20"]],
      n2o_indirect = n2o_deposition + n2o_leaching
    )
  for (component in c("indirect", "deposition", "leaching")) {
    total(
      indirect,
      component,
      paste0("soil_activity_", component),
      1000,
      status = if (component == "indirect") "proxy_not_equivalent" else "ok"
    )
    total(
      indirect,
      paste0("n2o_", component),
      paste0("soil_n2o_", component),
      1e6
    )
  }
  # Table 3 category 3.D: direct + atmospheric deposition + leaching N2O.
  soil_total <- soil |>
    dplyr::mutate(
      n2o_total = N2O_d_kgCO2e + N2O_ATD_kgCO2e + N2O_L_kgCO2e
    )
  total(soil_total, "n2o_total", "soil_total", 1e6)
  # Crop production and land area are derived from crop-level hectares and
  # yields, then summed to the CRT comparison groups.
  crops <- object@crop |>
    dplyr::mutate(production = area_ha * yield)
  for (crop in c("wheat", "barley", "maize")) {
    codes <- switch(
      crop,
      wheat = c("CWHTC", "CWHTD"),
      barley = "CBRL",
      maize = "CMZ"
    )
    total(
      crops |> dplyr::filter(FADN_code_letter %in% codes),
      "production",
      paste0("production_", crop)
    )
  }
  dplyr::bind_rows(results) |>
    dplyr::mutate(YEAR = as.character(YEAR)) |>
    dplyr::semi_join(catalogue, by = "metric_id")
}

h_unfccc_coverage_plot_data <- function(raw, processed, inventory, eurostat = NULL) {
  metrics <- c(
    "population_cattle", "population_dairy_cattle",
    "population_non_dairy_cattle", "population_swine",
    "population_sheep", "population_poultry",
    "area_cropland", "area_grassland"
  )
  estimates <- dplyr::bind_rows(
    dplyr::mutate(raw, estimate = "Raw FADN"),
    dplyr::mutate(processed, estimate = "FADN2Footprint")
  )
  if (!is.null(eurostat)) {
    required <- c("iso3", "YEAR", "livcat", "population_heads")
    if (!all(required %in% names(eurostat))) {
      stop("Eurostat population data lack required comparison columns.")
    }
    eurostat <- eurostat |>
      dplyr::mutate(
        iso3 = as.character(iso3),
        YEAR = as.character(YEAR),
        livcat = as.character(livcat)
      )
    if (
      !is.numeric(eurostat$population_heads) ||
        anyDuplicated(eurostat[required[1:3]]) ||
        any(!is.na(eurostat$population_heads) &
              !is.finite(eurostat$population_heads)) ||
        any(eurostat$population_heads < 0, na.rm = TRUE)
    ) {
      stop("Eurostat populations must be numeric, nonnegative, and unique by country, year, and category.")
    }
    eurostat_estimates <- eurostat |>
      dplyr::semi_join(
        estimates |> dplyr::distinct(iso3, YEAR),
        by = c("iso3", "YEAR")
      ) |>
      dplyr::transmute(
        iso3, YEAR,
        metric_id = paste0("population_", livcat),
        f2f_value = population_heads,
        f2f_status = "ok",
        estimate = "Eurostat"
      )
    estimates <- dplyr::bind_rows(estimates, eurostat_estimates)
  }
  values <- estimates |>
    dplyr::filter(
      metric_id %in% metrics,
      f2f_status == "ok",
      is.finite(f2f_value)
    ) |>
    dplyr::select(iso3, YEAR, metric_id, estimate, f2f_value) |>
    dplyr::left_join(
      inventory |>
        dplyr::select(iso3, YEAR, metric_id, crt_value, crt_status),
      by = c("iso3", "YEAR", "metric_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(crt_status == "ok", is.finite(crt_value), crt_value > 0) |>
    dplyr::mutate(coverage_percent = 100 * f2f_value / crt_value) |>
    dplyr::filter(is.finite(coverage_percent))

  dplyr::bind_rows(
    values |>
      dplyr::distinct(iso3, YEAR, metric_id) |>
      dplyr::mutate(estimate = "UNFCCC CRT", coverage_percent = 100),
    values |>
      dplyr::select(iso3, YEAR, metric_id, estimate, coverage_percent)
  ) |>
    dplyr::mutate(
      metric_id = dplyr::recode(
        metric_id,
        population_cattle = "Cattle",
        population_dairy_cattle = "Dairy cattle",
        population_non_dairy_cattle = "Non-dairy cattle",
        population_swine = "Swine",
        population_sheep = "Sheep",
        population_poultry = "Poultry",
        area_cropland = "Cropland",
        area_grassland = "Grassland"
      ),
      estimate = factor(
        estimate,
        levels = c("UNFCCC CRT", "Raw FADN", "FADN2Footprint", "Eurostat")
      ),
      country_year = factor(
        paste(iso3, YEAR, sep = " - "),
        levels = sort(unique(paste(iso3, YEAR, sep = " - ")))
      )
    )
}

#' Summarize FADN2Footprint agreement with UNFCCC reference values
#'
#' @param comparison Country-year comparisons with metric identifiers, statuses
#'   and F2F/CRT values in the same physical unit given by `unit`.
#' @param rescale Logical; use coverage-rescaled F2F values in the same physical
#'   units, except raw area/population coverage and unchanged per-head energy.
#' @return A tibble with one row per variable/category and estimate type across
#'   country-years. `MPE` and `MAPE` are percentages; `R_squared` is dimensionless.
#'   Pair counts and status columns identify exclusions and undefined metrics.
#' @details UNFCCC is the reference: MPE = 100 * mean((F2F - CRT) / CRT),
#'   MAPE = 100 * mean(abs(F2F - CRT) / CRT), and
#'   R_squared = 1 - sum((CRT - F2F)^2) / sum((CRT - mean(CRT))^2).
#'   Only finite pairs with both statuses `ok` are used, with equal weight per
#'   country-year. Zero CRT values are excluded only from percentage errors.
#'   R squared can be negative; it is undefined with fewer than two valid pairs
#'   or a constant reference. No variables or units are pooled.
#'   A two-sided one-sample t-test tests percentage errors against zero.
#'   `mpe_p_value`, `mpe_t_statistic` and `mpe_df` are dimensionless;
#'   `mpe_ci_lower` and `mpe_ci_upper` give a 95% confidence interval in percent.
#'   Tests with fewer than two errors, nonfinite errors or essentially constant
#'   errors are explicitly unavailable. P-values are unadjusted and assume
#'   independent observations; repeated country-years may violate this assumption.
h_unfccc_metrics <- function(comparison, rescale = FALSE) {
  # Keep each metric, CRT sheet, category and physical unit in its own group.
  groups <- c("metric_id", "sheet", "category_label", "unit")
  estimates <- c(raw = "f2f_value")
  # Reports use the selected coverage-adjusted estimate, while raw estimates
  # remain available for validation and diagnostics.
  if (rescale) {
    comparison <- h_unfccc_estimates(comparison)
    groups <- c(groups, "estimate", "comparison_basis")
    estimates <- c(selected = "comparison_value")
  }
  required <- c(
    groups,
    unname(estimates),
    "crt_value",
    "f2f_status",
    "crt_status"
  )
  if (!all(required %in% names(comparison))) {
    stop(
      "Comparison lacks required metric columns: ",
      paste(setdiff(required, names(comparison)), collapse = ", ")
    )
  }

  # Compute agreement statistics separately for every group and estimate.
  dplyr::bind_rows(lapply(names(estimates), function(estimate) {
    out <- comparison |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::group_modify(function(data, key) {
        prediction <- data[[estimates[[estimate]]]]
        reference <- data$crt_value
        valid <- !is.na(data$f2f_status) &
          data$f2f_status == "ok" &
          !is.na(data$crt_status) &
          data$crt_status == "ok" &
          is.finite(prediction) &
          is.finite(reference)
        # Exclude invalid pairs consistently from every statistic.
        prediction <- prediction[valid]
        reference <- reference[valid]
        # Percentage errors are undefined at a zero CRT reference, but those
        # pairs can still contribute to R-squared.
        nonzero <- reference != 0
        percentage_error <- 100 *
          (prediction[nonzero] - reference[nonzero]) /
          reference[nonzero]
        absolute_percentage_error <- 100 *
          abs(prediction[nonzero] - reference[nonzero]) /
          reference[nonzero]
        n_pairs <- length(reference)
        reference_variation <- sum((reference - mean(reference))^2)
        r_squared_status <- if (n_pairs < 2L) {
          "insufficient_pairs"
        } else if (reference_variation == 0) {
          "constant_reference"
        } else {
          "ok"
        }
        n_errors <- length(percentage_error)
        # Rescaling can match references up to rounding; do not test numerical noise.
        precision_match <- all(
          abs(prediction[nonzero] - reference[nonzero]) <=
            10 *
              .Machine$double.eps *
              pmax(abs(prediction[nonzero]), abs(reference[nonzero]))
        )
        # Also match t.test's constant-data guard without swallowing errors.
        standard_error <- if (n_errors >= 2L) {
          stats::sd(percentage_error) / sqrt(n_errors)
        } else {
          NA_real_
        }
        mpe_test_status <- if (n_errors < 2L) {
          "insufficient_pairs"
        } else if (
          any(!is.finite(percentage_error)) || !is.finite(standard_error)
        ) {
          "nonfinite_errors"
        } else if (
          precision_match ||
            standard_error == 0 ||
            standard_error <
              10 * .Machine$double.eps * abs(mean(percentage_error))
        ) {
          "constant_errors"
        } else {
          "ok"
        }
        mpe_test <- if (mpe_test_status == "ok") {
          stats::t.test(
            percentage_error,
            mu = 0,
            alternative = "two.sided",
            conf.level = 0.95
          )
        } else {
          NULL
        }

        # Return the statistics together with counts and statuses explaining
        # which observations were used or why a statistic is unavailable.
        tibble::tibble(
          estimate_type = if (rescale) key$estimate else estimate,
          n_pairs = n_pairs,
          n_excluded = nrow(data) - n_pairs,
          n_percentage_pairs = sum(nonzero),
          n_zero_reference = sum(!nonzero),
          MPE = if (any(nonzero)) mean(percentage_error) else NA_real_,
          MAPE = if (any(nonzero)) {
            mean(absolute_percentage_error)
          } else {
            NA_real_
          },
          R_squared = if (r_squared_status == "ok") {
            1 - sum((reference - prediction)^2) / reference_variation
          } else {
            NA_real_
          },
          mpe_p_value = if (!is.null(mpe_test)) mpe_test$p.value else NA_real_,
          mpe_t_statistic = if (!is.null(mpe_test)) {
            unname(mpe_test$statistic)
          } else {
            NA_real_
          },
          mpe_df = if (!is.null(mpe_test)) {
            unname(mpe_test$parameter)
          } else {
            NA_real_
          },
          mpe_ci_lower = if (!is.null(mpe_test)) {
            mpe_test$conf.int[1]
          } else {
            NA_real_
          },
          mpe_ci_upper = if (!is.null(mpe_test)) {
            mpe_test$conf.int[2]
          } else {
            NA_real_
          },
          mpe_test_status = mpe_test_status,
          percentage_status = if (any(nonzero)) {
            "ok"
          } else {
            "no_nonzero_reference"
          },
          r_squared_status = r_squared_status
        )
      }) |>
      dplyr::ungroup()
    if (rescale) {
      dplyr::select(out, -estimate_type)
    } else {
      dplyr::rename(out, estimate = estimate_type)
    }
  }))
}

#' Select the estimates used throughout the UNFCCC report
#' @param comparison Country-year F2F/CRT values in the physical units of `unit`.
#' @return Comparisons with `comparison_value` in the same units, estimate type
#'   and an explicit coverage-scaling description. Missing scales remain missing.
h_unfccc_estimates <- function(comparison) {
  # Population and area are coverage diagnostics, not totals to rescale.
  # Gross energy is already expressed per head; other totals use adjusted
  # estimates where the required coverage factor is available.
  comparison |>
    dplyr::mutate(
      estimate = dplyr::case_when(
        grepl("^(population_|area_)", metric_id) ~ "raw",
        grepl("^gross_energy_", metric_id) ~ "per_head",
        TRUE ~ "rescaled"
      ),
      comparison_value = ifelse(
        estimate == "rescaled",
        f2f_rescaled,
        f2f_value
      ),
      comparison_basis = dplyr::case_when(
        grepl(
          "^population_",
          metric_id
        ) ~ "Raw F2F livestock population (coverage comparison)",
        grepl("^area_", metric_id) ~ "Raw F2F area (coverage comparison)",
        estimate ==
          "per_head" ~ "Unchanged F2F gross energy per head; no coverage rescaling",
        grepl(
          "^manure_(volatilised_n|indirect_n2o)$",
          metric_id
        ) ~
          "F2F rescaled by species-specific CRT or Eurostat/F2F livestock-population ratios, then summed",
        grepl("^(soil_|production_)", metric_id) ~
          "F2F rescaled by CRT/F2F combined cropland-plus-grassland area ratio",
        TRUE ~ "F2F rescaled by matching CRT or Eurostat/F2F livestock-population ratio"
      )
    )
}

#' Read the ordered comparison sections from the selected CRT workbook sheet
#' @param path Path to the comparison workbook; no physical units.
#' @param catalogue Base metric catalogue with physical units in `unit`.
#' @return One row per selected workbook row, with section metadata and a
#'   list of base metric identifiers; values and units are not changed.
h_unfccc_selected <- function(path, catalogue = h_unfccc_catalogue()) {
  selected <- readxl::read_excel(
    path,
    sheet = "selected",
    range = readxl::cell_cols("A:I"),
    col_types = "text"
  )
  required <- c("use", "Type", "Comparison (metric_id)", "UNFCCC CRT table")
  if (!all(required %in% names(selected))) {
    stop(
      "Selected sheet lacks columns: ",
      paste(setdiff(required, names(selected)), collapse = ", ")
    )
  }
  comparison <- trimws(selected[["Comparison (metric_id)"]])
  if (
    !length(comparison) || anyNA(comparison) ||
      any(!grepl("^.+\\([^()]+\\)$", comparison))
  ) {
    stop("Each selected row must name a comparison and parenthesized metric identifiers.")
  }

  # The workbook names soil families, while the catalogue names their pathways.
  # These aliases select existing measures; they do not redefine calculations.
  aliases <- list(
    soil_activity_ = paste0(
      "soil_activity_", c("inorganic", "organic", "grazing", "residues")
    ),
    soil_n2o_direct_ = paste0(
      "soil_n2o_", c("direct", "inorganic", "organic", "grazing", "residues")
    ),
    soil_n2o_indirect_ = paste0(
      "soil_n2o_", c("indirect", "deposition", "leaching")
    )
  )
  specifications <- sub("^.*\\(([^()]+)\\)$", "\\1", comparison)
  metric_ids <- lapply(seq_along(specifications), function(i) {
    tokens <- trimws(strsplit(specifications[i], ",", fixed = TRUE)[[1]])
    unique(unlist(lapply(tokens, function(token) {
      alias <- aliases[[sub("\\*$", "", token)]]
      ids <- if (!is.null(alias)) {
        alias[alias %in% catalogue$metric_id]
      } else {
        # Include a family's unsuffixed total (e.g. manure_indirect_n2o)
        # when the workbook uses a trailing "_*" for that family.
        catalogue$metric_id[
          grepl(utils::glob2rx(token), catalogue$metric_id) |
            catalogue$metric_id == sub("_\\*$", "", token)
        ]
      }
      if (!length(ids)) {
        stop("Selected workbook row ", i + 1L, " has no mapped metrics: ", token)
      }
      ids
    }), use.names = FALSE))
  })
  tibble::tibble(
    workbook_row = seq_along(comparison) + 1L,
    title = trimws(sub("\\s*\\([^()]+\\)$", "", comparison)),
    use = selected[["use"]],
    type = selected[["Type"]],
    crt_table = selected[["UNFCCC CRT table"]],
    metric_ids = metric_ids
  )
}

#' Split a selected section into comparable plot pages
#' @param comparison Country-year comparisons in the physical units of `unit`.
#' @param metric_ids Base identifiers selected for this section; no units.
#' @return A list of comparison tables, each with one sheet, unit, estimate and
#'   comparison basis and at most 12 variables. Totals and intensities stay separate.
h_unfccc_section_pages <- function(comparison, metric_ids) {
  # Intensities retain the base identifier plus an explicit denominator suffix.
  base_ids <- sub("_(per_head|per_ha|per_t_meat)$", "", comparison$metric_id)
  values <- comparison[base_ids %in% metric_ids, , drop = FALSE]
  if (!nrow(values)) {
    stop("No comparison rows match the selected metrics.")
  }
  groups <- values |>
    dplyr::group_by(sheet, unit, estimate, comparison_basis) |>
    dplyr::group_split()
  # Keep totals before intensities within each workbook section.
  basis_order <- vapply(groups, function(data) {
    suffix <- sub("^.*(_per_head|_per_ha|_per_t_meat)$", "\\1", data$metric_id[1])
    match(suffix, c("_per_head", "_per_ha", "_per_t_meat"), nomatch = 0L)
  }, integer(1))
  groups <- groups[order(basis_order)]
  unlist(lapply(groups, function(data) {
    ids <- unique(data$metric_id)
    pages <- split(ids, ceiling(seq_along(ids) / 12L))
    lapply(pages, function(page) data[data$metric_id %in% page, , drop = FALSE])
  }), recursive = FALSE, use.names = FALSE)
}

#' Plot CRT versus the selected FADN2Footprint estimate
#' @param data Country-year comparison page in one physical `unit`, sheet and
#'   comparison basis; `comparison_value` and `crt_value` use that same unit.
#' @return A faceted ggplot scatterplot in the input units, coloured by country
#'   and shaped by year. Invalid pairs are omitted, without a raw-value fallback.
h_unfccc_scatter_plot <- function(data) {
  plot_data <- data |>
    dplyr::filter(
      f2f_status == "ok", crt_status == "ok",
      is.finite(crt_value), is.finite(comparison_value)
    ) |>
    dplyr::mutate(
      plot_value = comparison_value,
      YEAR = factor(YEAR, levels = sort(unique(as.character(data$YEAR))))
    )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = crt_value, y = plot_value, colour = iso3, shape = YEAR)
  ) +
    # Retain empty variable panels when no valid estimate is available.
    ggplot2::geom_blank(
      data = dplyr::distinct(data, metric_id),
      mapping = ggplot2::aes(x = NA_real_, y = NA_real_),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::facet_wrap(~metric_id, scales = "free", ncol = 3) +
    ggplot2::labs(
      title = paste(data$sheet[1], "-", data$unit[1], "comparison"),
      caption = paste(unique(data$comparison_basis), collapse = "\n"),
      x = paste("CRT", data$unit[1]),
      y = paste("FADN2Footprint", data$unit[1]),
      colour = "Country", shape = "Year"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot country-year relative errors for a comparison page
#' @param data Country-year comparison values in the physical units of `unit`.
#' @param metric_page Unique metric metadata for one sheet and estimate type,
#'   including `unit` and `comparison_basis`.
#' @return A faceted ggplot heatmap of relative differences in percent, with
#'   unavailable comparisons and zero CRT references shown in grey.
h_unfccc_country_year_plot <- function(data, metric_page) {
  estimate <- unique(metric_page$estimate)
  sheet_name <- unique(metric_page$sheet)
  if (length(estimate) != 1L || length(sheet_name) != 1L) {
    stop("Country-year heatmap needs one sheet and estimate type.")
  }
  plot_data <- data |>
    dplyr::filter(
      sheet == sheet_name,
      unit %in% metric_page$unit,
      metric_id %in% metric_page$metric_id
    ) |>
    dplyr::mutate(
      estimate_value = comparison_value,
      percentage_error = dplyr::if_else(
        f2f_status == "ok" & crt_status == "ok" &
          is.finite(estimate_value) & is.finite(crt_value) & crt_value != 0,
        100 * (estimate_value - crt_value) / crt_value,
        NA_real_
      ),
      YEAR = factor(YEAR, levels = sort(unique(as.character(YEAR)))),
      iso3 = factor(iso3, levels = sort(unique(as.character(iso3)))),
      metric_id = factor(metric_id, levels = metric_page$metric_id),
      tile_label = dplyr::if_else(
        is.finite(percentage_error), sprintf("%.1f%%", percentage_error), ""
      )
    ) |>
    # Show absent country-year pairs as grey tiles rather than white gaps.
    tidyr::complete(iso3, YEAR, metric_id, fill = list(tile_label = ""))
  ggplot2::ggplot(
    plot_data, ggplot2::aes(x = YEAR, y = iso3, fill = percentage_error)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = tile_label), size = 3) +
    ggplot2::facet_wrap(~metric_id, ncol = 3) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-100, 100), oob = scales::squish,
      na.value = "#DDDDDD", name = "Relative Difference (%)"
    ) +
    ggplot2::labs(
      title = paste(sheet_name, "-", unique(metric_page$unit),
                    estimate, "country-year relative difference"),
      caption = paste(unique(metric_page$comparison_basis), collapse = "\n"),
      x = "Year", y = "Country"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot agreement metrics using report acceptability thresholds
#'
#' @param metrics A nonempty subset of `h_unfccc_metrics()` for one sheet and
#'   estimate type. MPE/MAPE are percentages, R squared and p-values dimensionless.
#' @return A ggplot heatmap of metric values and MPE t-test p-values, with
#'   dimensionless threshold classes and valid country-year counts in row labels.
#' @details MAPE below 20% is good, 20--50% acceptable and above 50% poor.
#'   R squared above 0.5 is satisfactory, below zero worse than the reference mean.
#'   MPE colours use a two-sided t-test: p > 0.05 means no detected bias, not
#'   evidence of equivalence. Undefined tests and metrics are shown in grey.
h_unfccc_metric_plot <- function(metrics) {
  # A single sheet and estimate type keeps the plot's thresholds interpretable.
  required <- c(
    "metric_id",
    "sheet",
    "estimate",
    "MPE",
    "MAPE",
    "R_squared",
    "mpe_p_value",
    "n_pairs",
    "n_percentage_pairs"
  )
  if (!all(required %in% names(metrics))) {
    stop(
      "Metric plot lacks required columns: ",
      paste(setdiff(required, names(metrics)), collapse = ", ")
    )
  }
  if (
    !nrow(metrics) ||
      dplyr::n_distinct(metrics$sheet) != 1L ||
      dplyr::n_distinct(metrics$estimate) != 1L ||
      anyDuplicated(metrics$metric_id)
  ) {
    stop("Metric plot needs unique variables from one sheet and estimate type.")
  }
  basis <- if ("comparison_basis" %in% names(metrics)) {
    paste(unique(metrics$comparison_basis), collapse = "\n")
  } else {
    paste("F2F estimate basis:", metrics$estimate[1])
  }

  # Include the number of usable pairs beside each variable so metrics based
  # on different amounts of data are not mistaken for equally supported.
  rows <- paste0(
    gsub("_", " ", metrics$metric_id),
    "\npaired n=",
    metrics$n_pairs,
    "; nonzero n=",
    metrics$n_percentage_pairs
  )
  plot_data <- metrics |>
    dplyr::mutate(row_label = factor(rows, levels = rev(rows))) |>
    tidyr::pivot_longer(
      c(MAPE, MPE, R_squared, mpe_p_value),
      names_to = "statistic",
      values_to = "value"
    ) |>
    dplyr::left_join(
      metrics |> dplyr::select(metric_id, bias_p = mpe_p_value),
      by = "metric_id",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      # Colour each statistic using the report thresholds. The MPE column is
      # coloured by its bias-test result rather than by the signed MPE value.
      rating = dplyr::case_when(
        !is.finite(value) ~ "Unavailable",
        statistic == "MAPE" & value < 0 ~ "Unavailable",
        statistic == "MAPE" & value < 20 ~ "Meets criterion",
        statistic == "MAPE" & value <= 50 ~ "Intermediate",
        statistic == "MAPE" ~ "Outside criterion",
        statistic == "R_squared" & value > 0.5 ~ "Meets criterion",
        statistic == "R_squared" & value > 0 ~ "Intermediate",
        statistic == "R_squared" ~ "Outside criterion",
        !is.finite(bias_p) ~ "Unavailable",
        bias_p > 0.05 ~ "Meets criterion",
        TRUE ~ "Outside criterion"
      ),
      column = factor(
        statistic,
        levels = c("MAPE", "MPE", "R_squared", "mpe_p_value"),
        labels = c("MAPE (%)", "MPE (%)", "R squared", "MPE test p")
      ),
      label = ifelse(
        is.finite(value),
        formatC(value, digits = 3, format = "g"),
        "NA"
      )
    )
  # Display the metric values and their threshold class in a compact heatmap.
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = column, y = row_label, fill = rating)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.8, show.legend = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 4) +
    ggplot2::scale_fill_manual(
      values = c(
        "Meets criterion" = "#94C9AD",
        "Intermediate" = "#F4D58D",
        "Outside criterion" = "#ECA29A",
        "Unavailable" = "#DDDDDD"
      ),
      limits = c(
        "Meets criterion",
        "Intermediate",
        "Outside criterion",
        "Unavailable"
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = paste(metrics$sheet[1], "-", metrics$estimate[1], "agreement"),
      subtitle = "MPE colour uses its t-test; grey = undefined metric or test",
      x = NULL,
      y = NULL,
      fill = NULL,
      caption = paste(
        basis,
        "MAPE: <20 good, 20-50 acceptable, >50 poor. R squared: >0.5 satisfactory, <0 poor.\nMPE: p >0.05 no detected bias; p <=0.05 evidence of bias. See CSV for test statuses.",
        sep = "\n"
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text.y = ggplot2::element_text(size = 10)
    )
}

h_unfccc_compare <- function(
  object,
  crt,
  rescale = TRUE,
  eurostat_population = NULL
) {
  # Rescaling is required for the report's total-emission comparisons; fail
  # early rather than silently producing a different comparison basis.
  if (!isTRUE(rescale)) {
    stop("UNFCCC comparisons require coverage rescaling.")
  }
  catalogue <- h_unfccc_catalogue()
  f2f <- h_unfccc_fadn(object, catalogue)
  inventory <- h_unfccc_inventory(crt, catalogue)
  # Eurostat is used only when a CRT livestock population is unavailable.
  # Validate its keys and values before joining it to the comparison data.
  if (!is.null(eurostat_population)) {
    required_population <- c("iso3", "YEAR", "livcat", "population_heads")
    if (!all(required_population %in% names(eurostat_population))) {
      stop("Eurostat population data lack required comparison columns.")
    }
    if (
      !is.numeric(eurostat_population$population_heads) ||
        anyDuplicated(eurostat_population[required_population[1:3]]) ||
        any(
          !is.na(eurostat_population$population_heads) &
            !is.finite(eurostat_population$population_heads)
        ) ||
        any(eurostat_population$population_heads < 0, na.rm = TRUE)
    ) {
      stop(
        "Eurostat populations must be numeric, nonnegative, and unique by country, year, and category."
      )
    }
    eurostat_population <- eurostat_population |>
      dplyr::transmute(
        iso3 = as.character(iso3),
        YEAR = as.character(YEAR),
        livcat = as.character(livcat),
        eurostat_population = population_heads
      )
  } else {
    eurostat_population <- tibble::tibble(
      iso3 = character(),
      YEAR = character(),
      livcat = character(),
      eurostat_population = numeric()
    )
  }
  years <- object@farm |>
    dplyr::transmute(iso3 = Country_ISO_3166_1_A3, YEAR = as.character(YEAR)) |>
    dplyr::distinct()
  # Build a complete country-year by metric grid so missing values remain
  # visible in the output instead of disappearing during joins.
  comparison <- tidyr::crossing(years, catalogue) |>
    dplyr::left_join(
      f2f,
      by = c("iso3", "YEAR", "metric_id"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      inventory,
      by = c("iso3", "YEAR", "metric_id"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      f2f_status = dplyr::coalesce(f2f_status, "unavailable"),
      crt_status = dplyr::coalesce(crt_status, "missing_crt")
    )
  # Derive a population coverage ratio for each country-year and animal
  # category, preferring CRT and falling back to Eurostat.
  population <- comparison |>
    dplyr::filter(component == "population") |>
    dplyr::transmute(
      iso3,
      YEAR,
      livcat = sub("^population_", "", metric_id),
      f2f_value,
      crt_population = ifelse(crt_status == "ok", crt_value, NA_real_)
    ) |>
    dplyr::left_join(
      eurostat_population,
      by = c("iso3", "YEAR", "livcat"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      population_reference_value = dplyr::coalesce(
        crt_population,
        eurostat_population
      ),
      population_reference_source = dplyr::case_when(
        is.finite(crt_population) ~ "UNFCCC CRT",
        is.finite(eurostat_population) ~ "Eurostat",
        TRUE ~ NA_character_
      ),
      scale = h_unfccc_ratio(population_reference_value, f2f_value)
    ) |>
    dplyr::select(
      iso3,
      YEAR,
      livcat,
      scale,
      population_reference_value,
      population_reference_source
    )
  # Soil and crop totals use combined cropland-plus-grassland coverage.
  areas <- comparison |>
    dplyr::filter(family == "area") |>
    dplyr::summarise(
      scale = h_unfccc_ratio(
        h_unfccc_sum(ifelse(
          crt_status == "ok" & f2f_status == "ok",
          crt_value,
          NA_real_
        )),
        h_unfccc_sum(f2f_value)
      ),
      .by = c(iso3, YEAR)
    )
  # Volatilisation and indirect manure N2O are first rescaled by species, then
  # summed. This avoids applying one category's coverage to another species.
  if (!"f2f_species_values" %in% names(comparison)) {
    comparison$f2f_species_values <- rep(list(NULL), nrow(comparison))
  }
  comparison$manure_rescaled <- vapply(
    seq_len(nrow(comparison)),
    function(i) {
      values <- comparison$f2f_species_values[[i]]
      if (!length(values)) {
        return(NA_real_)
      }
      values <- values[!is.finite(values) | values != 0]
      if (!length(values)) {
        return(0)
      }
      scales <- population |>
        dplyr::filter(iso3 == comparison$iso3[i], YEAR == comparison$YEAR[i])
      species_scale <- scales$scale[match(names(values), scales$livcat)]
      # A missing ratio for any contributing species invalidates the whole total.
      h_unfccc_sum(values * species_scale)
    },
    numeric(1)
  )
  comparison <- comparison |>
    dplyr::mutate(livcat = tolower(gsub("[- ]", "_", category_label))) |>
    dplyr::left_join(
      population,
      by = c("iso3", "YEAR", "livcat"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      areas,
      by = c("iso3", "YEAR"),
      suffix = c("_population", "_area")
    ) |>
    dplyr::mutate(
      # Choose the matching coverage factor for each metric family. Per-head
      # gross energy is unchanged, and manure totals use species-level scaling.
      scaling_factor = dplyr::case_when(
        family == "livestock" & component != "gross_energy" ~ scale_population,
        family %in% c("soil", "crop") ~ scale_area,
        component == "gross_energy" ~ 1,
        TRUE ~ NA_real_
      ),
      f2f_rescaled = ifelse(
        family == "manure",
        manure_rescaled,
        f2f_value * scaling_factor
      ),
      scaling_factor = ifelse(
        family == "manure",
        h_unfccc_ratio(f2f_rescaled, f2f_value),
        scaling_factor
      ),
      rescaled_relative_difference = ifelse(
        f2f_status == "ok" & crt_status == "ok",
        h_unfccc_ratio(f2f_rescaled - crt_value, crt_value),
        NA_real_
      )
    ) |>
    h_unfccc_estimates() |>
    dplyr::mutate(
      # Keep missing data and missing scale factors explicit in the final
      # status, and calculate differences only for usable comparisons.
      comparison_status = dplyr::case_when(
        f2f_status != "ok" ~ f2f_status,
        crt_status != "ok" ~ crt_status,
        !is.finite(comparison_value) ~ "missing_rescaling_factor_or_value",
        TRUE ~ "ok"
      ),
      difference = ifelse(
        comparison_status == "ok",
        comparison_value - crt_value,
        NA_real_
      ),
      relative_difference = h_unfccc_ratio(difference, crt_value),
      ratio = h_unfccc_ratio(
        ifelse(comparison_status == "ok", comparison_value, NA_real_),
        crt_value
      )
    ) |>
    dplyr::select(
      -livcat,
      -scale_population,
      -scale_area,
      -manure_rescaled,
      -f2f_species_values
    )
  # Return the source mapping, detailed country-year results and summary
  # statistics used by the report.
  list(
    catalogue = catalogue,
    comparison = comparison,
    metrics = h_unfccc_metrics(comparison, rescale = rescale)
  )
}
