# Decode only reported observations instead of expanding every possible
# dimension combination, many of which are empty in the meat table.
parse_jsonstat <- function(js) {
  ids <- unlist(js$id, use.names = FALSE)
  sizes <- as.integer(unlist(js$size, use.names = FALSE))
  stopifnot(length(ids) == length(sizes), !is.null(names(js$value)))

  categories <- lapply(ids, function(dimension) {
    index <- unlist(
      js$dimension[[dimension]]$category$index,
      use.names = TRUE
    )
    names(index)[order(as.integer(index))]
  })
  names(categories) <- ids

  value_keys <- names(js$value)
  offsets <- as.integer(value_keys)
  coordinates <- setNames(vector("list", length(ids)), ids)
  # JSON-stat uses row-major order: the last dimension changes fastest.
  # https://json-stat.org/full/#value
  for (i in rev(seq_along(ids))) {
    positions <- offsets %% sizes[i] + 1L
    coordinates[[i]] <- categories[[i]][positions]
    offsets <- offsets %/% sizes[i]
  }

  result <- tibble::as_tibble(
    as.data.frame(coordinates, stringsAsFactors = FALSE)
  )
  result$value <- unname(vapply(js$value, as.numeric, numeric(1)))
  result$flag <- NA_character_
  if (length(js$status)) {
    status_keys <- names(js$status)
    status_rows <- match(as.integer(status_keys), as.integer(value_keys))
    present <- !is.na(status_rows)
    result$flag[status_rows[present]] <-
      vapply(js$status, as.character, character(1))[present]
  }
  result
}

build_eurostat_livestock_pop <- function(bovine, pigs, sheep, country_names) {
  required_dimensions <- c("freq", "month", "animals", "unit", "geo", "time", "value")
  for (table in list(bovine = bovine, pigs = pigs, sheep = sheep)) {
    if (!all(required_dimensions %in% names(table))) {
      stop("Eurostat population table lacks required dimensions.")
    }
  }
  if (!all(c(
    "country_eu", "Country_ISO_3166_1_A3"
  ) %in% names(country_names))) {
    stop("Country mapping lacks Eurostat and ISO3 country codes.")
  }

  country_map <- country_names |>
    dplyr::transmute(
      geo = as.character(country_eu),
      iso3 = as.character(Country_ISO_3166_1_A3)
    ) |>
    dplyr::filter(!is.na(geo), !is.na(iso3)) |>
    dplyr::distinct()
  if (anyDuplicated(country_map$geo)) {
    stop("Eurostat country codes must map to one ISO3 code.")
  }

  summarize_population <- function(data, animal_codes) {
    data |>
      dplyr::filter(
        freq == "A",
        unit == "THS_HD",
        animals %in% animal_codes,
        is.finite(value)
      ) |>
      dplyr::left_join(country_map, by = "geo", relationship = "many-to-one") |>
      dplyr::filter(!is.na(iso3)) |>
      dplyr::mutate(
        YEAR = as.character(time),
        heads = value * 1000
      ) |>
      dplyr::summarise(
        population_heads = mean(heads),
        n_periods = dplyr::n_distinct(month),
        .by = c(iso3, YEAR, animals)
      )
  }

  bovine_population <- summarize_population(bovine, c("A2000", "A2300F"))
  cattle_total <- bovine_population |>
    dplyr::filter(animals == "A2000") |>
    dplyr::transmute(
      iso3, YEAR,
      cattle_heads = population_heads,
      cattle_periods = n_periods
    )
  dairy_cattle <- bovine_population |>
    dplyr::filter(animals == "A2300F") |>
    dplyr::transmute(
      iso3, YEAR,
      dairy_heads = population_heads,
      dairy_periods = n_periods
    )
  cattle <- dplyr::full_join(cattle_total, dairy_cattle, by = c("iso3", "YEAR")) |>
    dplyr::transmute(
      iso3,
      YEAR,
      cattle = cattle_heads,
      dairy_cattle = dairy_heads,
      non_dairy_cattle = dplyr::if_else(
        is.finite(cattle_heads) & is.finite(dairy_heads) &
          cattle_heads >= dairy_heads,
        cattle_heads - dairy_heads,
        NA_real_
      ),
      cattle_periods,
      dairy_periods,
      non_dairy_periods = dplyr::if_else(
        is.finite(cattle_periods) & is.finite(dairy_periods),
        pmin(cattle_periods, dairy_periods),
        NA_integer_
      )
    ) |>
    tidyr::pivot_longer(
      cols = c(cattle, dairy_cattle, non_dairy_cattle),
      names_to = "livcat",
      values_to = "population_heads"
    ) |>
    dplyr::mutate(
      n_periods = dplyr::case_when(
        livcat == "cattle" ~ cattle_periods,
        livcat == "dairy_cattle" ~ dairy_periods,
        TRUE ~ non_dairy_periods
      )
    ) |>
    dplyr::select(iso3, YEAR, livcat, population_heads, n_periods)

  other_livestock <- dplyr::bind_rows(
    summarize_population(pigs, "A3100") |>
      dplyr::filter(animals == "A3100") |>
      dplyr::transmute(
        iso3, YEAR, livcat = "swine",
        population_heads, n_periods
      ),
    summarize_population(sheep, c("A4100", "A4200")) |>
      dplyr::transmute(
        iso3, YEAR,
        livcat = dplyr::recode(
          animals,
          A4100 = "sheep",
          A4200 = "goat"
        ),
        population_heads, n_periods
      )
  )

  result <- dplyr::bind_rows(cattle, other_livestock) |>
    dplyr::arrange(iso3, YEAR, livcat)
  if (anyDuplicated(result[c("iso3", "YEAR", "livcat")])) {
    stop("Eurostat population output is not unique by country, year, and category.")
  }
  result
}
