test_that("dictionary template expands pipe-separated crop and livestock codes", {
  output_path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(output_path), add = TRUE)

  data_extra_fixture <- list(
    crops = data.frame(
      FADN_code_letter = "CROP",
      FADN_code_description = "Test crop",
      RICA_code_number = "101|102"
    ),
    livestock = data.frame(
      FADN_code_letter = "LIVESTOCK",
      FADN_code_description = "Test livestock",
      RICA_code_number = "201|202"
    ),
    output = data.frame(
      FADN_code_letter = "OUTPUT",
      FADN_code_description = "Test output",
      RICA_code_number = "301"
    )
  )
  raw_tables <- list(
    crops = data.frame(ID = "farm", CODE = "101", A = 1),
    livestock = data.frame(ID = "farm", CODE = "201", A = 1),
    output = data.frame(ID = "farm", CODE = "301", A = 1)
  )
  table_config <- list(
    CROPS = list(table = "crops", code_col = "CODE"),
    LIVESTOCK = list(table = "livestock", code_col = "CODE"),
    `ANIMAL PRODUCTS AND SERVICES` = list(table = "output", code_col = "CODE")
  )

  h_create_dictionary_template(
    output_path = output_path,
    raw_tables = raw_tables,
    table_config = table_config,
    data_extra = data_extra_fixture
  )
  category_pattern <- openxlsx::read.xlsx(
    output_path,
    sheet = "category_pattern"
  )

  expect_setequal(
    as.character(category_pattern$national_code[
      category_pattern$Group == "CROPS" &
        category_pattern$FADN_code_letter == "CROP"
    ]),
    c("101", "102")
  )
  expect_setequal(
    as.character(category_pattern$national_code[
      category_pattern$Group == "LIVESTOCK" &
        category_pattern$FADN_code_letter == "LIVESTOCK"
    ]),
    c("201", "202")
  )
})

test_that("crop and livestock CSVs keep list values in their mapped columns", {
  data_dir <- system.file("ext_data", package = "FADN2Footprint")
  expect_true(nzchar(data_dir))

  crops <- read.csv(
    file.path(data_dir, "data_extra_crops.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  livestock <- read.csv(
    file.path(data_dir, "data_extra_livestock.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_equal(ncol(crops), 20L)
  expect_equal(ncol(livestock), 17L)
  crop_list_columns <- c(
    "RICA_code_number", "product_name", "TFI_crop_name",
    "EPK_ferti_crop_name", "EUROSTAT_feedstuff", "feed_tables",
    "IPCC_default_values"
  )
  livestock_list_columns <- c(
    "RICA_code_number", "LIBELLE", "IPCC_main_cat", "IPCC_sub_cat",
    "IPCC_mix_cat", "AROPAJ", "rearing_stage", "farm_workshop"
  )
  expect_false(any(grepl(
    ";",
    unlist(crops[crop_list_columns], use.names = FALSE),
    fixed = TRUE
  ), na.rm = TRUE))
  expect_false(any(grepl(
    ";",
    unlist(livestock[livestock_list_columns], use.names = FALSE),
    fixed = TRUE
  ), na.rm = TRUE))
  expect_true(grepl(
    "|",
    crops$TFI_crop_name[crops$FADN_code_letter == "CVEGOTH"],
    fixed = TRUE
  ))
  expect_equal(
    crops$feed_tables[crops$FADN_code_letter == "CRAPE"],
    "Rapeseed,_whole"
  )
  expect_true(grepl(
    "|",
    livestock$RICA_code_number[livestock$FADN_code_letter == "LBOV1"],
    fixed = TRUE
  ))
})
