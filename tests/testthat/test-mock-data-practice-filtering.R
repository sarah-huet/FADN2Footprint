test_that("about 90% of EU mock farms pass infer_practices", {
  data(mock_data)
  mock_farms <- mock_data$fadn_fict

  object <- data_4FADN2Footprint(mock_farms)
  inferred <- infer_practices(object, overwrite = TRUE)
  pass_rate <- nrow(inferred@farm) / nrow(mock_farms)

  expect_gt(nrow(mock_farms), 0L)
  expect_gt(nrow(inferred@farm), 0L)
  expect_lt(nrow(inferred@farm), nrow(mock_farms))
  expect_equal(pass_rate, 0.9, tolerance = 0.01)
})
