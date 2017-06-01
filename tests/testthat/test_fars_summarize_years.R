
test_that("fars_summarize_years works as expected", {
  expect_is(fars_summarize_years(2015), "tbl_df")
  expect_equal(ncol(fars_summarize_years(2015)), 2) # number of columns should be 2 - a month variable and number of accidents
  expect_equal(nrow(fars_summarize_years(2015)), 12)
  expect_equal(colnames(fars_summarize_years(2015))[2], "2015")
  })
