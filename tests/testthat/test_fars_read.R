
test_that("fars_read works as expected", {
  expect_is(fars_read(make_filename(2015)), "tbl_df")
})
