
test_that("make_filename works as expected", {
  expect_is(make_filename(2015), "character")
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")
})
