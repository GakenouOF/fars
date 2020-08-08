context("Just testing some Fars functionality")

test_that("Whether the make_filename creates file name", {
  make_filename(2014)

  expect_equal(make_filename(2014), "accident_2014.csv")
})


