test_that("S3 methods works", {

  # initialize result
  pkg_result <- linear_regression("age", "glucose.level", data_glucose_simple)

  # test S3 method - summary.lm_result
  expect_equal(length(summary(pkg_result)), 6)
  expect_equal(class(summary(pkg_result)), "summary.lm_result")

  # test S3 methods - print() related
  expect_snapshot_output(print.lm_result(pkg_result))
  expect_snapshot_output(print.summary.lm_result(summary(pkg_result)))
})
