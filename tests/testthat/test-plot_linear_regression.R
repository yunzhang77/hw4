test_that("plot_linear_regression works", {

  # check if plot_linear_regression produces correct output
  pkg_result <- linear_regression("age", "glucose.level", data_glucose_simple)
  figure <- plot_linear_regression(pkg_result, data_glucose_simple)
  expect_equal(class(figure), c("gg", "ggplot"))
})
