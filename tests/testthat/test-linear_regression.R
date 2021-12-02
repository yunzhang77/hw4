test_that("linear_regression work", {

  # check if error message is generated correctly
  expect_error(linear_regression("error", "glucose.level", data_glucose_simple), "Variable x not in the data.")
  expect_error(linear_regression("age", "error", data_glucose_simple), "Variable y not in the data.")

  # check if linear_regression produces correct answer
  lm_result <- lm(formula = glucose.level ~ age, data = data_glucose_simple)
  pkg_result <- linear_regression("age", "glucose.level", data_glucose_simple)
  expect_equal(as.vector(lm_result$coefficients), c(pkg_result$intercept, pkg_result$slope))
  expect_equal(summary(lm_result)$r.squared, pkg_result$R2)
  expect_equal(summary(lm_result)$adj.r.squared, pkg_result$R2_adj)
  expect_equal(summary(lm_result)$coefficients[2,2], pkg_result$std_err)
  expect_equal(summary(lm_result)$coefficients[2,4], pkg_result$pval)
  expect_equal(as.vector(summary(lm_result)$fstatistic[1]), pkg_result$f_stat)
})
