#' Calculate simple linear regression
#'
#' This function takes three parameters and fits simple linear regression model.
#' The function asks for independent variable name (string), dependent variable name (string), and a data frame
#' (data.frame) that contains both independent and dependent variables. If either variable name is not found in
#' the data frame, this function will return an error message. Otherwise, it will compute slope and intercept,
#' calculate standard error, R-squared, and adjusted R-squared values, and perform t-test and f-test on the
#' independent variable. This function will return a lm_result object containing all results. This function and
#' a series of subsequent functions rely on S3 method to print user-friendly results.
#'
#' @usage linear_regression(x_name, y_name, data)
#'
#' @param x_name independent variable name
#' @param y_name dependent variable name
#' @param data data.frame that contains both variables
#'
#' @examples linear_regression("age", "glucose.level", data)
#' @examples results <- linear_regression("age", "glucose.level", data)
#'
#' @export
#'
linear_regression <- function(x, ...) {UseMethod("linear_regression", x)}

#' @export
linear_regression.default <- function(x_name, y_name, data){
  # acquire result by calling fit_linear_regression and change it to new class object
  result <- fit_linear_regression(x_name, y_name, data)
  result$call <- match.call()
  class(result) <- 'lm_result'
  result
}

#' Print lm_result object
#'
#' This function uses S3 method to print user-friendly result for linear regression.
#' This function will return command used to call linear_regression, intercept, and slope
#' for the simple linear regression.
#'
#' @usage print(x)
#' @usage print.lm_result(x)
#'
#' @param x lm_result object
#' @param ... ignored
#'
#' @export
#'
print.lm_result <- function(x, ...){
  # modify print method - print command used and coefficients to the console
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  cat("(Intercept)\t", x$name, "\n")
  cat(x$intercept, "\t", x$slope)
}

#' Generate correct summary for lm_result object
#'
#' This function uses S3 method to generate correct summary for lm_object.
#'
#' @usage summary(x)
#' @usage summary.lm_result(x)
#'
#' @param x lm_result object
#' @param ... ignored
#'
#' @export
#'
summary.lm_result <- function(object, ...){
  # modify summary method to contain statistical analysis result
  coefficient <- cbind(Estimate = object$slope, StdErr = object$std_error,
                       T.statistic = object$t_stat, P.value = object$pval)
  significance <- list(name = object$name, coefficient = coefficient, R2 = object$R2,
                       R2_adj = object$R2_adj, f_stat = object$f_stat, f_test_pval = object$f_test_pval)
  class(significance) <- "summary.lm_result"
  significance
}

#' Print summary for lm_result object
#'
#' This function uses S3 method to print summary result of lm_result object. This functions returns
#' user-friends result contaning test statistics for the linear regression model, including standard
#' error, t-statistics, P-value, R-squared, adjusted R-squared, F-statistics, and corresponding P-value.
#'
#' @usage summary(x)
#'
#' @param x summary.lm_result object
#' @param ... ignored
#'
#' @import stats
#'
#' @export
#'
print.summary.lm_result <- function(x, ...){
  # modify print summary method to print statistical analysis result
  row.names(x$coefficient) <- x$name
  printCoefmat(x$coefficient, P.values = TRUE, has.Pvalue = TRUE, signif.stars = TRUE, signif.legend = T)
  cat("\nR-squared:", x$R2, "\tAdjusted R-squared:", x$R2_adj)
  cat("\nF-statistic:", x$f_stat, "\tp-value:", x$f_test_pval, "\n")
}

#' Fit simple linear regression model
#'
#' This function takes three parameters and fits simple linear regression model.
#' The function asks for independent variable name (string), dependent variable name (string), and a data frame
#' (data.frame) that contains both independent and dependent variables. If either variable name is not found in
#' the data frame, this function will return an error message. Otherwise, it will compute slope and intercept,
#' calculate standard error, R-squared, and adjusted R-squared values, and perform t-test and f-test on the
#' independent variable. This function will return a list containing all results.
#'
#' @note Please note that this function should not be called by user. It is an internal function used for fitting. User should call linear_regression() instead.
#'
#' @usage fit_linear_regression(x_name, y_name, data)
#'
#' @param x_name independent variable name
#' @param y_name dependent variable name
#' @param data data.frame that contains both variables
#'
#' @examples fit_linear_regression("age", "glucose.level", data)
#'
#' @export
#'
fit_linear_regression <- function(x_name, y_name, data){
  # terminate the function if variable name(s) is not found
  if (!(x_name %in% colnames(data))){
    stop("Variable x not in the data.")
  } else if (!(y_name %in% colnames(data))){
    stop("Variable y not in the data.")
  }

  # change variable names to x and y
  names(data)[names(data) == x_name] <- "x"
  names(data)[names(data) == y_name] <- "y"

  # calculate components required to calculate intercept and slope
  denominator <- nrow(data) * sum(data$x^2) - sum(data$x)^2
  y_sum <- sum(data$y)
  x_sum <- sum(data$x)
  xy_prod_sum <- sum(data$x * data$y)
  x_squared_sum <- sum(data$x^2)
  df <- nrow(data) - 2

  # calculate intercept and slope
  intercept <- ((y_sum * x_squared_sum) - (x_sum * xy_prod_sum)) / denominator
  slope <- ((nrow(data) * xy_prod_sum) - (x_sum * y_sum)) / denominator

  # calculate standard error, t statistics, and p-value for t-test
  y_hat <- data$x * slope + intercept
  std_error <- sqrt(sum((data$y - y_hat)^2) / df) / sqrt(sum((data$x - mean(data$x))^2))
  t_statistics <- slope / std_error
  p_value <- 2 * pt(-abs(t_statistics), df)

  # calculate R2 value and adjusted R2 value
  residual <- data$y - y_hat
  SSE <- sum(residual^2)
  SST <- sum((data$y - mean(data$y))^2)
  R_squared <- 1 - (SSE / SST)
  R_adjusted <- 1 - ((nrow(data) - 1) / df) * (1 - R_squared)

  # calculate f statistics and p-value for f-test
  SSR <- sum((y_hat - mean(data$y))^2)
  f_statistics <- SSR / (SSE / df)
  f_test_p_value <- pf(f_statistics, 1, df, lower.tail = F)

  # create objects holding all results
  results <- list(name = x_name, y_name = y_name, intercept = intercept, slope = slope, t_stat = t_statistics,
                  pval = p_value, R2 = R_squared, R2_adj = R_adjusted, f_stat = f_statistics,
                  f_test_pval = f_test_p_value, std_err = std_error)
  results
}

#' Plot linear regression line
#'
#' This function uses result acquired by calling linear_regression and original data
#' to plot both original data points and linear regression line on the same plot.
#' Original data points will be labeled as black dots, whereas regression line will
#' be red.
#'
#' @usage plot_linear_regression(result, data)
#'
#' @param result result from linear_regression
#' @param data original data frame used in linear_regression
#'
#' @examples plot_linear_regression(results, data)
#'
#' @import ggplot2
#'
#' @export
#'
plot_linear_regression <- function(result, data){
  # change variable names to x and y
  names(data)[names(data) == result$name] <- "x"
  names(data)[names(data) == result$y_name] <- "y"

  # generate regression line for plotting
  regression_points <- data.frame(cbind(data$x, result$slope * data$x + result$intercept))

  # plot linear regression line and original data points using ggplot2
  ggplot(data = regression_points, aes(X1, X2)) +
    geom_line(color = "red", size = 1.5) +
    geom_point(data = data, aes(x, y)) +
    xlab(result$name) +
    ylab(result$y_name) +
    ggtitle("Linear Regression of Data")
}




