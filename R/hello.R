library(ggplot2)
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

  # plot linear regression plot using ggplot2
  ggplot(data, aes(x, y)) +
    geom_point() +
    geom_smooth(method = 'lm')
}
