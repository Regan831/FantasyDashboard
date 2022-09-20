create_best_fit <- function(data = contracts) {
  y <- data$Fantasy.Points
  X <- data$salary

  lm_model <- linear_reg() %>%
    set_engine('lm') %>%
    set_mode('regression') %>%
    fit(Fantasy.Points ~ salary, data = data)

  x_range <- seq(min(X), max(X), length.out = 100)
  x_range <- matrix(x_range, nrow=100, ncol=1)
  xdf <- data.frame(x_range)
  colnames(xdf) <- c('salary')

  ydf <- lm_model %>% predict(xdf)
  colnames(ydf) <- c('Fantasy.Points')
  xy <- data.frame(xdf, ydf)
  return(xy)
}

create_position_regression <- function(pos) {
  lm_model <- linear_reg() %>%
    set_engine('lm') %>%
    set_mode('regression') %>%
    fit(Fantasy.Points ~ salary, data = contracts %>% filter(Pos == pos))

  return(lm_model)
}

get_position_regression <- function(pos) {
  models[[pos]]
}

determine_contract_value <- function(x) {
  xdf <- data.frame(salary=c(as.numeric(x[["salary"]])))
  exp_points <- get_position_regression(x[["Pos"]]) %>%
    predict(xdf)

  percent_diff <- round((as.numeric(x[["Fantasy.Points"]]) - exp_points[[1]]) /as.numeric(x[["Fantasy.Points"]]), 3)
  return(percent_diff)
}
