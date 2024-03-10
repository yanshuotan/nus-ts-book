require(rlang)
require(magrittr)

gg_custom_season <- function(data, y, period, start = 1) {
  # Make a seasonal plot with period specified in integer
  # start argument specifies the row number that will be the first season
  # in the period
  y <- enquo(y)
  data |>
    mutate(Season = (row_number() - start) %% period + start,
           Iteration = as.factor((row_number() - start) %/% period + 1)) |>
    ggplot(aes(x = Season, y = !!y, color = Iteration)) +
    geom_line()
}

sim_ar <- function(ar_model) {
  n <- ar_model |> augment() |> nrow()
  alpha <- ar_model |> tidy() |> pluck("estimate", 1)
  ar_coefs <- ar_model |> tidy() |> pluck("estimate") |> extract(-1)
  p <- length(ar_coefs)
  mu <- alpha / (1 - sum(ar_coefs))
  sigma2 <- ar_model |> pluck(1, 1, "fit", "sigma2")
  xt <- arima.sim(n = n, model = list(order = c(p, 0, 0), ar = ar_coefs), 
                  sd = sqrt(sigma2)) + mu
  xt
}