# Run this notebook in the root folder for the book

library(astsa)
library(tidyverse)
library(fpp3)

OUTPUT_DIR <- "images/c1_plots/"
DATA_DIR <- "datasets/"

goog <- read_csv(str_c(DATA_DIR, "GOOG.csv"))

plt <- goog |> 
  as_tsibble() |>
  autoplot(Close) + 
  ylab("USD")

ggsave(str_c(OUTPUT_DIR, "goog_price.png"), plt)

goog |> as_tsibble() |>
  autoplot(Volume)

sgcpi <- read_csv(str_c(DATA_DIR, "sg-cpi.csv"), skip = 10, n_max = 152)
col_names <- sgcpi$`Data Series`
time_index <- colnames(sgcpi)[-1]
sgcpi <- sgcpi |> 
  select(-`Data Series`) |>
  t() |>
  as_tibble(cols())
colnames(sgcpi) <- col_names
selected_cols <- c("All Items", "Food", "Clothing & Footwear", "Health Care", "Transport", "Education")
sgcpi <- sgcpi |> 
  select(all_of(selected_cols)) |>
  mutate_all(as.double) |>
  mutate(Month = yearmonth(time_index))
sgcpi <- sgcpi |> 
  as_tsibble() |>
  pivot_longer(cols = all_of(selected_cols), names_to = c("Type"))

sgcpi |> autoplot()


