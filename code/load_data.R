process_sgcpi <- function() {
  # Return cleaned dataset for SG CPI data.
  sgcpi <- read_csv(str_c("datasets/", "sg-cpi.csv"), skip = 10, n_max = 152)
  col_names <- sgcpi$`Data Series`
  time_index <- colnames(sgcpi)[-1]
  sgcpi <- sgcpi |> 
    select(-`Data Series`) |>
    t() |>
    as_tibble(cols())
  colnames(sgcpi) <- col_names
  selected_cols <- c("All Items", "Food", "Clothing & Footwear", "Health Care", 
  "Transport", "Education")
  sgcpi <- sgcpi |> 
    select(all_of(selected_cols)) |>
    mutate_all(as.double) |>
    mutate(Month = yearmonth(time_index))
  sgcpi <- sgcpi |> 
    as_tsibble() |>
    pivot_longer(cols = all_of(selected_cols), names_to = c("Type"))
  sgcpi
}