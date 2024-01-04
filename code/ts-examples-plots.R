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

beijing_pm25 <- read_csv(str_c(DATA_DIR, "beijing-pm25.csv"))
beijing_pm25$Date <- as.POSIXct(paste(beijing_pm25$year, beijing_pm25$month, 
                                      beijing_pm25$day, beijing_pm25$hour, sep = " "), 
                                format = "%Y %m %d %H")
beijing_pm25 |> 
  as_tsibble() |>
  filter(year == 2011) |>
  autoplot(`pm2.5`) + labs(y = expression("Scatter Plot with " * LaTeX("Labels: \\alpha, \\beta, \\gamma")))

globtemp |> 
  as_tsibble() |> 
  autoplot() +
  labs(x = "Year", y = "Global Temperature Deviation (in Celsius)") 

heartbeat <- read_table(str_c(DATA_DIR, "ECG5000/", "ECG5000_TRAIN.txt"))

write_csv(heartbeat, "ecg.csv")

heartbeat2 <- read_csv(str_c(DATA_DIR, "ECG5000/", "ecg.csv"))

ecg <- read_csv(str_c(DATA_DIR, "ecg.csv"))

plt1 <- ecg[1,-1] |> t() |> drop() |> as_tibble() |> 
  mutate(x = 1:140) |> ggplot() + geom_line(aes(x = x, y = value)) + labs(y = "", x = "")

plt2 <- ecg[292,-1] |> t() |> drop() |> as_tibble() |> 
  mutate(x = 1:140) |> ggplot() + geom_line(aes(x = x, y = value)) + labs(y = "", x = "")

grid.arrange(plt1, plt2, nrow = 2)

tibble(y = rnorm(200), x = 1:200) |> ggplot() + geom_line(aes(x = x, y = y))


example1 <- tsibble(
  year = 2015:2019,               # <1>
  y = c(123, 39, 78, 52, 110),    # <2>
  index = year                    # <3>
)
str(example1)

