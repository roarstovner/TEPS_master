library(dplyr)

source("R/utils.R")

masters <- readRDS("data/masters.RDS")
tbl_missing <- summarise_missing(masters)

View(tbl_missing)

uit <- masters %>% filter(institution_short == "uit")

uit_missing <- uit |> summarise_missing()

sum(is.na(uit$year) & is.na(uit$authors))
sum(is.na(uit$year) & !is.na(uit$authors))
sum(!is.na(uit$year) & is.na(uit$authors))

# 1) Krysstabell: viser om noen kombinasjoner finnes
uit |>
  count(is_na_year = is.na(year),
        is_na_auth = is.na(authors))

# 2) Vis de faktiske radene med dobbelt-NA
uit |>
  filter(is.na(year) & is.na(authors)) |>
  select(id, collection, url, year, authors) |>
  head(20)

uit |>
  mutate(file = if_else(collection == "10037/8169", "8169", "8170")) |>
  count(file,
        is_na_year = is.na(year),
        is_na_auth = is.na(authors))
