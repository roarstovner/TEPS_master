summarise_missing <- function(df) {
  df |>
    dplyr::group_by(institution_short) |>
    dplyr::summarise(
      rows          = dplyr::n(),
      missing_year  = sum(is.na(year)),
      missing_GLU   = sum(is.na(GLU)),
      missing_authors = sum(is.na(authors)),
      missing_title = sum(is.na(title)),
      missing_lang  = sum(is.na(language)),
      missing_abs   = sum(is.na(abstract)),
      .groups = "drop"
    ) |>
    dplyr::arrange(institution_short)
}

tbl_missing <- summarise_missing(masters)
View(tbl_missing)


uit <- masters %>% filter(institution_short == "uit")

sum(is.na(uit$year))             # 207
sum(is.na(uit$authors))          # 207


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
