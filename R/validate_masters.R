# Validate whether expected columns are in the data, whether extra columns are
# in the data, and whether columns have the correct type.
validate_masters <- function(masters){

  expected_cols <- tribble(
    ~colname, ~type,
    "id" , "character",
    "institution_short", "character",
    "collection", "character",
    "GLU", "character",
    "year", "integer",
    "authors", "character",
    "n_authors", "integer",
    "url", "character",
    "language", "character",
    "full_text_available", "character",
    "subject", "character",
    "title", "character",
    "abstract", "character",
    "title_alt", "character",
    "abstract_alt", "character"
    )

  errors <- tibble(
    warning = character(),
    error = character()
    )

  missing_cols <- setdiff(expected_cols$colname, colnames(masters))
  if (length(missing_cols) == length(expected_cols$colname)){
    errors <- errors |> add_row(warning = "critical", error = "All columns missing")
    return(errors)
  }
  if (length(missing_cols) > 0) {
    errors <- errors |>
      add_row(
        warning = "error",
        error = paste("Missing columns:", paste(missing_cols, collapse = ", "))
      )
  }

  additional_cols <- setdiff(colnames(masters), expected_cols$colname)
  if (length(additional_cols) > 0) {
    errors <- errors |>
      add_row(
        warning = "warning",
        error = paste("Additional columns:", paste(additional_cols, collapse = ", "))
      )
  }

  type_errors <- expected_cols |>
    filter(colname %in% colnames(masters)) |>
    rowwise() |>
    mutate(actual_type = typeof(masters[[colname]])) |>
    filter(type != actual_type) |>
    ungroup() |>
    mutate(
      error = str_glue("{colname} ({actual_type}): Expected {type}")
    )

  errors <- errors |>
    add_row(
      warning = "error",
      error = type_errors$error
      )
  
  if(nrow(errors) == 0) errors <- "No errors"

  return(errors)
}
