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
      error = stringr::str_glue("{colname} ({actual_type}): Expected {type}")
    )

  errors <- errors |>
    add_row(
      warning = "error",
      error = type_errors$error
      )
  
  if(nrow(errors) == 0) errors <- "No errors"

  return(errors)
}


# duplications occurr when students have submitted twice. Most often, this happens when people write their master's in pairs.
deduplicate_masters <- function(masters){
  masters |>
    #dplyr::mutate(!(sammendrag %in% sammendrag_alt)) |> #sjekk at det ikke bare er språkene som er byttet ## FUNGERER IKKE! FJERNER MASTERNE HELT!
    dplyr::mutate(abstract = refinr::key_collision_merge(abstract),
                  #abstract = refinr::n_gram_merge(abstract)
    ) |>
    dplyr::distinct(abstract, .keep_all = TRUE)
}


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

# -------------------------------------------------------------------
# Helpers ------------------------------------------------------------
# -------------------------------------------------------------------

# Count number of entries in strings of the type "name1 || name2"
# and "abstract_nor || abstract_en"
count_entries <- function(x) {
  ifelse(
    is.na(x),
    NA_integer_,
    stringr::str_count(x, "\\|\\|") + 1L
  )
}

# Convert safely to integer year 
safe_year <- function(x) {
  if (inherits(x, "Date")) {
    as.integer(format(x, "%Y"))
  } else {
    suppressWarnings(as.integer(x))
  }
}

# Coalesce over columns selected with tidyselect
# (to be used inside dplyr verbs)
coalesce_cols <- function(...) {
  dplyr::coalesce(!!!dplyr::pick(...))
}
