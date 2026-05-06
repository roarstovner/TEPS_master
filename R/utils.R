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


# Duplications occur when students submit twice. Most often: pair-written
# masters where each co-author uploads their own record (same title, each row
# names only their own author), or straight institution-side double uploads
# (same title, same authors string, different ids).
#
# Strategy: group within institution_short by (norm_title, year). Rows with
# NA / placeholder title pass through untouched. Inside each group, merge the
# authors lists across rows (union of ||-separated tokens) and keep the most
# informative remaining fields (longest non-NA abstract, then title_alt).
deduplicate_masters <- function(masters) {

  norm_title <- function(x) {
    out <- stringr::str_to_lower(x)
    out <- stringr::str_replace_all(out, "[[:punct:][:space:]]+", " ")
    out <- stringr::str_trim(out)
    placeholder <- out %in% c("", "ingen tittel", "untitled")
    out[placeholder] <- NA_character_
    out
  }

  pick_longest <- function(x) {
    nz <- !is.na(x) & nchar(x) > 0
    if (!any(nz)) return(x[1])
    x[nz][which.max(nchar(x[nz]))]
  }

  merge_authors <- function(x) {
    tokens <- unlist(strsplit(x[!is.na(x)], "\\|\\|"))
    tokens <- stringr::str_trim(tokens)
    tokens <- tokens[nchar(tokens) > 0]
    if (length(tokens) == 0) return(NA_character_)
    seen_lower <- character()
    keep <- logical(length(tokens))
    for (i in seq_along(tokens)) {
      lower <- stringr::str_to_lower(tokens[i])
      if (!(lower %in% seen_lower)) {
        seen_lower <- c(seen_lower, lower)
        keep[i] <- TRUE
      }
    }
    paste(tokens[keep], collapse = "||")
  }

  collapse_group <- function(g) {
    if (nrow(g) == 1) return(g)
    merged_authors <- merge_authors(g$authors)
    out <- g[1, , drop = FALSE]
    out$authors    <- merged_authors
    out$n_authors  <- as.integer(count_entries(merged_authors))
    out$abstract   <- pick_longest(g$abstract)
    out$abstract_alt <- pick_longest(g$abstract_alt)
    out$title_alt  <- pick_longest(g$title_alt)
    out$language   <- dplyr::coalesce(!!!as.list(g$language))
    out$subject    <- dplyr::coalesce(!!!as.list(g$subject))
    out$full_text_available <- dplyr::coalesce(!!!as.list(g$full_text_available))
    out$GLU        <- dplyr::coalesce(!!!as.list(g$GLU))
    out
  }

  masters <- masters |>
    dplyr::mutate(.norm_title = norm_title(title))

  passthrough <- masters |> dplyr::filter(is.na(.norm_title))
  candidates  <- masters |> dplyr::filter(!is.na(.norm_title))

  deduped <- candidates |>
    dplyr::group_by(institution_short, .norm_title, year) |>
    dplyr::group_modify(~ collapse_group(.x)) |>
    dplyr::ungroup()

  dplyr::bind_rows(deduped, passthrough) |>
    dplyr::select(-.norm_title)
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
