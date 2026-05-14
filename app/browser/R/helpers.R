# helpers.R — Data loading and utilities for Masters Browser

# TODO(shared-labels): institution_labels and language_labels are duplicated
# between this app and ../../../TEPS_course_content_harvest/app/course_browser/R/helpers.R.
# When the institution list drifts (new institution added, short code renamed,
# display label changed), both copies must be updated in lockstep. Long-term
# fix: extract into a small shared package (e.g. `tepsmeta`) or a CSV both
# projects read at load time. Until then, grep for `institution_labels <- c(`
# across the TEPS_* repos before editing.
institution_labels <- c(
  hiof    = "HiOF",
  hivolda = "Hivolda",
  hvl     = "HVL",
  inn     = "INN",
  nord    = "Nord",
  oslomet = "OsloMet",
  uia     = "UiA",
  uit     = "UiT",
  usn     = "USN"
)

language_labels <- c(
  nob   = "Norwegian (bokmål)",
  nno   = "Norwegian (nynorsk)",
  eng   = "English",
  ger   = "German",
  swe   = "Swedish",
  "N/A" = "N/A"
)

#' Load masters thesis data
load_masters <- function(data_dir = "../../data") {
  path <- file.path(data_dir, "masters.RDS")
  if (!file.exists(path)) stop("Data file not found: ", path)
  readRDS(path)
}

#' Highlight search tokens in plain text by wrapping them in <mark>.
#'
#' HTML-escapes input first, then applies a case-insensitive regex
#' built from whitespace-separated tokens.
#'
#' @param text Character(1)
#' @param query Character(1) search string; whitespace splits tokens
#' @return HTML string (safe to pass to HTML())
highlight_tokens <- function(text, query) {
  if (is.null(text) || is.na(text) || !nzchar(text)) return("")
  escaped <- htmltools::htmlEscape(text)
  if (is.null(query) || is.na(query) || !nzchar(trimws(query))) return(escaped)

  tokens <- strsplit(trimws(query), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) return(escaped)

  # Escape regex metacharacters in tokens, then HTML-escape so they match
  # against the already-escaped text body.
  pattern_tokens <- vapply(tokens, function(tok) {
    re <- gsub("([\\\\.\\^\\$\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])", "\\\\\\1", tok, perl = TRUE)
    htmltools::htmlEscape(re)
  }, character(1))
  pattern <- paste0("(", paste(pattern_tokens, collapse = "|"), ")")

  gsub(pattern, "<mark>\\1</mark>", escaped, perl = TRUE, ignore.case = TRUE)
}

#' Test whether row matches free-text query (title + abstract + alt fields).
matches_query <- function(df, query) {
  if (is.null(query) || is.na(query) || !nzchar(trimws(query))) {
    return(rep(TRUE, nrow(df)))
  }
  tokens <- strsplit(trimws(query), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) return(rep(TRUE, nrow(df)))

  haystack <- paste(
    ifelse(is.na(df$title),        "", df$title),
    ifelse(is.na(df$title_alt),    "", df$title_alt),
    ifelse(is.na(df$abstract),     "", df$abstract),
    ifelse(is.na(df$abstract_alt), "", df$abstract_alt),
    sep = "  "
  )
  # AND-match across tokens (all tokens must appear somewhere)
  hits <- rep(TRUE, length(haystack))
  for (tok in tokens) {
    hits <- hits & grepl(tok, haystack, fixed = TRUE)
  }
  hits
}
