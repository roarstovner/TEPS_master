# R/process_masters.R
# Clean, unified processing functions for MGLU master theses

library(dplyr)
library(readr)
library(stringr)
library(purrr)

# ---------------------------
# Helpers
# ---------------------------

# Count "||" entries
count_entries <- function(x) {
  ifelse(is.na(x), NA_integer_, str_count(x, "\\|\\|") + 1)
}

# Convert safely to integer year 
safe_year <- function(x) {
  if (inherits(x, "Date")) {
    as.integer(format(x, "%Y"))
  } else {
    suppressWarnings(as.integer(x))
  }
}


# ---------------------------
# HIOF
# ---------------------------
process_hiof <- function(filename) {
  df <- read_csv(filename, show_col_types = FALSE)
  
  df |>
    mutate(
      id = as.character(id),
      institution_short = "hiof",
      GLU = case_match(
        collection,
        "11250/3011257" ~ "MGLU 1-7",
        "11250/3011260" ~ "MGLU 5-10",
        .default = NA_character_
      ),
      year = safe_year(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url = dc.identifier.uri,
      language = `dc.language.iso[en_US]`,
      subject = NA_character_,
      full_text_available = NA_character_,
      title = `dc.title[en_US]`,
      title_alt = NA_character_,
      abstract = NA_character_,
      abstract_alt = NA_character_,
      .keep = "none"
    )
}

# ---------------------------
# HVL
# ---------------------------
process_hvl <- function(filename) {
  df <- read_csv(filename, show_col_types = FALSE)
  
  localcode_cols <- intersect(
    c("dc.description.localcode",
      "dc.description.localcode[]",
      "dc.description.localcode[en_US]"),
    names(df)
  )
  
  df |>
    mutate(localcode = coalesce(!!!syms(localcode_cols))) |>
    filter(str_detect(localcode, regex("^mg[bu]", ignore_case = TRUE))) |>
    mutate(
      id = as.character(id),
      institution_short = "hvl",
      GLU = case_when(
        str_detect(localcode, regex("^mgb", ignore_case = TRUE)) ~ "MGLU 1-7",
        str_detect(localcode, regex("^mgu", ignore_case = TRUE)) ~ "MGLU 5-10",
        TRUE ~ NA_character_
      ),
      subject = case_when(
        str_detect(localcode, regex("^mg[bu]en")) ~ "Engelsk",
        str_detect(localcode, regex("^mg[bu]kh")) ~ "Kunst og h\u00e5ndverk",
        str_detect(localcode, regex("^mg[bu]kr")) ~ "KRLE",
        str_detect(localcode, regex("^mg[bu]k\u00f8")) ~ "Kropps\u00f8ving",
        str_detect(localcode, regex("^mg[bu]ma")) ~ "Matematikk",
        str_detect(localcode, regex("^mg[bu]mh")) ~ "Mat og helse",
        str_detect(localcode, regex("^mg[bu]mu")) ~ "Musikk",
        str_detect(localcode, regex("^mg[bu]na")) ~ "Naturfag",
        str_detect(localcode, regex("^mg[bu]no")) ~ "Norsk",
        str_detect(localcode, regex("^mg[bu]sa")) ~ "Samfunnsfag",
        TRUE ~ NA_character_
      ),
      year = safe_year(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url = dc.identifier.uri,
      language = `dc.language.iso[en_US]`,
      title = `dc.title[en_US]`,
      title_alt = `dc.title.alternative[en_US]`,
      abstract = `dc.description.abstract[en_US]`,
      abstract_alt = NA_character_,
      full_text_available = NA_character_,
      .keep = "none"
    )
}
# ---------------------------
# INN
# ---------------------------
process_inn <- function(filename) {
  df <- read_csv(filename, show_col_types = FALSE)
  
  df |>
    mutate(
      id = as.character(id),
      institution_short = "inn",
      GLU = case_match(
        collection,
        "11250/2980782" ~ "MGLU 1-7",
        "11250/2980784" ~ "MGLU 5-10",
        .default = NA_character_
      ),
      year = safe_year(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url = dc.identifier.uri,
      language = dc.language,
      full_text_available = case_when(
        str_detect(dc.description, "Full text not available") ~ "Nei",
        is.na(dc.description) ~ "Ja",
        TRUE ~ NA_character_
      ),
      subject = NA_character_,
      title = dc.title,
      title_alt = NA_character_,
      abstract = str_split_i(dc.description.abstract, "\\|\\|", 1),
      abstract_alt = str_split_i(dc.description.abstract, "\\|\\|", 2),
      .keep = "none"
    )
}

# ---------------------------
# Oslomet – Old
# ---------------------------
process_oslomet_old <- function(filename) {
  df <- read_csv(filename, show_col_types = FALSE)
  
  df |>
    mutate(
      id = as.character(id),
      institution_short = "oslomet",
      GLU = case_match(
        collection,
        "10642/6821" ~ "MGLU 1-7",
        "10642/6822" ~ "MGLU 5-10",
        .default = NA_character_
      ),
      year = safe_year(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url = dc.identifier.uri,
      language = `dc.language.iso[en_US]`,
      subject = NA_character_,
      full_text_available = NA_character_,
      title = `dc.title[en_US]`,
      title_alt = `dc.title.alternative[en_US]`,
      abstract = str_split_i(`dc.description.abstract[en_US]`,
                             "\\|\\||\\r\\n\\r\\n\\r\\n", 1),
      abstract_alt = str_split_i(`dc.description.abstract[en_US]`,
                                 "\\|\\||\\r\\n\\r\\n\\r\\n", 2),
      .keep = "none"
    )
}

# ---------------------------
# Oslomet – New
# ---------------------------
process_oslomet_new <- function(filename) {
  df <- read_csv(filename, show_col_types = FALSE)
  
  language_cols <- names(df)[str_detect(names(df), "^dc\\.language")]
  title_cols <- names(df)[str_detect(names(df), "^dc\\.title($|\\[)") &
                            !str_detect(names(df), "alternative")]
  abstract_cols <- names(df)[str_detect(names(df), "^dc\\.description\\.abstract")]
  
  df |>
    mutate(
      id = as.character(id),
      institution_short = "oslomet",
      GLU = case_match(
        collection,
        "10642/6821" ~ "MGLU 1-7",
        "10642/6822" ~ "MGLU 5-10",
        .default = NA_character_
      ),
      year = safe_year(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url = dc.identifier.uri,
      language = coalesce(!!!syms(language_cols)),
      subject = NA_character_,
      full_text_available = NA_character_,
      title = coalesce(!!!syms(title_cols)),
      title_alt = NA_character_,
      abstract = str_split_i(coalesce(!!!syms(abstract_cols)),
                             "\\|\\||\\r\\n\\r\\n\\r\\n", 1),
      abstract_alt = str_split_i(coalesce(!!!syms(abstract_cols)),
                                 "\\|\\||\\r\\n\\r\\n\\r\\n", 2),
      .keep = "none"
    )
}

# ---------------------------
# USN
# ---------------------------
process_usn <- function(filename){
  df <- read_csv(filename, show_col_types = FALSE)
  
  # Detect available columns by pattern
  language_cols    <- names(df)[str_detect(names(df), "^dc\\.language")]
  title_cols       <- names(df)[str_detect(names(df), "^dc\\.title($|\\[)") & !str_detect(names(df), "alternative")]
  title_alt_cols   <- names(df)[str_detect(names(df), "^dc\\.title\\.alternative")]
  abstract_cols    <- names(df)[str_detect(names(df), "^dc\\.description\\.abstract")]
  description_cols <- names(df)[str_detect(names(df), "^dc\\.description($|\\[)")]
  
  df |>
    mutate(
      id = id,
      institution_short = "usn",
      collection = collection,
      
      GLU = dplyr::case_match(
        collection,
        "11250/2732887" ~ "MGLU 1-7",
        "11250/2732886" ~ "MGLU 5-10",
        "11250/2732885" ~ "MGLU 1-7",
        .default = NA_character_
      ),
      
      year = as.integer(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url = dc.identifier.uri,
      
      language = if (length(language_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(language_cols))
      },
      
      full_text_available = case_when(
        length(description_cols) == 0 ~ NA_character_,
        TRUE ~ case_when(
          str_detect(coalesce(!!!syms(description_cols)), "Full text not available") ~ "Nei",
          is.na(coalesce(!!!syms(description_cols))) ~ "Ja",
          TRUE ~ NA_character_
        )
      ),
      
      subject = as.character(NA),
      
      title = if (length(title_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(title_cols))
      },
      
      title_alt = if (length(title_alt_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(title_alt_cols))
      },
      
      abstract = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        str_split_i(coalesce(!!!syms(abstract_cols)), "\\|\\|", 1)
      },
      
      abstract_alt = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        str_split_i(coalesce(!!!syms(abstract_cols)), "\\|\\|", 2)
      },
      
      .keep = "none"
    )
}

# ---------------------------
# UiA
# ---------------------------
process_uia <- function(filename) {
  df <- read_csv(filename, show_col_types = FALSE)
  
  file_base <- basename(filename)
  
  # Infer GLU from the file name
  glu_from_file <- case_when(
    str_detect(file_base, "1-7")  ~ "MGLU 1-7",
    str_detect(file_base, "5-10") ~ "MGLU 5-10",
    str_detect(file_base, "8-13") ~ "MGLU 8-13",
    TRUE                          ~ NA_character_
  )
  
  # Detect available columns by pattern
  language_cols    <- names(df)[str_detect(names(df), "^dc\\.language")]
  title_cols       <- names(df)[str_detect(names(df), "^dc\\.title($|\\[)") & !str_detect(names(df), "alternative")]
  title_alt_cols   <- names(df)[str_detect(names(df), "^dc\\.title\\.alternative")]
  abstract_cols    <- names(df)[str_detect(names(df), "^dc\\.description\\.abstract")]
  description_cols <- names(df)[str_detect(names(df), "^dc\\.description($|\\[)")]
  
  df |>
    mutate(
      id = id,
      institution_short = "uia",
      collection = collection,
      
      GLU = glu_from_file,
      
      year = as.integer(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url = dc.identifier.uri,
      
      language = if (length(language_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(language_cols))
      },
      
      full_text_available = case_when(
        length(description_cols) == 0 ~ NA_character_,
        TRUE ~ case_when(
          str_detect(coalesce(!!!syms(description_cols)), "Full text not available") ~ "Nei",
          is.na(coalesce(!!!syms(description_cols))) ~ "Ja",
          TRUE ~ NA_character_
        )
      ),
      
      subject = as.character(NA),
      
      title = if (length(title_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(title_cols))
      },
      
      title_alt = if (length(title_alt_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(title_alt_cols))
      },
      
      abstract = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        str_split_i(coalesce(!!!syms(abstract_cols)), "\\|\\|", 1)
      },
      
      abstract_alt = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        str_split_i(coalesce(!!!syms(abstract_cols)), "\\|\\|", 2)
      },
      
      .keep = "none"
    )
}

# ---------------------------
# UiT
# ---------------------------
process_uit <- function(filename) {
  df <- read_csv(filename, show_col_types = FALSE)
  
  file_base <- basename(filename)
  
  glu_from_file <- case_when(
    str_detect(file_base, "8169") ~ "MGLU 1-7",
    str_detect(file_base, "8170") ~ "MGLU 5-10",
    TRUE ~ NA_character_
  )
  
  language_cols <- names(df)[str_detect(names(df), "^dc\\.language")]
  title_cols <- names(df)[str_detect(names(df), "^dc\\.title($|\\[)") &
                            !str_detect(names(df), "alternative")]
  title_alt_cols <- names(df)[str_detect(names(df), "title.alternative")]
  abstract_cols <- names(df)[str_detect(names(df), "description.abstract")]
  description_cols <- names(df)[str_detect(names(df), "^dc\\.description")]
  
  df |>
    mutate(
      id = as.character(id),
      institution_short = "uit",
      collection = collection,
      
      GLU = glu_from_file,
      
      # We do NOT trust dc.date.issued for UiT yet → set to NA
      year = safe_year(dc.date.issued),
      
      authors = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url = dc.identifier.uri,
      
      language = if (length(language_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(language_cols))
      },
      
      full_text_available = if (length(description_cols) == 0) {
        NA_character_
      } else {
        case_when(
          str_detect(coalesce(!!!syms(description_cols)), "Full text not available") ~ "Nei",
          is.na(coalesce(!!!syms(description_cols))) ~ "Ja",
          TRUE ~ NA_character_
        )
      },
      
      subject = as.character(NA),
      
      title = if (length(title_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(title_cols))
      },
      
      title_alt = if (length(title_alt_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(title_alt_cols))
      },
      
      abstract = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        str_split_i(coalesce(!!!syms(abstract_cols)), "\\|\\|", 1)
      },
      
      abstract_alt = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        str_split_i(coalesce(!!!syms(abstract_cols)), "\\|\\|", 2)
      },
      
      .keep = "none"
    )
}

