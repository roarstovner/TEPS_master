# count number of entries in strings of the type "name1 || name2" and "abstract_nor || abstract_en"
count_entries <- function(x) {
  ifelse(is.na(x), NA_integer_, stringr::str_count(x, "\\|\\|") + 1)
}

# Convert safely to integer year 
safe_year <- function(x) {
  if (inherits(x, "Date")) {
    as.integer(format(x, "%Y"))
  } else {
    suppressWarnings(as.integer(x))
  }
}

coalesce_cols <- function(...) {
  dplyr::coalesce(!!!pick(...))
}


# ---------------------------
# HIOF
# ---------------------------
process_hiof <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  df |>
    dplyr::mutate(
      id = as.character(id),
      institution_short = "hiof",
      GLU = dplyr::case_match(
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
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  localcode_cols <- intersect(
    c("dc.description.localcode",
      "dc.description.localcode[]",
      "dc.description.localcode[en_US]"),
    names(df)
  )
  
  df |>
    dplyr::mutate(localcode = dplyr::coalesce(!!!syms(localcode_cols))) |>
    dplyr::filter(stringr::str_detect(localcode, regex("^mg[bu]", ignore_case = TRUE))) |>
    dplyr::mutate(
      id = as.character(id),
      institution_short = "hvl",
      GLU = dplyr::case_when(
        stringr::str_detect(localcode, regex("^mgb", ignore_case = TRUE)) ~ "MGLU 1-7",
        stringr::str_detect(localcode, regex("^mgu", ignore_case = TRUE)) ~ "MGLU 5-10",
        TRUE ~ NA_character_
      ),
      subject = dplyr::case_when(
        stringr::str_detect(localcode, regex("^mg[bu]en")) ~ "Engelsk",
        stringr::str_detect(localcode, regex("^mg[bu]kh")) ~ "Kunst og h\u00e5ndverk",
        stringr::str_detect(localcode, regex("^mg[bu]kr")) ~ "KRLE",
        stringr::str_detect(localcode, regex("^mg[bu]k\u00f8")) ~ "Kropps\u00f8ving",
        stringr::str_detect(localcode, regex("^mg[bu]ma")) ~ "Matematikk",
        stringr::str_detect(localcode, regex("^mg[bu]mh")) ~ "Mat og helse",
        stringr::str_detect(localcode, regex("^mg[bu]mu")) ~ "Musikk",
        stringr::str_detect(localcode, regex("^mg[bu]na")) ~ "Naturfag",
        stringr::str_detect(localcode, regex("^mg[bu]no")) ~ "Norsk",
        stringr::str_detect(localcode, regex("^mg[bu]sa")) ~ "Samfunnsfag",
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
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  df |>
    dplyr::mutate(
      id = as.character(id),
      institution_short = "inn",
      GLU = dplyr::case_match(
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
      full_text_available = dplyr::case_when(
        stringr::str_detect(dc.description, "Full text not available") ~ "Nei",
        is.na(dc.description) ~ "Ja",
        TRUE ~ NA_character_
      ),
      subject = NA_character_,
      title = dc.title,
      title_alt = NA_character_,
      abstract = stringr::str_split_i(dc.description.abstract, "\\|\\|", 1),
      abstract_alt = stringr::str_split_i(dc.description.abstract, "\\|\\|", 2),
      .keep = "none"
    )
}

# ---------------------------
# Oslomet – Old
# ---------------------------
process_oslomet_old <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  df |>
    dplyr::mutate(
      id = as.character(id),
      institution_short = "oslomet",
      GLU = dplyr::case_match(
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
      abstract = stringr::str_split_i(`dc.description.abstract[en_US]`,
                             "\\|\\||\\r\\n\\r\\n\\r\\n", 1),
      abstract_alt = stringr::str_split_i(`dc.description.abstract[en_US]`,
                                 "\\|\\||\\r\\n\\r\\n\\r\\n", 2),
      .keep = "none"
    )
}

# ---------------------------
# Oslomet – New
# ---------------------------
process_oslomet_new <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  language_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.language")]
  title_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.title($|\\[)") &
                            !stringr::str_detect(names(df), "alternative")]
  abstract_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.description\\.abstract")]
  
  df |>
    dplyr::mutate(
      id = as.character(id),
      institution_short = "oslomet",
      GLU = dplyr::case_match(
        collection,
        "10642/6821" ~ "MGLU 1-7",
        "10642/6822" ~ "MGLU 5-10",
        .default = NA_character_
      ),
      year = safe_year(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url = dc.identifier.uri,
      language = dplyr::coalesce(!!!syms(language_cols)),
      subject = NA_character_,
      full_text_available = NA_character_,
      title = dplyr::coalesce(!!!syms(title_cols)),
      title_alt = NA_character_,
      abstract = stringr::str_split_i(dplyr::coalesce(!!!syms(abstract_cols)),
                             "\\|\\||\\r\\n\\r\\n\\r\\n", 1),
      abstract_alt = stringr::str_split_i(dplyr::coalesce(!!!syms(abstract_cols)),
                                 "\\|\\||\\r\\n\\r\\n\\r\\n", 2),
      .keep = "none"
    )
}

# ---------------------------
# USN
# ---------------------------
process_usn <- function(filename){
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  # Detect available columns by pattern
  language_cols    <- names(df)[stringr::str_detect(names(df), "^dc\\.language")]
  title_cols       <- names(df)[stringr::str_detect(names(df), "^dc\\.title($|\\[)") & !stringr::str_detect(names(df), "alternative")]
  title_alt_cols   <- names(df)[stringr::str_detect(names(df), "^dc\\.title\\.alternative")]
  abstract_cols    <- names(df)[stringr::str_detect(names(df), "^dc\\.description\\.abstract")]
  description_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.description($|\\[)")]
  
  df |>
    dplyr::mutate(
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
        dplyr::coalesce(!!!syms(language_cols))
      },
      
      full_text_available = dplyr::case_when(
        length(description_cols) == 0 ~ NA_character_,
        TRUE ~ dplyr::case_when(
          stringr::str_detect(dplyr::coalesce(!!!syms(description_cols)), "Full text not available") ~ "Nei",
          is.na(dplyr::coalesce(!!!syms(description_cols))) ~ "Ja",
          TRUE ~ NA_character_
        )
      ),
      
      subject = NA_character_,
      
      title = if (length(title_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!syms(title_cols))
      },
      
      title_alt = if (length(title_alt_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!syms(title_alt_cols))
      },
      
      abstract = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        stringr::str_split_i(dplyr::coalesce(!!!syms(abstract_cols)), "\\|\\|", 1)
      },
      
      abstract_alt = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        stringr::str_split_i(dplyr::coalesce(!!!syms(abstract_cols)), "\\|\\|", 2)
      },
      
      .keep = "none"
    )
}

# ---------------------------
# UiA
# ---------------------------
process_uia <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  file_base <- basename(filename)
  
  # Infer GLU from the file name
  glu_from_file <- dplyr::case_when(
    stringr::str_detect(file_base, "1-7")  ~ "MGLU 1-7",
    stringr::str_detect(file_base, "5-10") ~ "MGLU 5-10",
    stringr::str_detect(file_base, "8-13") ~ "MGLU 8-13",
    TRUE                          ~ NA_character_
  )
  
  # Detect available columns by pattern
  language_cols    <- names(df)[stringr::str_detect(names(df), "^dc\\.language")]
  title_cols       <- names(df)[stringr::str_detect(names(df), "^dc\\.title($|\\[)") & !stringr::str_detect(names(df), "alternative")]
  title_alt_cols   <- names(df)[stringr::str_detect(names(df), "^dc\\.title\\.alternative")]
  abstract_cols    <- names(df)[stringr::str_detect(names(df), "^dc\\.description\\.abstract")]
  description_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.description($|\\[)")]
  
  df |>
    dplyr::mutate(
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
        dplyr::coalesce(!!!syms(language_cols))
      },
      
      full_text_available = dplyr::case_when(
        length(description_cols) == 0 ~ NA_character_,
        TRUE ~ dplyr::case_when(
          stringr::str_detect(dplyr::coalesce(!!!syms(description_cols)), "Full text not available") ~ "Nei",
          is.na(dplyr::coalesce(!!!syms(description_cols))) ~ "Ja",
          TRUE ~ NA_character_
        )
      ),
      
      subject = NA_character_,
      
      title = if (length(title_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!syms(title_cols))
      },
      
      title_alt = if (length(title_alt_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!syms(title_alt_cols))
      },
      
      abstract = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        stringr::str_split_i(dplyr::coalesce(!!!syms(abstract_cols)), "\\|\\|", 1)
      },
      
      abstract_alt = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        stringr::str_split_i(dplyr::coalesce(!!!syms(abstract_cols)), "\\|\\|", 2)
      },
      
      .keep = "none"
    )
}

# ---------------------------
# UiT
# ---------------------------
process_uit <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  file_base <- basename(filename)
  
  glu_from_file <- dplyr::case_when(
    stringr::str_detect(file_base, "8169") ~ "MGLU 1-7",
    stringr::str_detect(file_base, "8170") ~ "MGLU 5-10",
    TRUE ~ NA_character_
  )
  
  language_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.language")]
  title_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.title($|\\[)") &
                            !stringr::str_detect(names(df), "alternative")]
  title_alt_cols <- names(df)[stringr::str_detect(names(df), "title.alternative")]
  abstract_cols <- names(df)[stringr::str_detect(names(df), "description.abstract")]
  description_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.description")]
  
  df |>
    dplyr::mutate(
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
        dplyr::coalesce(!!!syms(language_cols))
      },
      
      full_text_available = if (length(description_cols) == 0) {
        NA_character_
      } else {
        dplyr::case_when(
          stringr::str_detect(dplyr::coalesce(!!!syms(description_cols)), "Full text not available") ~ "Nei",
          is.na(dplyr::coalesce(!!!syms(description_cols))) ~ "Ja",
          TRUE ~ NA_character_
        )
      },
      
      subject = NA_character_,
      
      title = if (length(title_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!syms(title_cols))
      },
      
      title_alt = if (length(title_alt_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!syms(title_alt_cols))
      },
      
      abstract = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        stringr::str_split_i(dplyr::coalesce(!!!syms(abstract_cols)), "\\|\\|", 1)
      },
      
      abstract_alt = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        stringr::str_split_i(dplyr::coalesce(!!!syms(abstract_cols)), "\\|\\|", 2)
      },
      
      .keep = "none"
    )
}

