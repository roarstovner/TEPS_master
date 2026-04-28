# R/process_masters.R
# Processing functions for master theses metadata (MGLU)

process_hiof <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  df |>
    dplyr::mutate(
      id                = as.character(id),
      collection        = collection,          # NY
      institution_short = "hiof",
      GLU = dplyr::recode_values(
        collection,
        from = c("11250/3011257", "11250/3011260"),
        to   = c("MGLU 1-7",      "MGLU 5-10")
      ),
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),  # integer
      url       = dc.identifier.uri,
      language  = `dc.language.iso[en_US]`,
      subject   = NA_character_,
      full_text_available = NA_character_,
      title     = `dc.title[en_US]`,
      title_alt = NA_character_,
      abstract      = NA_character_,
      abstract_alt  = NA_character_,
      .keep = "none"
    )
}


# -------------------------------------------------------------------
# HVL ----------------------------------------------------------------
# -------------------------------------------------------------------
process_hvl <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  df |>
    # Samle localcode fra alle aktuelle kolonner
    dplyr::mutate(
      localcode = coalesce_cols(
        dplyr::any_of(c(
          "dc.description.localcode",
          "dc.description.localcode[]",
          "dc.description.localcode[en_US]"
        ))
      )
    ) |>
    # Velg bare MGLU
    dplyr::filter(
      stringr::str_detect(
        localcode,
        stringr::regex("^mg[bu]", ignore_case = TRUE)
      )
    ) |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "hvl",
      collection        = collection,   # ← NYTT: beholder collection
      GLU = dplyr::case_when(
        stringr::str_detect(localcode, stringr::regex("^mgb", ignore_case = TRUE)) ~ "MGLU 1-7",
        stringr::str_detect(localcode, stringr::regex("^mgu", ignore_case = TRUE)) ~ "MGLU 5-10",
        TRUE ~ NA_character_
      ),
      subject = dplyr::case_when(
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]en", ignore_case = TRUE)
        ) ~ "Engelsk",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]kh", ignore_case = TRUE)
        ) ~ "Kunst og h\u00e5ndverk",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]kr", ignore_case = TRUE)
        ) ~ "KRLE",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]k\u00f8", ignore_case = TRUE)
        ) ~ "Kropps\u00f8ving",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]ma", ignore_case = TRUE)
        ) ~ "Matematikk",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]mh", ignore_case = TRUE)
        ) ~ "Mat og helse",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]mu", ignore_case = TRUE)
        ) ~ "Musikk",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]na", ignore_case = TRUE)
        ) ~ "Naturfag",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]no", ignore_case = TRUE)
        ) ~ "Norsk",
        stringr::str_detect(
          localcode, stringr::regex("^mg[bu]sa", ignore_case = TRUE)
        ) ~ "Samfunnsfag",
        TRUE ~ NA_character_
      ),
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url       = dc.identifier.uri,
      language  = `dc.language.iso[en_US]`,
      title     = `dc.title[en_US]`,
      title_alt = `dc.title.alternative[en_US]`,
      abstract      = `dc.description.abstract[en_US]`,
      abstract_alt  = NA_character_,
      full_text_available = NA_character_,
      .keep = "none"
    )
}

# -------------------------------------------------------------------
# INN ----------------------------------------------------------------
# -------------------------------------------------------------------
process_inn <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  df |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "inn",
      collection        = collection,   # ← NY
      GLU = dplyr::recode_values(
        collection,
        from = c("11250/2980782", "11250/2980784"),
        to   = c("MGLU 1-7",      "MGLU 5-10")
      ),
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url       = dc.identifier.uri,
      language  = dc.language,
      full_text_available = dplyr::case_when(
        stringr::str_detect(dc.description, "Full text not available") ~ "Nei",
        is.na(dc.description) ~ "Ja",
        TRUE ~ NA_character_
      ),
      subject   = NA_character_,
      title     = dc.title,
      title_alt = NA_character_,
      abstract      = stringr::str_split_i(dc.description.abstract, "\\|\\|", 1),
      abstract_alt  = stringr::str_split_i(dc.description.abstract, "\\|\\|", 2),
      .keep = "none"
    )
}

# -------------------------------------------------------------------
# Oslomet – Old ------------------------------------------------------
# -------------------------------------------------------------------
process_oslomet_old <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  df |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "oslomet",
      collection        = collection,   # ← NY
      GLU = dplyr::recode_values(
        collection,
        from = c("10642/6821", "10642/6822"),
        to   = c("MGLU 1-7",   "MGLU 5-10")
      ),
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url       = dc.identifier.uri,
      language  = `dc.language.iso[en_US]`,
      subject   = NA_character_,
      full_text_available = NA_character_,
      title     = `dc.title[en_US]`,
      title_alt = `dc.title.alternative[en_US]`,
      abstract      = stringr::str_split_i(
        `dc.description.abstract[en_US]`,
        "\\|\\||\\r\\n\\r\\n\\r\\n",
        1
      ),
      abstract_alt  = stringr::str_split_i(
        `dc.description.abstract[en_US]`,
        "\\|\\||\\r\\n\\r\\n\\r\\n",
        2
      ),
      .keep = "none"
    )
}

# -------------------------------------------------------------------
# Oslomet – New ------------------------------------------------------
# -------------------------------------------------------------------
process_oslomet_new <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  # Finn kolonner ved navn (enkelt, uten rlang)
  language_cols <- grep("^dc\\.language", names(df), value = TRUE)
  title_cols    <- grep("^dc\\.title($|\\[)", names(df), value = TRUE)
  title_cols    <- title_cols[!grepl("alternative", title_cols)]
  abstract_cols <- grep("^dc\\.description\\.abstract", names(df), value = TRUE)
  
  # Språk: coalesce over språk-kolonnene hvis de finnes
  if (length(language_cols) == 0) {
    language_vec <- rep(NA_character_, nrow(df))
  } else {
    language_vec <- do.call(dplyr::coalesce, df[language_cols])
  }
  
  # Tittel: coalesce over tittel-kolonnene
  if (length(title_cols) == 0) {
    title_vec <- rep(NA_character_, nrow(df))
  } else {
    title_vec <- do.call(dplyr::coalesce, df[title_cols])
  }
  
  # Abstract: coalesce over abstract-kolonner, så splitte i norsk/engelsk
  if (length(abstract_cols) == 0) {
    abstract_raw <- rep(NA_character_, nrow(df))
  } else {
    abstract_raw <- do.call(dplyr::coalesce, df[abstract_cols])
  }
  
  abstract_vec     <- stringr::str_split_i(abstract_raw, "\\|\\||\\r\\n\\r\\n\\r\\n", 1)
  abstract_alt_vec <- stringr::str_split_i(abstract_raw, "\\|\\||\\r\\n\\r\\n\\r\\n", 2)
  
  # full_text_available – Oslomet-new har ikke pålitelig felt → sett NA
  full_text_vec <- rep(NA_character_, nrow(df))
  
  df |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "oslomet",
      collection        = collection,
      GLU = dplyr::recode_values(
        collection,
        from = c("10642/6821", "10642/6822"),
        to   = c("MGLU 1-7",   "MGLU 5-10")
      ),
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url       = dc.identifier.uri,
      
      language  = language_vec,
      subject   = NA_character_,
      title     = title_vec,
      title_alt = NA_character_,
      
      abstract      = abstract_vec,
      abstract_alt  = abstract_alt_vec,
      full_text_available = full_text_vec,
      
      .keep = "none"
    )
}


# -------------------------------------------------------------------
# USN ----------------------------------------------------------------
# -------------------------------------------------------------------
process_usn <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  # Detekter språk / tittel / abstract-kolonner
  language_cols  <- grep("^dc\\.language", names(df), value = TRUE)
  title_cols     <- grep("^dc\\.title($|\\[)", names(df), value = TRUE)
  title_cols     <- title_cols[!grepl("alternative", title_cols)]
  title_alt_cols <- grep("^dc\\.title\\.alternative", names(df), value = TRUE)
  abstract_cols  <- grep("^dc\\.description\\.abstract", names(df), value = TRUE)
  
  # Språk
  if (length(language_cols) == 0) {
    language_vec <- rep(NA_character_, nrow(df))
  } else {
    language_vec <- do.call(dplyr::coalesce, df[language_cols])
  }
  
  # Tittel
  if (length(title_cols) == 0) {
    title_vec <- rep(NA_character_, nrow(df))
  } else {
    title_vec <- do.call(dplyr::coalesce, df[title_cols])
  }
  
  # Alternativ tittel
  if (length(title_alt_cols) == 0) {
    title_alt_vec <- rep(NA_character_, nrow(df))
  } else {
    title_alt_vec <- do.call(dplyr::coalesce, df[title_alt_cols])
  }
  
  # Abstract
  if (length(abstract_cols) == 0) {
    abstract_raw <- rep(NA_character_, nrow(df))
  } else {
    abstract_raw <- do.call(dplyr::coalesce, df[abstract_cols])
  }
  abstract_vec     <- stringr::str_split_i(abstract_raw, "\\|\\|", 1)
  abstract_alt_vec <- stringr::str_split_i(abstract_raw, "\\|\\|", 2)
  
  # full_text_available basert på dc.description hvis den finnes
  if ("dc.description" %in% names(df)) {
    desc_vec <- df$dc.description
    full_text_vec <- dplyr::case_when(
      stringr::str_detect(desc_vec, "Full text not available") ~ "Nei",
      is.na(desc_vec)                                          ~ "Ja",
      TRUE                                                     ~ NA_character_
    )
  } else {
    full_text_vec <- rep(NA_character_, nrow(df))
  }
  
  df |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "usn",
      collection        = collection,
      GLU = dplyr::recode_values(
        collection,
        from = c("11250/2732887", "11250/2732886", "11250/2732885"),
        to   = c("MGLU 1-7",      "MGLU 5-10",     "MGLU 1-7")
      ),
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url       = dc.identifier.uri,
      
      language  = language_vec,
      subject   = NA_character_,
      title     = title_vec,
      title_alt = title_alt_vec,
      abstract      = abstract_vec,
      abstract_alt  = abstract_alt_vec,
      full_text_available = full_text_vec,
      
      .keep = "none"
    )
}



# -------------------------------------------------------------------
# UiA ----------------------------------------------------------------
# -------------------------------------------------------------------
process_uia <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  file_base <- basename(filename)
  
  glu_from_file <- dplyr::case_when(
    stringr::str_detect(file_base, "1-7")  ~ "MGLU 1-7",
    stringr::str_detect(file_base, "5-10") ~ "MGLU 5-10",
    stringr::str_detect(file_base, "8-13") ~ "MGLU 8-13",
    TRUE ~ NA_character_
  )
  
  language_cols  <- grep("^dc\\.language", names(df), value = TRUE)
  title_cols     <- grep("^dc\\.title($|\\[)", names(df), value = TRUE)
  title_cols     <- title_cols[!grepl("alternative", title_cols)]
  title_alt_cols <- grep("^dc\\.title\\.alternative", names(df), value = TRUE)
  abstract_cols  <- grep("^dc\\.description\\.abstract", names(df), value = TRUE)
  
  # Språk
  if (length(language_cols) == 0) {
    language_vec <- rep(NA_character_, nrow(df))
  } else {
    language_vec <- do.call(dplyr::coalesce, df[language_cols])
  }
  
  # Titler
  if (length(title_cols) == 0) {
    title_vec <- rep(NA_character_, nrow(df))
  } else {
    title_vec <- do.call(dplyr::coalesce, df[title_cols])
  }
  
  if (length(title_alt_cols) == 0) {
    title_alt_vec <- rep(NA_character_, nrow(df))
  } else {
    title_alt_vec <- do.call(dplyr::coalesce, df[title_alt_cols])
  }
  
  # Abstract
  if (length(abstract_cols) == 0) {
    abstract_raw <- rep(NA_character_, nrow(df))
  } else {
    abstract_raw <- do.call(dplyr::coalesce, df[abstract_cols])
  }
  abstract_vec     <- stringr::str_split_i(abstract_raw, "\\|\\|", 1)
  abstract_alt_vec <- stringr::str_split_i(abstract_raw, "\\|\\|", 2)
  
  # full_text_available fra dc.description hvis den finnes
  if ("dc.description" %in% names(df)) {
    desc_vec <- df$dc.description
    full_text_vec <- dplyr::case_when(
      stringr::str_detect(desc_vec, "Full text not available") ~ "Nei",
      is.na(desc_vec)                                          ~ "Ja",
      TRUE                                                     ~ NA_character_
    )
  } else {
    full_text_vec <- rep(NA_character_, nrow(df))
  }
  
  df |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "uia",
      collection        = collection,
      GLU               = glu_from_file,
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url       = dc.identifier.uri,
      
      language  = language_vec,
      subject   = NA_character_,
      title     = title_vec,
      title_alt = title_alt_vec,
      abstract      = abstract_vec,
      abstract_alt  = abstract_alt_vec,
      full_text_available = full_text_vec,
      
      .keep = "none"
    )
}


# -------------------------------------------------------------------
# UiT ----------------------------------------------------------------
# -------------------------------------------------------------------
process_uit <- function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  
  file_base <- basename(filename)
  
  glu_from_file <- dplyr::case_when(
    stringr::str_detect(file_base, "8169") ~ "MGLU 1-7",
    stringr::str_detect(file_base, "8170") ~ "MGLU 5-10",
    TRUE ~ NA_character_
  )
  
  # Finn aktuelle kolonner etter mønster
  language_cols    <- names(df)[stringr::str_detect(names(df), "^dc\\.language")]
  title_cols       <- names(df)[stringr::str_detect(names(df), "^dc\\.title($|\\[)") &
                                  !stringr::str_detect(names(df), "alternative")]
  title_alt_cols   <- names(df)[stringr::str_detect(names(df), "title.alternative")]
  abstract_cols    <- names(df)[stringr::str_detect(names(df), "description.abstract")]
  description_cols <- names(df)[stringr::str_detect(names(df), "^dc\\.description($|\\[)")]
  
  # Lag én samlekolonne med "description" FØR vi gjør .keep = "none"
  if (length(description_cols) == 0) {
    df <- df |>
      dplyr::mutate(desc_tmp = NA_character_)
  } else {
    df <- df |>
      dplyr::mutate(
        desc_tmp = dplyr::coalesce(!!!rlang::syms(description_cols))
      )
  }
  
  df |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "uit",
      collection        = collection,
      GLU               = glu_from_file,
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = count_entries(dc.contributor.author),
      url       = dc.identifier.uri,
      
      language  = if (length(language_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!rlang::syms(language_cols))
      },
      
      subject   = NA_character_,
      
      title     = if (length(title_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!rlang::syms(title_cols))
      },
      title_alt = if (length(title_alt_cols) == 0) {
        NA_character_
      } else {
        dplyr::coalesce(!!!rlang::syms(title_alt_cols))
      },
      
      abstract = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        stringr::str_split_i(
          dplyr::coalesce(!!!rlang::syms(abstract_cols)), "\\|\\|", 1
        )
      },
      abstract_alt = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        stringr::str_split_i(
          dplyr::coalesce(!!!rlang::syms(abstract_cols)), "\\|\\|", 2
        )
      },
      
      # Bruker desc_tmp til å sette full_text_available
      full_text_available = dplyr::case_when(
        stringr::str_detect(desc_tmp, "Full text not available") ~ "Nei",
        is.na(desc_tmp)                                          ~ "Ja",
        TRUE                                                     ~ NA_character_
      ),
      
      .keep = "none"
    )
}

