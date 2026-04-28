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


# -------------------------------------------------------------------
# Hivolda ------------------------------------------------------------
# -------------------------------------------------------------------
# Hivolda's Brage export (data-raw/2025-03_hivolda_11250-3012619 (4).csv)
# arrived corrupted in three ways. We undo each layer in memory before
# parsing as CSV.
#
#   1. Norwegian and typographic characters are stored as multi-byte
#      mojibake (e.g. `å` is the byte pair 0xC7 0xBE; smart quotes are
#      3-byte sequences with literal "?" filler). Decoding the raw
#      bytes as Mac OS Roman makes the patterns visible as known
#      printable strings, which we then rewrite to proper UTF-8.
#
#   2. Every physical line has ";;;;;;" (or sometimes ";;;") appended
#      as junk -- presumably trailing empty fields from a different
#      delimiter convention.
#
#   3. The original CSV's multi-line abstract fields have been wrapped
#      with a spurious `"` at every physical-line break, so naive CSV
#      parsing splits abstracts into separate fields. We strip those
#      wrappers per-thesis chunk.
#
# After cleaning, 43 thesis rows parse to the expected 34-column
# schema. One stray `"` inside a citation (`s. 111";` in the Norhagen
# 2024 thesis) is patched explicitly before parsing.
process_hivolda <- function(filename) {
  raw <- readr::read_file_raw(filename)

  # Decode raw bytes as Mac OS Roman so the mojibake byte sequences
  # become identifiable strings. (This is the "lens that makes the
  # corruption visible"; it is not the original encoding of the data.)
  text <- stringi::stri_encode(raw, from = "macintosh", to = "UTF-8")

  # Rewrite the mojibake patterns. Order matters: longer patterns
  # first so they match before shorter overlapping ones.
  text <- stringr::str_replace_all(text, c(
    "«æ"        = "å",   # «æ -> å
    "«›"        = "æ",   # «› -> æ
    "«˜"        = "ø",   # «˜ -> ø
    "«∏"        = "é",   # «∏ -> é
    "«˝"        = "ó",   # «˝ -> ó
    "∂Æ"        = "«",   # ∂Æ -> «
    "∂Ø"        = "»",   # ∂Ø -> »
    "∂ı"        = "§",   # ∂ı -> §
    "∂Ô"        = "’",   # ∂Ô -> ’
    "É\\?ù"     = "”",   # É?ù -> ”
    "«\\? "          = "Å ",  # «? <space> -> Å (infinitive at sentence start)
    "«\\?"           = "Ø",   # «? -> Ø (most common in surnames)
    "É\\?\\?"        = "’"    # É?? -> ’
  ))

  # One source-level data error (stray quote inside a citation):
  text <- stringr::str_replace(text, stringr::fixed("s. 111\"; Grims"), "s. 111; Grims")

  # Pull out the header (everything before the first thesis UUID).
  # The pattern `"<UUID>,` only occurs at thesis-record starts in this file.
  uuid_re <- "\"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12},"
  starts <- stringr::str_locate_all(text, uuid_re)[[1]][, "start"]
  if (length(starts) == 0L) stop("process_hivolda: no thesis records found in ", filename)

  header_text <- substr(text, 1L, starts[1] - 1L)
  header_text <- stringr::str_remove(header_text, ";;;;;;\\s*$")
  header_text <- stringr::str_trim(header_text)

  # Slice out one chunk per thesis.
  ends <- c(starts[-1] - 1L, nchar(text))
  chunks <- substring(text, starts, ends)

  # Per-thesis cleanup: drop the trailing record terminator, drop
  # internal `;;;` markers and the spurious `"` wrappers that flank
  # them at line breaks, and collapse any leftover newlines to spaces.
  clean_chunk <- function(s) {
    s <- stringr::str_remove(s, ";;;;;+\\s*$")
    s <- stringr::str_replace_all(s, '";;;+\\n"', " ")
    s <- stringr::str_replace_all(s, '";;;+\\n', " ")
    s <- stringr::str_replace_all(s, ';;;+\\n"', " ")
    s <- stringr::str_replace_all(s, ";;;+\\n", " ")
    s <- stringr::str_replace_all(s, ";;;+", " ")
    s <- stringr::str_replace_all(s, "\\n", " ")
    stringr::str_trim(s)
  }
  chunks <- vapply(chunks, clean_chunk, character(1), USE.NAMES = FALSE)

  # Each chunk is now `"<inner csv row>"` with `""` for embedded
  # quotes. Strip the outer quotes and un-double, then parse together
  # with the header as a normal CSV.
  inners <- substr(chunks, 2L, nchar(chunks) - 1L)
  inners <- gsub('""', '"', inners, fixed = TRUE)

  clean_csv <- paste0(header_text, "\n", paste(inners, collapse = "\n"))
  df <- readr::read_csv(I(clean_csv), show_col_types = FALSE)

  # Map to the unified 15-column schema. Localcode encodes GLU
  # explicitly for ~5 theses; the remainder fall through to NA.
  localcode_combined <- dplyr::coalesce(
    df$`dc.description.localcode`,
    df$`dc.description.localcode[en_US]`
  )

  df |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "hivolda",
      collection        = collection,
      GLU = dplyr::case_when(
        stringr::str_detect(localcode_combined, "5-10") ~ "MGLU 5-10",
        stringr::str_detect(localcode_combined, "1-7")  ~ "MGLU 1-7",
        TRUE                                            ~ NA_character_
      ),
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url       = dc.identifier.uri,
      language  = dplyr::coalesce(`dc.language.iso[en_US]`, dc.language.iso),
      subject   = NA_character_,
      full_text_available = NA_character_,
      title = dplyr::coalesce(
        `dc.title[en_US]`, `dc.title[nb_NO]`, `dc.title[nn_NO]`
      ),
      title_alt = dplyr::coalesce(
        `dc.title.alternative[en_US]`,
        `dc.title.alternative[nb_NO]`,
        `dc.title.alternative[nn_NO]`
      ),
      abstract = dplyr::coalesce(
        `dc.description.abstract[en_US]`,
        `dc.description.abstract[nb_NO]`,
        `dc.description.abstract[nn_NO]`
      ),
      abstract_alt = dplyr::coalesce(
        `dc.description.abstract[nb_NO]`,
        `dc.description.abstract[nn_NO]`
      ),
      .keep = "none"
    )
}


# -------------------------------------------------------------------
# NORD ---------------------------------------------------------------
# -------------------------------------------------------------------
# Nord ships its dump as .xlsx (read with openxlsx2). All proper MGLU
# theses sit under collection "11250/2721983"; a handful of rows in
# the export have shifted columns (Brage export issue) and are
# dropped here. GLU (1-7 vs 5-10) is not in `collection`; it is
# inferred from dc.description[en_US], which contains strings like
# "Master i grunnskolelærerutdanning 1-7. Matematikk 4 - 2023".
process_nord <- function(filename) {
  df <- openxlsx2::read_xlsx(filename)

  df |>
    dplyr::filter(collection == "11250/2721983") |>
    dplyr::mutate(
      id                = as.character(id),
      institution_short = "nord",
      collection        = collection,
      GLU = dplyr::case_when(
        stringr::str_detect(`dc.description[en_US]`, "1-7")  ~ "MGLU 1-7",
        stringr::str_detect(`dc.description[en_US]`, "5-10") ~ "MGLU 5-10",
        TRUE                                                 ~ NA_character_
      ),
      year      = safe_year(dc.date.issued),
      authors   = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url       = dc.identifier.uri,
      language  = `dc.language.iso[en_US]`,
      subject   = NA_character_,
      full_text_available = NA_character_,
      title     = `dc.title[en_US]`,
      title_alt = `dc.title.alternative[en_US]`,
      abstract     = NA_character_,
      abstract_alt = NA_character_,
      .keep = "none"
    )
}

