# Count number of entries in strings of the type "name1 || name2"
# and "abstract_nor || abstract_en"
count_entries <- function(var){
  str_count(var, "\\|\\|") + 1
}

process_hiof <- function(filename){
  df <- read_csv(filename, show_col_types = FALSE)
  
  df |>
    mutate(
      id = id,
      institution_short = "hiof",
      collection = collection,
      GLU = dplyr::case_match(
        collection,
        "11250/3011257" ~ "MGLU 1-7",
        "11250/3011260" ~ "MGLU 5-10"
      ),
      year = as.integer(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url = dc.identifier.uri,
      language = `dc.language.iso[en_US]`,
      full_text_available = as.character(NA),
      subject = as.character(NA),
      title = `dc.title[en_US]`,
      abstract = as.character(NA),
      title_alt = as.character(NA),
      abstract_alt = as.character(NA),
      .keep = "none"
    )
}

process_hvl <- function(filename){
  df <- read_csv(filename, show_col_types = FALSE)
  
  # Local subject codes that indicate teacher education (MGLU)
  mglu_subjects_regex <- regex("^mg[bu]", ignore_case = TRUE)
  
  df |>
    mutate(
      # Coalesce the three possible localcode columns if they exist
      localcode = coalesce(
        !!!syms(
          intersect(
            c(
              "dc.description.localcode",
              "dc.description.localcode[]",
              "dc.description.localcode[en_US]"
            ),
            names(df)
          )
        )
      )
    ) |>
    filter(str_detect(localcode, mglu_subjects_regex)) |>
    mutate(
      id = id,
      institution_short = "hvl",
      collection = collection,
      GLU = case_when(
        # "mgb" = primary school track (1–7)
        str_detect(localcode, regex("^mgb", ignore_case = TRUE)) ~ "MGLU 1-7",
        # "mgu" = lower secondary track (5–10)
        str_detect(localcode, regex("^mgu", ignore_case = TRUE)) ~ "MGLU 5-10"
      ),
      subject = case_when(
        str_detect(localcode, regex("^mg[bu]en", ignore_case = TRUE)) ~ "Engelsk",
        str_detect(localcode, regex("^mg[bu]kh", ignore_case = TRUE)) ~ "Kunst og håndverk",
        str_detect(localcode, regex("^mg[bu]kr", ignore_case = TRUE)) ~ "KRLE",
        str_detect(localcode, regex("^mg[bu]kø", ignore_case = TRUE)) ~ "Kroppsøving",
        str_detect(localcode, regex("^mg[bu]ma", ignore_case = TRUE)) ~ "Matematikk",
        str_detect(localcode, regex("^mg[bu]mh", ignore_case = TRUE)) ~ "Mat og helse",
        str_detect(localcode, regex("^mg[bu]mu", ignore_case = TRUE)) ~ "Musikk",
        str_detect(localcode, regex("^mg[bu]na", ignore_case = TRUE)) ~ "Naturfag",
        str_detect(localcode, regex("^mg[bu]no", ignore_case = TRUE)) ~ "Norsk",
        str_detect(localcode, regex("^mg[bu]sa", ignore_case = TRUE)) ~ "Samfunnsfag"
      ),
      full_text_available = as.character(NA),
      year = as.integer(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url = dc.identifier.uri,
      language = `dc.language.iso[en_US]`,
      title = `dc.title[en_US]`,
      abstract = `dc.description.abstract[en_US]`,
      title_alt = `dc.title.alternative[en_US]`,
      abstract_alt = as.character(NA),
      .keep = "none"
    )
}

process_inn <- function(filename){
  df <- read_csv(filename, show_col_types = FALSE)
  
  df |>
    mutate(
      id = id,
      institution_short = "inn",
      collection = collection,
      GLU = dplyr::case_match(
        collection,
        "11250/2980782" ~ "MGLU 1-7",
        "11250/2980784" ~ "MGLU 5-10"
      ),
      year = as.integer(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url = dc.identifier.uri,
      language = dc.language,
      full_text_available = case_when(
        str_detect(dc.description, "Full text not available") ~ "Nei",
        is.na(dc.description) ~ "Ja"
      ),
      subject = as.character(NA),
      title = dc.title,
      title_alt = as.character(NA),
      abstract = str_split_i(dc.description.abstract, "\\|\\|", 1),
      abstract_alt = str_split_i(dc.description.abstract, "\\|\\|", 2),
      .keep = "none"
    )
}

# The OsloMet files have two different column naming conventions
process_oslomet_old <- function(filename){
  df <- read_csv(filename, show_col_types = FALSE)
  
  df |>
    mutate(
      id = id,
      institution_short = "oslomet",
      collection = collection,
      GLU = dplyr::case_match(
        collection,
        "10642/6821" ~ "MGLU 1-7",
        "10642/6822" ~ "MGLU 5-10"
      ),
      year = as.integer(dc.date.issued),
      authors = dc.contributor.author,
      n_authors = as.integer(count_entries(dc.contributor.author)),
      url = dc.identifier.uri,
      language = `dc.language.iso[en_US]`,
      full_text_available = as.character(NA),
      subject = as.character(NA),
      title = `dc.title[en_US]`,
      title_alt = `dc.title.alternative[en_US]`,
      abstract = str_split_i(
        `dc.description.abstract[en_US]`,
        "\\|\\||\\r\\n\\r\\n\\r\\n",
        1
      ),
      abstract_alt = str_split_i(
        `dc.description.abstract[en_US]`,
        "\\|\\||\\r\\n\\r\\n\\r\\n",
        2
      ),
      .keep = "none"
    )
}


process_oslomet_new <- function(filename){
  df <- read_csv(filename, show_col_types = FALSE)
  
  # Detect available columns by pattern
  language_cols <- names(df)[str_detect(names(df), "^dc\\.language")]
  title_cols    <- names(df)[str_detect(names(df), "^dc\\.title($|\\[)") & !str_detect(names(df), "alternative")]
  abstract_cols <- names(df)[str_detect(names(df), "^dc\\.description\\.abstract")]
  
  df |>
    mutate(
      id = id,
      institution_short = "oslomet",
      collection = collection,
      GLU = dplyr::case_match(
        collection,
        "10642/6821" ~ "MGLU 1-7",
        "10642/6822" ~ "MGLU 5-10",
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
      
      full_text_available = as.character(NA),
      subject = as.character(NA),
      
      title = if (length(title_cols) == 0) {
        NA_character_
      } else {
        coalesce(!!!syms(title_cols))
      },
      
      title_alt = as.character(NA),
      
      abstract = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        str_split_i(coalesce(!!!syms(abstract_cols)), "\\|\\||\\r\\n\\r\\n\\r\\n", 1)
      },
      
      abstract_alt = if (length(abstract_cols) == 0) {
        NA_character_
      } else {
        str_split_i(coalesce(!!!syms(abstract_cols)), "\\|\\||\\r\\n\\r\\n\\r\\n", 2)
      },
      
      .keep = "none"
    )
}

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


