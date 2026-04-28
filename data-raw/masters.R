library(dplyr)
library(purrr)
library(readr)
library(stringr)

source("R/utils.R")
source("R/process_masters.R")


filenames <- list.files(path = "data-raw", pattern = "\\.csv$", full.names = TRUE)
filenames <- filenames[!str_detect(filenames, "hivolda")]  # dropp den korrupte fila
nord_file <- list.files(path = "data-raw", pattern = "nord.*\\.xlsx$", full.names = TRUE)

# HIOF
hiof_files <- filenames[str_detect(filenames, "hiof")]
hiof <- map(hiof_files, process_hiof) |> bind_rows()
validate_masters(hiof)

# HVL
hvl_files <- filenames[str_detect(filenames, "hvl")]
hvl <- map(hvl_files, process_hvl) |> bind_rows()
validate_masters(hvl)

# INN
inn_files <- filenames[str_detect(filenames, "inn_")]
inn <- map(inn_files, process_inn) |> bind_rows()
validate_masters(inn)

# OSLOMET – gamle og nye
oslomet_old_files <- filenames[str_detect(filenames, "oslomet_.*5-10")]
oslomet_new_files <- filenames[str_detect(filenames, "oslomet_.*1-7")]

oslomet <- map(oslomet_old_files, process_oslomet_old) |> bind_rows()
oslomet_new <- map(oslomet_new_files, process_oslomet_new) |> bind_rows()
validate_masters(oslomet)
validate_masters(oslomet_new)

# USN 
usn_files <- filenames[str_detect(filenames, "usn_")]
usn <- map(usn_files, process_usn) |> bind_rows()
validate_masters(usn)

# UiA 
uia_files <- filenames[str_detect(filenames, "uia_")]
uia <- map(uia_files, process_uia) |> bind_rows()
validate_masters(uia)

# UiT
uit_files <- filenames[str_detect(filenames, "UiT_")]
uit <- map(uit_files, process_uit) |> bind_rows()
validate_masters(uit)

# Nord (xlsx)
nord <- map(nord_file, process_nord) |> bind_rows()
validate_masters(nord)


masters <- list(
  hiof,
  hvl,
  inn,
  oslomet,
  oslomet_new,
  usn,
  uia,
  uit,
  nord
) |> reduce(bind_rows)

validate_masters(masters)

write_rds(masters, file = "data/masters.RDS")

