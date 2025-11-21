library(dplyr)
library(purrr)
library(readr)
library(stringr)

source("R/process_masters.R")
source("R/validate_masters.R")

filenames <- list.files(path = "data-raw", pattern = "\\.csv$", full.names = TRUE)
filenames <- filenames[!str_detect(filenames, "hivolda")]  # dropp den korrupte fila

# HIOF: to filer
hiof_files <- filenames[str_detect(filenames, "hiof")]
hiof <- map(hiof_files, process_hiof) |> bind_rows()

# HVL: flere år
hvl_files <- filenames[str_detect(filenames, "hvl")]
hvl <- map(hvl_files, process_hvl) |> bind_rows()

# INN
inn_files <- filenames[str_detect(filenames, "inn_")]
inn <- map(inn_files, process_inn) |> bind_rows()

# OSLOMET – gamle og nye
oslomet_old_files <- filenames[str_detect(filenames, "oslomet_.*5-10")]
oslomet_new_files <- filenames[str_detect(filenames, "oslomet_.*1-7")]

oslomet <- map(oslomet_old_files, process_oslomet_old) |> bind_rows()
oslomet_new <- map(oslomet_new_files, process_oslomet_new) |> bind_rows()

# USN (når du har laget process_usn)
usn_files <- filenames[str_detect(filenames, "usn_")]
usn <- map(usn_files, process_usn) |> bind_rows()

# UiA (når du har laget process_uia)
# uia_files <- filenames[str_detect(filenames, "uia_")]
# uia <- map(uia_files, process_uia) |> bind_rows()

# UiT (når du har laget process_uit)
# uit_files <- filenames[str_detect(filenames, "uit_")]
# uit <- map(uit_files, process_uit) |> bind_rows()



masters <- list(
  hiof,
  hvl,
  inn,
  oslomet,
  oslomet_new,
  usn
  # uia,
  # uit
) |> reduce(bind_rows)

validate_masters(masters)

write_rds(masters, file = "data/masters.RDS")

