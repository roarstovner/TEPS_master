library(dplyr)
library(purrr)
library(readr)
library(stringr)
#source("R/variable_defs.R")

source("R/process_masters.R")
source("R/validate_masters.R")

filenames <- list.files(path = "data-raw", pattern = "\\.csv$", full.names = TRUE)
filenames <- filenames[c(1:2,4:19)] #skip the file hivolda because it's a corrupted csv. 

hiof <- map(filenames[1:2], process_hiof) |> reduce(bind_rows)
hvl <- map(filenames[3:5], process_hvl) |> reduce(bind_rows)
oslomet <- map(filenames[c(9,11)], process_oslomet_old) |> reduce(bind_rows) # et av årene med OsloMet-data var annerledes
oslomet_new <- filenames[10] |> process_oslomet_new()

validate_masters(hiof)
validate_masters(hvl)
validate_masters(oslomet)
validate_masters(oslomet_new)

# Til slutt, samle alt i 'masters' og skriv den til "data/masters.RDS"
masters <- list(hiof, hvl, oslomet, oslomet_new) |> reduce(bind_rows)
write_rds(masters, file = "data/masters.RDS")
