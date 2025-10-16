library(RDCOMClient)
library(tidyverse)
library(openxlsx2)


# Prepare data
library <- wb_load("data-raw/library.xlsm")
libraries <- library |> wb_to_df("bibliotek") |> as_tibble()
collections <- library |> wb_to_df("samling") |> as_tibble() |>
  filter(include == "ja")

collections <- left_join(collections, libraries, by = "institution_short")
rm(libraries, library)


# email function

send_email <- function(to_address, subject, body){
    OutApp <- COMCreate("Outlook.Application")

    outMail <- OutApp$CreateItem(0)

    outMail[["To"]] = to_address
    outMail[["subject"]] = subject
    outMail[["body"]] = body

    outMail$Send()
}

email_template <- read_file("data-raw/epost")

# SHOULD I INCLUDE EXTRA?
email_df <- collections |>
  summarise(
    institution = first(institution),
    email = first(email),
    url_base = first(url_base),
    collections = paste(paste(collection, " ", collection_url), collapse = "\n"),
    .by = "institution_short"
  ) |>
  mutate(
    to_address = email,
    subject = "Hjelp til å eksportere samlinger",
    body = as.character(str_glue(email_template)),
    .keep = "none"
  )


email_df |>
  pmap(send_email)
