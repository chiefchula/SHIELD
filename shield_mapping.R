library(dplyr)
library(tidyr)
library(robotoolbox)
library(readr)
library(readxl)
library(rlang)
library(janitor)
library(labelled)
library(ggplot2)
library(leaflet)

# credentials
usethis::edit_r_environ()

kobo_url <- Sys.getenv("KOBOTOOLBOX_URL")
kobo_token <- Sys.getenv("KOBOTOOLBOX_TOKEN")

form_uid <- 'adZCkYBD5roeAMPNd2NubN'

kobo_lang(form_uid)

shield_raw <- kobo_data(form_uid, lang = "English (en)")

# Cleaning ---------------------------------------------------------------------
shield_cleaned <- shield_raw |> 
  remove_constant() |> 
  mutate(across(where(is.labelled), ~ haven::as_factor(.x, levels = "labels")))
