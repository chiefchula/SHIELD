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
library(stringr)

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
  mutate(across(where(is.labelled), ~ haven::as_factor(.x, levels = "labels"))) |> 
  mutate(across(.cols = c("health_area", "health_zone", "village", "respondent_name"), .fns = ~ str_to_title(.))) |> 
  mutate(phone_number_cleaned = str_remove_all(phone_number, "[:space:]\\+"), .after = phone_number)
