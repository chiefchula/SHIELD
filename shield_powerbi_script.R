library(robotoolbox)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

# ---- 1. credentials ---------------------------------------------------------
# Do NOT use Sys.getenv() here unless you've set the variable
# for the Power BI service account. Paste the token instead.
kobo_setup(
  url   = "https://kf.kobotoolbox.org/",
  token = "0b54751849b3c8e8d7d57948f3695edd9d7d2cb8"
)

asset <- kobo_asset("adZCkYBD5roeAMPNd2NubN")
shield_cleaned <- kobo_submissions(asset)

stopifnot(is.data.frame(shield_cleaned), nrow(shield_cleaned) > 0)

# ---- 2. specs (unchanged) ---------------------------------------------------
geo_cols  <- c("province", "territory", "health_zone", "health_area")
demo_cols <- c("institution_type", "faith")

bar_specs <- list(
  "Institutional Capacity" = tribble(
    ~category,                     ~col,
    "Previous Ebola response",     "institution_involved_ebola",
    "Other epidemic response",     "involved_in_other_outbreaks",
    "Ebola training",              "leader_ebola_training",
    "Educational materials",       "ebola_iec",
    "Emergency committee",         "institution_has_ebola_emergency_response",
    "Case-processing procedures",  "ebola_sop"
  ),
  "Barriers to Engagement" = tribble(
    ~category,                ~col,
    "Lack of training",       "ebola_challenges_lack_of_training",
    "Limited funding",        "ebola_challenges_limited_funding",
    "Misinformation",         "ebola_challenges_disinformation",
    "Lack of PPE",            "ebola_challenges_lack_of_ppe",
    "Community mistrust",     "ebola_challenges_community_mistrust",
    "Poor communication",     "ebola_challenges_poor_communication",
    "Other barriers",         "ebola_challenges_other_ebola_challenges",
    "Poor coordination",      "ebola_challenges_poor_coordination",
    "Cultural barriers",      "ebola_challenges_cultural_barriers"
  ),
  "Support Required" = tribble(
    ~category,                     ~col,
    "Training",                    "support_required_education",
    "Funding",                     "support_required_funding",
    "Transport",                   "support_required_transportation",
    "PPE",                         "support_required_ppe",
    "Communication equipment",     "support_required_communication_materials",
    "IEC materials",               "support_required_iec_support",
    "Other support",               "support_required_other_support_required",
    "Coordination meetings",       "support_required_coordination_meetings"
  ),
  # KPI-only block — no bars are drawn from this in the report,
  # but the rows must exist so the KPI measures can filter to them.
  "KPI" = tribble(
    ~category,                     ~col,
    "Involved in Ebola response",  "institution_involved_ebola",
    "Trained on Ebola",            "leader_ebola_training",
    "Willing to participate",      "faith_leaders_willing"
  )
)

# ---- 3. base + builder (unchanged, with the case_when fix) ------------------
yes_no_to_int <- function(x) {
  chr <- if (is.factor(x)) {
    as.character(x)                                    # factor first
  } else if (inherits(x, "haven_labelled")) {
    as.character(haven::as_factor(x, levels = "labels"))
  } else if (is.numeric(x)) {
    return(as.integer(x > 0))
  } else if (is.logical(x)) {
    return(as.integer(x))
  } else {
    as.character(x)
  }
  
  chr <- tolower(trimws(chr))
  as.integer(chr %in% c("yes", "y", "oui", "true", "1"))
}

resp_base <- shield_cleaned %>%
  transmute(
    respondent_id = `_id`,
    interview_date,
    across(all_of(geo_cols)),
    across(all_of(demo_cols)),
    assessment = as.character(assessment)
  )


build_block <- function(chart_name, specs) {
  resp_base %>%
    bind_cols(shield_cleaned %>% select(all_of(specs$col))) %>%
    mutate(across(all_of(specs$col), yes_no_to_int)) %>%
    pivot_longer(all_of(specs$col), names_to = "col", values_to = "value") %>%
    left_join(specs, by = "col") %>%
    mutate(
      chart      = chart_name,
      sort_order = match(category, specs$category)
    ) %>%
    select(-col)
}

# ---- 4. assemble ------------------------------------------------------------
levels_prep <- c("High", "Moderate", "Low")
prep_block <- resp_base %>%                 # was: base
  crossing(category = levels_prep) %>%
  mutate(
    chart      = "Preparedness Level",
    value      = as.integer(assessment == category),
    sort_order = match(category, levels_prep)
  )

dash_long <- bind_rows(
  prep_block,
  imap_dfr(bar_specs, ~ build_block(.y, .x))
) %>%
  mutate(chart = factor(
    chart,
    levels = c(
      "Preparedness Level",
      "Institutional Capacity",
      "Barriers to Engagement",
      "Support Required",
      "KPI"
    )
  )) %>%
  arrange(chart, sort_order, respondent_id)

dash_long