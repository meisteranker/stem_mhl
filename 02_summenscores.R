#### Load packages

library(tidyverse)
library(easystats)

#### Build new variables

### Ethniticy

# Ethnische Variablen in Prioritätsreihenfolge
eth_vars <- c(
  "eth_white", "eth_black", "eth_latinx", "eth_asian",
  "eth_indigenous_native", "eth_pacific", "eth_unspecified")

# Labels in derselben Reihenfolge
eth_labels <- c(
  "White", "Black", "Latinx", "Asian",
  "Indigenous/Native", "Pacific", "Unspecified")

# Neue Variable `ethnicity`
df_all <- df_all %>%
  mutate(ethnicity = NA_character_) 

for (i in seq_along(eth_vars)) {
  df_all <- df_all %>%
    mutate(
      ethnicity = if_else(
        is.na(ethnicity) & .data[[eth_vars[i]]] == 2,
        eth_labels[i],
        ethnicity))
}

### Gender

# Gender-Variablen + Labels in der gewünschten Reihenfolge
gender_vars <- c(
  "gender_female", "gender_male", "gender_cis", "gender_dyadic",
  "gender_inter", "gender_nonbinary", "gender_questioning",
  "gender_trans", "gender_na", "gender_other"
)

gender_labels <- c(
  "Female", "Male", "Cis", "Dyadic",
  "Intersex", "Nonbinary", "Questioning",
  "Trans", "Prefer not to say", "Other"
)

# Neue Variable "gender_combined" mit allen gecheckten Labels (durch Semikolon getrennt)
df_all <- df_all %>%
  mutate(
    gender = pmap_chr(
      select(., all_of(gender_vars)),
      ~ {
        checked <- c(...)
        labels <- gender_labels[checked == 2]
        str_c(labels, collapse = "; ")
      }))

# gender simple

df_all <- df_all %>%
  mutate(
    gender_simple = case_when(
      str_detect(gender, "Male") & !str_detect(gender, "Nonbinary|Trans|Questioning|Other|Female") ~ "Male",
      str_detect(gender, "Female") & !str_detect(gender, "Nonbinary|Trans|Questioning|Other|Male") ~ "Female",
      str_detect(gender, "Nonbinary") ~ "Nonbinary",
      str_detect(gender, "Trans") ~ "Trans",
      str_detect(gender, "Other") ~ "Other",
      str_detect(gender, "Prefer not to say") ~ "Prefer not to say",
      TRUE ~ NA_character_))

# gender dichotomous

df_all <- df_all %>%
  mutate(
    gender_binary = case_when(
      gender_simple == "Male" ~ "Male",
      gender_simple == "Female" ~ "Female",
      TRUE ~ NA_character_))

### Position

# Relevante Variablen in gewünschter Priorität
position_vars <- c(
  "pos_professor", "pos_junior_prof", "pos_priv_docent", "pos_group_leader", 
  "pos_postdoc", "pos_akad_council", "pos_other", "pos_other_text",
  "pos_assist_prof", "pos_assoc_prof", "pos_full_prof", "pos_dist_prof",
  "pos_lecturer", "pos_instructor", "pos_assist_teach_prof", 
  "pos_assoc_teach_prof", "pos_teach_prof", 
  "pos_assist_res_prof", "pos_assoc_res_prof", "pos_res_prof",
  "pos_prof"
)

# Neue Variable 'position' erstellen

df_all <- df_all %>%
  mutate(position = pmap_chr(across(all_of(position_vars)), function(...) {
    vals <- c(...)
    first_checked <- which(vals == 2)[1]  # erste "Checked" Position finden
    if (!is.na(first_checked)) {
      return(position_vars[first_checked])
    } else {
      return(NA_character_)
    }
  }))

# Position simple

library(dplyr)

df_all <- df_all %>%
  mutate(
    position_simple = case_when(
      position %in% c("pos_professor", "pos_full_prof", "pos_dist_prof", "pos_prof") ~ "Professor",
      position %in% c("pos_assoc_prof", "pos_assoc_res_prof", "pos_assoc_teach_prof") ~ "Associate Professor",
      position %in% c("pos_assist_prof", "pos_assist_res_prof", "pos_assist_teach_prof") ~ "Assistant Professor",
      position %in% c("pos_junior_prof", "pos_priv_docent") ~ "Junior Professor",
      position %in% c("pos_group_leader", "pos_akad_council") ~ "Group Leader / Academic Staff",
      position %in% c("pos_lecturer", "pos_instructor", "pos_teach_prof") ~ "Lecturer / Instructor",
      position %in% c("pos_postdoc") ~ "Postdoc",
      position %in% c("pos_other") ~ "Other",
      TRUE ~ NA_character_
    )
  )


### field

field_labels <- c(
  field_biology     = "Biology",
  field_chemistry   = "Chemistry",
  field_engineering = "Engineering (gen.)",
  field_aerospace   = "Aerospace",
  field_biomedicine = "Biomedicine",
  field_comp_eng    = "Computer Eng.",
  field_chem_eng    = "Chemical Eng.",
  field_civil_eng   = "Civil Eng.",
  field_electrical  = "Electrical Eng.",
  field_env_eng     = "Environmental Eng.",
  field_nuclear     = "Nuclear Eng.",
  field_materials   = "Materials",
  field_math        = "Math",
  field_physics     = "Physics",
  field_cs          = "Computer Science",
  field_mechanical  = "Mechanical Eng.",
  field_other       = "Other"
)

# Extrahiere nur Variablennamen (technische Namen)
field_vars <- names(field_labels)

# Erstelle neue "field" Variable mit Labels
df_all <- df_all %>%
  mutate(
    field = pmap_chr(across(all_of(field_vars)), function(...) {
      vals <- c(...)
      checked_fields <- field_labels[which(vals == 2)]
      if (length(checked_fields) > 0) {
        return(str_c(checked_fields, collapse = ", "))
      } else {
        return(NA_character_)
      }
    })
  )

# field simple

library(tidyverse)

df_all <- df_all %>%
  mutate(
    field_simple = case_when(
      str_detect(field, "Biology") ~ "Biology",
      str_detect(field, "Chemistry") ~ "Chemistry",
      str_detect(field, "Physics") ~ "Physics",
      str_detect(field, "Math") ~ "Math",
      str_detect(field, "Computer Science|Computer Eng.") ~ "Computer Science",
      str_detect(field, "Electrical Eng.") ~ "Electrical Engineering",
      str_detect(field, "Mechanical Eng.") ~ "Mechanical Engineering",
      str_detect(field, "Civil Eng.") ~ "Civil Engineering",
      str_detect(field, "Environmental Eng.") ~ "Environmental Engineering",
      str_detect(field, "Chemical Eng.") ~ "Chemical Engineering",
      str_detect(field, "Nuclear Eng.") ~ "Nuclear Engineering",
      str_detect(field, "Materials") ~ "Materials Science",
      str_detect(field, "Engineering") ~ "Engineering (Other)",
      str_detect(field, "Aerospace") ~ "Aerospace",
      str_detect(field, "Biomedicine") ~ "Biomedicine",
      str_detect(field, "Other") ~ "Other",
      TRUE ~ NA_character_))

### study

study_labels <- c(
  study_biology     = "Biology",
  study_chemistry   = "Chemistry",
  study_aerospace   = "Aerospace",
  study_biomedicine = "Biomedicine",
  study_comp_eng    = "Computer Eng.",
  study_chem_eng    = "Chemical Eng.",
  study_civil_eng   = "Civil Eng.",
  study_electrical  = "Electrical Eng.",
  study_env_eng     = "Environmental Eng.",
  study_nuclear     = "Nuclear Eng.",
  study_materials   = "Materials",
  study_math        = "Math",
  study_physics     = "Physics",
  study_cs          = "Computer Science",
  study_other       = "Other"
)

# Variablennamen extrahieren
study_vars <- names(study_labels)

# Neue Variable "study" erstellen
df_all <- df_all %>%
  mutate(
    study = pmap_chr(across(all_of(study_vars)), function(...) {
      vals <- c(...)
      checked <- study_labels[which(vals == 2)]
      if (length(checked) > 0) {
        str_c(checked, collapse = ", ")
      } else {
        NA_character_}}))

# study simple

library(tidyverse)

df_all <- df_all %>%
  mutate(
    study_simple = case_when(
      str_detect(study, "Biology") ~ "Biology",
      str_detect(study, "Chemistry") ~ "Chemistry",
      str_detect(study, "Physics") ~ "Physics",
      str_detect(study, "Math") ~ "Math",
      str_detect(study, "Computer Science|Computer Eng.") ~ "Computer Science",
      str_detect(study, "Electrical Eng.") ~ "Electrical Engineering",
      str_detect(study, "Mechanical Eng.") ~ "Mechanical Engineering",
      str_detect(study, "Civil Eng.") ~ "Civil Engineering",
      str_detect(study, "Environmental Eng.") ~ "Environmental Engineering",
      str_detect(study, "Chemical Eng.") ~ "Chemical Engineering",
      str_detect(study, "Nuclear Eng.") ~ "Nuclear Engineering",
      str_detect(study, "Materials") ~ "Materials Science",
      str_detect(study, "Engineering") ~ "Engineering (Other)",
      str_detect(study, "Aerospace") ~ "Aerospace",
      str_detect(study, "Biomedicine") ~ "Biomedicine",
      str_detect(study, "Other") ~ "Other",
      TRUE ~ NA_character_))

### who

df_all <- df_all |>
  mutate(across(
    starts_with("who5_"),
    ~ as.numeric(na_if(., -9))
  )) |>
  mutate(
    who5_raw  = rowSums(across(starts_with("who5_")), na.rm = TRUE),
    who5_norm = who5_raw * 4)

df_all <- df_all %>%
  mutate(who5_rev = 100 - who5_norm)

### ghq

ghq_items_r <- c(
  "ghq12_useful",
  "ghq12_deal_problems",
  "ghq12_contentment",
  "ghq12_daily_duties"
)

# 2. Erstelle invertierte Versionen mit Suffix _r
df_all <- df_all %>%
  mutate(across(all_of(ghq_items_r), ~ ifelse(is.na(.x), NA, 5 - .x), .names = "{.col}_r"))

# 3. Ersetze Original durch invertierte für Scoring
ghq_all_vars <- c(
  setdiff(c(
    "ghq12_sleep_worries", "ghq12_pressure", "ghq12_concentration", "ghq12_useful",
    "ghq12_deal_problems", "ghq12_decision_difficulty", "ghq12_overwhelmed",
    "ghq12_contentment", "ghq12_daily_duties", "ghq12_depressed",
    "ghq12_low_confidence", "ghq12_worthless"
  ), ghq_items_r),
  paste0(ghq_items_r, "_r")
)

# 4. Berechne Likert-Score (0–1–2–3), bei 1:0, 2:1, 3:2, 4:3
df_all <- df_all %>%
  rowwise() %>%
  mutate(
    ghq12_likert = sum(c_across(all_of(ghq_all_vars)) - 1, na.rm = TRUE)
  ) %>%
  ungroup()

# 5. Berechne GHQ-Score (0–0–1–1), bei 1/2: 0, 3/4: 1
df_all <- df_all %>%
  rowwise() %>%
  mutate(
    ghq12_binary = sum(ifelse(c_across(all_of(ghq_all_vars)) >= 3, 1, 0), na.rm = TRUE)
  ) %>%
  ungroup()

### phq

df_all <- df_all %>%
  mutate(
    # Umkodierung: 1–4 → 0–3
    across(
      c(phq4_low_interest, phq4_feeling_down, phq4_nervous, phq4_worry_control),
      ~ .x - 1,
      .names = "{.col}_scored"
    ),
    # Subscores
    phq4_depression = phq4_low_interest_scored + phq4_feeling_down_scored,
    phq4_anxiety    = phq4_nervous_scored + phq4_worry_control_scored,
    # Gesamtscore
    phq4_total      = phq4_depression + phq4_anxiety)

### GAD7

df_all <- df_all %>%
  mutate(across(
    c(
      gad7_excess_worry,
      gad7_relax_difficulty,
      gad7_restlessness,
      gad7_irritability,
      gad7_fear_bad_happen
    ),
    ~ as.numeric(.) - 1
  ))

df_all <- df_all %>%
  mutate(
    gad = rowSums(across(
      c(
        gad7_excess_worry,
        gad7_relax_difficulty,
        gad7_restlessness,
        gad7_irritability,
        gad7_fear_bad_happen), ~ as.numeric(.)), na.rm = TRUE))

### fsozuk6

df_all <- df_all %>%
  mutate(
    fsozuk6 = rowMeans(across(
      c(
        soz_support_understood,
        soz_support_trusted,
        soz_support_borrow,
        soz_support_activities,
        soz_support_illness,
        soz_support_distress
      ), ~ as.numeric(.)), na.rm = TRUE))

### brs

df_all <- df_all %>%
  # Umkodieren der invertierten Items
  mutate(
    brs_handle_stress_r = 6 - as.numeric(brs_handle_stress),
    brs_get_through_r = 6 - as.numeric(brs_get_through),
    brs_take_long_to_recover_r = 6 - as.numeric(brs_take_long_to_recover)
  ) %>%
  # Gesamt-Score berechnen (alle Items numerisch + invertierte verwenden)
  mutate(
    brs_score = rowMeans(
      tibble(
        as.numeric(brs_recover_quickly),
        brs_handle_stress_r,
        as.numeric(brs_recover_fast),
        as.numeric(brs_back_to_normal),
        brs_get_through_r,
        brs_take_long_to_recover_r
      ), na.rm = TRUE))

### mhlq

mhlq_positive <- c(
  "mhlq_sport", "mhlq_depr_symp", "mhlq_schizo_wahn", "mhlq_help_fam",
  "mhlq_help_psy", "mhlq_sleep", "mhlq_self_help_psy", "mhlq_anx_panik",
  "mhlq_psych_help", "mhlq_friend_help", "mhlq_nutri", "mhlq_interest",
  "mhlq_duration", "mhlq_depr_real", "mhlq_drugs", "mhlq_thoughts",
  "mhlq_fun", "mhlq_schizo_hall", "mhlq_stress", "mhlq_self_psych")

mhlq_negative <- c(
  "mhlq_no_behav",     # mental disorders don’t affect behavior
  "mhlq_income",       # income affects mental health (if phrased misleading)
  "mhlq_nonjdm",       # possibly a judgmental attitude
  "mhlq_no_affect",    # mental disorders don’t affect emotions
  "mhlq_early",        # early treatment irrelevant (if phrased that way)
  "mhlq_adults_only",  # only adults get mental illness
  "mhlq_helpless",     # feeling helpless = norm
  "mhlq_alc"           # alcohol ≠ mental health issue (if misbelief)
  )

library(dplyr)

# 1. Invertiere negativ gepolte Items (Skala: 1–5)
df_all <- df_all %>%
  mutate(across(all_of(mhlq_negative), ~ ifelse(is.na(.x), NA, 6 - .x), .names = "{.col}_r"))

# 2. Kombiniere alle Items in einem Score
mhlq_all_items <- c(mhlq_positive, paste0(mhlq_negative, "_r"))

df_all <- df_all %>%
  rowwise() %>%
  mutate(mhlq_score = mean(c_across(all_of(mhlq_all_items)), na.rm = TRUE)) %>%
  ungroup()

## sub scales

mhlq_knowledge <- c(
  "mhlq_depr_symp", "mhlq_schizo_wahn", "mhlq_anx_panik",
  "mhlq_schizo_hall", "mhlq_thoughts", "mhlq_duration", "mhlq_depr_real")

mhlq_stereotypes <- c(
  "mhlq_no_affect_r", "mhlq_no_behav_r", "mhlq_adults_only_r",
  "mhlq_nonjdm_r", "mhlq_income_r", "mhlq_alc_r", "mhlq_helpless_r", "mhlq_drugs")

mhlq_help_skills <- c(
  "mhlq_help_fam", "mhlq_help_psy", "mhlq_self_help_psy")

mhlq_self_help <- c(
  "mhlq_sport", "mhlq_sleep", "mhlq_nutri", "mhlq_fun")

df_all <- df_all %>%
  rowwise() %>%
  mutate(
    mhlq_knowledge     = mean(c_across(all_of(mhlq_knowledge)), na.rm = TRUE),
    mhlq_stereotypes   = mean(c_across(all_of(mhlq_stereotypes)), na.rm = TRUE),
    mhlq_help_skills   = mean(c_across(all_of(mhlq_help_skills)), na.rm = TRUE),
    mhlq_self_help     = mean(c_across(all_of(mhlq_self_help)), na.rm = TRUE)
  ) %>%
  ungroup()

### mhls

mhls_invert <- c(
  "mhls3_break_danger",  # hinzufügen!
  "mhls4_control",
  "mhls4_weakness",
  "mhls4_not_med",
  "mhls4_danger",
  "mhls4_avoidance",
  "mhls5_evening"
)

df_all <- df_all %>%
  mutate(across(
    all_of(mhls_invert),
    ~ ifelse(is.na(.), NA_real_, 6 - .),
    .names = "{.col}_r"
  ))

mhls_recognition <- c("mhls1_socphob", "mhls1_gad", "mhls1_depr")

mhls_risk_knowledge <- c(
  "mhls1_pers", "mhls1_dysth", "mhls1_agora", "mhls1_bipol", 
  "mhls1_subs", "mhls1_fem_more", "mhls1_male_anx")

mhls_help <- c("mhls2_sleep", "mhls2_avoid", "mhls3_kvt", "mhls3_break_danger_r", "mhls3_break_support")

mhls_info <- c("mhls4_info", "mhls4_digital", "mhls4_apt", "mhls4_sources")

mhls_attitudes <- c("mhls4_control_r", "mhls4_weakness_r", "mhls4_not_med_r", "mhls4_danger_r")

df_all <- df_all %>%
  rowwise() %>%
  mutate(
    # Subskalen gemäß MHLS-Struktur
    mhls_recog_disorders         = mean(c_across(all_of(mhls_recognition)), na.rm = TRUE),
    mhls_risk_factors_causes     = mean(c_across(all_of(mhls_risk_knowledge)), na.rm = TRUE),
    mhls_help_available          = mean(c_across(all_of(mhls_help)), na.rm = TRUE),
    mhls_info_seeking            = mean(c_across(all_of(mhls_info)), na.rm = TRUE),
    mhls_attitudes_help_seeking = mean(c_across(all_of(mhls_attitudes)), na.rm = TRUE),
    mhls_total = mean(c(
      mhls_recog_disorders,
      mhls_risk_factors_causes,
      mhls_help_available,
      mhls_info_seeking,
      mhls_attitudes_help_seeking
    ), na.rm = TRUE)
  ) %>%
  ungroup()

### mhsas

mhsas_invert <- c(
  "mhsas_important",
  "mhsas_good",
  "mhsas_healing",
  "mhsas_satisfying",
  "mhsas_desirable")

# Invertiere die negativ gepolten Items (7er Skala: 8 - x)
df_all <- df_all %>%
  mutate(across(
    all_of(mhsas_invert),
    ~ ifelse(is.na(.), NA_real_, 6 - .),
    .names = "{.col}_r"
  ))

# Neue Vektor mit allen Items (invers & nicht-invers)
mhsas_items <- c(
  "mhsas_important_r",
  "mhsas_good_r",
  "mhsas_healing_r",
  "mhsas_satisfying_r",
  "mhsas_desirable_r",
  "mhsas_useful",
  "mhsas_healthy",
  "mhsas_effective",
  "mhsas_empowering")

# Berechne den Mittelwert als Gesamtscore
df_all <- df_all %>%
  rowwise() %>%
  mutate(mhsas = mean(c_across(all_of(mhsas_items)), na.rm = TRUE)) %>%
  ungroup()

