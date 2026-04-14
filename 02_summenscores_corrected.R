#### Load packages

library(tidyverse)
library(easystats)

### data frame laden

df_all <- data_read("data_all_tidy.csv")

### -9 in NA

df_all <- df_all |>
  mutate(across(
    everything(),
    ~ ifelse(.x == -9 | .x == "-9", NA, .x)
  ))

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
# FIX [minor]: pick() statt across() in pmap_chr (deprecated ohne Funktion)
df_all <- df_all %>%
  mutate(
    gender = pmap_chr(
      pick(all_of(gender_vars)),
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

# FIX [minor]: pos_other_text entfernt (Character-Variable, == 2 Vergleich sinnlos)
position_vars <- c(
  "pos_professor", "pos_junior_prof", "pos_priv_docent", "pos_group_leader", 
  "pos_postdoc", "pos_akad_council", "pos_other",
  "pos_assist_prof", "pos_assoc_prof", "pos_full_prof", "pos_dist_prof",
  "pos_lecturer", "pos_instructor", "pos_assist_teach_prof", 
  "pos_assoc_teach_prof", "pos_teach_prof", 
  "pos_assist_res_prof", "pos_assoc_res_prof", "pos_res_prof",
  "pos_prof"
)

# Neue Variable 'position' erstellen
# FIX [minor]: pick() statt across() ohne Funktion
df_all <- df_all %>%
  mutate(position = pmap_chr(pick(all_of(position_vars)), function(...) {
    vals <- c(...)
    first_checked <- which(vals == 2)[1]
    if (!is.na(first_checked)) {
      return(position_vars[first_checked])
    } else {
      return(NA_character_)
    }
  }))

# Position clean

df_all <- df_all %>%
  mutate(
    position_clean = case_when(
      str_detect(position, "full_prof|professor|dist_prof|^pos_prof$") ~ "Professor",
      str_detect(position, "junior_prof|assist_prof") ~ "Assistant/Junior Professor",
      str_detect(position, "assoc_prof") ~ "Associate Professor",
      str_detect(position, "teach_prof|lecturer|instructor") ~ "Lecturer/Instructor",
      str_detect(position, "postdoc|group_leader") ~ "Postdoc / Group Leader",
      str_detect(position, "akad_council|priv_docent") ~ "Priv. Dozent / Council",
      str_detect(position, "res_prof") ~ "Research Professor",
      is.na(position) | str_detect(position, "other") ~ "Other / Unknown",
      TRUE ~ "Other / Unknown"),
    position_clean = factor(position_clean))

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
# FIX [minor]: pick() statt across()
df_all <- df_all %>%
  mutate(
    field = pmap_chr(pick(all_of(field_vars)), function(...) {
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
# FIX [minor]: Redundantes library(tidyverse) entfernt

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

# field grouped

df_all <- df_all %>%
  mutate(
    field_grouped = case_when(
      str_detect(field_simple, "Physics|Nuclear Engineering") ~ "Physics",
      str_detect(field_simple, "Chemistry|Chemical Engineering") ~ "Chemistry",
      str_detect(field_simple, "Biology|Biomedicine") ~ "Biology",
      str_detect(field_simple, "Math|Computer Science") ~ "Math & CS",
      str_detect(field_simple, "Materials Science") ~ "Materials Science",
      str_detect(field_simple, "Engineering|Electrical Engineering|Mechanical Engineering|Civil Engineering|Aerospace|Environmental Engineering") ~ "Engineering",
      TRUE ~ "Other / Interdisciplinary"),
    field_grouped = factor(field_grouped,
                           levels = sort(c("Physics", "Chemistry", "Biology", "Math & CS", "Engineering", "Materials Science", "Other / Interdisciplinary"))))

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
# FIX [minor]: pick() statt across()
df_all <- df_all %>%
  mutate(
    study = pmap_chr(pick(all_of(study_vars)), function(...) {
      vals <- c(...)
      checked <- study_labels[which(vals == 2)]
      if (length(checked) > 0) {
        str_c(checked, collapse = ", ")
      } else {
        NA_character_}}))

# study simple
# FIX [minor]: Redundantes library(tidyverse) entfernt

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

###############################################################################
### SUMMENSCORES
###############################################################################

### WHO-5
# Codebook: 1 = "die ganze Zeit" ... 6 = "zu keinem Zeitpunkt"
# Transformation: 6 - x ergibt 0-5 Skala (5 = maximales Wohlbefinden) --> korrekt

## Items umkodieren

df_all <- df_all %>%
  mutate(across(
    c(who5_positive_mood, who5_relaxed, who5_energetic, who5_restful_sleep, who5_interest_life),
    ~ 6 - .x
  ))

## Summenscore berechnen

df_all <- df_all %>%
  mutate(
    who5_raw = rowSums(
      select(., who5_positive_mood, who5_relaxed, who5_energetic, who5_restful_sleep, who5_interest_life),
      na.rm = TRUE
    ),
    who5_raw = if_else(
      rowSums(!is.na(select(., who5_positive_mood, who5_relaxed, who5_energetic, who5_restful_sleep, who5_interest_life))) == 0,
      NA_real_,
      who5_raw
    )
  )

## Normwerte berechnen

df_all <- df_all %>%
  mutate(who5_norm = who5_raw * 4)

### GHQ-12
# Codebook: 1-4 Skala, Likert-Scoring: (Wert - 1) ergibt 0-3, Summe 0-36

## Items umkodieren

ghq_items <- c(
  "ghq12_sleep_worries", "ghq12_pressure", "ghq12_concentration", "ghq12_useful",
  "ghq12_deal_problems", "ghq12_decision_difficulty", "ghq12_overwhelmed",
  "ghq12_contentment", "ghq12_daily_duties", "ghq12_depressed",
  "ghq12_low_confidence", "ghq12_worthless"
)

df_all <- df_all %>%
  mutate(across(
    all_of(ghq_items),
    ~ .x - 1
  ))

## Summenwert berechnen (Likert)

df_all <- df_all %>%
  mutate(
    ghq12_sum = rowSums(
      select(., all_of(ghq_items)),
      na.rm = TRUE
    ),
    ghq12_sum = if_else(
      rowSums(!is.na(select(., all_of(ghq_items)))) == 0,
      NA_real_,
      ghq12_sum
    )
  )


### PHQ-4
# Codebook: 1 = "überhaupt nicht" ... 4 = "beinahe jeden Tag"
# Scoring: Wert - 1 ergibt 0-3, Summe 0-12

## Variablen umkodieren

df_all <- df_all %>%
  mutate(
    across(
      c(phq4_low_interest, phq4_feeling_down, phq4_nervous, phq4_worry_control),
      ~ as.numeric(.) - 1,
      .names = "{.col}_scored"
    )
  )   # FIX [Syntax]: Schließende Klammer für mutate() hinzugefügt

## Summenscore

df_all <- df_all %>%
  mutate(
    phq4_total = rowSums(
      across(c(phq4_low_interest_scored, phq4_feeling_down_scored,
               phq4_nervous_scored, phq4_worry_control_scored)),
      na.rm = TRUE
    ),
    phq4_total = if_else(
      rowSums(!is.na(across(c(phq4_low_interest_scored, phq4_feeling_down_scored,
                              phq4_nervous_scored, phq4_worry_control_scored)))) == 0,
      NA_real_,
      phq4_total)
  )   # FIX [Syntax]: Schließende Klammer für mutate() hinzugefügt

### GAD-7
# FIX [kritisch]: Alle 7 Items einbeziehen.
# Die ersten 2 GAD-7 Items entsprechen den PHQ-4-Angst-Items (phq4_nervous, phq4_worry_control).
# Diese wurden oben bereits mit _scored-Suffix umkodiert (0-3).
# Die restlichen 5 Items (gad7_*) werden hier in-place umkodiert (ebenfalls zu 0-3).
# Für den GAD-7-Gesamtscore werden die _scored Varianten der PHQ-4 Items verwendet.

gad_items_extra <- c(
  "gad7_excess_worry",
  "gad7_relax_difficulty",
  "gad7_restlessness",
  "gad7_irritability",
  "gad7_fear_bad_happen"
)

df_all <- df_all |>
  mutate(across(
    all_of(gad_items_extra),
    ~ as.numeric(.) - 1
  ))

# Alle 7 GAD-7 Items für den Summenscore (Range: 0-21)
gad_items_all <- c(
  "phq4_nervous_scored",       # GAD-7 Item 1 (aus PHQ-4)
  "phq4_worry_control_scored", # GAD-7 Item 2 (aus PHQ-4)
  "gad7_excess_worry",         # GAD-7 Item 3
  "gad7_relax_difficulty",     # GAD-7 Item 4
  "gad7_restlessness",         # GAD-7 Item 5
  "gad7_irritability",         # GAD-7 Item 6
  "gad7_fear_bad_happen"       # GAD-7 Item 7
)

df_all <- df_all |>
  rowwise() |>
  mutate(
    gad = {
      vals <- c_across(all_of(gad_items_all))
      if (all(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE)
    }
  ) |>
  ungroup()

### F-SOZUK6

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

### BRS
# Codebook (Smith et al., 2008): 6 Items, 1-5 Skala
# Positiv formuliert (hohe Zustimmung = hohe Resilienz): Items 1, 3, 5
# Negativ formuliert (hohe Zustimmung = geringe Resilienz): Items 2, 4, 6
#
# FIX [kritisch]: Korrekte Zuordnung der invertierten Items:
#   Item 1: brs_recover_quickly         -> positiv (NICHT invertieren)
#   Item 2: brs_handle_stress           -> negativ (invertieren) ✓
#   Item 3: brs_recover_fast            -> positiv (NICHT invertieren)
#   Item 4: brs_back_to_normal          -> negativ (invertieren) ✓ NEU
#   Item 5: brs_get_through             -> positiv (NICHT invertieren) ✓ KORRIGIERT
#   Item 6: brs_take_long_to_recover    -> negativ (invertieren) ✓

df_all <- df_all %>%
  mutate(
    brs_handle_stress_r        = 6 - as.numeric(brs_handle_stress),        # Item 2 (negativ)
    brs_back_to_normal_r       = 6 - as.numeric(brs_back_to_normal),       # Item 4 (negativ) - NEU
    brs_take_long_to_recover_r = 6 - as.numeric(brs_take_long_to_recover)  # Item 6 (negativ)
  ) %>%
  mutate(
    brs_score = rowMeans(
      tibble(
        as.numeric(brs_recover_quickly),     # Item 1 (positiv)
        brs_handle_stress_r,                 # Item 2 (invertiert)
        as.numeric(brs_recover_fast),        # Item 3 (positiv)
        brs_back_to_normal_r,                # Item 4 (invertiert) - FIX
        as.numeric(brs_get_through),         # Item 5 (positiv)   - FIX: nicht mehr invertiert
        brs_take_long_to_recover_r           # Item 6 (invertiert)
      ), na.rm = TRUE))

### MHLQ

   ### MHLQ
# 29 Items in DE, 16 Items in US. Nur die 16 gemeinsamen Items gehen in den Score ein.
# Fixes:
#   - mhlq_brain hinzugefügt (fehlte komplett)
#   - mhlq_depr_real: sample-spezifische Invertierung (DE: negativ, US: positiv)

# 1. Positiv gepolte Items (hohe Zustimmung = hohe MHL)
mhlq_positive <- c(
    "mhlq_sport", "mhlq_depr_symp", "mhlq_schizo_wahn", "mhlq_help_fam",
    "mhlq_help_psy", "mhlq_sleep", "mhlq_self_help_psy", "mhlq_anx_panik",
    "mhlq_psych_help", "mhlq_friend_help", "mhlq_nutri", "mhlq_interest",
    "mhlq_duration", "mhlq_drugs", "mhlq_thoughts", "mhlq_fun",
    "mhlq_schizo_hall", "mhlq_stress", "mhlq_self_psych",
    "mhlq_alc", "mhlq_nonjdm", "mhlq_early",
    "mhlq_brain"    # FIX: fehlte komplett
)

# 2. Negativ gepolte Items (hohe Zustimmung = niedrige MHL)
#    FIX: mhlq_depr_real entfernt (wird separat behandelt wegen unterschiedlichem Inhalt DE/US)
mhlq_negative <- c(
    "mhlq_no_behav",
    "mhlq_income",
    "mhlq_no_affect",
    "mhlq_adults_only",
    "mhlq_helpless"
)

# 3. DE-only Items ausschließen (13 Items + 2 _r Versionen)
exclude_vars <- c(
    "mhlq_depr_symp", "mhlq_schizo_wahn", "mhlq_anx_panik", "mhlq_income",
    "mhlq_nonjdm", "mhlq_alc", "mhlq_early", "mhlq_psych_help", "mhlq_friend_help",
    "mhlq_interest", "mhlq_helpless", "mhlq_drugs", "mhlq_self_psych",
    "mhlq_income_r", "mhlq_helpless_r"
)

# 4. Umwandlung in numeric
df_all <- df_all |>
    mutate(across(
        all_of(c(mhlq_positive, mhlq_negative, "mhlq_depr_real")),
        ~ as.numeric(.)
    ))

# 5. Invertierung der regulären negativ gepolten Items
df_all <- df_all |>
    mutate(across(
        all_of(mhlq_negative),
        ~ ifelse(is.na(.x), NA, 6 - .x),
        .names = "{.col}_r"
    ))

# 6. mhlq_depr_real: sample-spezifische Invertierung
#    DE (D602_23): "Depression ist keine echte psychische Störung" → negativ, invertieren
#    US (U602_03): "Loss of interest or pleasure is a symptom of depression" → positiv, NICHT invertieren
df_all <- df_all |>
    mutate(
        mhlq_depr_real_r = case_when(
            sample == "de" ~ 6 - mhlq_depr_real,
            sample == "us" ~ mhlq_depr_real,
            TRUE           ~ NA_real_
        )
    )

# 7. Zusammenführen aller Items, dann Ausschluss anwenden
mhlq_all_items <- c(mhlq_positive, paste0(mhlq_negative, "_r"), "mhlq_depr_real_r")
mhlq_all_items <- setdiff(mhlq_all_items, exclude_vars)

# Kontrolle: sollte 16 Items ergeben
cat("MHLQ Items im Score:", length(mhlq_all_items), "\n")
print(mhlq_all_items)

# 8. Summen- und Mittelwertscore berechnen
df_all <- df_all |>
    rowwise() |>
    mutate(
        mhlq_score = {
            vals <- c_across(all_of(mhlq_all_items))
            if (all(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE)
        },
        mhlq_mean = mean(c_across(all_of(mhlq_all_items)), na.rm = TRUE)
    ) |>
    ungroup()

## MHLQ Subskalen
# FIX: mhlq_depr_real_r (korrekte sample-spezifische Invertierung)
# FIX: Vektor- und Spaltennamen getrennt (Name Collision vermieden)
# Hinweis: mhlq_brain geht in Gesamtscore ein, ist aber keiner Subskala zugeordnet

mhlq_knowledge_items <- c(
    "mhlq_schizo_hall", "mhlq_thoughts", "mhlq_duration", "mhlq_depr_real_r"
)

mhlq_stereotypes_items <- c(
    "mhlq_no_affect_r", "mhlq_no_behav_r", "mhlq_adults_only_r"
)

mhlq_help_skills_items <- c(
    "mhlq_help_fam", "mhlq_help_psy", "mhlq_self_help_psy"
)

mhlq_self_help_items <- c(
    "mhlq_sport", "mhlq_sleep", "mhlq_nutri", "mhlq_fun"
)

df_all <- df_all %>%
    rowwise() %>%
    mutate(
        mhlq_knowledge_score   = sum(c_across(all_of(mhlq_knowledge_items)), na.rm = TRUE),
        mhlq_stereotypes_score = sum(c_across(all_of(mhlq_stereotypes_items)), na.rm = TRUE),
        mhlq_help_skills_score = sum(c_across(all_of(mhlq_help_skills_items)), na.rm = TRUE),
        mhlq_self_help_score   = sum(c_across(all_of(mhlq_self_help_items)), na.rm = TRUE)
    ) %>%
    ungroup()

### MHLS

mhls_exclude <- c(
  "mhls4_avoidance", "mhls4_disclose", "mhls5_evening", "mhls5_vote", "mhls5_hire"
)

mhls_max_4 <- c(
  "mhls1_socphob", "mhls1_gad", "mhls1_depr", "mhls1_pers", "mhls1_dysth",
  "mhls1_agora", "mhls1_bipol", "mhls1_subs", "mhls1_fem_more", "mhls1_male_anx",
  "mhls2_sleep", "mhls2_avoid", "mhls3_kvt", "mhls3_break_danger", "mhls3_break_support"
)

# Items mit 1-5 Skala
mhls_max_5 <- c(
  "mhls4_info", "mhls4_digital", "mhls4_apt", "mhls4_sources",
  "mhls4_control", "mhls4_weakness", "mhls4_not_med", "mhls4_danger",
  "mhls4_avoidance", "mhls4_disclose", "mhls5_evening", "mhls5_vote", "mhls5_hire"
)

# FIX: Invertierte Items korrigiert (4er-Skala: 5 - x)
#   - mhls3_break_danger ENTFERNT (positiv gepolt, korrekte Antwort = hoch)
#   - mhls3_break_support HINZUGEFÜGT (negativ gepolt, korrekte Antwort = niedrig)
#   - mhls2_avoid HINZUGEFÜGT (Vermeidung ist nicht hilfreich, korrekte Antwort = niedrig)
#   - mhls1_male_anx HINZUGEFÜGT (Frauen häufiger betroffen, korrekte Antwort = niedrig)
mhls_invert_4 <- c("mhls3_break_support", "mhls2_avoid", "mhls1_male_anx")

# Invertierte Items (5er-Skala: 6 - x) – unverändert, korrekt
mhls_invert_5 <- c("mhls4_control", "mhls4_weakness", "mhls4_not_med", "mhls4_danger")

# Umkodierung
df_all <- df_all |>
  mutate(across(
    all_of(mhls_invert_4),
    ~ ifelse(is.na(.), NA_real_, 5 - .),
    .names = "{.col}_r"
  )) |>
  mutate(across(
    all_of(mhls_invert_5),
    ~ ifelse(is.na(.), NA_real_, 6 - .),
    .names = "{.col}_r"
  ))

## Summenscore
# FIX: mhls3_break_danger ohne _r (nicht mehr invertiert)
#      mhls3_break_support_r statt mhls3_break_support (jetzt invertiert)
#      mhls2_avoid_r statt mhls2_avoid (jetzt invertiert)
#      mhls1_male_anx_r statt mhls1_male_anx (jetzt invertiert)

mhls_items <- c(
  "mhls1_socphob", "mhls1_gad", "mhls1_depr",
  "mhls1_pers", "mhls1_dysth", "mhls1_agora",
  "mhls1_bipol", "mhls1_subs", "mhls1_fem_more",
  "mhls1_male_anx_r",
  "mhls2_sleep",
  "mhls2_avoid_r",
  "mhls3_kvt",
  "mhls3_break_danger",
  "mhls3_break_support_r",
  "mhls4_info", "mhls4_digital", "mhls4_apt", "mhls4_sources",
  "mhls4_control_r", "mhls4_weakness_r", "mhls4_not_med_r", "mhls4_danger_r"
)

df_all <- df_all |>
  rowwise() |>
  mutate(
    mhls_total_score = {
      vals <- c_across(all_of(mhls_items))
      if (all(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE)
    }
  ) |>
  ungroup()

### MHSAS
# Codebook: 5-Punkt semantisches Differential (1-5)
# Invertierung mit 6 - x ist korrekt für 5-Punkt-Skala

mhsas_invert <- c(
  "mhsas_important",
  "mhsas_good",
  "mhsas_healing",
  "mhsas_satisfying",
  "mhsas_desirable")

# FIX [minor]: Kommentar korrigiert (5er-Skala, nicht 7er)
# Invertiere die negativ gepolten Items (5er Skala: 6 - x)
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

#### Abteilungsklima

# --- Schritt 1: Umpolen und harmonisieren ---

df_all <- df_all %>%
  mutate(
    # SUBSKALA 1: Interpersonelles Klima (10 Items)
    # Umpolen wo nötig, dann DE und US zusammenführen
    
    cl1_friendly   = coalesce(6 - abtkl_freundlich, 6 - dep_climate_friendly),
    cl1_nonracist  = coalesce(abtkl_rassistisch, dep_climate_racist),
    cl1_diverse    = coalesce(abtkl_vielfalt, dep_climate_diverse),
    cl1_respectful = coalesce(abtkl_respekt, dep_climate_respectful),
    cl1_collegial  = coalesce(6 - abtkl_kollegial, 6 - dep_climate_collegial),
    cl1_nonsexist  = coalesce(6 - abtkl_sexismus, 6 - dep_climate_sexist),
    cl1_collab     = coalesce(6 - abtkl_kollab, 6 - dep_climate_individualistic),
    cl1_coop       = coalesce(6 - abtkl_kooperativ, 6 - dep_climate_competitive),
    cl1_nonhomoph  = coalesce(abtkl_homophob, dep_climate_homophobic),
    cl1_supportive = coalesce(abtkl_support, dep_climate_supportive),
    
    # Score
    climate1_mean = rowMeans(cbind(
      cl1_friendly, cl1_nonracist, cl1_diverse, cl1_respectful,
      cl1_collegial, cl1_nonsexist, cl1_collab, cl1_coop,
      cl1_nonhomoph, cl1_supportive), na.rm = TRUE),
    
    # SUBSKALA 2: Arbeitsatmosphäre (12 Items)
    
    cl2_research_valued = coalesce(abtkl_forsch_geschaetzt, dep_climate_research_valued),
    cl2_no_fitin_press  = coalesce(6 - abtkl_anpassdruck, 6 - dep_climate_pressure_fitin),
    cl2_no_tenure_press = coalesce(6 - abtkl_festanstellung_druck, 6 - dep_climate_pressure_tenure),
    cl2_questions_ok    = coalesce(abtkl_leistung_fragen, dep_climate_questions_ok),
    cl2_no_fear_speak   = coalesce(6 - abtkl_prob_ansprechen, 6 - dep_climate_fear_speakup),
    cl2_no_gender_stereo = coalesce(6 - abtkl_gender_view, 6 - dep_climate_gender_stereotype),
    cl2_no_ethnic_stereo = coalesce(6 - abtkl_ethnie_view, 6 - dep_climate_ethnicity_stereotype),
    cl2_no_low_expect   = coalesce(6 - abtkl_erwartungen, 6 - dep_climate_low_expectations),
    cl2_no_scrutiny     = coalesce(6 - abtkl_beobachtet, 6 - dep_climate_under_scrutiny),
    cl2_no_work_harder  = coalesce(6 - abtkl_harter_weg, 6 - dep_climate_work_harder),
    cl2_no_unwritten    = coalesce(6 - abtkl_regeln, 6 - dep_climate_unwritten_rules),
    cl2_no_fit_in_diff  = coalesce(6 - abtkl_dazugehoeren, 6 - dep_climate_fit_in),
    
    # Score
    climate2_mean = rowMeans(cbind(
      cl2_research_valued, cl2_no_fitin_press, cl2_no_tenure_press,
      cl2_questions_ok, cl2_no_fear_speak, cl2_no_gender_stereo,
      cl2_no_ethnic_stereo, cl2_no_low_expect, cl2_no_scrutiny,
      cl2_no_work_harder, cl2_no_unwritten, cl2_no_fit_in_diff), na.rm = TRUE),
    
    # SUBSKALA 3: Wahrgenommener Einfluss (9 Items)
    
    cl3_curriculum  = coalesce(abtkl_lehrplan, dep_climate_curriculum),
    cl3_salary      = coalesce(abtkl_gehalt, dep_climate_salary_increase),
    cl3_travel      = coalesce(abtkl_reisekosten, dep_climate_travel_funding),
    cl3_equipment   = coalesce(abtkl_geraete, dep_climate_research_resources),
    cl3_students    = coalesce(abtkl_promiwahl, dep_climate_select_students),
    cl3_faculty     = coalesce(abtkl_fakwahl, dep_climate_select_faculty),
    cl3_tenure      = coalesce(abtkl_festanstellung, dep_climate_tenure_decision),
    cl3_leadership  = coalesce(abtkl_leitung, dep_climate_select_leadership),
    cl3_culture     = coalesce(abtkl_klima, dep_climate_overall_culture),
    
    # Score
    climate3_mean = rowMeans(cbind(
      cl3_curriculum, cl3_salary, cl3_travel, cl3_equipment,
      cl3_students, cl3_faculty, cl3_tenure, cl3_leadership,
      cl3_culture), na.rm = TRUE)
  )

# Berechne den Mittelwert als Gesamtscore
df_all <- df_all %>%
  rowwise() %>%
  mutate(mhsas = mean(c_across(all_of(mhsas_items)), na.rm = TRUE)) %>%
  ungroup()

data_write(df_all, "data_all_tidy.csv")
