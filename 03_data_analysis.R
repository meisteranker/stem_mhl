#### load packages

library(tidyverse)
library(easystats)
library(sjPlot)
library(ggeffects)
library(gtsummary)
library(psych)
library(rempsyc)


### NA

df_all <- df_all %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))

## remove NA

df_all <- df_all %>%
  filter(rowMeans(is.na(.)) <= 0.9)

####

df_all <- df_all %>% mutate(sample = as.factor(sample))
df_all <- df_all %>% mutate(mentorship = as.factor(mentorship))
df_all <- df_all %>% mutate(ethnicity = as.factor(ethnicity))
df_all <- df_all %>% mutate(gender = as.factor(gender))
df_all <- df_all %>% mutate(field = as.factor(field))
df_all <- df_all %>% mutate(study = as.factor(study))
df_all <- df_all %>% mutate(position = as.factor(position))
df_all <- df_all %>% mutate(age = as.numeric(age))
df_all <- df_all %>% mutate(ghq12_sum = as.numeric(ghq12_sum))
df_all <- df_all %>% mutate(gad = as.numeric(gad))
df_all <- df_all %>% mutate(brs_score = as.numeric(brs_score))
df_all <- df_all %>% mutate(mhlq_score = as.numeric(mhlq_score))

#### descriptive statistics

### age, ethnicity, gender

df_all %>%
  select(sample, age, ethnicity, gender_simple) %>%
  tbl_summary(
    by = sample,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p() %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

df_all %>%
  count(
    sample,
    ethnicity = replace_na(ethnicity, "Unspecified"),
    name = "n"
  ) %>%
  group_by(sample) %>%
  mutate(
    sample_total = sum(n),
    percent = round(100 * n / sample_total, 2)
  ) %>%
  ungroup() %>%
  arrange(sample, ethnicity)

df_all %>%
  mutate(
    gender_simple = case_when(
      gender_simple %in%
        c("Other", "Trans", "Nonbinary") ~ "Other/Trans/Nonbinary",
      TRUE ~ gender_simple
    )
  ) %>%
  count(
    sample,
    gender_simple = replace_na(gender_simple, "Prefer not to say"),
    name = "n"
  ) %>%
  group_by(sample) %>%
  mutate(
    sample_total = sum(n),
    percent = round(n / sample_total * 100, 2)
  ) %>%
  ungroup() %>%
  arrange(sample, gender_simple)

## plots: age, ethnicity, gender

plot_grpfrq(df_all$ethnicity, df_all$sample, show.values = T)
plot_grpfrq(df_all$gender_simple, df_all$sample, show.values = T)
plot_grpfrq(df_all$age, df_all$sample, show.values = F)

### Table: position_simple

df_all %>%
  select(sample, position_simple) %>%
  tbl_summary(
    by = sample,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

## plot: position_simple

plot_grpfrq(df_all$position_simple, df_all$sample, show.values = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Position harmonized

df_all %>%
  mutate(
    position_harmonized = fct_explicit_na(position_harmonized, "Other")
  ) %>%
  count(sample, position_harmonized, name = "n") %>%
  group_by(sample) %>%
  mutate(percent = round(n / sum(n) * 100, 2)) %>%
  ungroup() %>%
  arrange(sample, position_harmonized)

### Table: field_simple

df_all %>%
  select(sample, field_simple) %>%
  tbl_summary(
    by = sample,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

### field_harmonized

df_all %>%
  count(sample, field_harmonized, name = "n") %>%
  group_by(sample) %>%
  mutate(percent = round(n / sum(n) * 100, 2)) %>%
  ungroup() %>%
  arrange(sample, field_harmonized)

### Table: study_simple

df_all %>%
  select(sample, study_simple) %>%
  tbl_summary(
    by = sample,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

###  Tenure

df_all %>%
  mutate(
    tenure_simple = case_when(
      tenure == 1 ~ "yes",
      tenure == 2 ~ "no",
      TRUE ~ NA_character_
    )
  ) %>%
  count(sample, tenure_simple, name = "n") %>%
  group_by(sample) %>%
  mutate(percent = round(n / sum(n) * 100, 2)) %>%
  ungroup() %>%
  arrange(sample, tenure_simple)

### Table: who, ghq, phq, gad, brs

df_all %>%
  select(sample, who5_rev, ghq12_likert, phq4_total, gad, brs_score) %>%
  tbl_summary(
    by = sample,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

## Table nach gender (dichotom)

df_all %>%
  select(gender_binary, who5_rev, ghq12_likert, phq4_total, gad, brs_score) %>%
  tbl_summary(
    by = gender_binary,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

### Table: mhlq, mhls, mhsas

df_all %>%
  select(sample, mhlq_score, mhls_total, mhsas) %>%
  tbl_summary(
    by = sample,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

## Table nach gender (dichotom)

df_all %>%
  select(gender_binary, mhlq_score, mhls_total, mhsas) %>%
  tbl_summary(
    by = gender_binary,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

## Table: mhlq subscales

df_all %>%
  select(
    sample,
    mhlq_knowledge,
    mhlq_stereotypes,
    mhlq_help_skills,
    mhlq_self_help
  ) %>%
  tbl_summary(
    by = sample,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

## Table: mhls subscales

df_all %>%
  select(
    sample,
    mhls_recog_disorders,
    mhls_risk_factors_causes,
    mhls_help_available,
    mhls_info_seeking,
    mhls_attitudes_help_seeking
  ) %>%
  tbl_summary(
    by = sample,
    type = list(
      mhls_recog_disorders ~ "continuous",
      mhls_risk_factors_causes ~ "continuous",
      mhls_help_available ~ "continuous",
      mhls_info_seeking ~ "continuous",
      mhls_attitudes_help_seeking ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing = "no"
  ) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**")

### Korrelation zwischen den mental health literacy scales und mental health

df_all %>%
  select(
    who5_norm,
    ghq12_likert,
    phq4_total,
    gad,
    brs_score,
    mhlq_score,
    mhls_total
  ) %>%
  correlation() %>%
  summary()

df_all %>%
  select(
    who5_norm,
    ghq12_likert,
    phq4_total,
    gad,
    brs_score,
    mhlq_score,
    mhls_total
  ) %>%
  correlation() %>%
  summary() %>%
  plot()

## Network plot

library(qgraph)
library(corrr)

vars <- c(
  "who5_norm",
  "ghq12_likert",
  "phq4_total",
  "gad",
  "brs_score",
  "mhlq_score",
  "mhls_total",
  "mhsas"
)

# new df without NA

network_data <- df_all %>%
  select(all_of(vars)) %>%
  na.omit()

cor_matrix <- cor_auto(network_data)

# plot network (LASSO)

qgraph(
  cor_matrix,
  layout = "spring",
  graph = "glasso",
  sampleSize = nrow(network_data),
  tuning = 0.5,
  minimum = 0.1,
  cut = 0.3,
  vsize = 6,
  theme = "colorblind",
  labels = colnames(cor_matrix),
  title = "Mental Health Network"
)

# plot network (Korrelationen)

qgraph(
  cor_matrix,
  layout = "spring",
  graph = "cor", # <-- wichtig!
  minimum = 0.1, # optional: unterdrückt schwache Kanten
  cut = 0.3, # optional: Schwellenwert für Anzeige
  vsize = 6,
  theme = "colorblind",
  labels = colnames(cor_matrix),
  title = "Korrelationen zwischen mental health Variablen"
)

### Alpha und Omega der mental health literacy scales

## mhlq

mhlq_items <- df_all %>%
  select(
    mhlq_sport,
    mhlq_depr_symp,
    mhlq_schizo_wahn,
    mhlq_help_fam,
    mhlq_help_psy,
    mhlq_sleep,
    mhlq_self_help_psy,
    mhlq_anx_panik,
    mhlq_nonjdm_r,
    mhlq_alc_r,
    mhlq_no_affect_r,
    mhlq_early_r,
    mhlq_adults_only_r,
    mhlq_brain,
    mhlq_psych_help,
    mhlq_friend_help,
    mhlq_nutri,
    mhlq_interest,
    mhlq_helpless_r,
    mhlq_duration,
    mhlq_depr_real,
    mhlq_drugs,
    mhlq_thoughts,
    mhlq_fun,
    mhlq_self_psych,
    mhlq_schizo_hall,
    mhlq_no_behav_r,
    mhlq_income_r,
    mhlq_stress
  )

omega(mhlq_items, plot = F)
sjt.itemanalysis(mhlq_items)

### Faktorenanalyse der Mental Health Literacy Skalen

fa.parallel(mhlq_items)

### Save data frame

data_write(df_all, "df_all.csv")

### Omega der Abteilungsklima Skalen

for (s in c("de", "us")) {
  cat("\n=== Sample:", toupper(s), "===\n")
  sub <- df_all %>% filter(sample == s)

  cat("\nSubskala 1: Interpersonelles Klima\n")
  sub %>%
    select(starts_with("cl1_")) %>%
    drop_na() %>%
    omega(plot = FALSE) %>%
    print()

  cat("\nSubskala 2: Arbeitsatmosphäre\n")
  sub %>%
    select(starts_with("cl2_")) %>%
    drop_na() %>%
    omega(plot = FALSE) %>%
    print()

  cat("\nSubskala 3: Wahrgenommener Einfluss\n")
  sub %>%
    select(starts_with("cl3_")) %>%
    drop_na() %>%
    omega(plot = FALSE) %>%
    print()
}

### Omega der BRS

for (s in c("de", "us")) {
  cat("\nBRS Sample:", toupper(s), "\n")
  df_all %>%
    filter(sample == s) %>%
    select(
      brs_recover_quickly,
      brs_handle_stress_r,
      brs_recover_fast,
      brs_back_to_normal_r,
      brs_get_through,
      brs_take_long_to_recover_r
    ) %>%
    mutate(across(everything(), as.numeric)) %>%
    drop_na() %>%
    omega(plot = FALSE) %>%
    print()
}

### Omega der F-SozU K-6

cat("\n=== Soziale Unterstützung Reliabilität ===\n")
for (s in c("de", "us")) {
  cat("\nSample:", toupper(s), "\n")
  df_all %>%
    filter(sample == s) %>%
    select(
      soz_support_understood,
      soz_support_trusted,
      soz_support_borrow,
      soz_support_activities,
      soz_support_illness,
      soz_support_distress
    ) %>%
    drop_na() %>%
    omega(plot = FALSE) %>%
    print()
}
