#### Pakete laden

library(tidyverse)
library(easystats)
library(gtsummary)

#### Regressionen

### Class zuweisen

df_all$who5_raw<- as.numeric(df_all$who5_raw)
df_all$gad <- as.numeric(df_all$gad)
df_all$ghq12_sum<- as.numeric(df_all$ghq12_sum)
df_all$age <- as.numeric(df_all$age)
df_all$gender_binary <- as.factor(df_all$gender_binary)
df_all$ethnicity <- as.factor(df_all$ethnicity)
df_all$tenure <- as.factor(df_all$tenure)
df_all$sample <- as.factor(df_all$sample)
df_all$mhlq_score <- as.numeric(df_all$mhlq_score)
df_all$mhlq_mean <- as.numeric(df_all$mhlq_mean)
df_all$mhls_total_score <- as.numeric(df_all$mhls_total_score)
df_all$mhsas <- as.numeric(df_all$mhsas)
df_all$has_children <- as.factor(df_all$has_children)
df_all$partnered <- as.factor(df_all$partnered)

### ethnicity dichotom

df_all <- df_all |>
  mutate(
    ethnicity_dich = case_when(
      ethnicity == "White" ~ "white",
      TRUE ~ "non_white"
    ),
    ethnicity_dich = as_factor(ethnicity_dich),
    ethnicity_dich = fct_relevel(ethnicity_dich, "white")
  )

### Children umkodieren

df_all <- df_all |> mutate (children = has_children)

df_all <- df_all |>
  mutate(
    children = fct_recode(children,
      yes = "1",
      no  = "2"))

### Partnerschaft umkodieren

df_all <- df_all |>
  mutate(
    partnered = fct_recode(partnered,
      yes = "1",
      no  = "2"
    ),
    partnered = fct_relevel(partnered, "yes")
  )

### Tenure umkodieren

df_all <- df_all |>
  mutate(
    tenure = fct_recode(tenure,
      yes = "1",
      no  = "2"
    ),
    tenure = fct_relevel(tenure, "yes")
  )

### PHQ Items in GAD Items umwandeln

#df_all <- df_all |>
 # mutate(
  #  gad7_nervous = phq4_nervous,
  #  gad7_worry_control = phq4_worry_control
  #)

## Neuer Summenscore

df_all <- df_all |>
  mutate(across(
    c(
      gad7_excess_worry,
      gad7_relax_difficulty,
      gad7_restlessness,
      gad7_irritability,
      gad7_fear_bad_happen,
      gad7_nervous,
      gad7_worry_control
    ),
    as.numeric
  ))

  df_all <- df_all |>
    rowwise() |>
    mutate(
      gad_sum = {
        vals <- c_across(c(
          gad7_excess_worry,
          gad7_relax_difficulty,
          gad7_restlessness,
          gad7_irritability,
          gad7_fear_bad_happen,
          gad7_nervous,
          gad7_worry_control
        ))
        if (all(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE)
      }
    ) |>
    ungroup()

### well-being

## Model

who_reg <- lm(who5_raw ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(who_reg, include_info = T)
standardize_parameters(who_reg)
model_parameters(who_reg, include_info = T) |> print_html()

### Mental distress

## Model

ghq_reg <- lm(ghq12_sum ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergbenis

model_parameters(ghq_reg, include_info = T)
standardize_parameters(ghq_reg)
model_parameters(ghq_reg, include_info = T) |> print_html()

### Anxiety

## Model

gad_reg <- lm(gad ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(gad_reg, include_info = T)
standardize_parameters(gad_reg)
model_parameters(gad_reg, include_info = T) |> print_html()

### MHLQ

## model

mhlq_reg <- lm(mhlq_mean ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(mhlq_reg, include_info = T)
standardize_parameters(mhlq_reg)

# MHLQ Subscales

df_all |>
  select(sample, mhlq_knowledge, mhlq_stereotypes, mhlq_help_skills, mhlq_self_help) |>
  tbl_summary(
    by = sample,
    missing = "no"
  ) |> 
  add_p()

### mhsas

mhsas_reg <- lm(mhsas ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(mhsas_reg, include_info = T)
standardize_parameters(mhsas_reg)


