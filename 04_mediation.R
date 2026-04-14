#### Pakete laden

library(tidyverse)
library(easystats)
library(lmtest)
library(pwr)

#### Daten laden

df_all <- read_csv("data_all_tidy.csv")

df_all <- df_all %>% filter((sample == "de" & exclusion == 1 & consent == 3) |
    (sample == "us" & exclusion_criteria == 1 & consent == 1))

#### Regressionen

### Class zuweisen

df_all$who5_raw <- as.numeric(df_all$who5_raw)
df_all$gad <- as.numeric(df_all$gad)
df_all$age <- as.numeric(df_all$age)
df_all$gender_binary <- as.factor(df_all$gender_binary)
df_all$ethnicity <- as.factor(df_all$ethnicity)
df_all$tenure <- as.factor(df_all$tenure)
df_all$sample <- as.factor(df_all$sample)
df_all$mhlq_score <- as.numeric(df_all$mhlq_score)
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

#### Regressionen

### well-being

## Model

who_reg <- lm(who5_raw ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(who_reg, include_info = T) |> print_html()

### Mental distress

## Model

ghq_reg <- lm(ghq12_likert ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergbenis

model_parameters(ghq_reg, include_info = T) |> print_html()

### Anxiety

## Model

gad_reg <- lm(gad_sum ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(gad_reg, include_info = T) |> print_html()

### MHLQ

## model

mhlq_reg <- lm(mhlq_score ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(mhlq_reg, include_info = T) |> print_html()

### mhsas

mhsas_reg <- lm(mhsas ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(mhsas_reg, include_info = T)
standardize_parameters(mhsas_reg)

model_parameters(mhsas_reg, include_info = T) |> print_html()

### Resilienz

## Model

brs_reg <- lm(brs_score ~ sample + tenure + age + gender_binary + ethnicity_dich + children + partnered, data = df_all)

## Ergebnis

model_parameters(brs_reg, include_info = TRUE) 
standardize_parameters(brs_reg)

## Voraussetzungen

shapiro.test(residuals(brs_reg))
bptest(brs_reg)

## Robuste SE (HC3)

model_parameters(brs_reg, vcov = "HC3")

## Post-hoc Power

n_obs  <- nobs(brs_reg)
k_pred <- length(coef(brs_reg)) - 1

r2 <- performance::r2(brs_reg)$R2
pwr.f2.test(u = k_pred, v = n_obs - k_pred - 1, f2 = r2 / (1 - r2), sig.level = 0.05)

