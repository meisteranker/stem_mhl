#### Pakete laden

library(tidyverse)
library(easystats)
library(lmtest)
library(pwr)

#### Daten laden

df_all <- read_csv("data_all_tidy_2.csv")

df_all <- df_all %>%
  filter(
    (sample == "de" & exclusion == 1 & consent == 3) |
      (sample == "us" & exclusion_criteria == 1 & consent == 1)
  )

#### Neue Variable: ethnicity_dich

df_all <- df_all |>
  mutate(
    ethnicity_dich = case_when(
      str_to_lower(ethnicity) == "white" ~ "caucasian",
      ethnicity == "Unspecified" ~ NA_character_,
      TRUE ~ "non_caucasian"
    )
  )

#### Climate Variablen uumbenennen

df_all <- df_all |>
  rename(
    `interpersonal_climate` = `climate1_mean`,
    `work_atmosphere` = `climate2_mean`,
    `perceived_influence` = `climate3_mean`
  )

#### Regressionen

### Class zuweisen

df_all$who5_raw <- as.numeric(df_all$who5_raw)
df_all$who5_norm <- as.numeric(df_all$who5_norm)
df_all$ghq12_sum <- as.numeric(df_all$ghq12_sum)
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
df_all$field_reduced <- as.factor(df_all$field_reduced)
df_all$brs_score <- as.numeric(df_all$brs_score)
df_all$fsozuk6 <- as.numeric(df_all$fsozuk6)
df_all$ethnicity_dich <- as.factor(df_all$ethnicity_dich)
df_all$interpersonal_climate <- as.numeric(df_all$interpersonal_climate)
df_all$work_atmosphere <- as.numeric(df_all$work_atmosphere)
df_all$perceived_influence <- as.numeric(df_all$perceived_influence)

### ethnicity dichotom: non_caucasian als Referenzkategorie

df_all <- df_all |>
  mutate(
    ethnicity_dich = factor(ethnicity_dich),
    ethnicity_dich = fct_relevel(ethnicity_dich, "non_caucasian")
  )

### Children umkodieren

df_all <- df_all |> mutate(children = has_children)

df_all <- df_all |>
  mutate(
    children = fct_recode(children, yes = "1", no = "2")
  )

### Partnerschaft umkodieren

df_all <- df_all |>
  mutate(
    partnered = fct_recode(partnered, yes = "1", no = "2"),
    partnered = fct_relevel(partnered, "yes")
  )

#### Regressionen

### well-being

## Model

who_reg <- lm(
  who5_raw ~ sample +
    tenure +
    age +
    ethnicity_dich +
    gender_binary +
    children +
    partnered +
    interpersonal_climate +
    work_atmosphere +
    perceived_influence +
    field_reduced +
    brs_score +
    fsozuk6,
  data = df_all
)

## Ergebnis

summary(who_reg)
model_parameters(who_reg, include_info = TRUE)
standardize_parameters(who_reg)

## Voraussetzungen

shapiro.test(residuals(who_reg))
bptest(who_reg)

## Robuste SE (HC3)

model_parameters(who_reg, vcov = "HC3")

## Post-hoc Power

n_obs <- nobs(who_reg)
k_pred <- length(coef(who_reg)) - 1

r2 <- performance::r2(who_reg)$R2
pwr.f2.test(
  u = k_pred,
  v = n_obs - k_pred - 1,
  f2 = r2 / (1 - r2),
  sig.level = 0.05
)

### Mental distress

## Model

ghq_reg <- lm(
  ghq12_sum ~ sample +
    tenure +
    age +
    ethnicity_dich +
    gender_binary +
    children +
    partnered +
    interpersonal_climate +
    work_atmosphere +
    perceived_influence +
    field_reduced +
    brs_score +
    fsozuk6,
  data = df_all
)

## Ergebnis

model_parameters(ghq_reg, include_info = TRUE)
standardize_parameters(ghq_reg)

## Voraussetzungen

shapiro.test(residuals(ghq_reg))
bptest(ghq_reg)

## Robuste SE (HC3)

model_parameters(ghq_reg, vcov = "HC3")

## Post-hoc Power

n_obs <- nobs(ghq_reg)
k_pred <- length(coef(ghq_reg)) - 1

r2 <- performance::r2(ghq_reg)$R2
pwr.f2.test(
  u = k_pred,
  v = n_obs - k_pred - 1,
  f2 = r2 / (1 - r2),
  sig.level = 0.05
)

### Anxiety

## Model

gad_reg <- lm(
  gad ~ sample +
    tenure +
    age +
    ethnicity_dich +
    gender_binary +
    children +
    partnered +
    interpersonal_climate +
    work_atmosphere +
    perceived_influence +
    field_reduced +
    brs_score +
    fsozuk6,
  data = df_all
)

## Ergebnis

model_parameters(gad_reg, include_info = TRUE)
standardize_parameters(gad_reg)

## Voraussetzungen

shapiro.test(residuals(gad_reg))
bptest(gad_reg)

## Robuste SE (HC3)

model_parameters(gad_reg, vcov = "HC3")

## Post-hoc Power

n_obs <- nobs(gad_reg)
k_pred <- length(coef(gad_reg)) - 1

r2 <- performance::r2(gad_reg)$R2
pwr.f2.test(
  u = k_pred,
  v = n_obs - k_pred - 1,
  f2 = r2 / (1 - r2),
  sig.level = 0.05
)

### MHLQ

## model

mhlq_reg <- lm(
  mhlq_mean ~ sample +
    tenure +
    age +
    ethnicity_dich +
    gender_binary +
    children +
    partnered +
    interpersonal_climate +
    work_atmosphere +
    perceived_influence +
    field_reduced +
    brs_score +
    fsozuk6,
  data = df_all
)

## Ergebnis

model_parameters(mhlq_reg, include_info = TRUE)
standardize_parameters(mhlq_reg)

## Voraussetzungen

shapiro.test(residuals(mhlq_reg))
bptest(mhlq_reg)

## Robuste SE (HC3)

model_parameters(mhlq_reg, vcov = "HC3")

## Post-hoc Power

n_obs <- nobs(mhlq_reg)
k_pred <- length(coef(mhlq_reg)) - 1

r2 <- performance::r2(mhlq_reg)$R2
pwr.f2.test(
  u = k_pred,
  v = n_obs - k_pred - 1,
  f2 = r2 / (1 - r2),
  sig.level = 0.05
)

### mhsas

mhsas_reg <- lm(
  mhsas ~ sample +
    tenure +
    age +
    ethnicity_dich +
    gender_binary +
    children +
    partnered +
    interpersonal_climate +
    work_atmosphere +
    perceived_influence +
    field_reduced +
    brs_score +
    fsozuk6,
  data = df_all
)

## Ergebnis

model_parameters(mhsas_reg, include_info = TRUE)
standardize_parameters(mhsas_reg)

## Voraussetzungen

shapiro.test(residuals(mhsas_reg))
bptest(mhsas_reg)

## Robuste SE (HC3)

model_parameters(mhsas_reg, vcov = "HC3")

## Post-hoc Power

n_obs <- nobs(mhsas_reg)
k_pred <- length(coef(mhsas_reg)) - 1

r2 <- performance::r2(mhsas_reg)$R2
pwr.f2.test(
  u = k_pred,
  v = n_obs - k_pred - 1,
  f2 = r2 / (1 - r2),
  sig.level = 0.05
)

### Resilienz

## Model

brs_reg <- lm(
  brs_score ~ sample +
    tenure +
    age +
    gender_binary +
    ethnicity_dich +
    children +
    partnered,
  data = df_all
)

## Ergebnis

model_parameters(brs_reg, include_info = TRUE)
standardize_parameters(brs_reg)

## Voraussetzungen

shapiro.test(residuals(brs_reg))
bptest(brs_reg)

## Robuste SE (HC3)

model_parameters(brs_reg, vcov = "HC3")

## Post-hoc Power

n_obs <- nobs(brs_reg)
k_pred <- length(coef(brs_reg)) - 1

r2 <- performance::r2(brs_reg)$R2
pwr.f2.test(
  u = k_pred,
  v = n_obs - k_pred - 1,
  f2 = r2 / (1 - r2),
  sig.level = 0.05
)

#### Adjustierte Mittelwerte berechnen

estimate_means(who_reg, by = "sample")
estimate_means(ghq_reg, by = "sample")
estimate_means(gad_reg, by = "sample")
estimate_means(mhlq_reg, by = "sample")
estimate_means(mhsas_reg, by = "sample")

estimate_contrasts(who_reg, contrast = "sample")
estimate_contrasts(ghq_reg, contrast = "sample")
estimate_contrasts(gad_reg, contrast = "sample")
estimate_contrasts(mhlq_reg, contrast = "sample")
estimate_contrasts(mhsas_reg, contrast = "sample")

#### Test ob Korrelation zwischen Ehtnicity und Sample

tab_ethnicity <- table(df_all$sample, df_all$ethnicity_dich)

df_all |>
  filter(!is.na(sample), !is.na(ethnicity_dich)) |>
  count(sample, ethnicity_dich) |>
  group_by(sample) |>
  mutate(percent = round(n / sum(n) * 100, 1))

fisher.test(tab_ethnicity)
