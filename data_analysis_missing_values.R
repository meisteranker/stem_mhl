#### Pakete laden

library(tidyverse)
library(naniar)

#### Daten laden

de <- read_csv("data_tidy_de.csv")
us <- read_csv("data_tidy_us.csv")

### Fälle ausschließen

de <- de |> filter(exclusion == 1)
us <- us |> filter(exclusion_criteria == 1, consent == 1)

#### Überblick über missing values

miss_var_summary(de)
miss_var_summary(us)

### Nur Outcome Variablen

outcome_vars <- c(
  "who5_positive_mood", "who5_relaxed", "who5_energetic",
  "who5_restful_sleep", "who5_interest_life",
  "ghq12_sleep_worries", "ghq12_pressure", "ghq12_concentration",
  "ghq12_useful", "ghq12_deal_problems", "ghq12_decision_difficulty",
  "ghq12_overwhelmed", "ghq12_contentment", "ghq12_daily_duties",
  "ghq12_depressed", "ghq12_low_confidence", "ghq12_worthless",
  "gad7_excess_worry", "gad7_relax_difficulty", "gad7_restlessness",
  "gad7_irritability", "gad7_fear_bad_happen",
  "phq4_nervous", "phq4_worry_control",
  "mhsas_useful", "mhsas_important", "mhsas_healthy", "mhsas_effective",
  "mhsas_good", "mhsas_healing", "mhsas_empowering",
  "mhsas_satisfying", "mhsas_desirable"
)

mhlq_de <- names(de)[str_starts(names(de), "mhlq_")]
mhlq_us <- names(us)[str_starts(names(us), "mhlq_")]

de |> select(all_of(c(outcome_vars, mhlq_de))) |> miss_var_summary() |> filter(n_miss > 0) |> print(n= "inf")
us |> select(all_of(c(outcome_vars, mhlq_us))) |> miss_var_summary() |> filter(n_miss > 0) |> print(n= "inf")

de |> select(all_of(c(outcome_vars, mhlq_de))) |> complete.cases() |> sum()
us |> select(all_of(c(outcome_vars, mhlq_us))) |> complete.cases() |> sum()

### Upset-Plot: welche Kombination von Missings tritt gemeinsam auf?

de |> select(all_of(c(outcome_vars, mhlq_de))) |> gg_miss_upset()
us |> select(all_of(c(outcome_vars, mhlq_us))) |> gg_miss_upset()

nrow(de) 
nrow(us)
