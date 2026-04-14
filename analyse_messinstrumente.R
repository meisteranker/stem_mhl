#### Pakete laden

library(tidyverse)
library(easystats)
library(psych)

#### dfs erstellen

data_us <- df_all |>
filter(sample == "United States")

data_de <- df_all |>
filter(sample == "Germany")

#### Omega berechnen

### WHO-5

## de

who5_items <- data_de |>
  select(
    who5_positive_mood,
    who5_relaxed,
    who5_energetic,
    who5_restful_sleep,
    who5_interest_life
  )

  omega(who5_items, nfactors = 1)

## us

who5_items <- data_us |>
  select(
    who5_positive_mood,
    who5_relaxed,
    who5_energetic,
    who5_restful_sleep,
    who5_interest_life
  )

  omega(who5_items, nfactors = 1)

### GHQ-12

## de

ghq12_items <- data_de |>
  select(ghq12_concentration, ghq12_contentment_r, ghq12_daily_duties_r, ghq12_deal_problems_r, ghq12_decision_difficulty, ghq12_depressed, ghq12_low_confidence,
  ghq12_overwhelmed, ghq12_pressure, ghq12_sleep_worries, ghq12_useful_r, ghq12_worthless)

omega(ghq12_items)

## us

ghq12_items <- data_us |>
  select(ghq12_sleep_worries:ghq12_worthless)

omega(ghq12_items)

### PHQ-4

## de

phq_items <- data_de |>
  select(phq4_low_interest:phq4_worry_control)

omega(phq_items)

## us

phq_items <- data_us |>
  select(phq4_low_interest:phq4_worry_control)

omega(phq_items)

### GAD-7

## de

gad_items <- data_de |> 
  select(gad7_excess_worry, gad7_relax_difficulty, gad7_restlessness, gad7_irritability, gad7_irritability, gad7_fear_bad_happen)

omega(gad_items)

## us

gad_items <- data_us |> 
  select(gad7_excess_worry, gad7_relax_difficulty, gad7_restlessness, gad7_irritability, gad7_irritability, gad7_fear_bad_happen)

omega(gad_items)

### BRS

## de

brs_items <- data_de |> 
  select(brs_back_to_normal, brs_get_through_r, brs_handle_stress_r, brs_recover_fast, brs_recover_quickly, brs_take_long_to_recover_r)

omega(brs_items)

## us 

brs_items <- data_us |> 
  select(brs_back_to_normal, brs_get_through_r, brs_handle_stress_r, brs_recover_fast, brs_recover_quickly, brs_take_long_to_recover_r)

omega(brs_items)

### MHLQ

## de


