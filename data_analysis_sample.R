#### Pakete laden

library(tidyverse)
library(gtsummary)

#### Daten laden

de <- read_csv("data_tidy_de.csv")
us <- read_csv("data_tidy_us.csv")

### Fälle ausschließen

de <- de |> filter(exclusion == 1)
us <- us |> filter(exclusion_criteria == 1, consent == 1)

#### Fälle

### Insgesamt

nrow(de) + nrow(us)

### US and DE
nrow(de)
nrow(us)

#### Unterschiede US und DE

### Gemeinsamen Datensatz mit Ländervariable erstellen

## Position Simple

de_prep <- de |>
  mutate(
    position_simple = case_when(
      # Niedrigste zuerst → gewinnt bei Mehrfachauswahl
      pos_postdoc      == 2 ~ "Postdoc",
      pos_group_leader == 2 ~ "Group Leader",
      pos_akad_council == 2 ~ "Akademischer Rat",
      pos_priv_docent  == 2 ~ "Privatdozent",
      pos_junior_prof  == 2 ~ "Junior Professor",
      pos_professor    == 2 ~ "Professor",
      TRUE                  ~ "Other"
    ) |> factor(levels = c(
      "Professor", "Junior Professor", "Privatdozent",
      "Akademischer Rat", "Group Leader", "Postdoc", "Other"
    )),
    country = "Germany"
  )

us_prep <- us |>
  mutate(
    position_simple = case_when(
      # Niedrigste zuerst → gewinnt bei Mehrfachauswahl
      pos_assist_prof == 2 | pos_assist_teach_prof == 2 |
        pos_assist_res_prof == 2                    ~ "Assistant Professor",
      pos_lecturer == 2 | pos_instructor == 2 |
        pos_assoc_teach_prof == 2 |
        pos_teach_prof == 2                         ~ "Lecturer / Instructor",
      pos_assoc_prof == 2 |
        pos_assoc_res_prof == 2                     ~ "Associate Professor",
      pos_full_prof == 2 | pos_dist_prof == 2 |
        pos_prof == 2 | pos_res_prof == 2           ~ "Professor",
      TRUE                                          ~ "Other"
    ) |> factor(levels = c(
      "Professor", "Associate Professor", "Assistant Professor",
      "Lecturer / Instructor", "Other"
    )),
    country = "USA"
  )

### Datensätze kombinieren

combined <- bind_rows(de_prep, us_prep) |>
  mutate(
    gender = case_when(
      gender_female == 1 ~ "Female",
      gender_male   == 1 ~ "Male",
      TRUE               ~ "Other"
    ) |> factor(),
    ethnicity = case_when(
      eth_white             == 1 ~ "White",
      eth_asian             == 1 ~ "Asian",
      eth_black             == 1 ~ "Black",
      eth_latinx            == 1 ~ "Latinx",
      eth_indigenous_native == 1 ~ "Indigenous/Native",
      TRUE                       ~ "Unspecified"
    ) |> factor(),
    tenure       = factor(tenure),
    partnered    = factor(partnered,    labels = c("No", "Yes")),
    has_children = factor(has_children, labels = c("No", "Yes"))
  )

## Tabelle

combined |>
  select(country, age, gender, ethnicity, position_simple,
         partnered, has_children, tenure) |>
  tbl_summary(
    by = country,
    statistic = list(
      all_continuous()  ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 3,
      all_categorical() ~ c(0, 3)
    ),
    missing = "no",
    label = list(
      age             ~ "Age (M ± SD)",
      gender          ~ "Gender",
      ethnicity       ~ "Ethnicity",
      position_simple ~ "Position",
      partnered       ~ "Partnered",
      has_children    ~ "Has Children",
      tenure          ~ "Tenure"
    )
  ) |>
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",
      ethnicity         ~ "fisher.test",
      position_simple   ~ "fisher.test",
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  add_overall() |>
  bold_labels()