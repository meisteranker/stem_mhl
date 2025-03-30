df_all %>%
  select(tenure, who5_rev, ghq12_likert, phq4_total, gad, brs_score) %>%
  tbl_summary(
    by = tenure,                        
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      all_categorical() ~ "{n} ({p}%)"),
    missing = "no",
    label = list(
      who5_rev ~ "WHO-5",
      ghq12_likert ~ "GHQ-12",
      phq4_total ~ "PHQ-4",
      gad ~ "GAD-7",
      brs_score ~ "BRS"
    )
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Skala**")

df_all %>%
  select(tenure, mhlq_score, mhls_total, mhsas) %>%
  tbl_summary(
    by = tenure,                        
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      all_categorical() ~ "{n} ({p}%)"),
    missing = "no",
    label = list(
      mhlq_score ~ "MHLQ",
      mhls_total ~ "MHLS",
      mhsas ~ "MHSAS"
    )
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Skala**")

ghq_reg <- lm(ghq12_likert ~ sample + age + ethnicity + gender_simple + tenure + mentorship, 
              data = df_all)
(ghq.table <- report_table(ghq_reg))

model_parameters(ghq_reg)

df_all %>%
  select(gender_binary, who5_rev, ghq12_likert, phq4_total, gad, brs_score) %>%
  tbl_summary(
    by = gender_binary,                        
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      all_categorical() ~ "{n} ({p}%)"),
    missing = "no",
    label = list(
      who5_rev ~ "WHO-5",
      ghq12_likert ~ "GHQ-12",
      phq4_total ~ "PHQ-4",
      gad ~ "GAD-7",
      brs_score ~ "BRS"
    )
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Skala**")

df_all %>%
  select(gender_binary, mhlq_score, mhls_total, mhsas) %>%
  tbl_summary(
    by = gender_binary,                        
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      all_categorical() ~ "{n} ({p}%)"),
    missing = "no",
    label = list(
      mhlq_score ~ "MHLQ",
      mhls_total ~ "MHLS",
      mhsas ~ "MHSAS"
    )
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_header(label ~ "**Skala**")

df_all %>% filter(sample == "us") %>% summarise(p = mean(who5_raw < 13, na.rm = TRUE) * 100) %>% pull(p) %>% sprintf("%.2f", .)

df_all %>% filter(sample == "us") %>% summarise(p = mean(gad > 10, na.rm = TRUE) * 100) %>% pull(p) %>% sprintf("%.2f", .)
df_all %>% filter(sample == "us") %>% summarise(p = mean(gad > 10, na.rm = TRUE) * 100) %>% pull(p) %>% sprintf("%.2f", .)
df_all %>% filter(sample == "us") %>% summarise(p = mean(phq4_total > 5, na.rm = TRUE) * 100) %>% pull(p) %>% sprintf("%.2f", .)
df_all %>% filter(sample == "de") %>% summarise(p = mean(phq4_total > 5, na.rm = TRUE) * 100) %>% pull(p) %>% sprintf("%.2f", .)

options(gtsummary.suppress_warnings = TRUE)

suppressMessages({
  df_all %>%
    select(gender_binary, who5_rev, ghq12_likert, phq4_total, gad, brs_score) %>%
    tbl_summary(
      by = gender_binary,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      missing = "no",
      label = list(
        who5_rev ~ "WHO-5",
        ghq12_likert ~ "GHQ-12",
        phq4_total ~ "PHQ-4",
        gad ~ "GAD-7",
        brs_score ~ "BRS"
      )
    ) %>%
    add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
    bold_labels() %>%
    modify_header(label ~ "**Skala**")
})
