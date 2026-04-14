library(tidyverse)
library(easystats)
library(gtsummary)
library(ggeffects)
library(sjPlot)
library(broom.helpers)


#### Regressionstabelle mit gtsummary mit q-Werten (Korrektur für multiple Vergleiche)

df_all <- df_all %>% mutate(sample = as.factor(sample))
df_all <- df_all %>% mutate(ethnicity = as.factor(ethnicity))
df_all <- df_all %>% mutate(gender_simple = as.factor(gender_simple))
df_all <- df_all %>% mutate(tenure = as.factor(tenure))

ghq_reg <- lm(ghq12_likert ~  age + sample + ethnicity + gender_simple + tenure, 
              data = df_all)

ghq_reg %>%
  tbl_regression(
    exponentiate = FALSE,
    label = list(
      sample ~ "Sample",
      age ~ "Alter",
      ethnicity ~ "Herkunft",
      gender_simple ~ "Gender",
      tenure ~ "Tenure"
    )
  ) %>%
  add_glance_source_note() %>%
  add_q(method = "fdr") %>%  
  bold_labels() %>%
  modify_header(label ~ "**Prädiktor**") %>%
  modify_fmt_fun(
    p.value = style_pvalue, 
    q.value = style_pvalue
  )

#### Plot für die Regression

ghq_plot <- ggpredict(ghq_reg) %>% 
  plot() %>% 
  sjPlot::plot_grid()

#### Report für die Regression

report_performance(ghq_reg)
report_effectsize(ghq_reg)
