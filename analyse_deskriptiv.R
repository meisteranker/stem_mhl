#### Pakete laden

library(tidyverse)
library(gtsummary)
library(gtsummary)

### Position

df_all |>
  select(sample, position_simple) |>
  tbl_summary(
    by = sample,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) |>
  modify_header(label ~ "**Position**") |>
  modify_caption("**Verteilung von Positionen nach Stichprobe**")

df_all %>%
  select(sample, mhlq_mean) %>%
  tbl_summary(by = sample) %>%
  add_p()


df_all %>%
  select(sample) %>%
  tbl_summary()