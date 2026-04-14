library(tidyverse)
library(gtsummary)
library(ggpubr)
library(psych)


df_all <- df_all %>%
  mutate(position_group = case_when(
    position %in% c("pos_professor", "pos_full_prof", "pos_dist_prof", "pos_prof") ~ "Professor",
    position %in% c("pos_assist_prof", "pos_assoc_prof", "pos_junior_prof",
                    "pos_akad_council", "pos_priv_docent") ~ "Assistant/Associate Professor",
    position %in% c("pos_postdoc", "pos_group_leader") ~ "Post-Doc",
    position %in% c("pos_assist_res_prof", "pos_res_prof", "pos_assoc_res_prof") ~ "Research Professor",
    TRUE ~ "Other"
  ))


df_all <- df_all %>%
  mutate(sample = factor(sample, 
                         levels = c("de", "us"),
                         labels = c("Germany", "United States")))

my_comparisons <- list(c("de", "us"))

# Berechne Wilcoxon-Test
pval <- compare_means(mhlq_score ~ sample, data = df_all, method = "wilcox.test") %>%
  pull(p)

# Formatierter p-Wert für APA
p_string <- if (pval < 0.001) {
  "p < .001"
} else {
  paste0("p = ", formatC(pval, format = "f", digits = 3))
}

y_max <- max(df_all$mhlq_score, na.rm = TRUE)
y_bracket <- y_max + 1.5
y_label <- y_max + 3

# Erstelle Boxplot mit APA-Stil
ggboxplot(df_all, x = "sample", y = "mhlq_score",
          fill = "sample",
          palette = c("Germany" = "gray50", "United States" = "gray80"),
          width = 0.6,
          outlier.shape = NA,
          add = "jitter",
          add.params = list(size = 1, alpha = 0.3)) +
  
  # Manuell gezeichneter Balken
  annotate("segment", x = 1, xend = 2, y = y_bracket, yend = y_bracket, size = 0.6) +
  annotate("segment", x = 1, xend = 1, y = y_bracket, yend = y_bracket - 0.7, size = 0.6) +
  annotate("segment", x = 2, xend = 2, y = y_bracket, yend = y_bracket - 0.7, size = 0.6) +
  
  # Formatierter p-Wert als Text
  annotate("text", x = 1.5, y = y_label, label = p_string, size = 4.5) +
  
  labs(
    title = NULL,
    x = "Group",
    y = ""
  ) +
  scale_y_continuous(limits = c(50, y_label + 5)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "none",
    text = element_text(family = "sans")
  )