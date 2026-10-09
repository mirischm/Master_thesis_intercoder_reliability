library(dplyr)

# Main topic frequencies PER CODER
freq_coder1 <- table(analyse_daten$maintopic_coder1)
freq_coder2 <- table(analyse_daten$maintopic_coder2) 
freq_coder3 <- table(analyse_daten$maintopic_coder3)
freq_ai     <- table(analyse_daten$maintopic_chat)
freq_truth <- table(analyse_daten$true_main)




# Percentages per coder (rounded to 1 decimal)
pct_coder1 <- round(prop.table(freq_coder1) * 100, 1)
pct_coder2 <- round(prop.table(freq_coder2) * 100, 1)
pct_coder3 <- round(prop.table(freq_coder3) * 100, 1)
pct_ai     <- round(prop.table(freq_ai) * 100, 1)
pct_truth  <- round(prop.table(freq_truth) * 100, 1)

# Combined table, codes 1-9 (codes that were never used show as 0)
codes <- factor(1:9)

pct_table <- data.frame(
  code   = 1:9,
  coder1 = as.numeric(prop.table(table(factor(analyse_daten$maintopic_coder1, levels = codes))) * 100),
  coder2 = as.numeric(prop.table(table(factor(analyse_daten$maintopic_coder2, levels = codes))) * 100),
  coder3 = as.numeric(prop.table(table(factor(analyse_daten$maintopic_coder3, levels = codes))) * 100),
  ai     = as.numeric(prop.table(table(factor(analyse_daten$maintopic_chat,   levels = codes))) * 100),
  truth  = as.numeric(prop.table(table(factor(analyse_daten$true_main,        levels = codes))) * 100)
) %>%
  mutate(across(-code, ~ round(.x, 1)))

print(pct_table)




# Severity per coder 
severity_dist <- data.frame(
  Coder = c("Coder1", "Coder2", "Coder3", "Truth", "AI")
) %>%
  mutate(
    Severe = c(
      sum(analyse_daten$severity_coder1 == 1, na.rm = TRUE),
      sum(analyse_daten$severity_coder2 == 1, na.rm = TRUE),
      sum(analyse_daten$severity_coder3 == 1, na.rm = TRUE),
      sum(analyse_daten$true_severity == 1, na.rm = TRUE),
      sum(analyse_daten$severity_chat == 1, na.rm = TRUE)
    ),
    Not_Severe = 300 - Severe,
    Perc_Severe = round(Severe / 300 * 100, 1)
  )

print(severity_dist)
