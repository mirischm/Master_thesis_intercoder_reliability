library(dplyr)

# Main topic frequencies PER CODER
freq_coder1 <- table(analyse_daten$maintopic_coder1)
freq_coder2 <- table(analyse_daten$maintopic_coder2) 
freq_coder3 <- table(analyse_daten$maintopic_coder3)
freq_ai     <- table(analyse_daten$maintopic_chat)

# Combine into data frame
main_dist <- data.frame(
  Category = names(freq_coder1),
  Coder1 = as.numeric(freq_coder1),
  Coder2 = as.numeric(freq_coder2[match(names(freq_coder1), names(freq_coder2))]),
  Coder3 = as.numeric(freq_coder3[match(names(freq_coder1), names(freq_coder3))]),
  AI = as.numeric(freq_ai[match(names(freq_coder1), names(freq_ai))])
) %>%
  mutate(
    Average_Count = round((Coder1 + Coder2 + Coder3 + AI) / 4, 1),
    Perc_Total = round(Average_Count / 300 * 100, 1)
  ) %>%
  arrange(desc(Average_Count))

print(main_dist)



# Severity per coder 
severity_dist <- data.frame(
  Coder = c("Coder1", "Coder2", "Coder3", "AI")
) %>%
  mutate(
    Severe = c(
      sum(analyse_daten$severity_coder1 == 1, na.rm = TRUE),
      sum(analyse_daten$severity_coder2 == 1, na.rm = TRUE),
      sum(analyse_daten$severity_coder3 == 1, na.rm = TRUE),
      sum(analyse_daten$severity_chat == 1, na.rm = TRUE)
    ),
    Not_Severe = 300 - Severe,
    Perc_Severe = round(Severe / 300 * 100, 1)
  )

print(severity_dist)

