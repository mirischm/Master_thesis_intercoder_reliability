library(dplyr)
library(irr)

data <- analyse_daten

d <- data %>%
  select(maintopic_coder1, maintopic_coder2, maintopic_coder3) %>%
  na.omit()

nrow(d)   

#  Contingency tables
table(d$maintopic_coder1, d$maintopic_coder2)
table(d$maintopic_coder1, d$maintopic_coder3)
table(d$maintopic_coder2, d$maintopic_coder3)

# Krippendorff´s alpha 
alpha <- function(x) {
  kripp.alpha(t(as.matrix(x)), method = "nominal")$value
}

# Count how often two categories were confused
count_confusions <- function(d, a, b) {
  coder_pairs <- list(c(1, 2), c(1, 3), c(2, 3))
  sum(sapply(coder_pairs, function(p) {
    x <- d[[p[1]]]
    y <- d[[p[2]]]
    sum((x == a & y == b) | (x == b & y == a))
  }))
}

# merge two categories
merge_pair <- function(d, keep, drop) {
  d[d == drop] <- keep
  d
}

pairs <- data.frame(
  pair = c("Sexuality + Sexual health",
           "Relationships + Mental health",
           "Body + Sexual health",
           "Body + Mental health"),
  keep = c(2, 4, 1, 1),
  drop = c(3, 5, 3, 5)
)


original <- alpha(d)

results <- data.frame(
  pair = pairs$pair,
  disagreements = mapply(function(k, dr) count_confusions(d, k, dr),
                         pairs$keep, pairs$drop),
  alpha_original = round(original, 3),
  alpha_merged = round(mapply(function(k, dr) alpha(merge_pair(d, k, dr)),
                              pairs$keep, pairs$drop), 3)
)
results$change <- round(results$alpha_merged - results$alpha_original, 3)

results