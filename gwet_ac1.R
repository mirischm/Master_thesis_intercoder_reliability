library(tidyverse)
library(irr)
library(irrCAC)
library(dplyr)

dat <- analyse_daten

x1 <- dat[, c("severity_coder1","severity_coder2")]
x2 <- dat[, c("severity_coder2","severity_coder3")]
x3 <- dat[, c("severity_coder1","severity_coder3")]

x4 <- dat[, c("severity_coder1","severity_coder2","severity_coder3")]
x5 <- dat[, c("severity_coder1","severity_coder3","severity_chat")]
x6 <- dat[, c("severity_coder2","severity_coder3","severity_chat")]
x7 <- dat[, c("severity_coder1","severity_coder2","severity_chat")]

x8 <- dat[, c("severity_coder1","severity_coder2","severity_coder3","severity_chat")]


#ICR Gwet ac1
irrCAC::gwet.ac1.raw(x1, weights = "unweighted", categ.labels = c(0,1))
irrCAC::gwet.ac1.raw(x2, weights = "unweighted", categ.labels = c(0,1))
irrCAC::gwet.ac1.raw(x3, weights = "unweighted", categ.labels = c(0,1))
irrCAC::gwet.ac1.raw(x4, weights = "unweighted", categ.labels = c(0,1))
irrCAC::gwet.ac1.raw(x5, weights = "unweighted", categ.labels = c(0,1))
irrCAC::gwet.ac1.raw(x6, weights = "unweighted", categ.labels = c(0,1))
irrCAC::gwet.ac1.raw(x7, weights = "unweighted", categ.labels = c(0,1))
irrCAC::gwet.ac1.raw(x8, weights = "unweighted", categ.labels = c(0,1))

#per category
# Per-category agreement on the 1s for each subset

agree_on_1 <- function(df) {
  ones <- df[rowSums(df == 1) == ncol(df), ]   # rows where all raters said 1
  total_ones <- sum(rowSums(df == 1) > 0)       # rows where at least one rater said 1
  cat("Full agreement on 1:", nrow(ones), "/ At least one said 1:", total_ones,
      "→ pa(1) =", round(nrow(ones) / total_ones, 3), "\n")
}

agree_on_1(x1)
agree_on_1(x2)
agree_on_1(x3)
agree_on_1(x4)
agree_on_1(x5)
agree_on_1(x6)
agree_on_1(x7)
agree_on_1(x8)