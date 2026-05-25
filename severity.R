library(tidyverse)
library(irr)
library(krippendorffsalpha)
library(dplyr)

dat<-analyse_daten

x1 <- dat[, c("severity_coder1","severity_coder2")]
x2 <- dat[, c("severity_coder2","severity_coder3")]
x3 <- dat[, c("severity_coder1","severity_coder3")]

x4 <- dat[, c("severity_coder1","severity_coder2","severity_coder3")]
x5 <- dat[, c("severity_coder1","severity_coder3","severity_chat")]
x6 <- dat[, c("severity_coder2","severity_coder3","severity_chat")]
x7 <- dat[, c("severity_coder1","severity_coder2","severity_chat")]

x8 <- dat[, c("severity_coder1","severity_coder2","severity_coder3", "severity_chat")]

ratings1 <- t(x1)
irr::kripp.alpha(ratings1, method = "nominal")

ratings2 <- t(x2)
irr::kripp.alpha(ratings2, method = "nominal")

ratings3 <- t(x3)
irr::kripp.alpha(ratings3, method = "nominal")

ratings4 <- t(x4)
irr::kripp.alpha(ratings4, method = "nominal")

ratings5 <- t(x5)
irr::kripp.alpha(ratings5, method = "nominal")

ratings6 <- t(x6)
irr::kripp.alpha(ratings6, method = "nominal")


ratings7 <- t(x7)
irr::kripp.alpha(ratings7, method = "nominal")


ratings8 <- t(x8)
irr::kripp.alpha(ratings8, method = "nominal")