categories <- 1:9

# save results
results <- data.frame(
  category = integer(),
  alpha_human = numeric(),
  alpha_mit_ai = numeric()
)

for (k in categories) {
  
  # Just Human Coder (binary)
  cat_human <- dat %>%
    mutate(
      c1 = as.integer(maintopic_coder1 == k),
      c2 = as.integer(maintopic_coder2 == k),
      c3 = as.integer(maintopic_coder3 == k)
    ) %>%
    select(c1, c2, c3)
  
  # Human Coder + AI (binary)
  cat_ai <- dat %>%
    mutate(
      c1 = as.integer(maintopic_coder1 == k),
      c2 = as.integer(maintopic_coder2 == k),
      c3 = as.integer(maintopic_coder3 == k),
      c4 = as.integer(maintopic_chat == k)
    ) %>%
    select(c1, c2, c3, c4)
  
  alpha_human <- irr::kripp.alpha(t(cat_human), method = "nominal")$value
  alpha_ai    <- irr::kripp.alpha(t(cat_ai),    method = "nominal")$value
  
  results <- rbind(results, data.frame(
    category    = k,
    alpha_human  = round(alpha_human, 3),
    alpha_mit_ai = round(alpha_ai, 3)
  ))
}


results$prevalence <- sapply(categories, function(k) {
  mean(dat$maintopic_coder1 == k | 
         dat$maintopic_coder2 == k | 
         dat$maintopic_coder3 == k)
})

print(results)
