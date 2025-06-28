# Data-reanalysis code
install.packages("meta")
install.packages("metafor")
library(meta)
library(metafor)

# dichotomous variable
# data input
or_values <- c()
lower_ci <- c()
upper_ci <- c()
log_or <- log(or_values)
sei <- (log(upper_ci) - log(lower_ci)) / (2 * 1.96)
studlab <- paste("Study", 1:length(or_values))
meta_analysis <- metagen(TE = log_or, seTE = sei, studlab = studlab, sm = "OR", method.tau = "DL")

# forestplot
forest(meta_analysis)
summary(meta_analysis)

# p-value
p_value <- meta_analysis$pval.random
p_value

# Egger regression
res <- rma(yi = log_or, sei = sei, method = "DL")
regtest_res <- regtest(res, model = "lm")
print(regtest_res)
summary(res)
regtest(res, model = "lm")
predict(res, transf=exp)

# Excess Significance test
excesss_res <- tes(res, H0=0, alternative="two.sided", alpha=.05)
cat("Excess significance P-value: ", format(excesss_res$pval, digits=7), "\n")

# continuous variable
# data input
SMD <- c()
lower <- c()
upper <- c()
data <- data.frame(
  effect_size = SMD,
  lower = lower,
  upper = upper,
  study = paste("Study", 1:length(SMD))
)

data$seTE <- (data$upper - data$lower) / (2 * qnorm(0.975))
meta_analysis <- metagen(
  TE = data$effect_size, 
  seTE = data$seTE, 
  data = data, 
  studlab = data$study,
  sm = "SMD",       
  method.tau = "DL" )
summary(meta_analysis)

# forestplot
forest(meta_analysis)

# p-value
p_value_DL <- meta_analysis$pval.random
cat("P-value (REML model):", p_value_DL, "\n")

# Egger regression
egger_res <- regtest(res, model="lm")
cat("Egger test P-value: ", format(egger_res$pval, digits=7), "\n")
