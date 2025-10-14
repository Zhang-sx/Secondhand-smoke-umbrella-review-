
# install.packages(c("readxl", "metafor", "metasens", "writexl"))

library(readxl)
library(metafor)
library(metasens)   # tes()
library(writexl)


dat <- read_excel("C:/patchwork", sheet = )

sheet1_name <- names(dat)[]

# Safety Digitalization
numify <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Rosenthal FSN：Stouffer Alternative implementation (when metafor:: FSN reports an error)
rosenthal_fsn_stouffer <- function(yi, sei, alpha=0.05){
  ok <- is.finite(yi) & is.finite(sei) & sei > 0
  if(sum(ok) < 1) return(NA_real_)
  yi_ok  <- yi[ok]
  sei_ok <- sei[ok]
  k <- length(yi_ok)
  zi <- yi_ok / sei_ok
  sum_z <- sum(zi)
  z_crit <- qnorm(1 - alpha/2)
  # NS，FSN=0
  if (abs(sum_z / sqrt(k)) <= z_crit) return(0)
  fsn_val <- (sum_z / z_crit)^2 - k
  return(max(0, ceiling(fsn_val)))
}

# Automatically identify the "effect size + confidence interval" column (compatible with or/rr/hr or difference effect)
detect_effect <- function(df){
  cn <- names(df)
  ratio_cols    <- c("OR","RR","HR")
  all_eff_cols  <- c(ratio_cols,"TE","yi","ES","SMD","g","d","effect","Effect")
  
  eff_col_name <- intersect(all_eff_cols, cn)[1]
  lci_col <- intersect(c("lci","LCI","lower","Lower","lo","loCI","ci.lb"), cn)[1]
  uci_col <- intersect(c("uci","UCI","upper","Upper","hi","hiCI","ci.ub"), cn)[1]
  
  if (is.na(eff_col_name) || is.na(lci_col) || is.na(uci_col)) {
    stop("Missing effect size or interval column (or/rr/hr/te/smd + LCI + UCI required)")
  }
  
  eff <- numify(df[[eff_col_name]])
  lci <- numify(df[[lci_col]])
  uci <- numify(df[[uci_col]])
  keep <- is.finite(eff) & is.finite(lci) & is.finite(uci)
  eff <- eff[keep]; lci <- lci[keep]; uci <- uci[keep]
  if (length(eff) < 2) stop("Insufficient valid data (<2 entries)")
  
  is_ratio <- eff_col_name %in% ratio_cols
  list(eff=eff, lci=lci, uci=uci, is_ratio=is_ratio)
}

#More robust random effects fitting (multiple control parameters / optimizer attempts)
fit_random <- function(yi, vi, try_hksj=TRUE){
  ctrl_list <- list(
    list(),
    list(stepadj=0.5, maxiter=10000),
    list(optimizer="nlminb"),
    list(optimizer="optim", "opt.method"="BFGS")
  )
  tests <- if (try_hksj) c("knha","t","z") else c("z")
  for (test in tests){
    for (ctrl in ctrl_list){
      fit <- try(rma(yi=yi, vi=vi, method="REML", test=test, control=ctrl), silent=TRUE)
      if (!inherits(fit,"try-error")) return(fit)
    }
    fit_ml <- try(rma(yi=yi, vi=vi, method="ML", test=test), silent=TRUE)
    if (!inherits(fit_ml,"try-error")){
      fit_reml <- try(update(fit_ml, method="REML", test=test), silent=TRUE)
      if (!inherits(fit_reml,"try-error")) return(fit_reml)
    }
  }
  stop("REML method failed to converge")
}

# The minimum number of new studies reaching "nominal significance" (default two-sided alpha=0.05)
min_m_for_nominal_sig <- function(yi, vi, tau2, theta_future=NULL, w_future=NULL, alpha=0.05){
  w <- 1/(vi + tau2)
  W <- sum(w); S <- sum(w*yi)
  zc <- qnorm(1 - alpha/2)
  if (is.null(theta_future)) theta_future <- sum(w*yi)/W
  if (is.null(w_future))     w_future     <- mean(w)
  A <- (w_future*theta_future)
  a <- A^2
  b <- (2*A*S) - (zc^2)*w_future
  c <- S^2 - (zc^2)*W
  if (abs(A) < .Machine$double.eps) return(Inf)
  disc <- b^2 - 4*a*c
  if (disc < 0) return(Inf)
  r1 <- (-b - sqrt(disc)) / (2*a); r2 <- (-b + sqrt(disc)) / (2*a)
  m_needed <- ceiling(max(r1, r2, 0))
  as.integer(m_needed)
}

#The minimum number of new studies reaching 80% of the condition assurance
min_m_for_CP <- function(yi, vi, tau2, theta_future=NULL, w_future=NULL, power=0.80, alpha=0.05, max_add=2000){
  w <- 1/(vi + tau2)
  W <- sum(w); S <- sum(w*yi)
  zc <- qnorm(1 - alpha/2)
  if (is.null(theta_future)) theta_future <- sum(w*yi)/W
  if (is.null(w_future))     w_future     <- mean(w)
  for (m in 1:max_add){
    Wt <- W + m*w_future
    mu <- (S + m*w_future*theta_future) / sqrt(Wt)
    sd <- sqrt( (m*w_future) / Wt )
    # Condition assurance of two-sided test
    cp <- pnorm(-zc, mean=mu, sd=sd) + (1 - pnorm(zc, mean=mu, sd=sd))
    if (cp >= power) return(m)
  }
  Inf
}

#Single run (core): input effect + interval, output all statistics
run_meta_once <- function(eff, lci, uci, is_ratio){
  if (is_ratio){
    yi  <- log(eff)
    sei <- (log(uci) - log(lci)) / (2*1.96)
  } else {
    yi  <- eff
    sei <- (uci - lci) / (2*1.96)
  }
  vi <- sei^2
  ok <- is.finite(yi) & is.finite(vi) & vi > 0
  yi <- yi[ok]; vi <- vi[ok]; sei <- sei[ok]
  if (length(yi) < 2) stop("有效研究条目不足（<2条）")
  
  fit <- fit_random(yi, vi, try_hksj = TRUE)
  
  b   <- as.numeric(coef(fit))
  se  <- sqrt(diag(vcov(fit)))[1]
  p   <- if(!is.null(fit$pval)) fit$pval else 2*pnorm(abs(b/se), lower.tail=FALSE)
  tau2 <- if(!is.null(fit$tau2)) fit$tau2 else NA_real_
  I2   <- if(!is.null(fit$I2)) fit$I2 else {
    k <- length(yi); df <- k - 1
    QE <- if(!is.null(fit$QE)) fit$QE else NA_real_
    if (is.finite(QE) && df>0) max(0, (QE - df)/QE)*100 else NA_real_
  }
  
  if (is_ratio){
    TE <- exp(b)
  } else {
    TE <- b
  }
  lo <- if(is_ratio) exp(fit$ci.lb) else fit$ci.lb
  hi <- if(is_ratio) exp(fit$ci.ub) else fit$ci.ub
  # ============================================
  
  egger_p <- tryCatch({
    regtest(yi, sei, model="lm")$pval
  }, error=function(e) NA_real_)
  
  tes_p <- tryCatch({
    tes(fit, H0=0, alternative="two.sided", alpha=.05)$pval
  }, error=function(e) NA_real_)
  
  rosenberg  <- NA_real_
  rosenthal  <- NA_real_
  m_nom_pool <- NA_integer_; m_nom_max <- NA_integer_
  m_cp80_pool<- NA_integer_; m_cp80_max<- NA_integer_
  notes <- ""
  
  w      <- 1/(vi + ifelse(is.finite(tau2), tau2, 0))
  wbar   <- mean(w)
  theta_pool <- b
  theta_max  <- yi[ which.max(w) ]
  
  if (isTRUE(p < 0.05)) {
    rosenberg <- tryCatch({
      as.numeric(fsn(yi=yi, vi=vi, type="Rosenberg")$fsnum)
    }, error=function(e) NA_real_)
    rosenthal <- tryCatch({
      as.numeric(fsn(yi=yi, vi=vi, type="Rosenthal")$fsnum)
    }, error=function(e) {
      rosenthal_fsn_stouffer(yi, sqrt(vi))
    })
  } else {
    if (!is.finite(tau2)) tau2 <- 0
    m_nom_pool  <- min_m_for_nominal_sig(yi, vi, tau2, theta_future=theta_pool, w_future=wbar, alpha=0.05)
    m_nom_max   <- min_m_for_nominal_sig(yi, vi, tau2, theta_future=theta_max,  w_future=wbar, alpha=0.05)
    m_cp80_pool <- min_m_for_CP(yi, vi, tau2, theta_future=theta_pool, w_future=wbar, power=0.80, alpha=0.05)
    m_cp80_max  <- min_m_for_CP(yi, vi, tau2, theta_future=theta_max,  w_future=wbar, power=0.80, alpha=0.05)
    if (is.infinite(m_nom_pool) && is.infinite(m_cp80_pool)) {
      notes <- "在默认情景下，即使增加大量研究也难以达成名义显著/80%把握度；请检查θ*与权重设定"
    }
  }
  
  list(
    k                = length(yi),
    TE_random        = TE,
    lower_random     = lo,
    upper_random     = hi,
    pval_random      = p,
    tau2             = tau2,
    I2               = I2,
    Egger_p          = egger_p,
    TES_p            = tes_p,
    FSN_Rosenberg    = rosenberg,
    FSN_Rosenthal    = rosenthal,
    m_nominal_theta_pool = m_nom_pool,
    m_nominal_theta_max  = m_nom_max,
    m_CP80_theta_pool    = m_cp80_pool,
    m_CP80_theta_max     = m_cp80_max,
    notes            = notes
  )
}


det <- detect_effect(dat)
res <- run_meta_once(det$eff, det$lci, det$uci, det$is_ratio)

# Organize into data frame (write_xlsx requires data.frame)
result_df <- data.frame(
  k_studies              = res$k,
  pooled_effect          = round(res$TE_random, 3),
  lower_ci               = round(res$lower_random, 3),
  upper_ci               = round(res$upper_random, 3),
  p_value                = signif(res$pval_random, 3),
  tau2                   = round(res$tau2, 4),
  I2                     = round(res$I2, 1),
  Egger_p                = signif(res$Egger_p, 3),
  TES_p                  = signif(res$TES_p, 3),
  FSN_Rosenberg          = res$FSN_Rosenberg,
  FSN_Rosenthal          = res$FSN_Rosenthal,
  m_nominal_theta_pool   = res$m_nominal_theta_pool,
  m_nominal_theta_max    = res$m_nominal_theta_max,
  m_CP80_theta_pool      = res$m_CP80_theta_pool,
  m_CP80_theta_max       = res$m_CP80_theta_max,
  notes                  = res$notes,
  stringsAsFactors = FALSE
)

#Print on console at the same time (optional)
print(result_df)


folder_path <- "patchwork"  # 
# timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
# output_file <- file.path(folder_path, paste0("meta_analysis_results_", timestamp, ".xlsx"))

output_file <- file.path(folder_path, "name")  

# write Excel（会新建或覆盖同名文件）
write_xlsx(result_df, output_file)
cat("Results saved to folder successfully：", output_file, "\n")


