effect_size_d <- function (x, y, conf.level = 0.95){ 
  
  #First load function from MBESS package so this function works without installing the entire MBESS package (which is often difficult and takes long)
  source("conf_limits_nct.R")  
  
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  n1 <- length(x) #number of pairs
  n2 <- length(y) #number of pairs
  m_diff <- mean(y-x)
  sd_pooled <- (sqrt((((n1 - 1) * ((sd1^2))) + (n2 - 1) * ((sd2^2))) / ((n1 + n2 -2)))) #pooled standard deviation
  #Calculate Hedges' correction. Uses gamma, unless this yields a nan (huge n), then uses approximation
  j <- ifelse(is.na(gamma((n1 + n2 - 2)/2)/(sqrt((n1 + n2 - 2)/2) * gamma(((n1 + n2 - 2) - 1)/2))),
                    (1 - 3/(4 * (n1 + n2 - 2)-1)),
                    gamma((n1 + n2 - 2)/2)/(sqrt((n1 + n2 - 2)/2) * gamma(((n1 + n2 - 2) - 1)/2)))
  t_value <- m_diff / sqrt(sd_pooled^2 / n1 + sd_pooled^2 / n2)
  
  d <- m_diff / sd_pooled #Cohen's d
  d_unb <- d*j #Hedges g, of unbiased d

  ncp <- d / sqrt(1/n1 + 1/n2) #Calculate non-centrality parameter from d, Cumming (2013), 11.16, p. 305. Checked against g*power. Same as   ncp <- d * sqrt((n1 * n2)/(n1 + n2)) using inside mbess. This is also identical to the t-value
  ncp <- d * sqrt((n1 * n2)/(n1 + n2))
  
  nct_limits_d <- conf.limits.nct(ncp = d / sqrt(1/n1 + 1/n2), #non-centrality parameter for d
                  df = n1 + n2 - 2,
                  conf.level = 0.95)
  
  nct_limits_d_unb <- conf.limits.nct(ncp = d_unb / sqrt(1/n1 + 1/n2), #non-centrality parameter for d unbiased (hedges' g)
                          df = n1 + n2 - 2,
                          conf.level = 0.95)

  ci_l_d <- nct_limits_d$Lower.Limit * sqrt((n1 + n2)/(n1 * n2))
  ci_u_d <- nct_limits_d$Upper.Limit * sqrt((n1 + n2)/(n1 * n2))
  ci_l_d_unb <- nct_limits_d_unb$Lower.Limit * sqrt((n1 + n2)/(n1 * n2))
  ci_u_d_unb <- nct_limits_d_unb$Upper.Limit * sqrt((n1 + n2)/(n1 * n2))
  
  #perform t-test for 95% CI around mean difference
  test_res <- t.test(y, x, var.equal = TRUE, conf.level = conf.level)
  
  cat("Mean Difference: ", paste0(round(m_diff, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(test_res$conf.int[1], digits = 4)),";",paste0(round(test_res$conf.int[2], digits = 4)), "]", sep = "")
  cat("\n")
  cat("Cohen's d_unb  : ", paste0(round(d_unb, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(ci_l_d_unb, digits = 4)),";",paste0(round(ci_u_d_unb, digits = 4)), "]", sep = "")
  cat("\n")
  cat("Cohen's d      : ", paste0(round(d, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(ci_l_d, digits = 4)),";",paste0(round(ci_u_d, digits = 4)), "]",sep = "")
  cat("\n")
  cat("mean 1: ", paste0(round(mean(x), digits = 4)),
      ", mean 2: ", paste0(round(mean(y), digits = 4)),
      sep = "")
  cat("\n")
  cat("sd1: ", paste0(round(sd1, digits = 4)),
      ", sd2: ", paste0(round(sd2, digits = 4)),
      sep = "")
  cat("\n")
  cat("n1 = ", paste0(n1),", n2 = ", paste0(n2),
      sep = "")
  invisible(list(m_diff = m_diff,
                 m1 <- mean(x),
                 m2 <- mean(y),
                 ci_l_m_diff = test_res$conf.int[1], 
                 ci_u_m_diff = test_res$conf.int[2], 
                 d = d, 
                 d_unb = d_unb, 
                 sd_pooled = sd_pooled, 
                 ci_l_d = ci_l_d, 
                 ci_u_d = ci_u_d, 
                 ci_l_d_unb = ci_l_d_unb, 
                 ci_u_d_unb = ci_u_d_unb, 
                 n1 = n1, 
                 n2 = n2, 
                 m1 = mean(x), 
                 m2 = mean(y), 
                 sd1 = sd(x), 
                 sd2 = sd(y), 
                 cor = cor(x,y), 
                 t_value = t_value, 
                 df = n1 + n2 - 2, 
                 ncp = ncp,
                 p_value = test_res$p.value,
                 CL = pnorm(abs(m_diff)/sqrt(sd1^2 + sd2^2))))
}
