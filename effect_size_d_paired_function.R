effect_size_d_paired <- function (x, y, conf.level = 0.95){ 
  source("conf_limits_nct.R")  
  
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(x-y) #standard deviation of the difference scores
  N <- length(x) #number of pairs
  s_av <- sqrt((sd1^2+sd2^2)/2) #averaged standard deviation of both measurements

  #Cohen's d_av, using s_av as standardizer
  m_diff <- mean(y-x)
  d_av <- m_diff/s_av
  d_av
  d_av_unb <- (1-(3/(4*(N-1)-1)))*d_av
  d_av_unb
  
  #get the t-value for the CI
  t_value <- m_diff/(s_diff/sqrt(N))
  
  nct_limits <- conf.limits.nct(t.value = t_value, df = N-1, conf.level = 0.95)
  ci_l_d_av <- nct_limits$Lower.Limit*s_diff/(s_av*sqrt(N))
  ci_u_d_av <- nct_limits$Upper.Limit*s_diff/(s_av*sqrt(N))
  ci_l_d_av
  ci_u_d_av

  #Cohen's d_z, using s_diff as standardizer
  d_z <- t_value/sqrt(N)
  d_z
  d_z_unb <- (1-(3/(4*(N-1)-1)))*d_z
  ci_l_d_z <- nct_limits$Lower.Limit/sqrt(N-1)
  ci_u_d_z <- nct_limits$Upper.Limit/sqrt(N-1)
  ci_l_d_z
  ci_u_d_z

  #perform t-test for 95% CI around mean difference
  test_res <- t.test(y, x, paired = TRUE, conf.level = conf.level)
  
  cat("Mean Difference    : ", paste0(round(m_diff, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(test_res$conf.int[1], digits = 4)),";",paste0(round(test_res$conf.int[2], digits = 4)), "]", sep = "")
  cat("\n")
  cat("Cohen's d_z_unb    : ", paste0(round(d_z_unb, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(ci_l_d_z, digits = 4)),";",paste0(round(ci_u_d_z, digits = 4)), "]", sep = "")
  cat("\n")
  cat("Cohen's d_av_unb   : ", paste0(round(d_av_unb, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(ci_l_d_av, digits = 4)),";",paste0(round(ci_u_d_av, digits = 4)), "]",sep = "")
  cat("\n")
  cat("s_diff: ", paste0(round(s_diff, digits = 4)),
      ", s_av: ", paste0(round(s_av, digits = 4)),
      ", sd1: ", paste0(round(sd1, digits = 4)),
      ", sd2: ", paste0(round(sd2, digits = 4)),
      ", cor: ", paste0(round(cor(x,y), digits = 4)),
      sep = "")
  cat("\n")
  cat("N = ", paste0(N)," pairs.",
      sep = "")
  invisible(list(m_diff = m_diff, 
                 m1 <- mean(x),
                 m2 <- mean(y),
                 ci_l_m_diff = test_res$conf.int[1], 
                 ci_u_m_diff = test_res$conf.int[2], 
                 d_av = d_av, 
                 d_av_unb = d_av_unb, 
                 s_av = s_av, 
                 s_diff = s_diff, 
                 ci_l_d_av = ci_l_d_av, 
                 ci_u_d_av = ci_u_d_av, 
                 d_z = d_z, 
                 d_z_unb = d_z_unb, 
                 ci_l_d_z = ci_l_d_z, 
                 ci_u_d_z = ci_u_d_z, 
                 N = N, 
                 m1 = mean(x), 
                 m2 = mean(y), 
                 sd1 = sd(x), 
                 sd2 = sd(y), 
                 t_value = t_value, 
                 df = N-1, 
                 cor = cor(x,y),
                 p_value = test_res$p.value))
}
