
## Alice C. wrote a function that I can modify to pull out r-hats: 
  
  stan_psum <- function(fit){
    
    iter <- fit@stan_args[[1]]$iter #1000
    s_init <- as.data.frame(rstan::summary(fit)$summary)
    s_init$pars <- row.names(s_init)
    s_init$n_eff_pct <- s_init$n_eff/iter  ## effective samples are the number of independent samples with the same estimation power as the N autocorrelated samples
    s_init$n_eff_less10pct <- ifelse(s_init$n_eff_pct < 0.10, yes = "true", no = "false") # 10% is often used as a threshold, below which the chains for a parameter did not properly converge
    s <- s_init[,c("pars","Rhat","n_eff_less10pct")]
    s <- s[!grepl('^mu', row.names(s_init)),]
    s <- s[s$Rhat > 1.1 | s$n_eff_less10pct == 'true',]
    spars <- get_sampler_params(fit, inc_warmup = FALSE)
    divtrans <- sum(sapply(spars, function(x) sum(x[,'divergent__'])))
    
    return(list(par_conv = s,
                divergent = divtrans)
    )
  }

```