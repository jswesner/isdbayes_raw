library(brms)

rparetocounts <- function(n, mu, vreal2, vreal3) {
  samples <- numeric(n)
  {
    if(vreal2 <= 0 | vreal2 >= vreal3) stop("Parameters out of bounds in rPLB")
    u <- runif(n)
    if(mu != -1)
    { y <- ( u*vreal3^(mu+1) +  (1-u) * vreal2^(mu+1) ) ^ (1/(mu+1))
    } else
    { y <- vreal3^u * vreal2^(1-u)
    }
    return(y)
  }
  return(samples)
}

dparetocounts <- function(x, mu, vreal2, vreal3) {
  if (vreal2 <= 0 || vreal2 >= vreal3)
    stop("Parameters out of bounds in dPLB")

  if (x < vreal2 || x > vreal3)
    return(0)

  if (mu != -1) {
    density <- (mu + 1) * (x^(mu+1)) / (vreal3^(mu+1) - vreal2^(mu+1))
  } else {
    density <- x^(-2) / (vreal2 * log(vreal3/vreal2))
  }

  density
}


log_lik_paretocounts <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  y <- prep$data$Y[i]
  dparetocounts(x, mu, vreal2, vreal3)
}

posterior_predict_paretocounts <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  vreal2 = prep$data$vreal2[i]
  vreal3 = prep$data$vreal3[i]
  rparetocounts(prep$ndraws, mu, vreal2, vreal3)
}

posterior_epred_paretocounts <- function(prep) {
  mu <- prep$dpars$mu
  return(mu)
}

# paretocounts <- custom_family(
#   "paretocounts", dpars = c("mu"),
#   links = c("identity"),
#   lb = -Inf, ub = Inf,
#   type = "real", vars = c("vreal1[n]",
#                           "vreal2[n]",
#                           "vreal3[n]"),
#   posterior_predict = posterior_predict_paretocounts)

stan_funs <- "
real paretocounts_lpdf(real Y, real mu, real vreal1, real vreal2, real vreal3){
    if(mu != -1)
    return(vreal1*(log((mu+1) / ( vreal3^(mu+1) - vreal2^(mu+1))) + mu*log(Y)));
    else
    return(vreal1*(log(log(vreal2) - log(vreal3)) + mu*log(Y)));
}
"
stanvars <- stanvar(scode = stan_funs, block = "functions")

paretocounts <- function(){custom_family(
  "paretocounts",
  dpars = c("mu"),
  links = c("identity"),
  lb = -Inf,
  ub = Inf,
  type = "real",
  vars = c("vreal1[n]",
           "vreal2[n]",
           "vreal3[n]"),
  posterior_predict = function(prep) {
    mu <- prep$dpars$mu
    return(mu)
    },
  posterior_epred = function(prep) {
    mu <- prep$dpars$mu
    return(mu)
  },
  log_lik = function(i, prep) {
    mu <- brms::get_dpar(prep, "mu", i = i)
    y <- prep$data$Y[i]
    dparetocounts(x, mu, vreal2, vreal3)
  })
}

library(usethis)
use_r("paretocounts")
use_r("rparetocounts")
use_r("dparetocounts")
use_r("log_lik_paretocounts")
use_r("posterior_predict_paretocounts")

devtools::load_all()

library(tidyverse)
lambda = -1.2 # lambda value
xmin = 1 # minimum size
xmax = 1000 # maximum size

sim_data = tibble(x = rparetocounts(n = 100,
                                    mu = lambda, # use brms terminology
                                    vreal2 = xmin,
                                    vreal3 = xmax)) %>%
  mutate(xmin = xmin, xmax = xmax) %>%
  group_by(x) %>%
  add_count(name = "counts") # column for density of each body size

fit = brm(x | vreal(counts, xmin, xmax) ~ 1,
          family = paretocounts,
          data = sim_data,
          stanvars = stanvars,
          prior = c(prior(normal(-1.2, 0.2), class = "Intercept")))

devtools::load_all()
rparetocounts()

