rm(list = ls())
set.seed(1423)

library(evd)
library(VineCopula)

# Defining required functions

fit_gev_min = function(mins) {
  # Function to fit GEV distribution to minima
  
  fit = fgev(-mins, std.err = FALSE)
  p = fit$estimate
  
  return(list(
    params = p,
    F = function(x) 1 - pgev(-x, loc = p[1], scale = p[2], shape = p[3]),
    q = function(u) -qgev(1 - u, loc = p[1], scale = p[2], shape = p[3])
  ))
}

fit_gev_max = function(maxs) {
  # Function to fit GEV distribution to maxima
  
  fit = fgev(maxs, std.err = FALSE)
  p = fit$estimate
  
  return(list(
    params = p,
    F = function(x) pgev(x, loc = p[1], scale = p[2], shape = p[3]),
    q = function(u) qgev(u, loc = p[1], scale = p[2], shape = p[3])
  ))
}

copula_est = function(U, V) {
  # Function to estimate the Gumbel copula parameter
  
  U = pmin(pmax(U, 1e-6), 1 - 1e-6)
  V = pmin(pmax(V, 1e-6), 1 - 1e-6)
  
  fit = BiCopEst(U, V, family = 4) # for Gumbel copula we have family=4
  
  return(fit$par)
}

copula_gof = function(x1, xk, min_model, max_model, theta, n) {
  # Function to perform the KS tests
  
  #Simulating from estimated distribution
  sim = BiCopSim(n, family = 4, par = theta)
  x1_sim = min_model$q(sim[,1])
  xk_sim = max_model$q(sim[,2])
  
  # Testing for Midpoint
  sample_mp = (x1 + xk) / 2
  sim_mp = (x1_sim + xk_sim) / 2
  mp = ks.test(sample_mp, sim_mp)
  
  # Testing for Range
  sample_range = xk - x1
  sim_range = xk_sim - x1_sim
  r = ks.test(sample_range, sim_range)
  
  return(c(mp = mp$p.value, r = r$p.value))
}

calculations = function(data) {
  # Function to get the required results for a particular dataset

  # Defining required variables
  high = as.numeric(gsub(",", "", data[,4]))
  low = as.numeric(gsub(",", "", data[,5]))
  prev_close = as.numeric(gsub(",", "", data[,6]))
  n = length(prev_close)
  # Calculating price returns
  x1 = low / prev_close - 1
  xk = high / prev_close - 1
  
  # Fitting marginal distributions
  min_model = fit_gev_min(x1)
  max_model = fit_gev_max(xk)
  
  # Transforming to uniform margins
  U = min_model$F(x1)
  V = max_model$F(xk)
  
  # Estimating the Gumbel copula parameter
  theta = copula_est(U, V)
  
  # KS tests
  gof = copula_gof(x1, xk, min_model, max_model, theta, n)
  
  p_min = min_model$params
  p_max = max_model$params
  
  return(c(
    n = as.integer(n),
    # GEV min
    mu_min = round(unname(p_min[1]), 4),
    sigma_min = round(unname(p_min[2]), 4),
    xi_min = round(unname(p_min[3]), 4),
    # GEV max
    mu_max = round(unname(p_max[1]), 4),
    sigma_max = round(unname(p_max[2]), 4),
    xi_max = round(unname(p_max[3]), 4),
    # Copula parameter
    theta = round(theta, 4),
    # KS test results
    mp = round(gof[["mp"]], 4),
    r = round(gof[["r"]], 4)
  ))
}

# Main

comp = c("SB", "PG", "IR", "RL", "TC", "HD")
time = c("1m", "3m", "6m", "1y")

results = list()

for (cmp in comp) {
  for (t in time) {
    data = data = read.csv(file.path("Data", "Time", t, paste0(cmp, ".csv")))
    # Excluding HDFCBANK 1-year data for 25-28Aug'2025
    if (t == "1y" && cmp == "HD") {
      data = data[!(data$DATE %in% c(
        "25-Aug-2025", "26-Aug-2025", "28-Aug-2025"
      )), ]
    }
    res = calculations(data)
    results[[length(results) + 1]] = data.frame(
      Stock = cmp,
      Duration = t,
      n = unname(res[["n"]]),
      mu_min = unname(res[["mu_min"]]),
      sigma_min = unname(res[["sigma_min"]]),
      xi_min = unname(res[["xi_min"]]),
      mu_max = unname(res[["mu_max"]]),
      sigma_max = unname(res[["sigma_max"]]),
      xi_max = unname(res[["xi_max"]]),
      theta = unname(res[["theta"]]),
      Midpoint_p_value = unname(res[["mp"]]),
      Range_p_value = unname(res[["r"]]),
      check.names = FALSE
    )
  }
}

results = do.call(rbind, results)

# Displaying results
cat("\nGEV-Gumbel copula parameter estimates\n")
print(results[, c(
  "Stock", "Duration", "n",
  "mu_min", "sigma_min", "xi_min",
  "mu_max", "sigma_max", "xi_max", "theta"
)], row.names = FALSE)
cat("\nGEV-Gumbel copula KS goodness-of-fit p-values\n")
print(results[, c(
  "Stock", "Duration", "Midpoint_p_value", "Range_p_value"
)], row.names = FALSE)

# Saving results
write.csv(results, file.path("Results", "gumbel_copula_results.csv"), row.names=FALSE)
