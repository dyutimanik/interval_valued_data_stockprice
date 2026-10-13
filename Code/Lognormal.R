rm(list=ls())
set.seed(1234)

# Defining required functions

logn_est = function(x1,xk,k,n) {
  # Function to estimate miu, sigma
  
  ak = qnorm((1-0.375)/(k+0.25))
  bk = -ak
  log_x1_bar = mean(log(x1))
  log_xk_bar = mean(log(xk))
  sigma = (log_xk_bar-log_x1_bar)/(bk-ak)
  miu = log_xk_bar-sigma*bk
  
  return(c(miu=miu,sigma=sigma))
}

logn_gof = function(x1,xk,k,miu,sigma,n) {
  # Function to perform the KS tests
  
  # Simulating from Lognormal(miu_hat,sigma_hat)
  sim = replicate(n,rlnorm(k,miu,sigma))
  x1_sim = apply(sim,2,min)
  xk_sim = apply(sim,2,max)
  
  # Testing for Midpoint
  sample_mp = (x1+xk)/2
  sim_mp = (x1_sim+xk_sim)/2
  mp = ks.test(sample_mp, sim_mp)
  
  # Testing for Range
  sample_range = xk-x1
  sim_range = xk_sim-x1_sim
  r = ks.test(sample_range, sim_range)
  
  return(c(mp=mp$p.value,r=r$p.value))
}

calculations = function(data,k) {
  # Function to get the required results for a particular dataset
  
  # Defining required variables
  high = as.numeric(gsub(",","",data[,4]))
  low = as.numeric(gsub(",","",data[,5]))
  prev_close = as.numeric(gsub(",","",data[,6]))
  n = length(prev_close)
  # Calculating price ratios
  x1=low/prev_close
  xk=high/prev_close
  
  # Obtaining parameter estimates
  est=logn_est(x1,xk,k,n)
  miu=est[["miu"]]; sigma=est[["sigma"]]
  
  # KS tests
  gof=logn_gof(x1,xk,k,miu,sigma,n)

  return(c(n=as.integer(n),miu=round(miu,4),sigma=round(sigma,4),
           mp=round(gof[["mp"]],4),r=round(gof[["r"]],4)))
}

# Main

comp = c("SB","PG","IR","RL","TC","HD")
time = c("1m","3m","6m","1y")

results=list()

for(k in c(2,4)){
  for(cmp in comp){
    for(t in time){
      data = read.csv(file.path("Data", t, paste0(cmp, ".csv")))
      # Excluding HDFCBANK 1-year data for 25-28Aug'2025
      if(t=="1y" && cmp=="HD"){
        data=data[!(data$DATE %in% c("25-Aug-2025","26-Aug-2025","28-Aug-2025")),]
      }
      res=calculations(data,k)
      results[[length(results)+1]]=data.frame(
        k=k,Stock=cmp,Duration=t,n=res["n"],mu=res["miu"],sigma=res["sigma"],
        Midpoint_p_value=res["mp"],Range_p_value=res["r"])
    }
  }
}

results=do.call(rbind,results)

# Displaying results
for(k in c(2,4)){
  cat("\nLognormal parameter estimates (k =",k,")\n")
  print(results[results$k==k,c("Stock","Duration","n","mu","sigma")],row.names=FALSE)
  cat("\nLognormal KS goodness-of-fit p-values (k =",k,")\n")
  print(results[results$k==k,c("Stock","Duration","Midpoint_p_value","Range_p_value")],row.names=FALSE)
}

# Saving results
write.csv(results, file.path("Results", "lognormal_results.csv"), row.names=FALSE)
