rm(list=ls())
set.seed(1234)


# Defining required functions

uniform_est = function(x1,xk,k,n) {
  # Function to estimate a, b
  
  a = mean(x1)*k/(k-1) - mean(xk)/(k-1)
  b = mean(xk)*k/(k-1) - mean(x1)/(k-1)
  sa = sqrt((k*(b-a)^2)/(n*(k-1)*(k+1)*(k+2)))
  sb = sa
  
  return(c(a=a,b=b,se_a=sa,se_b=sb))
}

uniform_gof = function(x1,xk,k,a,b,n) {
  # Function to perform the KS tests
  
  # Simulating from U(a_hat,b_hat)
  sim = replicate(n,runif(k,a,b))
  x1_sim = apply(sim,2,min)
  xk_sim = apply(sim,2,max)
  
  # Testing for Midpoint
  sample_mp = (x1+xk)/2
  sim_mp = (x1_sim+xk_sim)/2
  mp = ks.test(sample_mp,sim_mp)
  
  # Testing for Range
  sample_range = xk-x1
  sim_range = xk_sim-x1_sim
  r = ks.test(sample_range,sim_range)
  
  return(c(mp=mp$p.value,r=r$p.value))
}

calculations = function(data,k) {
  # Function to get the required results for a particular dataset
  
  # Defining required variables
  high = as.numeric(gsub(",","",data[,4]))
  low = as.numeric(gsub(",","",data[,5]))
  prev_close = as.numeric(gsub(",","",data[,6]))
  n = length(prev_close)
  # Calculating price returns
  x1 = low/prev_close-1
  xk = high/prev_close-1
  
  # Obtaining parameter estimates
  est = uniform_est(x1,xk,k,n)
  a = est[["a"]]; b = est[["b"]]
  se_a = est[["se_a"]]; se_b = est[["se_b"]]
  
  # KS tests
  gof = uniform_gof(x1,xk,k,a,b,n)
  
  return(c(n=as.integer(n),a=round(a,4),b=round(b,4),
           se_a=round(se_a,4),se_b=round(se_b,4),
           mp=round(gof[["mp"]],4),r=round(gof[["r"]],4)))
}

# Main

setwd("C://Users//DYUTI MANIK//Desktop//Arnab_Sir//ASMBI//Github//Data//Time")

comp = c("SB","PG","IR","RL","TC","HD")
time = c("1m","3m","6m","1y")

results = list()

for(k in c(2,4)){
  for(cmp in comp){
    for(t in time){
      data = read.csv(paste0(t,"//",cmp,".csv"))
      # Excluding HDFCBANK 1-year data for 25-28Aug'2025
      if(t=="1y" && cmp=="HD"){
        data = data[!(data$DATE %in% c("25-Aug-2025","26-Aug-2025","28-Aug-2025")),]
      }
      res = calculations(data,k)
      results[[length(results)+1]] = data.frame(
        k=k,Stock=cmp,Duration=t,n=res["n"],a=res["a"],b=res["b"],
        se_a=res["se_a"],se_b=res["se_b"],
        Midpoint_p_value=res["mp"],Range_p_value=res["r"],
        check.names=FALSE
      )
    }
  }
}

results = do.call(rbind,results)

# Displaying results
for(k in c(2,4)){
  cat("\nUniform parameter estimates and standard errors (k =",k,")\n")
  print(results[results$k==k,c("Stock","Duration","n","a","b","se_a","se_b")],row.names=FALSE)
  cat("\nUniform KS goodness-of-fit p-values (k =",k,")\n")
  print(results[results$k==k,c("Stock","Duration","Midpoint_p_value","Range_p_value")],row.names=FALSE)
}

# Saving results
setwd("C://Users//DYUTI MANIK//Desktop//Arnab_Sir//ASMBI//Github")
write.csv(results,"uniform_results.csv",row.names=FALSE)
