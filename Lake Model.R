# Lake Model - Mean Field
# Based on Serizawa et al (2008)

library(ggplot2)

##### Simulation Parameters #####
  dt = 0.01
  t.i = 0
  t.f = 10000
  t = seq(t.i,t.f,by=dt)
  timelength = length(t)
  

##### Model Parameters #####
  mu = 0.5 # max growth rate of phytoplankton
  k = 0.01  # nutrient concentration in phytoplankton
  f.P = 1.8 # max predation rate on phytoplankton
  m.N = 0.015 # removal rate of nutrients
  h.N = 0.005 # half saturation of nutrients
  h.P = 4.0 # half saturation of phytoplankton
  I.N = 0.0225 # base input rate of nutrient
  P.i = 1 # initial concentration of phytoplankton
  N.i = 0.0025 # initial concentration of nutirents


##### Variables #####
  I.N.data = seq(I.N, I.N, length.out = timelength/2) #+ rnorm(timelength, mean = I.N, sd = I.N/10) # input rate of nutrients
  I.N.data = seq(I.N/2, I.N/2, length.out = timelength/2) 
  I.N.data = abs(I.N.data)
  N.data = c(N.i, rep(0, timelength-1)) # Nutrient concentration over time
  P.data = c(P.i, rep(0, timelength-1)) # Phytoplankton concentration over time
  N = N.i # current nutrient concentration
  P = P.i # current phytoplankton concentration
  
##### Functions #####
  
  dN <- function(n,p,i){
    dt*(i - k*mu*n*p/(h.N+n) - N*m.N)
  }
  
  dP <- function(n,p){
    dt*(mu*p*N/(h.N+n) - f.P*p/(h.P+p))
  }
  
  
##### Simulation #####
  for(i in 2:timelength){
    N <- N.data[i-1] + dN(N,P,I.N.data[i])
    P <- P.data[i-1] + dP(N,P)
    N.data[i] <- N
    P.data[i] <- P
  }


##### Plot Parameters #####  
  plot.resolution = 5000
  plot.ti = 0
  plot.tf = 10000
  plot.ti.index = as.integer(plot.ti/dt)
  plot.tf.index = as.integer(plot.tf/dt)
  
 
##### Plotting #####
  
  P.RED <- P.data[seq(plot.ti.index,plot.tf.index,by=as.integer(timelength/plot.resolution))]
  N.RED <- N.data[seq(plot.ti.index,plot.tf.index,by=as.integer(timelength/plot.resolution))]
  I.RED <- I.N.data[seq(plot.ti.index,plot.tf.index,by=as.integer(timelength/plot.resolution))]
  t.RED <- t[seq(plot.ti.index,plot.tf.index,by=as.integer(timelength/plot.resolution))]
  
  
  P.ts <- data.frame(Time = t.RED, Conc = N.RED, Var = rep("Phytoplankton",length(t.RED)))
  N.ts <- data.frame(Time = t.RED, Conc = P.RED, Var = rep("Nutrients",length(t.RED)))
  I.ts <- data.frame(Time = t.RED, Conc = I.RED, Var = rep("Inflow",length(t.RED)))
  
  DATA.ts <- rbind(P.ts, N.ts, I.ts)
  
  p <- ggplot(data = DATA.ts, aes(x=Time,y=Conc)) + geom_line()+facet_grid(Var ~ ., scales = "free")
  p

