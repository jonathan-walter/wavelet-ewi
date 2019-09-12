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
  I.N = 0.0225/2 # base input rate of nutrient
  P.i = 3 # initial concentration of phytoplankton
  N.i = 0 # initial concentration of nutirents

  stdev = I.N/100

##### Variables #####
  #I.N.data1 = seq(I.N, I.N, length.out = timelength/2) #+ rnorm(timelength, mean = I.N, sd = I.N/10) # input rate of nutrients
  #I.N.data2 = seq(I.N/2, I.N/2, length.out = timelength/2) 
  #I.N.data = abs(c(I.N.data1, I.N.data2))
  I.N.data1 = seq(I.N, I.N, length.out = timelength/2-1) + rnorm(timelength/2, mean = I.N,   sd = stdev)
  I.N.data2 = seq(I.N, I.N/2, length.out = timelength/4-1) + rnorm(timelength/4, mean = I.N,   sd = stdev)
  I.N.data3 = seq(I.N/2, I.N/2, length.out = timelength/4-1) + rnorm(timelength/4, mean = I.N,   sd = stdev)
  I.N.data = c(I.N.data1,I.N.data2,I.N.data3)
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
  plot.res.skip = as.integer(1/dt)
  plot.ti = 2500
  plot.tf = t.f
  plot.ti.index = as.integer(plot.ti/dt)
  plot.tf.index = as.integer(plot.tf/dt)
  
 
##### Plotting #####
  
  P.RED <- P.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
  N.RED <- N.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
  I.RED <- I.N.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
  t.RED <- t[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
  
  
  P.ts <- data.frame(Time = t.RED, Conc = N.RED, Var = rep("Phytoplankton",length(t.RED)))
  N.ts <- data.frame(Time = t.RED, Conc = P.RED, Var = rep("Nutrients",length(t.RED)))
  I.ts <- data.frame(Time = t.RED, Conc = I.RED, Var = rep("Inflow",length(t.RED)))
  
  DATA.ts <- rbind(P.ts, N.ts, I.ts)
  
  p <- ggplot(data = DATA.ts, aes(x=Time,y=Conc)) + geom_line()+facet_grid(Var ~ ., scales = "free")
  p

