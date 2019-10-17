# Lake Model - Mean Field
# Based on Serizawa et al (2008)

library(ggplot2)
require(colorednoise)
rm(list=ls())

min.per.month = 43829.0639
min.per.day = 1440

##### Simulation Parameters #####
  dt = 0.01
  t.i = dt
  t.f = min.per.month
  t = seq(t.i,t.f,by=dt)
  timelength = length(t)
  

  ##### Model Parameters #####
  mu = 0.5 # max growth rate of phytoplankton
  k = 0.01  # nutrient concentration in phytoplankton
  
  m.N = 0.015 # removal rate of nutrients
  h.N = 0.005 # half saturation of nutrients
  h.P = 4.0 # half saturation of phytoplankton
  P.i = 1.3  # initial concentration of phytoplankton
  N.i = 0.0025 # initial concentration of nutirents
  
  
  #Critical Parameters
  f.p = 0.9 # max predation rate on phytoplankton
  in1 = 0.8
  in2 = 2.4
  F.P = f.p*mu*h.P
  I.N1 =  in1*mu*h.N
  I.N2 = in2*mu*h.N

  stdev = (I.N1+I.N2)/15
  
  sequence = c(1,2,1)/4
  sequence = round(sequence*timelength)
  
  daily.PHI = 0.5
  dt.per.day = min.per.day/dt
  PHI = daily.PHI^(1/dt.per.day) 
  
  ##### Variables #####
  #noise with period of 1440 minutes
  #I.N.data1 = rnorm(sequence[1], mean = I.N1,   sd = stdev)
  #I.N.data2 = seq(I.N1, I.N2, length.out = sequence[2]) 
  #I.N.data3 = rnorm(sequence[3], mean = I.N2,   sd = stdev)
  #I.N.data = c(I.N.data1,I.N.data2,I.N.data3)
  
  #redshifted noise with autocorrelation of PHI
  I.N.data1 = rep(I.N1, sequence[1])
  I.N.data2 = seq(I.N1, I.N2, length.out = sequence[2])
  I.N.data3 = rep(I.N2, sequence[3])
  I.N.data = c(I.N.data1,I.N.data2,I.N.data3)+ colored_noise(timesteps = timelength, mean = 0, sd = stdev, phi = PHI)
  
  #I.N.data1 = abs(rnorm(timelength/2, mean = I.N1,   sd = stdev))
  #I.N.data2 = abs(rnorm(timelength/2, mean = I.N2,   sd = stdev))
  #I.N.data = c(I.N.data1, I.N.data2)
  
  N.data = na.omit(c(N.i, rep(0, timelength-1))) # Nutrient concentration over time
  P.data = na.omit(c(P.i, rep(0, timelength-1))) # Phytoplankton concentration over time
  
##### Functions #####
  
  dN <- function(n,p,i){
    dt*(i - k*mu*n*p/(h.N+n) - n*m.N)
  }
  
  dP <- function(n,p){
    dt*(mu*p*n/(h.N+n) - F.P*p/(h.P+p))
  }
  
  
##### Simulation #####
  N.data[1] = N.i
  P.data[1] = P.i
  for(i in 2:timelength){
    N = N.data[i-1]
    P = P.data[i-1]
    P.data[i] <- P + dP(N,P)
    dn <- dN(N,P,I.N.data[i-1])
    N.data[i] <- (N + dn)*(N+dn >= 0)
  }

##### Plot Parameters #####  
  plot.res.skip = as.integer(15/dt)
  plot.ti = t.i
  plot.tf = t.f
  plot.ti.index = as.integer(plot.ti/dt)
  plot.tf.index = as.integer(plot.tf/dt)
  
 
##### Plotting #####
  
  P.RED <- P.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
  N.RED <- N.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
  i.RED <- I.N.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]/(mu*h.N)
  t.RED <- t[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
  
  
  P.ts <- data.frame(Time = t.RED, Conc = P.RED, Var = rep("Phytoplankton",length(t.RED)))
  N.ts <- data.frame(Time = t.RED, Conc = N.RED, Var = rep("Nutrients",length(t.RED)))
  i.ts <- data.frame(Time = t.RED, Conc = i.RED, Var = rep("Inflow",length(t.RED)))
  
  DATA.ts <- rbind(P.ts, N.ts, i.ts)
  
  p <- ggplot(data = DATA.ts, aes(x=Time,y=Conc)) + geom_line()+facet_grid(Var ~ ., scales = "free")
  p

  
  require(wsyn)
  
  P.4wav = P.RED - mean(P.RED)
  N.4wav = N.RED - mean(N.RED)
  i.4wav = i.RED - mean(i.RED)
  times = t.RED
  
  res <- wsyn::wt(N.4wav,times)
  res$mag = sqrt(Re(res$values)^2+Im(res$values)^2)
  
  
  
  
  #plot(times,res$mag[,1],"l",lty=1)
  
  #plot(res$timescales,res$mag[2500,])
  
  plotmag(res)
  

  