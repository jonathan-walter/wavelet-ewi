# Lake Model - Mean Field
# Based on Serizawa et al (2008)

run_lakemodel <- function(in1, in2, f.p, t.f, dt, run){
      
      library(ggplot2)
      
      ##### Simulation Parameters #####
      t.i = 0
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
      F.P = f.p*mu*h.P
      I.N1 =  in1*mu*h.N
      I.N2 = in2*mu*h.N
      
      stdev = (I.N1+I.N2)
      
      ##### Variables #####
      
      #noise with period of 1440 minutes
      
      I.N.data1 = rnorm(timelength/3, mean = I.N1,   sd = stdev)
      I.N.data2 = seq(I.N1, I.N2, length.out = timelength/3) + rnorm(timelength/3, mean = 0,   sd = stdev)
      I.N.data3 = rnorm(timelength/3 - 1, mean = I.N2,   sd = stdev)
      I.N.data = c(I.N.data1,I.N.data2,I.N.data3)
      
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
      plot.res.skip = as.integer(1/dt)
      plot.ti = 1000
      plot.tf = t.f
      plot.ti.index = as.integer(plot.ti/dt)
      plot.tf.index = as.integer(plot.tf/dt)
      
      
      ##### Plotting #####
      
      P.RED <- P.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
      N.RED <- N.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
      i.RED <- I.N.data[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]/(mu*h.N)
      t.RED <- t[seq(plot.ti.index,plot.tf.index,by=as.integer(plot.res.skip))]
      
      
      P.ts <- data.frame(Time = t.RED, Conc = P.RED, Var = rep("Phytoplankton",length(t.RED)), Run = run)
      N.ts <- data.frame(Time = t.RED, Conc = N.RED, Var = rep("Nutrients",length(t.RED)), Run = run)
      i.ts <- data.frame(Time = t.RED, Conc = i.RED, Var = rep("Inflow",length(t.RED)), Run = run)
      
      
      DATA.ts <- rbind(P.ts, N.ts, i.ts)
}
