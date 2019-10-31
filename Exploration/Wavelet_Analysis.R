##### Wavelet Analysis #####
  
  require(wsyn)
  
  P.4wav = P.RED - mean(P.RED)
  N.4wav = N.RED - mean(N.RED)
  i.4wav = i.RED - mean(i.RED)
  times = t.RED
  
  res <- wsyn::wt(N.4wav,times)
  res$mag = sqrt(Re(res$values)^2+Im(res$values)^2)
  
  
  
  
  plot(times,res$mag[,1],"l",lty=1)
  
  plot(res$timescales,res$mag[2500,])
  
  plotmag(res)
  
  
