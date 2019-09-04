#' Computes the wavelet transform of a timeseries. Also the creator function for the
#' \code{wt} class.
#' 
#' Computes the wavelet transform of a timeseries. Also the creator function for the
#' \code{wt} class. The \code{wt} class inherits from the \code{tts} class, which
#' inherits from the \code{list} class.
#' 
#' @param t.series A timeseries of real values
#' @param times A vector of time step values (e.g., years), spacing 1
#' @param scale.min The smallest scale of fluctuation that will be examined. At least 2.
#' @param scale.max.input The largest scale of fluctuation that is guaranteed to be examined 
#' @param sigma The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
#' @param f0 The ratio of the period of fluctuation to the width of the envelope. Defaults to 1.
#' 
#' @return \code{wt} returns an object of class \code{wt}.  Slots are: 
#' \item{values}{A matrix of complex numbers, of dimensions \code{length(t.series)} by the number of timescales. Entries not considered reliable (longer timescales, near the edges of the time span) are set to NA.}
#' \item{times}{The time steps specified (e.g. years)}
#' \item{wtopt}{The inputted wavelet transform options scale.min, scale.max.input, sigma, f0 in a list}
#' \item{timescales}{The timescales (1/frequency) computed for the wavelet transform}
#' \item{dat}{The data vector from which the transform was computed}
#' 
#' @note Important for interpreting the phase: the phases grow through time, i.e., they turn anti-clockwise. 
#' 
#' @author Lawrence Sheppard \email{lwsheppard@@ku.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}, Daniel Reuman \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{wt_methods}}, \code{\link{tts}}, \code{\link{plotmag}}, \code{\link{plotphase}},
#' \code{browseVignettes("wsyn")}
#' 
#' @examples
#' time1<-1:100
#' time2<-101:200
#' ts1p1<-sin(2*pi*time1/15)
#' ts1p2<-0*time1
#' ts2p1<-0*time2
#' ts2p2<-sin(2*pi*time2/8)
#' ts1<-ts1p1+ts1p2
#' ts2<-ts2p1+ts2p2
#' ts<-c(ts1,ts2)
#' ra<-rnorm(200,mean=0,sd=0.5)
#' t.series<-ts+ra
#' t.series<-t.series-mean(t.series)
#' times<-c(time1,time2)
#' res<-wt(t.series, times)
#' 
#' @export
#' @importFrom stats fft

wt <- function(t.series, times, scale.min=2, scale.max.input=NULL, sigma=1.05, f0=1)
{
  #error checking
  errcheck_tsdat(times,t.series,"wt")
  errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"wt")
    
  if(is.null(scale.max.input)){
    scale.max<-length(t.series)
  }
  else{
    scale.max<-scale.max.input
  }
  
  if (is.matrix(t.series))
  {
    t.series<-as.vector(t.series)
  }
  
  #for return
  wtopt<-list(scale.min=scale.min,scale.max.input=scale.max.input,
              sigma=sigma,f0=f0)
  
  #determine how many frequencies are in the range and make receptacle for results 
  scale.min <- f0*scale.min
  scale.max <- f0*scale.max
  m.max <- floor(log(scale.max/scale.min)/log(sigma))+1 #number of timescales
  s2 <- scale.min*sigma^seq(from=0, by=1, to=m.max) #widths of wavelet envelopes
  margin2 <- ceiling(sqrt(-(2*s2*s2)*log(0.5)))
  translength <- length(t.series)
  m.last <- max(which(margin2<0.5*translength))
  result <- matrix(NA, nrow=translength, ncol=m.max+1)   
  
  #wavsize determines the size of the calculated wavelet
  wavsize <- ceiling(sqrt(-(2*s2[m.last]*s2[m.last])*log(0.001)));
  
  #preparations for finding components  
  Y <- stats::fft(c(t.series,rep(0,2*wavsize)))
  lenY<-length(Y)
  freqs<-seq(from=0, by=1, to=lenY-1)/lenY;
  freqs2<-c(seq(from=0, by=1, to=floor(lenY/2)), seq(from=-(ceiling(lenY/2)-1), 
                                                     by=1, to=-1))/lenY;
  
  #find transform components using wavelets of each frequency
  for (stage in 1 : m.last)
  {
    s.scale<-s2[stage];
    
    #begin calculating wavelet
    
    #margin determines how close large wavelets can come to the edges of the timeseries
    margin<-margin2[stage];
    
    #perform convolution
    XX <- (2*pi*s.scale)^(0.5)*(exp(-s.scale^2*(2*pi*(freqs-((f0/s.scale))))^2/2) - 
                                  (exp(-s.scale^2*(2*pi*(freqs2))^2/2))*
                                  (exp(-0.5*(2*pi*f0)^2)))*exp(-1i*2*pi*wavsize*freqs);
    con <- stats::fft((XX*Y),inverse=TRUE)
    con <- con/length(con)
    
    #fit result into transform                                                                                                                      
    result[(margin+1):(translength-margin),stage] <- 
      con[(wavsize + margin + 1):(translength + wavsize - margin)]; 
  }
  if(is.null(scale.max.input)){
    result<-result[,1:m.last]
    timescales<-s2[1:m.last]/f0
    errcheck_tts(times,timescales,result,"wt")
    result<-list(values=result, times=times, wtopt=wtopt, timescales=timescales, dat=t.series)
    class(result)<-c("wt","tts","list")
    return(result)
  }
  else{
    timescales<-s2/f0
    errcheck_tts(times,timescales,result,"wt")
    result<-list(values=result, times = times, wtopt=wtopt, timescales=timescales, dat=t.series)
    class(result)<-c("wt","tts","list")
    return(result)
  }
}


