wt_windowed = function(timeseries, window_width, window_stride, transition_index = NULL){
  #requires all supplemental inputs to be in terms of indices and not time!!!
  #shift index of zero means there is no shift
  
  n = length(timeseries)
  s = window_stride
  w = window_width
  
  n_windows = floor((n-w-1)/s)
  
  index = 1
  endex = index+w   
  
  images = array(dim = c(n_windows, 128, 64))
  keys = array(0, dim = c(n_windows,2))
  
  for (i in 1:n_windows){
    
    fragment = timeseries[index:endex]
    wt_fragment = run_wsyn(fragment)
    
    images[i,,] = wt_fragment
    
    if(!is.null(transition_index)){
      keys[i,1+as.numeric(index >= transition_index)] = 1
    }
    
    ## outputs a 2d key, first col = 1 if the transition has NOT occurred, right col = 1 if the transition has occurred
    
    index = index+s
    endex = endex+s
  }
  
  output = NULL
  
  if(is.null(transition_index)){
    output = images
  }
  
  if(!is.null(transition_index)){
    output[[1]] = images
    output[[2]] = keys
  }

  output
}
