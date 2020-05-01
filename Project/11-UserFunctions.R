
###################################################################
#' wtCNN
###################################################################

#' Predict 1-Channel Image Classification
#' 
#' Uses a pre-trained model to classify an image or set of images
#' 
#' @param model a pre-trained CNN using the Keras package 
#' @param imageset an N by 128 by 64 dimensional array; N is the number of different imageset
#' @param type either "category" for binary classsification or "probability" for category likelihoods
#' 
#' @return N length vector containing a binary prediction of classification
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export
#' @import devtools
#' @import keras
#' @import tensorflow
#' @import abind
#' @import EBImage

predict = function(model,imageset, type = "category"){
  require(devtools)
  require(keras)
  require(tensorflow)
  require(abind)
  
  
  dims = dim(imageset)
  if (dims[2]!=128 | dims[3]!=64){
    warning("imageset must be 128 by 64... resizing...")
    resize_images(image)
  }
  
  if(length(dims)>3){
    warning("expected input is f")
    if (dims[4] != 1){
      stop("imageset must have only one channel")
    }
  }
  
  if(length(dims)==3){
    imageset = add_dimension(imageset)
  }
  
  singlebool = dims[1] == 1
  if (singlebool) {
    imageset = abind(imageset,imageset,along=1)
  }
  
  if(type == "category"){cla = keras::predict_classes(model,imageset)}
  if(type == "probability"){cla = keras::predict_proba(model,imageset)}
  if((type != "category")&(type != "probability")){stop("invalid type input; must be either category or probability")} 
  
  if(singlebool){
    cla = cla[1,]
  }
  cla
}

#' Turn 1-variable key into 2-variable key
#' 
#' @param key_vector key vector
#' 
#' @return a two column array with one FALSE and one TRUE per row as determined by the key vector
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
expand_key = function(key_vector){
  if(!is.null(dim(key_vector))){
    stop("key vector must be a vector")
  }
  out = as.logical(key_vector)
  if(sum( is.na(out) ) > 0){
    stop("key vector must contain only logical values")
  }
  key = cbind(key_vector, !key_vector)
  key
}

#' Turn 2-variable key into 1-variable key
#' 
#' @param key_array binary logical key array with two columns and exactly one FALSE and one TRUE per row
#' 
#' @return a key vector
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
collapse_key = function(key_array){
  key = key_array[,1]
  key
}

#' Prediction Errors
#' 
#' Calculate Accuracy, Type I and Type II Errors
#' 
#' @param model a pre-trained CNN using the Keras package 
#' @param images an N by 128 by 64 dimensional array; N is the number of different imageset
#' @param keys vector of image labels
#' 
#' @note Null hypothesis is assumed to be key = 0, alternative hypothesis is key = 1
#' 
#' @return A data frame with accuracy, Type I Error, and Type II Error
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export
predict_stats = function(model,images,keys){
  
  predictions = predict(model, images, type = "category")

  outcome = (keys == predictions)
  
  Accuracy = mean(outcome)
  Type1Error = mean(outcome[keys==0])
  Type2Error = mean(outcome[keys==1])

  out1 = data.frame(Accuracy, Type1Error, Type2Error)
  out1
}


#' Install Keras, Tensorflow and Supporting Packages
#' 
#' Installs packages necessary to make and run neural networks 
#' 
#' @return None
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export

install_packages = function(){
  packages = c("devtools","keras","tensorflow","abind","foreach","doParallel","wsyn")
  lapply(packages,package_check)
    
  if(!require("EBImage")){
    BiocManager::install("EBImage")
  } 
  require("EBImage")
  
  if(!require("keras")){
    devtools::install_github("rstudio/keras")
  }
  require("keras")
  
  if(!require("Emcdf")){
    devtools::install_github("cran/Emcdf")
  }
  require("Emcdf")
}


#' Check if a package is present
#' 
#' Install package if not present
#' 
#' @return None
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
package_check <- function(x){
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }

#' Resize Images to Model Standard
#' 
#' Takes a set of images concatenated along the 1st dimension and resizes them to 128 by 64
#' 
#' @param imageset an N by x by y dimensional array; N is the number of different images
#' 
#' @return N by 128 by 64 dimensional array
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export
#' @import EBImage

resize_images = function(imageset){
  for(i in 1:dim(imageset)[1]){
    imageset[i,,] = resize(imageset[i,,],128,64)
  } 
  imageset
}


#' Add a Dummy Dimension to an array
#' 
#' Adds a nominal dimension to the array to conform with Keras standards
#' 
#' @param array any array
#' 
#' @return a array with an added dummy dimension
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#'
#' @import abind

add_dimension = function(array){
  dims = dim(array)
  
  if(is.null(dims)){
    out = abind(array,NULL,along=2)
  }
  
  if(!is.null(dims)){
    out = abind(array,NULL,along=(length(dims)+1))
  }
  
  out
}

#' Build Keras Convolutional Neural Network
#' 
#' Constructs a Keras NN with 4 convolutional layers
#' 
#' @param LEARNINGRATE default set to 0.001
#' 
#' @note it might be necessary to run install_packages first
#' 
#' @return an untrained Keras CNN with randomized params
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export
#' @import devtools
#' @import keras
#' @import tensorflow
build_model <- function(LEARNINGRATE = 0.001){
  
  model = keras_model_sequential()
  
  PADDING = "same"
  ACTIVATION = "relu"
  DECAY = 0.000001
  
  model %>%
    
    #CONV LAYER 1
    layer_conv_2d(filter=32,kernel_size=c(6,3),padding=PADDING,input_shape = c(128,64,1)) %>%
    layer_activation(ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(4,2),padding=PADDING,data_format=("channels_last")) %>%
    
    #CONV LAYER
    layer_conv_2d(filter=16,kernel_size=c(3,3),padding=PADDING,data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
    
    #CONV LAYER 3
    layer_conv_2d(filter=8,kernel_size=c(3,3),padding=PADDING,data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
    
    #CONV LAYER 4
    layer_conv_2d(filter=4,kernel_size=c(3,3),padding=PADDING,data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
    
    
    #Fully Connected Layers
    layer_flatten(data_format="channels_last") %>%
    layer_dense(units=64) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_dense(units=16) %>% 
    layer_activation(activation = ACTIVATION) %>%
    layer_dense(units=2) %>%
    layer_activation(activation = ACTIVATION)
  
  
  opt = optimizer_adam(lr=LEARNINGRATE, decay=DECAY)
  
  model %>%
    compile(loss="binary_crossentropy",
            optimizer=opt, metrics = "accuracy")
  
  summary(model)
  
  model
}

#' Predict Probability of Time Series
#' 
#' Calculates probability of each point in the time series given all previous points
#' 
#' @note for forecasting, you should be using predict() outputs, not classify() outputs
#' 
#' @param timeseries2var a two column matrix of two-variable events
#' 
#' @return vector of probabilities for each of the time series values
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export

prob_timeseries = function(timeseries2var){
  dims = dim(timeseries2var)
  if(dims[2] != 2){stop("timeseries2var must be a two column matrix")}
  if(!is.matrix(timeseries2var)){
    warning("timeseries2var should be matrix... converting...")
    timeseries2var = as.matrix(timeseries)
  }
  
  tendex = round(dims[1]*0.1)
  
  if(tendex < 2){ stop("timeseries2var is too short to process")}
  
  output = array(c(dims[1]))
  output[1:(tendex-1)] = 1
  
  for (i in tendex:dims[1]){
    event = timeseries2var[i,]
    other_events = timeseries2var[1:(i-1),]
    prob = prob_event2d(event,other_events)
    output[i] = prob
  }
  
  output
} 

#' Predict Probability of a 2-Variable Event
#' 
#' Predicts the probability ofa 2D outcome given all other outcomes
#' 
#' @param event a vector of length two
#' @param other_events a two column matrix of paired values
#' 
#' @return probability of that event occuring with the other events
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export
#' @import Emcdf

prob_event2d = function(event, other_events){
  # gives the right-tailed probability of the output given other values 
  require(Emcdf)
  
  # test dimensions
  if (length(event) != 2) {stop("Event must be a vector of two values")}
  if (dim(other_events)[2] != 2) {stop("other_events must have two variables sepearated into two columns")}
  if(length(dim(other_events)) != 2) {stop("other_events matrix must be two dimensional")}
  if(!is.matrix(other_events)){
    warning("timeseries2var should be matrix... converting...")
    other_events = as.matrix(other_events)
  }
  
  x = event[1]
  y = event[2]
  
  n = 1:length(other_events[,1])  
  
  temp_x_vect = sort(c(other_events[,1],x))
  temp_y_vect = sort(c(other_events[,2],y))
  
  event1index = min(n[temp_x_vect == x])
  event2index = min(n[temp_y_vect == y])
  
  if(max(temp_x_vect) == x){
    event1index = length(other_events[,1])
  }
  if(max(temp_y_vect) == y){
    event2index = length(other_events[,2])
  }
  
  outputs = as.matrix(other_events)
  
  cdf = Biemcdf(other_events)
  
  probability = 1-cdf[event1index,event2index]
  
  c(probability)
}

#' Single Wavelet Transform a Timeseries
#' 
#' Gives the 2D wavelet transform of a one variable timeseries
#' 
#' @note inputting a key changes the output from matrix to list
#' 
#' @param timeseries a one-variable timeseries in vector form
#' @param key optional input is wrapped in a list with output wavelet transform
#' 
#' @return 128 by 64 matrix of wavelet power OR a list containing 1: wavelet matrix and 2: the input key
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' @import EBImage
#' @export
#' 
wt_simple <- function(timeseries, key = NULL){
  time = 1:length(timeseries)
  wt = wt_ns(timeseries-mean(timeseries),time)
  wt.power = Mod(wt$values^2)
  IM = resize(wt.power,128,64)
  if(!is.null(key)){
    IM[[1]] = IM
    IM[[2]] = collapse_key(expand_key(key))
  }
  IM
}

#' Wavelet Transform a Timeseries
#' 
#' Gives the 2D wavelet transform of a one variable timeseries
#' 
#' @param timeseries a one-variable timeseries in vector form
#' @param window_width width of analysis window measured in number of timeseries indices
#' @param window_stride stride of analysis window measured in number of timeseries indices
#' @param keyseries a one-variable timeseries labeling each point in the main timeseries
#' @return 128 by 64 matrix of wavelet power OR a list with 1: wavelet matrix and 2: key vector
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export
wt_windowed = function(timeseries, window_width, window_stride, keyseries = NULL){
  
  n = length(timeseries)
  s = window_stride
  w = window_width
  
  n_windows = floor((n-w-1)/s)
  
  index = 1
  endex = index+w   
  
  imageset = array(dim = c(n_windows, 128, 64))
  keys = array(0, dim = c(n_windows))
  
  for (i in 1:n_windows){
    
    fragment = timeseries[index:endex]
    wt_fragment = run_wsyn(fragment)
    
    imageset[i,,] = wt_fragment
    if(!is.null(keyseries)){
      key_fragment = keyseries[index:endex]
      mode = round(mean(key_fragment))
      keys[i] = mode
    }
    
    index = index+s
    endex = endex+s
  }
  output = NULL
  if(!is.null(key)){
    output[[1]] = imageset
    output[[2]] = keys
  }
  if(is.null(key)){
    output = imageset
  }
  output
}

#' Computes the wavelet transform of a timeseries without scalloping the edges. Also the creator function for the
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
#' @importFrom stats fft
wt_ns = function(t.series, times, scale.min=2, scale.max.input=NULL, sigma=1.04, f0=1){
  #error checking
  wsyn:::errcheck_tsdat(times,t.series,"wt")
  wsyn:::errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"wt")
  
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
    #margin<-margin2[stage];
    margin<-0
    
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
    wsyn:::errcheck_tts(times,timescales,result,"wt")
    result<-list(values=result, times=times, wtopt=wtopt, timescales=timescales, dat=t.series)
    class(result)<-c("wt","tts","list")
    return(result)
  }
  else{
    timescales<-s2/f0
    wsyn:::errcheck_tts(times,timescales,result,"wt")
    result<-list(values=result, times = times, wtopt=wtopt, timescales=timescales, dat=t.series)
    class(result)<-c("wt","tts","list")
    return(result)
  }
}

#' Produce trained model from images and their keys
#' 
#' Train a convolutional neural network from a labeled data set
#' 
#' @param imageset an N x W x H set of images with features for classification training
#' @param keys a vector binary keys/labels corresponding to features in imageset
#' @param model input a model for a training
#' @note not tested with non-logical keys
#' 
#' @return a trained cnn 
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#'
#' @export
#' 
train_model = function(model, wt_data, test_iterations = 500, train_test_ratio = 0.7, batch_size = 128){
  
  imageset = check_imageset(wt_data[[1]])
  keyset = expand_key(wt_data[[2]])
  
  PREdata = NULL
  PREdata[[1]] = imageset
  PREdata[[2]] = !keyset
  
  data = prep_data(PREdata,train_test_ratio)

  train_x = data[[1]]
  train_y = data[[2]]
  test_x = data[[3]]
  test_y = data[[4]]
  
  
  model %>% fit(train_x, train_y, 
                batch_size = batch_size, epochs = test_iterations,
                validation_data = list(test_x,test_y),
                shuffle = TRUE)
  
  predictions = predict(model,test_x)
  accuracy = predict_stats(predictions,test_y)
  print(accuracy)
  model
}

#' Dimension check an image set
#' 
#' Check for the correct dimensions
#' 
#' @param imageset a set of images bound along axis 1
#' 
#' @return a set of images bound along axis 1 with appropriate dimensions (128 by 64)
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @import EBImage
#' 
#' @examples
#' df = array(c(100, 500, 300))
#' new_df = check_imageset(df)
#' 
#' @export 
#' 
check_imageset = function(imageset){
  dims = dim(imageset)
  if (dims[2]!=128 | dims[3]!=64){
    warning("imageset must be 128 by 64... resizing...")
    imageset = resize_images(image)
  }
  
  if(length(dims) > 3){
    if (dims[4] != 1){
      stop("imageset must have only one channel")
    }
  }
  if(length(dims) == 3){
    message("adding dummy dimension for processing")
    imageset = add_dimension(imageset)
  }
  if(length(dims) < 3){
    stop("imageset should be at least three dimensional")
  }
  imageset
}
  
  

#' Wavelet Transform a Timeseries Set
#' 
#' With or without a key set bound in a list
#' 
#' @param timeseries an array of timeseries bound by row; CAN include an ordered key vector attached by list where entry 1 is the timeseries array
#' 
#' @returna set of 128 by 64 images bound by dimension 1; will be in a list with keys if input was a list with keys
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @import foreach
#' @import doParallel
#' 
#' @export 
wt <- function(ts_data){
  cores = detectCores()
  c1 = makeCluster(cores[1]-1)
  registerDoParallel(c1)
  
  if(is.list(ts_data)){
    keys = collapse_key(expand_key((ts_data[[2]])))
    timeseries = ts_data[[1]]
    N = length(keys)
    
    images = array(dim = c(N,128,64))
    
    # foreach(i = 1:N)%parfor%{
    for (i in 1:N){
      t = timeseries[i,]
      time = 1:length(t)
      wt = wt_ns(t-mean(t),time)
      wt.power = Mod(wt$values^2)
      IM = resize(wt.power,128,64)
      images[i,,] = IM
    }
    out = NULL
    out[[1]] = images
    out[[2]] = keys
  }
  
  if(!is.list(ts_data)){
    
    N = dim(ts_data)[1]
    
    images = array(dim = c(N,128,64))
    
   # foreach(i = 1:N)%parfor%{
    for(i in 1:N){
      t = ts_data[i,]
      time = 1:length(t)
      wt = wt_ns(t-mean(t),time)
      wt.power = Mod(wt$values^2)
      IM = resize(wt.power,128,64)
      images[i,,] = IM
    }
    out = images
  }
  stopCluster(c1)
  out
}
#' Save Model to File
#' 
#' Save a model saved with the Keras standard
#' 
#' @param model a keras model to be saved
#' @param filename a string matching the name of the target file
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @import keras
#' @export
save_model = function(model,filename){
  save_model_tf(model,filename)
}

#' Load Model from File
#' 
#' Loads a model saved with the Keras standard
#' 
#' @param filename a string matching the name of the target file
#' @return keras model
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @import keras
#' @export
load_model = function(filename){
 model = load_model_tf(filename)
 model
}

#' Load Pre-Trained Model from File
#' 
#' @return a pre-trained keras model
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @import keras
#' @export

load_premade_model = function(){
  t_filepath = "C:/Users/rimcl/OneDrive/School/Capstone/github/Project/Example/sample_model"
  model = load_model_tf(t_filepath)
}

#' Load Sample ImageSet and Keys
#' 
#' Loads a premade set of data based on a nutrient-phytoplankton model
#' 
#' @param n_samples max of 400; number of samples to be produced in the data set
#' @return an N by 128 by 64 imagest and N by 2 key bound into a list
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export
load_premade_data = function(n_samples){
  filepath = "C:/Users/rimcl/OneDrive/School/Capstone/github/Project/Example/sample_ts_data"
  temp = readRDS(filepath)
  m = dim(temp[[1]])[1]
  
  if(n_samples>m){stop("n_samples must not exceed 400")}
  samps = sample(1:m,n_samples,replace=FALSE)
  timeset = temp[[1]][samps,]
  keyset = temp[[2]][samps]
  out = NULL
  out[[1]] = timeset
  out[[2]] = keyset
  out
}

#' Prepare Data for Training
#' 
#' Separates data into training and testing sets
#' 
#' @param DATA a list containing image data and key data
#' @param p_train proportion of data set to allocate for training
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
prep_data = function(DATA, p_train){
  
  images = DATA[[1]]
  keys = DATA[[2]]
  
  dims = dim(images)
  
  n_samples = dims[1]
  n_train = round(n_samples*p_train)
  n_test = n_samples-n_train
  
  
  i_sample = sample(1:n_samples, size=n_samples, replace=F)
  #i_sample = 1:n_samples
  i_train = i_sample[1:n_train]
  if(n_train < n_samples){
    i_test = i_sample[(n_train+1):n_samples]
  }
  if(n_train >= n_samples) {i_test = NULL}
  
  
  train_x = array(dim=c(n_train,128,64,1))
  train_y = array(dim=c(n_train,2))
  test_x = array(dim=c(n_samples-n_train,128,64,1))
  test_y = array(dim=c(n_samples-n_train,2))
  
  index=1
  for (i in i_train){
    train_x[index,,,1] = images[i,,,]
    train_y[index,] = keys[i,]
    index = index+1
  }
  
  index=1
  for (i in i_test){
    test_x[index,,,1] = images[i,,,]
    test_y[index,] = keys[i,]
    index = index+1
  }
  
  output = NULL
  output[[1]] = train_x
  output[[2]] = train_y
  output[[3]] = test_x
  output[[4]] = test_y
  
  output
  
}
