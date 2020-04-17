require(devtools)
install_github("cran/Emcdf")

require(tensorflow)
##############################

# classify(model,images)
# 
# classify_analyze(model, ts_data){
#       registerDoParallel(c1)
#       foreach(i=1:samples) %dopar%{
#         wtout = wt(ts_data)
#         prediction = aperm(predict(model,wtout), c(3,2))
#         output(i,)
#         
# 
# }

###################################################################
      
add_dimension = function(matrix){
  require(abind)
  dims = dim(matrix)
  
  if(is.null(dims)){
    out = abind(matrix,NULL,along=2)
  }
  
  if(!is.null(dims)){
    out = abind(matrix,NULL,along=(length(dims)+1))
  }
  
  out
}


######################################################################
# win_classify = function(model,image)      
#   
#       
# win_classify_analyze = function(model, timeseries, time, windowed, w_width, w_step){
#        
#       if(is.null(w_width)|is.null(w_step)|is.null(step_index)) {stop("If doing windowed analysis, must define w_width, w_step, step_index")
#       output = NULL
#       for (i = 1:samples) {
#         wtout = wt_windowed(ts_data,w.width,w.step,step_index)
#         wtout = wtout[[1]]
#         prediction = aperm(predict(model,wtout), c(3,2,1))
#         abind(output,prediction,along = 1)
#       }
# }

######################################################################3
      
raw_classify = function(model, images, time = NULL){
  require(EBImage)
  require(doParallel)
  require(keras)
  
  cores = detectCores()
  c1 = makeCluster(cores[1]-1)
  
  
  dimages = dim(images)
  registerDoParallel(c1)
  if(dimages[2] != 128 | dimages[3] != 64) {
    warning("images are wrong size... resizing to n by 128 by 64")
    temp = array(c(dimages[1],128,64))
    registerDoParallel(c1)
    foreach(i=1:dimages[1]) %dopar% {
        temp[i,,] = resize(images[i,,],128,64)
      }
    stopCluster(c1)
    images = temp
  }
  
  
  if (is.null(time)){time = 1:dimages[1]}
  if (dimages[1] != length(time)) {stop("time vector is not the same size as number of images")}
  
  output = predict(model,images)
  
  Time = time
  Absent = output[,1]
  Present = output[,2]
  
  result = data.frame(Time, Absent, Present)
  result
}
  
  
prob_timeseries = function(timeseries){
  dims = dim(timeseries)
  if(dims[2] != 2){stop("timeseries must be a two column matrix")}
  if(!is.matrix(timeseries)){
    warning("timeseries should be matrix... converting...")
    timeseries = as.matrix(timeseries)
    }
  
  tendex = round(dims[1]*0.1)
  
  if(tendex < 2){ stop("timeseries is too short to process")}
  
  output = array(c(dims[1]))
  output[1:(tendex-1)] = 1
  
  for (i in tendex:dims[1]){
    event = timeseries[i,]
    all_events = timeseries[1:(i-1),]
    prob = prob_event2d(event,all_events)
    output[i] = prob
  }
  
  output
} 

  
prob_event2d = function(event, all_events){
 # gives the right-tailed probability of the output given other values 
  require(Emcdf)
  
  
  ##do that events probability within the previous set
  
  #NEED TO UPDATE: find the index of the closest valued pair in all_events and 
  # find the index of that event, plug in that index into the cdf
  
  # test dimensions
  if (length(event) != 2) {stop("Event must be a vector of two values")}
  if (dim(all_events)[2] != 2) {stop("all_events must have two variables sepearated into two columns")}
  if(length(dim(all_events)) != 2) {stop("all_events matrix must be two dimensional")}
  
  x1min = min(all_events[,1])
  x1max = max(all_events[,1])
  x2min = min(all_events[,2])
  x2max = max(all_events[,2])
  
  outputs = as.matrix(all_events)
  
  cdf = Biemcdf(all_events)
  
  cdf1range = dim(cdf)[1]
  cdf2range = dim(cdf)[2]
  
  
  event1index = floor(cdf1range*(event[1]-x1min)/(x1max - x1min))
  event2index = floor(cdf2range*(event[2]-x2min)/(x2max - x2min))
  
  event1index = ifelse(event1index < 1, 1, event1index)
  event1index = ifelse(event1index > cdf1range, cdf1range, event1index)
  event2index = ifelse(event2index < 1, 1, event2index)
  event2index = ifelse(event2index > cdf2range, cdf2range, event2index)
  
  probability = 1-cdf[event1index,event2index]
  
  c(probability, event1index, event2index)
  
  ## I need to look at the code of this function to determine how it creates its bounds
  
}






# analyze_and_predict = function(model, ts_data, windowed = FALSE, w_width = NULL, w_step = NULL, step_index = NULL){
#   require(keras)
#   require(abind)
#   
#    cores = detectCores()
#   c1 = makeCluster(cores[1]-1)
#   
#   if(TRUE) warning("REMINDER: ts_data should be a matrix of sample (row) by time (column)")
#   
#   samples = dim(ts_data)[1]
#   images = array()
#   
#   if(windowed){
#     if (samples > 100) warning("Windowed analysis of 100+ ts_data could take a while..."){}
#     if(is.null(w_width)|is.null(w_step)|is.null(step_index)) stop("If doing windowed analysis, must define w_width, w_step, step_index"){}
#     output = NULL
#     for (i = 1:samples) {
#       wtout = wt_windowed(ts_data,w.width,w.step,step_index)
#       wtout = wtout[[1]]
#       prediction = aperm(predict(model,wtout), c(3,2,1))
#       abind(output,prediction,along = 1)
#     }
#   }
#   
#   if(!windowed){
#     output = array(c(samples,2,1)) 
#     
#     registerDoParallel(c1)
#     foreach(i=1:samples) %dopar%{
#       wtout = wt(ts_data)
#       prediction = aperm(predict(model,wtout), c(3,2))
#       output(i,)
#       
#       
#     }
#     
#     stopCluster(c1)
#     
#   }

ts_p= function(index){
  require(keras)
  
  key = readRDS(paste("windowed_key_",as.character(index),sep=""))
  images = readRDS(paste("windowed_wt_",as.character(index),sep=""))
  mod = load_model_tf("model-windowed-2020-02-29")
  
  images = images[1:131,,]
  key = key[1:131,]
  
  images = add_dimension(images)

  outputs = predict(mod, images)

  absent = outputs[,1]
  present = outputs[,2]
  
  key_ab = key[,1]
  key_pr = key[,2]
  
  prob = prob_timeseries(outputs)
  
  ts = data.frame(prob, key1, key2, absent, present)
  ts
}



