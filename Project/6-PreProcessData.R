compile_data <- function(p, channel, p_train, PATH){

  #channel 1 is nutrients, channel 2 is phytoplankton
  
  setwd(PATH)  
  require(abind)
  
  key = readRDS("key")
  
  n_max = 2833
  n_channels = length(channel)
  
 
  n_samples = length(key)
  n_train = round(round(p*n_samples)*p_train)
  n_test = round(p*n_samples)-n_train
  
  i_sample = sample(1:n_max, size=n_samples, replace=F)
  i_train = i_sample[1:n_train]
  i_test = i_sample[(n_train+1):n_samples]
  
  
  
  train_x = array(dim=c(n_train,64,128,n_channels))
  train_y = array(dim=c(n_train))
  test_x = array(dim=c(n_samples-n_train,64,128,n_channels))
  test_y = array(dim=c(n_samples-n_train))
  
  
  index = 1

  for (i in i_train){
    predata = readRDS(paste("wt_avg_",i,sep=""))
    df1 = t(predata[,,1])
    df2 = t(predata[,,2])
    predata = abind(df1,df2,along=3)
    
    train_x[index,,,1:n_channels] = predata[,,channel]
    train_y[index] = key[i]
    
    index = index+1
  }
  
  index=1
  for (i in i_test){
    predata = readRDS(paste("wt_avg_",i,sep=""))
    df1 = t(predata[,,1])
    df2 = t(predata[,,2])
    predata = abind(df1,df2,along=3)
    
    test_x[index,,,1:n_channels] = predata[,,channel]
    test_y[index] = key[i]
    
    index = index+1
  }
  
  data = NULL
  data[[1]] = train_x
  data[[2]] = train_y
  data[[3]] = test_x
  data[[4]] = test_y
  data[[5]] = i_train
  
  data
}

