compile_data <- function(indices, PATH){
  
  #channel 1 is nutrients, channel 2 is phytoplankton
  
  setwd(PATH)  
  require(abind)
  
  flname = "wt_whole_"
  
  key = readRDS("key")

  n_samples = length(indices)
  
  data = array(dim=c(n_samples,128,64))
  
  index = 1
  for (i in indices){
    temp = readRDS(paste(flname,i,sep=""))
    data[index,,] = temp
    index = index+1
  }
  
  output = NULL
  output[[1]] = data
  output[[2]] = key[indices]
  
  output
}

###############33
compile_data_windowed <- function(indices, PATH){
  
  #channel 1 is nutrients, channel 2 is phytoplankton
  
  setwd(PATH)  
  require(abind)
  
  flname = "windowed_wt_"
  keyname = "windowed_key_"
  
  data = NULL
  keys = NULL
  
  for (i in indices){
    tempdata = readRDS(paste(flname,i,sep=""))
    tempkey = readRDS(paste(keyname,i,sep=""))
    
    
    nt = sum(tempkey[,2])
    n = 1:length(tempkey[,2])
    
    
    
    indices = n[tempkey[,2]==0]
    it = n[tempkey[,2]==1]
    samples = sample(indices,2*nt,replace=FALSE)
    
    data[[i]] = tempdata[c(it,samples),,]
    keys[[i]] = tempkey[c(it,samples),]
    
  }
  
  data = abind(data,along=1)
  keys = abind(keys,along = 1)
  
  
  output = NULL
  output[[1]] = data
  output[[2]] = keys
  
  output
}

#########################################################################

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
    train_x[index,,,1] = images[i,,]
    train_y[index,] = keys[i,]
    index = index+1
  }
  
  index=1
  for (i in i_test){
    test_x[index,,,1] = images[i,,]
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

expand_data = function(DATA){
  # sample by 128 by 64  
  
  IMAGES = DATA[[1]]
  key = DATA[[2]]
  
  
  require(imager)
  
  temp = aperm(IMAGES,c(2,3,1,4))
  
  
  
  shift1 = aperm(imshift(temp,0,16,boundary=2),c(3,1,2,4))
  shift2 = aperm(imshift(temp,0,32,boundary=2),c(3,1,2,4))
  shift3 = aperm(imshift(temp,0,48,boundary=2),c(3,1,2,4))
  
  key = c(key,key,key,key)
  
  output = abind(IMAGES, shift1, shift2, shift3, along=1)
  
  DATA[[1]] = output
  DATA[[2]] = key
  DATA
}

fit_data = function(DATA) {
  #install.packages("BiocManager")
  #BiocManager::install("EBImage")
  require(EBImage)
  
  dims = dim(DATA[[1]])
  temp = array(dim = c(dims[1],128,64,dims[4]))
  
  for (i in dims[1]){ 
    temp[i,,,] = resize(temp,128,64)
  }
  
  DATA[[1]] = temp
  DATA
}
