#install.packages("tictoc")
#install.packages("foreach")
#install.packages("doParallel")

#runModel_getWT= function(sequences,window_width,n_samples,use_cores){

rm(list=ls())
require(tictoc)
require(foreach)
require(doParallel)
require(dplyr)
require(EBImage)

cores = detectCores()
c1 = makeCluster(cores[1]-1)
#c1 = cores_used

setwd("D:/Capstone")
source("1-MakeTrainingParameters.R")
source("2-RunLakeModel.R")
source("3-WaveletAnalysis.R")

sequences = rbind(
  #4 month
  c(0,1,0),
  c(0,1,0),

  #2 month
  c(1,2,1),
  c(1.125,2,0.875),
  c(0.875,2,1.125),
  c(1.25,2,0.75),
  c(0.75,2,1.25),
  c(1.5,2,0.5),
  c(0.5,2,1.5),
  
  #1 month
  c(1.5,1,1.5),
  c(1.75,1,1.25),
  c(1.25,1,1.75),
  c(2,1,1),
  c(1,1,2),
  c(0.75,1,2.25),
  c(2.25,1,0.75),

  
  #0.5 month
  c(3.5,1,3.5),
  c(2.5,1,4.5),
  c(4.5,1,2.5),
  c(5.5,1,1.5),
  c(1.5,1,5.5),
  c(1,1,6),
  c(6,1,1)
)
  

dt = 0.02
PATH = "D:/Capstone/Figure2Data-Serizawa.csv"
sequences = sequences/rowSums(sequences)
FP = c(0.9,1)
param = get_params_old(sequences,FP,PATH)
temp = rbind(param,param)
sigmas = c(rep(1,dim(temp)[1]/2),rep(0.5,dim(temp)[1]/2))
params = cbind(temp,sigmas)

# 
# nsamples = 2
# params = params[params$label == 1,]
# params = params[sample(1:dim(params)[1],nsamples),]

keys = params$label
n = 1:length(keys)
i_f = n[keys == 0]
i_t = n[keys == 1]
samps_f = sample(i_f, 200, replace = FALSE)
samps_t = sample(i_t, 200, replace = FALSE)
samps = c(samps_f,samps_t)
samps = sort(samps)



if(!dir.exists("C:/Capstone/")){
  dir.create("C:/Capstone/")
}
setwd("C:/Capstone/")


istart = 1
imax = dim(params)[1] + istart - 1

registerDoParallel(c1)
tic()

# foreach(i=samps) %dopar% {
for (i in samps){
  ts = run_lakemodel(params[i,1:7],params[i,8],dt)
  string = paste("ts_whole_",i,sep="")
  saveRDS(ts[,2], file = string)
  keytemp = c(!params$label[i],params$label[i])
  string = paste("ts_whole_key_",i,sep="")
  saveRDS(keytemp, file = string)
  #if ((i%%(floor(imax/100) == 0))){
  #  print(paste(round(100*i/imax, digits = 3), "% done...",sep = ""),quote = FALSE);
  #  toc();
  #}
}
toc()

stopCluster(c1)