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

setwd("C:/Capstone/github/Project")
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
  

dt = 0.01
PATH = "C:/Capstone/github/Figure2Data-Serizawa.csv"
sequences = sequences/rowSums(sequences)
FP = c(0.9,1)
param = get_params(sequences,FP,PATH)
temp = rbind(param,param)
sigmas = c(rep(1,dim(temp)[1]/2),rep(0.5,dim(temp)[1]/2))
params = cbind(temp,sigmas)

# 
# nsamples = 2
# params = params[params$label == 1,]
# params = params[sample(1:dim(params)[1],nsamples),]

keys = params$label


if(!dir.exists("C:/Capstone/Capstone_Data")){
  dir.create("C:/Capstone/Capstone_Data")
}
setwd("C:/Capstone/Capstone_Data")

saveRDS(keys,"key")

istart = 1
imax = dim(params)[1] + istart - 1

registerDoParallel(c1)
tic()
foreach(i=istart:imax) %dopar% {
  ts = run_lakemodel(params[i,1:7],params[i,8],dt)
  wt = run_wsyn(ts,2)
  string = paste("wt_whole_",i,sep="")
  saveRDS(wt, file = string)
  #if ((i%%(floor(imax/100) == 0))){
  #  print(paste(round(100*i/imax, digits = 3), "% done...",sep = ""),quote = FALSE);
  #  toc();
  #}
  print(i)
}
toc()

stopCluster(c1)

LST = NULL
if(TRUE){
  for (i in istart:imax){
    LST[[i]] = readRDS(paste("wt_avg_",i,sep=""))
    image(LST[[i]][,,1],col=hcl.colors(200,"Plasma"))
  } 
}
