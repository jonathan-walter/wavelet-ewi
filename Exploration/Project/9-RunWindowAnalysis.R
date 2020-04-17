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
source("8-wt_windowed.R")

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
FP = 0.9

tempP = get_params(sequences,FP,PATH)
tempT = tempP[tempP$label ==1,]
Tparams = tempT[sample(1:length(tempT[,1]),56),]
tempF = tempP[tempP$label == 0,]
Fparams = tempF[sample(1:length(tempF[,1]),165),]

params = rbind(Tparams,Fparams)

#nsamples = 1
#params = params[sample(1:dim(params)[1],nsamples),]


if(!dir.exists("C:/Capstone/Capstone_Data")){
  dir.create("C:/Capstone/Capstone_Data")
}
setwd("C:/Capstone/Capstone_Data")


istart = 110
imax = dim(params)[1] + istart - 1



registerDoParallel(c1)
tic()
key = c()

w.width = 7*24
w.step = 2*24

foreach(i=istart:imax) %dopar% {
  
  para = params[i-istart+1,]
  ts = run_lakemodel_step(para,1,dt)
  sequence = as.numeric(para[5:7])
  step_index = ceiling(length(ts[,1])*(sequence[1] + sequence[2]/2)/(sum(sequence)))
  wt1 = wt_windowed(ts,w.width,w.step,step_index)
  
  para = params[i-istart+1,]
  ts = run_lakemodel_step(para,0.5,dt)
  sequence = as.numeric(para[5:7])
  step_index = ceiling(length(ts[,1])*(sequence[1] + sequence[2]/2)/(sum(sequence)))
  wt2 = wt_windowed(ts,w.width,w.step,step_index)
  
  images = abind(wt1[[1]],wt2[[1]],along=1)
  keys = abind(wt1[[2]],wt2[[2]],along=1)
  
  string = paste("windowed_wt",i,sep="_")
  saveRDS(images, file = string)
  
  string = paste("windowed_key",i,sep="_")
  saveRDS(keys, file = string)
  
  # if ((i%%(floor(imax/100) == 0))){
  #   print(paste(round(100*i/imax, digits = 3), "% done...",sep = ""),quote = FALSE);
  #  toc();
  #}
}
toc()
stopCluster(c1)

