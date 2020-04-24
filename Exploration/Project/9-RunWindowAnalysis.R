#install.packages("tictoc")
#install.packages("foreach")
#install.packages("doParallel")

#runModel_getWT= function(sequences,window_width,n_samples,use_cores){

# rm(list=ls())
require(tictoc)
require(foreach)
require(doParallel)
require(dplyr)
require(EBImage)

cores = detectCores()
c1 = makeCluster(cores[1]-1)
#c1 = cores_used

setwd("C:/Capstone")
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


dt = 0.025
PATH = "D:/Capstone/Figure2Data-Serizawa.csv"
sequences = sequences/rowSums(sequences)
FP = 0.9

tempP = get_params(sequences,FP,PATH)
# tempP$lab = (tempP$label1 == 2)|(tempP$label2 == 2)
# tempT = tempP[tempP$lab == 1,]
# Tparams = tempT
# tempF = tempP[tempP$lab == 0,]
# Fparams = tempF[sample(1:length(tempF[,1]),165),]

params = tempP

#nsamples = 1
#params = params[sample(1:dim(params)[1],nsamples),]


if(!dir.exists("C:/Capstone/Capstone_Data325")){
  dir.create("C:/Capstone/Capstone_Data325")
}
setwd("C:/Capstone/Capstone_Data325")


istart = 1
imax = dim(params)[1] + istart - 1


registerDoParallel(c1)
tic()

w.width = 5*24
w.step = 1*24

foreach(i = errors) %dopar% {
  
  # string = paste("windowed_wt",i,sep="_")
  # #if(file.exists(string)){
  #   
  #   para = params[i-istart+1,]
  #   ts = run_lakemodel_step(para,1,dt)
  #   sequence = as.numeric(para[5:7])
  #   step_index = ceiling(length(ts[,1])*(sequence[1] + sequence[2]/2)/(sum(sequence)))
  #   wt = wt_windowed(ts,w.width,w.step,step_index)
  #   
  #   if(para$label2 != 2){
  #     wt[[2]] = !wt[[2]]
  #   }
  #   
  #   if(para$label1 == para$label2){
  #     wt[[2]] = 0*wt[[2]]
  #     wt[[2]][,1+as.numeric(para$label1==2)] = 1
  #   }
  #   saveRDS(wt, file = string)
  # #}
  
  string = paste("windowed_wt",i,sep="_")
  
  if(TRUE){
  
    para = params[i-istart-imax+1,]
    ts = run_lakemodel_step(para,0,dt)
    sequence = as.numeric(para[5:7])
    step_index = ceiling(length(ts[,1])*(sequence[1] + sequence[2]/2)/(sum(sequence)))
    wt = wt_windowed(ts,w.width,w.step,step_index)
    if(para$label2 != 2){
      wt[[2]] = !wt[[2]]
    }
    
    if(para$label1 == para$label2){
      wt[[2]] = 0*wt[[2]]
      wt[[2]][,1+as.numeric(para$label1==2)] = 1
    }
    
    saveRDS(wt, file = string)
  }
  
  # if ((i%%(floor(imax/100) == 0))){
  #   print(paste(round(100*i/imax, digits = 3), "% done...",sep = ""),quote = FALSE);
  #  toc();
  #}
}
toc()
stopCluster(c1)

