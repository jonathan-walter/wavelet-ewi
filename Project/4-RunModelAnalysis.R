
#install.packages("tictoc")
### put all of it together ###
rm(list=ls())
require(tictoc)

setwd("C:/Users/rimcl/OneDrive/School/Capstone/github/Project")
source("1-MakeTrainingParameters.R")
source("2-RunLakeModel.R")
source("3-WaveletAnalysis.R")


sequences = rbind(
  c(0,1,0),
  
  c(1,2,1),
  c(1.5,2,0.5),
  c(1.5,2,0.5),
  c(2,2,0),
  c(0,2,2),
  
  c(1.5,1,1.5),
  c(2,1,1),
  c(1,1,2),
  c(3,1,0),
  c(0,1,3),
  
  c(3.5,1,3.5),
  c(5,1,2),
  c(2,1,5),
  c(7,1,0),
  c(0,1,7)
)
  

dt = 0.01
params = get_params(sequences)
params = params[1,]

labels = params$label


if(~dir.exists("C:/Users/rimcl/Capstone_Data")){
  dir.create("C:/Users/rimcl/Capstone_Data")
}
setwd("C:/Users/rimcl/Capstone_Data")
saveRDS(labels,"key")


imax = dim(params)[1]

tic()
for(i in 1:imax){
  ts = run_lakemodel(params[i,],dt)
  wt = run_wsyn(ts, pooling = "mean")
  string = paste("wt_avg_",i)
  saveRDS(wt, file = string)
  if (i%%(floor(imax/100) == 0)){
    print(paste(round(100*i/imax, digits = 3), "% done...",sep = "")); toc();
  }
}