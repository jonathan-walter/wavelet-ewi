
#install.packages("tictoc")
### put all of it together ###
rm(list=ls())
require(tictoc)

setwd("C:/Users/rimcl/OneDrive/School/Capstone/github/Project")
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
params = get_params(sequences)


#params = params[params$label == 1,]
nsamples = 5
params = params[sample(1:dim(params)[1],nsamples),]

keys = params$label


if(!dir.exists("C:/Users/rimcl/Capstone_Data")){
  dir.create("C:/Users/rimcl/Capstone_Data")
}
setwd("C:/Users/rimcl/Capstone_Data")

saveRDS(keys,"key")

imax = dim(params)[1]

tic()
for(i in 1:imax){
  ts = run_lakemodel(params[i,],dt)
  wt = run_wsyn(ts,17,2, pooling = "mean")
  string = paste("wt_avg_",i,sep="")
  saveRDS(wt, file = string)
  if ((i%%(floor(imax/100) == 0))){
    print(paste(round(100*i/imax, digits = 3), "% done...",sep = ""),quote = FALSE);
    toc();
  }
}

LST = NULL
if(TRUE){
  for (i in 1:5){
    LST[[i]] = readRDS(paste("wt_avg_",i,sep=""))
    image(LST[[i]][,,1],col=hcl.colors(200,"Plasma"))
  }
}