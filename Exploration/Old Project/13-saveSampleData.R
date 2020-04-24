setwd("D:/Capstone/OldCapstoneData")
keys = t(readRDS("key"))
keys[,1] = !keys[,1]
sum(keys[,2])

n = 1:2833
i_f = n[keys[,2] == 0]
i_t = n[keys[,2] == 1]
samps_f = sample(i_f, 200, replace = FALSE)
samps_t = sample(i_t, 200, replace = FALSE)

samps = c(samps_f,samps_t)
samps = sort(samps)

imageset = array(0,c(400,128,64))
keyset = array(0,c(400,2)) 


index = 1
for(i in samps){
  string = paste("wt_avg_",i,sep="")
  temp = readRDS(string)
  imageset[index,,] = temp[,,2]
  keyset[index,] = keys[i,]
  index = index+1
}

out = NULL
out[[1]] = imageset
out [[2]] = keyset
setwd("D:/Capstone")
saveRDS(out,file="SampleData")

a =readRDS("SampleData")
