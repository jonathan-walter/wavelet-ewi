#install.packages("foreach")
### put all of it together ###

require(foreach)

### get simulation data

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
  
params = get_params(sequences)



#LST <- for(i=1:dim(params)[1]) %dopar% {
  
#}