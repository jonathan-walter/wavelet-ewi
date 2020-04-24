rm(list=ls())
setwd("C:/Capstone")
source("5-ConstructNeuralNetwork.R")
source("6-PreProcessData.R")


PATH =  "C:/Capstone/Capstone_Data325"

#test model trained on firsthalf on second data set

PREdata = compile_data_windowed(1:1932,PATH)
data = prep_data(PREdata,0.7)

##############################################

model = build_model()
 
train_x = data[[1]]
train_y = data[[2]]
n = 1:dim(train_x)[1]
twos = n[train_y[,2]==1]
ones = n[train_y[,1]==1]

one_samples = sample(ones, length(twos), replace = FALSE)


train_y = train_y[c(one_samples,twos),]
train_x = train_x[c(one_samples,twos),,,]
train_x =abind(train_x,NULL,along=4)



test_x = data[[3]]
test_y = data[[4]]


model %>% fit(train_x, train_y, 
               batch_size = 32, epochs = 1250,
               validation_data = list(test_x,test_y),
               shuffle = TRUE)

string = paste("model-windowed2",Sys.Date(),sep="-")
save_model_tf(model, string)

out = predict(model, test_x)
y_cat = as.numeric(test_y[,2]>test_y[,1])+1
out_cat = as.numeric(out[,2]>out[,1])+1
outcome = (y_cat == out_cat)


accuracy = mean(outcome)
accuracyNull = mean(outcome[test_y[,1]==1])
accuracyAlt = mean(outcome[test_y[,2]==1])

cbind(out, y_cat,(y_cat == out_cat))
out1 = c(accuracy, accuracyNull, accuracyAlt)
out1

