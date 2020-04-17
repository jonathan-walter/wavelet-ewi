rm(list=ls())
require(keras)
setwd("C:/Capstone")
source("5-ConstructNeuralNetwork.R")
source("6-PreProcessData.R")

PATH =  "C:/Capstone/OldCapstoneData"

#test model trained on firsthalf on second data set

#data = prep_data(expand_data(compile_data(1:100,2,PATH)),0.7)

#data_orig = prep_data(compile_data(1:2883,2,PATH),0.7)
temp = compile_data(1:2833,PATH)
data = prep_data(temp,0.7) 
##############################################

model1 = build_model()

train_x = data[[1]]
train_y = data[[2]]
test_x = data[[3]]
test_y = data[[4]]

model1 %>% fit(train_x, train_y, 
              batch_size = 128, epochs = 1000,
              validation_data = list(test_x,test_y),
              shuffle = TRUE)

string = paste("model-avg",Sys.Date(),sep="-")
save_model_tf(model1, file = string)

out = predict_classes(model1, test_x)
outcome = out == test_y[,2]


accuracy = mean(outcome)
accuracyNull = mean(outcome[test_y[,1]==1])
accuracyAlt = mean(outcome[test_y[,2]==1])

cbind(out, y_cat,(y_cat == out_cat))
out1 = c(accuracy, accuracyNull, accuracyAlt)
out1
