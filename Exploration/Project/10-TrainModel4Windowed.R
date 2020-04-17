rm(list=ls())
setwd("C:/Capstone")
source("5-ConstructNeuralNetwork.R")
source("6-PreProcessData.R")


PATH =  "C:/Capstone"

#test model trained on firsthalf on second data set

PREdata = compile_data_windowed(1:330,PATH)
data = prep_data(PREdata,0.7)

##############################################

model = build_model()

train_x = data[[1]]
train_y = data[[2]]

test_x = data[[3]]
test_y = data[[4]]


model %>% fit(train_x, train_y, 
               batch_size = 128, epochs = 2000,
               validation_data = list(test_x,test_y),
               shuffle = TRUE)

string = paste("model-windowed",Sys.Date(),sep="-")
save_model_tf(model, string)

out = predict(model1, test_x)
y_cat = as.numeric(test_y[,2]>test_y[,1])+1
out_cat = as.numeric(out[,2]>out[,1])+1
outcome = (y_cat == out_cat)


accuracy = mean(outcome)
accuracyNull = mean(outcome[test_y[,1]==1])
accuracyAlt = mean(outcome[test_y[,2]==1])

cbind(out, y_cat,(y_cat == out_cat))
out1 = c(accuracy, accuracyNull, accuracyAlt)
out1