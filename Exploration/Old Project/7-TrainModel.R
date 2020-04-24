rm(list=ls())
setwd("C:/Users/rimcl/OneDrive/School/Capstone/github/Project")
source("5-ConstructNeuralNetwork.R")
source("6-PreProcessData.R")

PATH =  "D:/CapstoneData"

#test model trained on firsthalf on second data set

#data = prep_data(expand_data(compile_data(1:100,2,PATH)),0.7)

#data_orig = prep_data(compile_data(1:2883,2,PATH),0.7)
predata = compile_data(94:4306,PATH)
predata[[2]] = cbind(predata[[2]],!predata[[2]])
tempkey = predata[[2]]
tempdata = predata[[1]]

nf = sum(tempkey[,1])
n = 1:length(tempkey[,1])
indices = n[tempkey[,1]==0]
it = n[tempkey[,1]==1]
samples = sample(indices,nf,replace=FALSE)

predata[[1]] = tempdata[c(ifalse,samples),,]
predata[[2]] = tempkey[c(ifalse,samples),]

data = prep_data(predata,0.7)




##############################################

model = build_model()

train_x = data[[1]]
train_y = data[[2]]

test_x = data[[3]]
test_y = data[[4]]


model %>% fit(train_x, train_y, 
              batch_size = 128, epochs = 200,
              validation_data = list(test_x,test_y),
              shuffle = TRUE)

string = paste("model-windowed",Sys.Date(),sep="-")
save_model_tf(model, string)

out= predict(model1, test_x)
y_cat = as.numeric(test_y[,2]>test_y[,1])+1
out_cat = as.numeric(out[,2]>out[,1])+1
outcome = (y_cat == out_cat)


accuracy = mean(outcome)
accuracyNull = mean(outcome[test_y[,1]==1])
accuracyAlt = mean(outcome[test_y[,2]==1])

cbind(out, y_cat,(y_cat == out_cat))
out1 = c(accuracy, accuracyNull, accuracyAlt)
out1