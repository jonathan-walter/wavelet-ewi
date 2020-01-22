rm(list=ls())
setwd("C:/Users/rimcl/OneDrive/School/Capstone/github/Project")
source("5-ALT.R")
source("6-PreProcessData.R")

PATH =  "C:/WorkSchoolOffline/CapstoneData"

model = build_model()
data = compile_data(1,2,0.5,PATH) #(n_samples,channel)

train_x = data[[1]]
train_y = data[[2]]
not_train_y = !train_y
train_y = cbind(not_train_y,train_y)
test_x = data[[3]]
test_y = data[[4]]
not_test_y = !data[[4]]             
test_y = cbind(not_test_y,test_y)

test_indices = data[[5]]



model %>% fit(train_x, train_y, 
              batch_size = 128, epochs = 1000,
              validation_data = list(test_x,test_y),
              shuffle = TRUE)

saveRDS(R, file = "MODEL")

out = predict(model, test_x)
y_cat = as.numeric(test_y[,2]>test_y[,1])+1
out_cat = as.numeric(out[,2]>out[,1])+1
outcome = (y_cat == out_cat)

accuracy = mean(outcome)
accuracyNull = mean(outcome[test_y[,1]==1])
accuracyAlt = mean(outcome[test_y[,2]==1])

c(accuracy, accuracyNull, accuracyAlt)
