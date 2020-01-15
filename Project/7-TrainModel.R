setwd("C:/Users/rimcl/OneDrive/School/Capstone/github/Project")
source("5-ALT.R")
source("6-PreProcessData.R")

PATH =  "C:/WorkSchoolOffline/CapstoneData"

model = build_model()
data = compile_data(1,2, PATH) #(n_samples,channel)

train_x = data[[1]]
train_y = data[[2]]
not_train_y = !train_y
train_y = cbind(not_train_y,train_y)
test_x[,,,1] = data[[3]]
test_y = data[[4]]
not_test_y = !test_y
test_y = cbind(not_test_y,test_y)

test_indices = data[[5]]



model %>% fit(train_x, train_y, 
              batch_size = 32, epochs = 80,
              validation_data = list(test_x,test_y),
              shuffle = TRUE)

