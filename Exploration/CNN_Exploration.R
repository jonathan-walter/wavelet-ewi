rm(list=ls())
require(mxnet)

##### Messing Around with Training Data #####

train_data = rep(0,100*100*200)
dim(train_data) = c(100,100,1,200)

j = sample(1:100,100,replace = FALSE)
k = sample(1:200,200,replace = FALSE)

for(i in 1:100) {
  train_data[j[i],,1,k[i]] = rep(1, 100)
}
for(i in 1:100) {
  train_data[,j[i],1,k[i+100]] = rep(1, 100)
}

key = rep(0,200)
key[k] = 1

##### Load and Shape Training Data #####

 # test_data <- # data set for training
 # test_key <- # annotation for test data set
    
 train_data <- train_data # data set for training (x,y,channel,index)
 train_key <- key # annotation for training data set

  num_x <- dim(train_data)[1]
  num_y <- dim(train_data)[2]
  num_features <- num_x*num_y  #number of features per sample
  num_channels <- dim(train_data)[3] #number of different variables in samples
  num_samples <- dim(train_data)[4] #number of samples in testing data
    
  shuffle_order = sample.int(num_samples, replace = FALSE)
  
  train <- train_data[,,,shuffle_order]
  
##### Set Up Symbolic Model #####
  
  # We are going to need help constructing this neural network
  
  
  data <- mx.symbol.Variable('data')
  
  # 1st convolutional layer
  conv_1 <- mx.symbol.Convolution(data = data,kernel = c(5, 5), num_filter = 20)
  tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
  pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
  
  # 2nd convolutional layer
  conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 50)
  tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
  pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
  
  # 1st fully connected layer
  flatten <- mx.symbol.flatten(data = pool_2)
  fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
  tanh_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")
  
  # 2nd fully connected layer
  fc_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 40)
  tanh_4 <- mx.symbol.Activation(data = fc_2, act_type = "tanh")
  fc_3 <- mx.symbol.FullyConnected(data = tanh_4, num_hidden = 1)
  
  # Output. Softmax output since we'd like to get some probabilities.
  NN_model <- mx.symbol.SoftmaxOutput(data = fc_3)

##### Initialize Network #####
  mx.set.seed(101)
  devices <- mx.cpu()
  
##### Train Model #####
  model <- mx.model.FeedForward.create(NN_model,
                                       X = train_data, #look at structure of this variable
                                       y = train_key, #look at structure of this variable too
                                       ctx = devices,
                                       num.round = 3, # number of rounds of training+testing
                                       array.batch.size = 40, #might need to change this
                                       learning.rate = 0.05,
                                       momentum = 0.9,
                                       eval.metric = mx.metric.accuracy,
                                       epoch.end.callback = mx.callback.log.train.metric(100))
  

p = predict(model, train_data)
dim(p)
  
  #...things to potential change about this model...
  # Activation Function...Options:
  #   - Sigmoid
  #   - ReLU
  #   - Leaky ReLU
  #   - tanh (current)
  #
  # Learning Rate
  #
  # Batch Size