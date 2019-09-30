rm(list=ls())
require(mxnet)

##### Load and Shape Training Data #####
  train_data <- # data set for training
  key <- # annotation for training data set
  
  test <- # data set for training
  test_key <- # annotation for test data set

  num_features <- #number of features per sample
  
  num_samples <- #number of samples in testing data
  
  shuffle_order = sample.int(num_samples, replace = FALSE)
  
  # reshape data set if necessary
  
  train <- train_data[shuffle_order,,]
  
##### Set Up Symbolic Model #####
  
  
  data <- mx.symbol.Variable('data')
  
  # 1st convolutional layer
  conv_1 <- mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = 20)
  tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
  pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
  
  # 2nd convolutional layer
  conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 50)
  tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
  pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
  
  # 1st fully connected layer
  flatten <- mx.symbol.Flatten(data = pool_2)
  fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
  tanh_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")
  
  # 2nd fully connected layer
  fc_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 40)
  
  # Output. Softmax output since we'd like to get some probabilities.
  NN_model <- mx.symbol.SoftmaxOutput(data = fc_2)

##### Initialize Network #####
  mx.set.seed(100)
  devices <- mx.cpu()
  
##### Train Model #####
  model <- mx.model.FeedForward.create(NN_model,
                                       X = train_array, #look at structure of this variable
                                       y = train_y, #look at structure of this variable too
                                       ctx = devices,
                                       num.round = 480, # look into what this means
                                       array.batch.size = 40, #might need to change this
                                       learning.rate = 0.01,
                                       momentum = 0.9,
                                       eval.metric = mx.metric.accuracy,
                                       epoch.end.callback = mx.callback.log.train.metric(100))
  

  
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