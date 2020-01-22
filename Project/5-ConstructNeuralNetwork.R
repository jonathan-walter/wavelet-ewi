build_model <- function(){
  require(devtools)
  
  #devtools::install_github("rstudio/keras")
  require(keras)
  require(tensorflow)
  #install_keras()
  #install_tensorflow(gpu=F)
  
  #DATA = input #DATA is 2843 by 64 by 128
  # 2843 samples
  # images 64 by 128
  
  ###### Model #########
  model = keras_model_sequential()
  
  PADDING = "same"
  ACTIVATION = "relu"
  LEARNINGRATE = 0.005
  DECAY = 0.000001
  
  model %>%
    
    #CONV LAYER 1
    layer_conv_2d(filter=32,kernel_size=c(3,6),padding=PADDING,input_shape = c(64,128,1),data_format=("channels_last")) %>%
    layer_activation(ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,4),padding=PADDING,data_format=("channels_last")) %>%
    
    ##figure out what channels last actually do
    ## what is a channel
    
    #CONV LAYER
    layer_conv_2d(filter=16,kernel_size=c(3,3),padding=PADDING,input_shape = c(32,32,32),data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
    
    #CONV LAYER 3
    layer_conv_2d(filter=8,kernel_size=c(3,3),padding=PADDING,input_shape = c(16,16,16),data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
    
    #CONV LAYER 4
    layer_conv_2d(filter=4,kernel_size=c(3,3),padding=PADDING,input_shape = c(8,8,8),data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
  
    
    #Fully Connected Layers
    layer_flatten(data_format="channels_last") %>%
    layer_dense(units=64,input_shape=(64),data_format="channels_last") %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_dense(units=16, input_shape=(16),data_format="channels_last") %>% 
    layer_activation(activation = ACTIVATION) %>%
    layer_dense(units=16, input_shape=(16), data_format="channels_last") %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_dense(units=2,activation="softmax",data_format="channels_last")
  
    
    opt = optimizer_adam(lr=LEARNINGRATE, decay=DECAY)
   
    model %>%
      compile(loss="categorical_crossentropy",
            optimizer=opt, metrics = "accuracy")
   
    summary(model)
    
    model
}
    
  
  
  
  
  
  

