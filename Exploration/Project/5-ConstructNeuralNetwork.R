#' Build Keras Convolutional Neural Network
#' 
#' Constructs a Keras NN with 4 convolutional layers
#' 
#' @note it might be necessary to run install_packages first
#' 
#' @return an untrained Keras CNN with randomized params
#' 
#' @author Ryan Taylor \email{rmt2dt@@virginia.edu}, Jonathan Walter 
#' \email{jaw3es@@virginia.edu}
#' 
#' @export
#' @import devtools
#' @import keras
#' @import tensorflow


build_model <- function(){

  model = keras_model_sequential()
  
  PADDING = "same"
  ACTIVATION = "relu"
  LEARNINGRATE = 0.001
  DECAY = 0.000001
  
  model %>%
    
    #CONV LAYER 1
    layer_conv_2d(filter=32,kernel_size=c(6,3),padding=PADDING,input_shape = c(128,64,1)) %>%
    layer_activation(ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(4,2),padding=PADDING,data_format=("channels_last")) %>%
    
    #CONV LAYER
    layer_conv_2d(filter=16,kernel_size=c(3,3),padding=PADDING,data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
    
    #CONV LAYER 3
    layer_conv_2d(filter=8,kernel_size=c(3,3),padding=PADDING,data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
    
    #CONV LAYER 4
    layer_conv_2d(filter=4,kernel_size=c(3,3),padding=PADDING,data_format=("channels_last")) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_average_pooling_2d(pool_size=c(2,2),padding=PADDING,data_format=("channels_last")) %>%
    
    
    #Fully Connected Layers
    layer_flatten(data_format="channels_last") %>%
    layer_dense(units=64) %>%
    layer_activation(activation = ACTIVATION) %>%
    layer_dense(units=64) %>% 
    layer_activation(activation = ACTIVATION) %>%
    layer_dense(units=16) %>% 
    layer_activation(activation = ACTIVATION) %>%
    layer_dense(units=2) %>%
    layer_activation(activation = ACTIVATION)
  
  
  opt = optimizer_adam(lr=LEARNINGRATE, decay=DECAY)
  
  model %>%
    compile(loss="binary_crossentropy",
            optimizer=opt, metrics = "accuracy")
  
  summary(model)
  
  model
}
