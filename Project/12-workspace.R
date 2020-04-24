
#' March 18
#' use the predict function and biecdf to make timeseries
#' use majority of the window to classify the the region
#' decide on how to set the key for windowed analysis
## change the key of things and retrain --> based on the domain
## rerun the windowed analysis

# 1. fixing key
# 2. running windowed analysis
# 3. training
# 4.keep documenting
# 5. user functions



# 3pm Friday
# whole window tests
# unit testing
## this can take as long as you have time for
## test out the workflow
## make sure eeach funnction does what it needs to do
## test particular failure cases

# technical report

# research report
## what worked what didnt
## next steps --> what variables do we think could make it work

# Figures
## schematic of the neural network
## emphasis on design
## flowchart for data processing
## table for accuracy meassures
## an example of the transient probability test


############################################################
# add_dimension()     not available to user 
# build_model()
# classify()
# install_packages()
# predict()
# prob_event2d()
# prob_timeseries()
# resize_images()
# wt()
# wt_windowed()
# wt_ns()               not available to user
# save_model()
# load_model()
# load_premade_model() <-- need to figure out how to do this
# check_images
# load_premade_data()
############################################################

setwd("D:/Capstone")
source("11-UserFunctions.R")
install_packages()
ts_data = load_premade_data(200)
wt_data = wt(ts_data)

model = train_model(wt_data,test_iterations = 1000)
imageset = images[[1]]
keyset = images[[2]]

predictions = predict(model, imageset, type = "category")
predict_stats(predictions,keyset)

#################333


setwd("C:/Capstone/Capstone_Data325")
install_packages()
wt_data = NULL
key_data = NULL
for (i in 1:1932){
  temp = readRDS(paste("windowed_wt_",i,sep=""))
  wt_data = abind(wt_data, temp[[1]], along = 1)
  key_data = abind(key_data, temp[[2]], along = 1)
}

model = train_model(wt_data,test_iterations = 1000)
imageset = images[[1]]
keyset = images[[2]]

predictions = predict(model, imageset, type = "category")
predict_stats(predictions,keyset)



