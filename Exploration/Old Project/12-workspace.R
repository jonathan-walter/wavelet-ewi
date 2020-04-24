
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


data = load_sample_data(200)
model = build_model()


