#rm(list=ls())
library(roxygen2)
library(devtools)

setwd("~/GitHub/wavelet-ewi/Package/")

#create("wtCNN")
build("wtCNN")   # need a folder named 'wtCNN, in which a file named DESCRIPTION and a sub-folder named 'R'. This will creat a zip file
document("wtCNN") # this will create a file 'NAMESPACE' and a folder 'man'
check("wtCNN")


install("wtCNN")
library("wtCNN")
