#load packages
library(ggplot2)
library(plyr)
library(reshape2)
library(bitops)
library(RCurl)


#load the data
x <- getURL("https://raw.githubusercontent.com/Amirosimani/collection/master/Artworks.csv")
moma.dataframe <- read.csv(text = x)
