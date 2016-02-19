### installing packages----
library(ggplot2)
library(plyr)
library(reshape)
library(stringr)
library(data.table)

### 0.importing data set----
moma.data = read.csv("MoMA.csv")

#keeoing the relevent columns in the data frame
data <-moma.data[(moma.data$Department =="Drawings") | 
                    (moma.data$Department =="Painting & Sculpture") |
                    (moma.data$Department =="Prints & Illustrated Books") |
                    (moma.data$Department =="Photography")
                    , ]
data <- data[!(data$Classification=="Illustrated Book"), ] 
data <- data[!(data$Classification=="Periodical"), ] 

#spliting multiple artists into a list
data$Artist <- as.character(data$Artist)
artists <- strsplit(as.character(data$Artist), ',')
artists <- gsub("Various Artists", "",artists)
setDT(data)[, paste0("Names", 1:5) := tstrsplit(artists, ",")]

#convert dates
data$DateAcquired <- as.Date(data$DateAcquired)
data$

media <- moma.data[(moma.data$Department == 'Prints & Illustrated Books' ),]
media$DateAcquired <- as.Date(media$DateAcquired)

media.sorted <- media[order(media$DateAcquired),] 

 <- arrange(media,asc(DateAcquired))

#artists1 <- sapply(strsplit(as.character(moma.data$Artist), ", "), "[", 1)

