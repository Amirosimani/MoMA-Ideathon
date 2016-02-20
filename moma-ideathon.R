### installing packages----
library(ggplot2)
library(plyr)
library(reshape)
library(stringr)
library(data.table)

### 0.importing and cleaning data set----
moma.data = read.csv("MoMA.csv")

#keeoing the relevent columns in the data frame
data <-moma.data[(moma.data$Department =="Drawings") | 
                    (moma.data$Department =="Painting & Sculpture") |
                    (moma.data$Department =="Prints & Illustrated Books") |
                    (moma.data$Department =="Photography")
                    , ]
data <- data[!(data$Classification=="Illustrated Book"), ] 
data <- data[!(data$Classification=="Periodical"), ] 

"#spliting multiple artists into a list
data$Artist <- as.character(data$Artist)
artists <- strsplit(as.character(data$Artist), ',')
artists <- gsub("Various Artists", "",artists)
setDT(data)[, paste0("Names", 1:5) := tstrsplit(artists, ",")]"

### 1.convert date----
library(lubridate)
data$DateAcquired <- as.Date(data$DateAcquired)
data[, "YrAcquired"] <- year(data$DateAcquired)
data$DateAcquired <- as.numeric(data$DateAcquired)

#cleaining creation date
data$Date = gsub("[a-z]", "", data$Date)
data$Date = gsub("[A-Z]", "", data$Date)
data$Date = str_replace_all(data$Date, " ", "")
data$Date = str_replace_all(data$Date, "[^[:alnum:]]", "")
data$Date = substr(data$Date,1,4) 
data$Date = as.numeric(data$Date)

### 2. read xls file to import gender-----
"
data2 <- read.csv("MoMAartist_data_w_bom.csv")
if (data2[2] == data[4]){
  print y}
data2$DisplayName <- as.character(data2$DisplayName)
data$Artist <- as.character(data$Artist)


i <- 0
for (i in 1:nrow(data)){
  for (j in 1:nrow(data2)){
  if(data[1,i] == data2[1,j]){
    print("Y")
  }
}
}
"

### 3. Graphs ----
#Adding blank space column - the time took for the artwork after creation to get to MoMa
data[, "BlankSpace"] <- data$YrAcquired - data$Date

#Blankspace 
ggplot(data, aes(BlankSpace, fill = factor(Gift), color = factor(Gift))) +
  geom_freqpoly(binwidth = 5) + xlim(0, 150) 
  
#Adding Artwork Age column - when the artwork was created
#change the x label/scale
data[, "ArtworkAge"] <- 2016 - data$YrBirth
ggplot(data, aes(ArtworkAge), fill = factor(Department), color = factor(Department)) +
  geom_freqpoly(binwidth = 5) + xlim(0, 220) 

#before and after 1960
#tune the scale for axis
data[data$YrAcquired>1960,]
ggplot(aa, aes(BlankSpace)) +
  geom_freqpoly(binwidth = 5) + xlim(0, 100) + ylim(0, 20000)

data[(data$YrAcquired <1960) & (data$YrAcquired > 1925) ,]
ggplot(bb, aes(BlankSpace)) +
  geom_freqpoly(binwidth = 5) + xlim(0, 100) + ylim(0, 2000)

#contemporary fact check
#data set has an issue!
contemp.data <- data[data$YrAcquired < data$YrDeath,]
contemp.comp <-contemp.data[complete.cases(contemp.data$BlankSpace),]
contemp.comp.painting <- contemp.comp[contemp.comp$Department == "Painting & Sculpture",]
count(data$Department)

ggplot(contemp.comp.painting, aes(BlankSpace, fill = factor(Gift), color = factor(Gift))) +
  geom_freqpoly(binwidth = 5) + xlim(0, 100)

#Nationalities
(count(contemp.comp.painting$Nationality))

