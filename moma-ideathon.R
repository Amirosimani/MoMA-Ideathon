### installing packages----
library(ggplot2)
library(plyr)
library(reshape)
library(stringr)
library(data.table)

### 0.importing and cleaning data set----
moma.data = read.csv("MoMA.csv")

#keeping the relevent columns in the data frame
data <-moma.data[(moma.data$Department =="Drawings") | 
                    (moma.data$Department =="Painting & Sculpture") |
                    (moma.data$Department =="Prints & Illustrated Books") |
                    (moma.data$Department =="Photography")
                    , ]
data <- data[!(data$Classification=="Illustrated Book"), ] 
data <- data[!(data$Classification=="Periodical"), ] 

data <- data[, !(colnames(data) %in% c("Dimensions","LenCm1","LenCm2", "LenCm3", "LenCm4", "LenCm5",
                                       "WidCm1","WidCm2","WidCm3","WidCm4","WidCm5",
                                       "HtCm1","HtCm2","HtCm3","HtCm4","HtCm5",
                                       "YrStart","YrComp"))]

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

#adding blnakSpace-the time took for the artwork after creation to get to MoMa
data[, "BlankSpace"] <- data$YrAcquired - data$Date

#Adding Artwork Age column - when the artwork was created
data[, "ArtworkAge"] <- 2016 - data$YrBirth


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
#Blankspace graph
ggplot(data, aes(BlankSpace)) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 150) +  theme(panel.background = element_blank()) +
  labs(title = "How long after completion did it take to get in MoMA?", x = "Year", Y = "Number of Artworks")

ggplot(data, aes(BlankSpace, fill = factor(Gift), color = factor(Gift))) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 150) +
  labs(title = "How long after completion did it take to get in MoMA?", x = "Year", Y = "Number of Artworks")
  
ggplot(data, aes(BlankSpace, fill = factor(Department), color = factor(Department))) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 150) +
  labs(title = "How long for each department?", x = "Year", Y = "Number of Artworks") 
  

#Artwork age
ggplot(data, aes(ArtworkAge)) +
  geom_freqpoly(binwidth = 10, size = 1.5) + xlim(0, 220) +
  labs(title = "How old was the Artwork?", x = "Year", Y = "Number of Artworks")

#Artwork age by type of acquisition
ggplot(data, aes(ArtworkAge, fill = factor(Gift), color = factor(Gift))) +
  geom_freqpoly(binwidth = 10, size = 1.5) 


#before and after 1960
#tune the scale for axis
post60 <- data[data$YrAcquired>1960,]
ggplot(post60, aes(BlankSpace, color = "red")) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) + ylim(0, 20000)+ theme(panel.background = element_blank())
  labs(title = "Blank space for post 1960 artworks", x = "Year", Y = "Number of Artworks")

ggplot(post60, aes(BlankSpace, , fill = factor(Gift), color = factor(Gift))) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) + ylim(0, 10000)+
  labs(title = "Blank space for post 1960 artworks", x = "Year", Y = "Number of Artworks")

ggplot(post60, aes(BlankSpace, , fill = factor(Department), color = factor(Department))) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) + ylim(0, 10000)+
  labs(title = "Blank space post 1960 artworks", x = "Year", Y = "Number of Artworks")


pre1960 <- data[(data$YrAcquired <1960) & (data$YrAcquired > 1925) ,]

ggplot(pre1960, aes(BlankSpace, color = "FF9999")) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) + ylim(0, 20000)+ theme(panel.background = element_blank())
  labs(title = "Blank space for post 1960 artworks", x = "Year", Y = "Number of Artworks")

ggplot(pre1960, aes(BlankSpace, , fill = factor(Gift), color = factor(Gift))) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) + ylim(0, 10000)+
  labs(title = "Blank space for post 1960 artworks", x = "Year", Y = "Number of Artworks")

ggplot(pre1960, aes(BlankSpace, , fill = factor(Department), color = factor(Department))) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) + ylim(0, 10000)+
  labs(title = "Blank space post 1960 artworks", x = "Year", Y = "Number of Artworks")


#contemporary fact check
#data set has an issue!
contemp.data <- data[data$YrAcquired < data$YrDeath,]
contemp.comp <-contemp.data[complete.cases(contemp.data$BlankSpace),]
contemp.comp.painting <- contemp.comp[contemp.comp$Department == "Painting & Sculpture",]
count(contemp.comp$Department)

ggplot(contemp.comp, aes(BlankSpace, fill = factor(Gift), color = factor(Gift))) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) +
  labs(title = "Blank space for contemporary artworks", x = "Year", Y = "Number of Artworks") 

ggplot(contemp.comp, aes(BlankSpace, fill = factor(Department), color = factor(Department))) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) +
  labs(title = "Blank space for contemporary artworks", x = "Year", Y = "Number of Artworks") 


### 4. statistics----

mean(contemp.comp$ArtworkAge, na.rm=TRUE)
sd(contemp.comp$ArtworkAge, na.rm=TRUE)

mean(data$BlankSpace, na.rm=TRUE)
sd(data$BlankSpace, na.rm=TRUE)


### every thing together----

plot1<- ggplot(data, aes(BlankSpace)) +
  geom_freqpoly(binwidth = 5, size = 1.5)

plot2<- ggplot(pre1960, aes(BlankSpace)) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) + ylim(0, 20000)

plot1 + plot2
plot2 + ggplot(post60, aes(BlankSpace)) +
  geom_freqpoly(binwidth = 5, size = 1.5) + xlim(0, 100) + ylim(0, 20000)+
  labs(title = "Blank space for post 1960 artworks", x = "Year", Y = "Number of Artworks")

