### installing packages----
library(ggplot2)
library(plyr)
library(reshape)
library(stringr)

### 0.importing data set----
moma.data = read.csv("MoMA.csv")


### 1. one variable plots----
####Birth and Death year----
ggplot(moma.data, aes(YrBirth)) +
  geom_freqpoly(binwidth = 10) + 
  xlab("Birth Year") + ylab("Number of Artists")

ggplot(moma.data, aes(x=YrDeath, colour ="red")) +
  geom_freqpoly(binwidth = 1)  +
  xlab("Death Year") + ylab("Number of Artists") 


ggplot(moma.data, aes(YrBirth)) +
  geom_freqpoly(binwidth = 10) + 
  geom_freqpoly(aes(x=YrDeath, colour ="red"), binwidth = 10)

moma.data$Age <- ifelse(is.na(moma.data$YrDeath), 2015 - moma.data$YrBirth, moma.data$YrDeath - moma.data$YrBirth)
moma.data<-moma.data[!(moma.data$Age > 100),]

ggplot(moma.data, aes(Age)) +
  geom_histogram(binwidth = 1) + 
  xlab("Age") + ylab("Number of Artists")
 "issues with birth year"


###Nationality----
#picking nationality column and year of painting
str(moma.data$Nationality)
nationality = data.frame(count(moma.data$Nationality))
nationality = arrange(nationality,desc(freq),x)

nationality.20 = nationality[1:20,]

p <- ggplot(nationality.20, aes(x,freq, size=freq, label=x))
p <- p+geom_point(colour="red") 
p
###medium----
tech <- sapply(strsplit(as.character(moma.data$Medium), " on"), "[", 1)
canvas <- sapply(strsplit(as.character(moma.data$Medium), " on"), "[", 2)

len <- moma.data$LenCm1
wid <- moma.data$WidCm1

medium = cbind(tech, canvas, len, wid)
medium <- as.data.frame(medium)

length(unique(medium$canvas))
row.has.na <- apply(medium, 1, function(x){any(is.na(x))})
medium.filtered <- medium[!row.has.na,]
medium.filtered  = arrange(medium.filtered,desc(canvas))

ggplot(medium, aes(len,wid, size=canvas)) +geom_point() 



### 2. subsetting data
moma.photos <- moma.data[grep("photo",moma.data$Medium), ]

£
"french <- table(moma.data$Gift[moma.data$Nationality == 'French']
             moma.data$Department[moma.data$Nationality == 'French'])"


#make centrury
moma.data$Century <- 'xx'
moma.data$Century[grep('18..', moma.data$YrComp)] <- 'XIX'
moma.data$Century[grep('20..', moma.data$YrComp)] <- 'XXI'
moma.data$Century[is.na(moma.data$YrComp)] <- NA
moma.data$Century = as.factor(moma.data$Century)

threewaytable <- table(moma.data$Gift, moma.data$Department, moma.data$Century)


