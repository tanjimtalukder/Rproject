#Tanjim Talukder
#Milestone Project 
# Due 4 OCT

# Set Directory 
setwd("~/Desktop")


#Read The file
fatalities<-read.csv('2012_workplace_Fatalities_by_State.csv')


#Recall the Data
fat1<-fatalities
fat2<-fatalities


# Deal with the missing data
fat2$Number.of.Injuries.Illnesses.2012[which(is.na(fat1$Number.of.Injuries.Illnesses.2012))] <-mean(fat2$Number.of.Injuries.Illnesses.2012,na.rm = TRUE)


library(e1071)


#1  column Number of Fatalities, 2012
# Mean
mean(fat2$Number.of.Fatalities..2012)
#standard deviations
sd(fat2$Number.of.Fatalities..2012)
median(fat2$Number.of.Fatalities..2012)
skewness(fat2$Number.of.Fatalities..2012)
kurtosis(fat2$Number.of.Fatalities..2012)
2*sqrt(6/length(fat2$Number.of.Fatalities..2012))
4*sqrt(6/length(fat2$Number.of.Fatalities..2012))



#2 column Penalties FY 2013 (Average$)
# Mean
mean(fat2$Penalties.FY.2013..Average...)
#standard deviations
sd(fat2$Penalties.FY.2013..Average...)
median(fat2$Penalties.FY.2013..Average...)
skewness(fat2$Penalties.FY.2013..Average...)
kurtosis(fat2$Penalties.FY.2013..Average...)
2*sqrt(6/length(fat2$Penalties.FY.2013..Average...))
4*sqrt(6/length(fat2$Penalties.FY.2013..Average...))



#3 Column Number.of.Injuries.Illnesses.2012
mean(fat2$Number.of.Injuries.Illnesses.2012)
#standard deviations
sd(fat2$Number.of.Injuries.Illnesses.2012)
median(fat2$Number.of.Injuries.Illnesses.2012)
skewness(fat2$Number.of.Injuries.Illnesses.2012)
kurtosis(fat2$Number.of.Injuries.Illnesses.2012)
2*sqrt(6/length(fat2$Number.of.Injuries.Illnesses.2012))
4*sqrt(6/length(fat2$Number.of.Injuries.Illnesses.2012))




#Box Plot each of the numeric data 
boxplot(fat2$Number.of.Fatalities..2012,horizontal=TRUE,main="Boxplot (Number of Fatalities in 2012)",xlab="Number of Fatalities")
boxplot(fat2$State.Rank..Fatalities.2012,horizontal=TRUE,xlab="State Rank" ,main="Boxplot (State Rank Fatalities in 2012)")
boxplot(fat2$Number.of.Injuries.Illnesses.2012,horizontal=TRUE,xlab="Number of Injuries",main="Boxplot (Number of Injuries Illnesses 2012)")
boxplot(fat2$Penalties.FY.2013..Average...,horizontal=TRUE,xlab="Pelanties in $",main="Boxplot (Penalties in 2013(Average $)")
boxplot((fat2$Inspectors..less.than.),horizontal=TRUE,main="Boxplot of number of inspector",xlab="Number of inspector(Less than)")


# Histogram each of the numeric column 
hist(fat2$Number.of.Fatalities..2012,main="hist (Number of Fatalities in 2012)",xlab = "Number of Fatalities")
hist(fat2$State.Rank..Fatalities.2012, main="hist (State Rank Fatalities in 2012)",xlab = "Rank")
hist(fat2$Number.of.Injuries.Illnesses.2012,main="hist (Number of Injuries Illnesses 2012)",xlab = "Number of Injuries")
hist(fat2$Penalties.FY.2013..Average...,main="hist (Penalties in 2013(Average $)",xlab="Dollar Amount $")
hist((fat2$Inspectors..less.than.),main="Hist of number of inspector",xlab="Number of inspector(Less than)")












