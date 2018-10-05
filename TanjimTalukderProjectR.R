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


#4 Colum Number of Inspector in each State 
mean(fat2$Inspectors..less.than.)
#standard deviations
sd(fat2$Inspectors..less.than.)
median(fat2$Inspectors..less.than.)
skewness(fat2$Inspectors..less.than.)
kurtosis(fat2$Number.of.Injuries.Illnesses.2012)
2*sqrt(6/length(fat2$Inspectors..less.than.))
4*sqrt(6/length(fat2$Inspectors..less.than.))



#5 Column Rank Number 
mean(fat2$State.Rank..Fatalities.2012)
#standard deviations
sd(fat2$State.Rank..Fatalities.2012)
median(fat2$State.Rank..Fatalities.2012)
skewness(fat2$State.Rank..Fatalities.2012)
kurtosis(fat2$State.Rank..Fatalities.2012)
2*sqrt(6/length(fat2$State.Rank..Fatalities.2012))
4*sqrt(6/length(fat2$State.Rank..Fatalities.2012))



#Box Plot each of the numeric data 
boxplot(fat2$Number.of.Fatalities..2012,horizontal=TRUE,main="Boxplot (Number of Fatalities in 2012)",xlab="Number of Fatalities")
boxplot(fat2$Number.of.Injuries.Illnesses.2012,horizontal=TRUE,xlab="Number of Injuries",main="Boxplot (Number of Injuries Illnesses 2012)")
boxplot(fat2$Penalties.FY.2013..Average...,horizontal=TRUE,xlab="Pelanties in $",main="Boxplot (Penalties in 2013(Average $)")
boxplot(fat2$State.Rank..Fatalities.2012,horizontal=TRUE,xlab="State Rank" ,main="Boxplot (State Rank Fatalities in 2012)")
boxplot(fat2$Inspectors..less.than.,horizontal=TRUE,main="BoxPlot of Inspector",xlab="Number of Inspector")
boxplot(fat2$State.Rank..Fatalities.2012,horizontal=TRUE,main="Boxplot of State Rank")


# Histogram each of the numeric column 
hist(fat2$Number.of.Fatalities..2012,main="hist (Number of Fatalities in 2012)",xlab = "Number of Fatalities")
hist(fat2$State.Rank..Fatalities.2012, main="hist (State Rank Fatalities in 2012)",xlab = "Rank")
hist(fat2$Number.of.Injuries.Illnesses.2012,main="hist (Number of Injuries Illnesses 2012)",xlab = "Number of Injuries")
hist(fat2$Penalties.FY.2013..Average...,main="hist (Penalties in 2013(Average $)",xlab="Dollar Amount $")
hist(fat2$Inspectors..less.than.,main="Histo of Inspector",xlab="Number of Inspector")
hist(fat2$State.Rank..Fatalities.2012,main="Histo of State Rank", xlab = "Rank Rumber")

# Scatter Plot each of the numeric column 
plot(fat2$Number.of.Fatalities..2012,ylab="Number of Fatalities",main="ScatterPlot (Number of Fatalities in 2012)")
plot(fat2$State.Rank..Fatalities.2012, main="ScatterPlot (State Rank Fatalities in 2012)")
plot(fat2$Number.of.Injuries.Illnesses.2012,ylab="Number of Injuries",main="ScatterPlot (Number of Injuries Illnesses 2012)")
plot(fat2$Penalties.FY.2013..Average...,main="ScatterPlot (Penalties in 2013(Average $)",ylab="Dollar Amount $")
plot(fat2$Inspectors..less.than.,main="ScatterPlot of Inspector",ylab="Number of Inspector")


# Box  Plot each of the numeric column 
barplot(fat2$Number.of.Fatalities..2012,,ylab="Number of fatalities",main="BarPlot (Number of Fatalities in 2012)")
barplot(fat2$State.Rank..Fatalities.2012, main="BarPlot (State Rank Fatalities in 2012)")
barplot(fat2$Number.of.Injuries.Illnesses.2012,main="BarPlot (Number of Injuries Illnesses 2012)")
barplot(fat2$Penalties.FY.2013..Average...,main="BarPlot (Penalties in 2013(Average $)",ylab="Dollar Amount $")
barplot(fat2$Inspectors..less.than.,main="Barplot of Inspector",ylab="Number of Inspector")













