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



# Histogram each of the numeric column 
hist(fat2$Number.of.Fatalities..2012,main="hist (Number of Fatalities in 2012)",xlab = "Number of Fatalities")
hist(fat2$State.Rank..Fatalities.2012, main="hist (State Rank Fatalities in 2012)",xlab = "Rank")
hist(fat2$Number.of.Injuries.Illnesses.2012,main="hist (Number of Injuries Illnesses 2012)",xlab = "Number of Injuries")
hist(fat2$Penalties.FY.2013..Average...,main="hist (Penalties in 2013(Average $)",xlab="Dollar Amount $")
hist(fat2$Inspectors..less.than.,main="Histo of Inspector",xlab="Number of Inspector")

# Scatter Plot each of the numeric column 
plot(fat2$Number.of.Fatalities..2012,ylab="Number of Fatalities",main="ScatterPlot (Number of Fatalities in 2012)")
plot(fat2$State.Rank..Fatalities.2012,ylab="Rank", main="ScatterPlot (State Rank Fatalities in 2012)")
plot(fat2$Number.of.Injuries.Illnesses.2012,ylab="Number of Injuries",main="ScatterPlot (Number of Injuries Illnesses 2012)")
plot(fat2$Penalties.FY.2013..Average...,main="ScatterPlot (Penalties in 2013(Average $)",ylab="Dollar Amount $")
plot(fat2$Inspectors..less.than.,main="ScatterPlot of Inspector",ylab="Number of Inspector")


# Box  Plot each of the numeric column 
barplot(fat2$Number.of.Fatalities..2012,,ylab="Number of fatalities",main="BarPlot (Number of Fatalities in 2012)")
barplot(fat2$State.Rank..Fatalities.2012,ylab="Rank", main="BarPlot (State Rank Fatalities in 2012)")
barplot(fat2$Number.of.Injuries.Illnesses.2012,main="BarPlot (Number of Injuries Illnesses 2012)")
barplot(fat2$Penalties.FY.2013..Average...,main="BarPlot (Penalties in 2013(Average $)",ylab="Dollar Amount $")
barplot(fat2$Inspectors..less.than.,main="Barplot of Inspector",ylab="Number of Inspector")


#Plotting my nomial or oridianl data 





fat3<-table(fat2$State.or.Federal.Program)
barplot(fat3[order(fat3,decreasing = T)],
        main="Graph of Federal or States Program",
        ylab = "Number of States",col =c("beige","bisque4"),
        legend=c("Federal","State"))


fat4<-table(fat2$State.Size)
barplot(fat4[order(fat4,decreasing = T)],
        main="Graph of Different States", 
        ylab = "Number of States",
        col =c("green","red","purple"),
        legend=c("Large","Mid","Small"))

#Pareto Graph of Qualitative data 

library("qcc")
pareto.chart(fat3,main="Pareto Chart of Federal or State Program")

pareto.chart(fat4, main="Pareto Chart of Different Size of States")





#Milestone Nov 1
#Numeric column 1
# confidence intervals for column Number of Fatalities in 2012 

mu_hat1 = mean(fat2$Number.of.Fatalities..2012)
sigma_hat1=sd(fat2$Number.of.Fatalities..2012)
n1=nrow(fat2)
alpha1=0.05
critical_values1=qt(alpha1/2,n1-1)
U1 = mu_hat1 + critical_values1*sigma_hat1/sqrt(n1)
U1

L1 = mu_hat1 - critical_values1*sigma_hat1/sqrt(n1)
L1

paste0("The Confidence Interval for Number of ", 
       "Fatalities in 2012 is " ,U1, " and ", L1)


#Numeric column 2
# confidence intervals for column Number of Injuries Illnesses 2012

mu_hat2 = mean(fat2$Number.of.Injuries.Illnesses.2012)
sigma_hat2=sd(fat2$Number.of.Injuries.Illnesses.2012)
n2=nrow(fat2)
alpha2=0.05
critical_values2=qt(alpha2/2,n2-1)
U2 = mu_hat2 + critical_values2*sigma_hat2/sqrt(n2)
U2

L2 = mu_hat2 - critical_values2*sigma_hat2/sqrt(n2)
L2

paste0("The Confidence Interval for Number of ", 
       "Injuries Illnesses 2012 is " ,U2, " and ", L2)

#Numeric column 3
# confidence intervals for column Penalties.FY.2013
mu_hat3 = mean(fat2$Penalties.FY.2013..Average...)
sigma_hat3=sd(fat2$Penalties.FY.2013..Average...)
n3=nrow(fat2)
alpha3=0.05
critical_values3=qt(alpha3/2,n3-1)
U3 = mu_hat3 + critical_values3*sigma_hat3/sqrt(n3)
U3

L3 = mu_hat3 - critical_values3*sigma_hat3/sqrt(n3)
L3

paste0("The Confidence Interval for amount of ", 
       "Penalties is " ,U3, " and ", L3)



# MileStone 7, Nov 15

#Hypothesis Test 1 

# Mean
mean_Fatalites<-mean(fat2$Number.of.Fatalities..2012)
#standard deviations
sd_Fatalites<-sd(fat2$Number.of.Fatalities..2012)
mean_Fatalites
sd_Fatalites

test_Fatalities<- t.test(fat2$Number.of.Fatalities..2012,
                         alternative = "two.sided",mu=92,conf.level = 0.95)

test_Fatalities


#Hypothesis Test 2

State_Program<-fat2$State.or.Federal.Program
federal<-State_Program == "Federal"
sum(federal)
prop.test(sum(federal),nrow(fat2),p=0.60,conf.level = 0.95)


#Hypothesis Test 3

only_small_State<-subset(fat2,fat2$State.Size=="Small")

only_Large_State<-subset(fat2,fat2$State.Size=="Large")

t.test(only_Large_State$Inspectors..less.than.,
       only_small_State$Inspectors..less.than.,conf.level = 0.95)

#Milestone 8

#Plotting injuries vs fatalities
plot(fat2$Number.of.Injuries.Illnesses.2012~fat2$Number.of.Fatalities..2012,
     main="Linear Regression line of fatalities vs Injured",
     ylab="Number of Injuries",xlab="Number of Fatalities")

cor(fat2$Number.of.Injuries.Illnesses.2012,fat2$Number.of.Fatalities..2012)
fat2.lm<-lm(fat2$Number.of.Injuries.Illnesses.2012~fat2$Number.of.Fatalities..2012)
summary(fat2.lm)
abline(fat2.lm,col="red",lwd=5)
confint((fat2.lm))
anova(fat2.lm)
par(mfrow=c(2,2))
plot(fat2.lm)

# Extra 3 Analysis 

#Linear Discriminant Analysis 










