#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("C:/Users/crowl/Documents/Trinity/ASDS Course/POP77003_Applied_Statistical_Analysis_1/StatsI_Fall2021-main/problemSets/PS01")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

z90 <- qnorm((1 - .90)/2, lower.tail = FALSE)
n <- length(y)
sample_mean <- mean(y, na.rm = TRUE)
sample_sd <- sd(y, na.rm = TRUE)
lower_90 <- sample_mean - (z90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (z90 * (sample_sd/sqrt(n)))
confint90 <- c(lower_90, upper_90) 

confint90 #relaised that I should use t after doing this

#t distribution 

m <- mean(y)
s <- sd(y)
n <- length(y)
error <- qt(0.95, df = n-1) * s/sqrt(n)
m + error
m - error

#############################
#Part 2#

#H0:>100

v <- sd(y)^2 #is not needed
z <- 1.96 #is not needed 

t.test(y, mu=100, alternative="greater", conf.level= 0.95) #one sided, this is the one we use 
t.test(y, mu=100, alternative="two.sided", conf.level= 0.95) #two sided
#we fail to reject the H0: >100

# if p-value ????? reject H0
# if p-value >?? fail to reject H0
# p-value = 0.7215, and our ?? is 0.05, this therefore means we are
# unable to reject the null hypothesis as p value > ??. 


#####################
# Problem 2
#####################
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
str(expenditure)
pairs(~expenditure$Y+expenditure$X1+expenditure$X2+expenditure$X3,data=expenditure,
      main="States Scatterplot Matrix")
plot(expenditure) #this didn't work for me 


#part 2 
hist(expenditure$Y) # don't use 
hist(expenditure$Y, expenditure$Region) #don't use 
plot(expenditure$Y,expenditure$Region, main="shelter/housing spending per region")
hist(expenditure$Y,expenditure$Region, main="shelter/housing spending per region")#error message doesn't work

ggplot(expenditure$Y, expenditure$Region) #tidyverse won't load so can't run it
RegionNE <- subset(expenditure, expenditure$Region == "1")
str(RegionNE)
plot(expenditure$Y,expenditure$RegionNE, main="shelter/housing spending Region 1")

RegionNC <- subset(expenditure, expenditure$Region == "2")
str(RegionNC)
plot(expenditure$Y,expenditure$RegionNC, main="shelter/housing spending Region 2")

RegionS <- subset(expenditure, expenditure$Region == "3")
str(RegionS)
plot(expenditure$Y,expenditure$RegionS, main="shelter/housing spending Region 3")

RegionW <- subset(expenditure, expenditure$Region == "4")
str(RegionW)
plot(expenditure$Y,expenditure$RegionW, main="shelter/housing spending Region 4")

#this produced no diffrennce in the 4 results when trying to make 4 distinct subsets of Region

plot(expenditure$Y,expenditure$Region,xlab="Spending Per Capita on shelter",ylab="Region",pch=21)
title("SP of Regions Spending on Shelter") 
aggregate(expenditure$Y, by = list(expenditure$Region), FUN = mean)

#part 3
lines(expenditure$Y, fitted(expenditure), col="red")
abline(lm(expenditure$Y ~ expenditure$X1), col = "red")
abline(lm(expenditure$X1  ~ expenditure$Y), col = "red")


# New variable
plot(expenditure$X1, expenditure$Y | expenditure$Region, 
     xlab="spending per capita on shelter", ylab= "per capita income", main="SP of State Income/Spending on Shelter/region"
     ,col=c("red", "yellow", "pink", "orange", "green"), pch=19)

plot(expenditure$Y,expenditure$X1,expenditure$RegionNC,expenditure$RegionNE,expenditure$RegionS,expenditure$RegionW)#didnt work

plot(expenditure$Y,expenditure$X1,expenditure$Region)#didnt work


