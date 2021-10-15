# Import the data from my desktop I created 
summary(Q1data)
str(Q1data)

chisq <- chisq.test(Q1data)
chisq

#part 2

pvalue <- pchisq(3.791168, df = 2,lower.tail=FALSE)
pvalue

###########################
#Question 2#
###########################

#firstly import the dataset 

summary(indiadata)
str(indiadata)


regmat <- matrix(indiadata)
r<- cov(regmat)[3,6]/sd(regmat[,3]) * sd(regmat[,6])

n <- dim(regmat)[3]
t_stat <- (r*sqrt(n-2))/sqrt(1-r^2)

2*pt(t_stat, n-2, lower.tail = FALSE)
cor(regmat[,3], regmat [,6])
cor.test(regmat[,3], regmat[,6])

plot(indiadata$female~indiadata$water)#doesn't tell me much 
plot(indiadata$reserved~indiadata$water)

reg <- lm(reserved~water,indiadata)
reg


lm(formula = water ~ reserved, data = indiadata)

summary(reg)




summary(reg)$r.squared

cor(indiadata$reserved,indiadata$water)^2

summary(reg)$r.squared == cor(indiadata$reserved,indiadata$water)^2

plot(indiadata$reserved,indiadata$water)
abline(reg)

boxplot(indiadata$water, indiadata$reserved)  #check again doesnt look correct

install.packages("tidyverse")
install.packages("scales")

library(ggplot2)

ggplot(data = indiadata, aes(x = water, y = reserved)) +
  geom_point() 


#########################
# Question 3 
#########################

#import other data that includes sleep
install.packages("faraway")
library(faraway)

flydata <- read.csv("http://stat2.org/datasets/FruitFlies.csv")

summary(flydata)
str(flydata)

hist(flydata$Longevity)


#part 2 plot lifespan vs thorax 
flydata$longevity.days #just shows days of life listed 
plot(flydata$longevity.days, flydata$thorax.mm, 
     main = "Scatter Plot of lifespan vs thorax",
     xlab = "Throax in mm",
     ylab = "Longevity no. days")

lm(flydata$longevity.days ~ flydata$thorax.mm)

library(ggplot2)

abline(lm(flydata$longevity.days ~ flydata$thorax.mm), col = "red")

cor(flydata$Longevity, flydata$Thorax)


#########
#part2 
#########
install.packages("ggpubr")
library("ggpubr")
ggscatter(flydata, x = "Longevity", y = "Thorax", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "days", ylab = "mm")



#####
#part 3

ggplot(flydata, aes(x = Longevity, y = Thorax)) +
     geom_point() +
     stat_smooth()

model <- lm(Longevity ~ Thorax, data = flydata)
model

ggplot(flydata, aes(x = Longevity, y = Thorax)) +
  geom_point() +
  stat_smooth(method = lm)

summary(model)


confint(model)

sigma(model)*100/mean(flydata$Longevity) #Residual Standard error

sigma(model)*100/mean(flydata$Thorax)

predict(lm(Longevity ~ Thorax), data = flydata, interval = "confidence", level=0.95)

class(model)
new_df <- data.frame(Thorax = c(runif(0.8)))
predict(model, newdata = new_df)
?predict()

newDF1 <- model ;newDF1$Thorex <- 0.8
predict(lm(newDF1$Thorex ~ newDF1$Longevity), newdata=newDF1, se.fit = T) 
predict(lm(Thorax~Longevity), newdata=newDF1, interval = "prediction", level = 0.95)

ggplot(aes(newDF1$Thorax ~ Longevity), data = flydata) +
     geom_point() +
     geom_smooth(method = lm)

        