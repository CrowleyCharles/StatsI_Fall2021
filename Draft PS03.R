incumbent <- read.csv(file.choose(), header = T)
View(incumbent)
str(incumbent)

#difflog is the difference in challenger vs incumbent spend

#Q1 Part1
lm(voteshare ~ difflog, data = incumbent)
lm1 <- lm(voteshare ~ difflog, data = incumbent)

#Part2
plot(incumbent$voteshare, incumbent$difflog,
     ylab="Incumbent Voter Share", 
     xlab="Incumbent/Challenger Spending", 
     main="Incumbent vs Challenger Spending")
     
abline(lm(incumbent$voteshare ~ incumbent$difflog), col = "red")

#Part3 
incumbent$Resids <- residuals(lm1)
incumbent$Resids


#Part4
coef(lm1) # I use this to get the slope of 0.041


#Q2 Part1
lm(presvote ~ difflog, data = incumbent)
lm2 <- lm(presvote ~ difflog, data = incumbent)

#Part2
plot(incumbent$presvote, incumbent$difflog,
     xlab="Incumbent/Challenger spending", 
     ylab="Presidental Vote Share", 
     main="Incumbent vs Challenger Spending")

abline(lm(incumbent$presvote ~ incumbent$difflog), col = "Orange")

#Part3 
incumbent$Resids2 <- residuals(lm2)
incumbent$Resids2

#Part4
coef(lm2) # I use this to get the slope of 0.0238

#Q3 Part1
lm(voteshare ~ presvote, data = incumbent)
lm3 <- lm(voteshare ~ presvote, data = incumbent)

#Part2
plot(incumbent$voteshare, incumbent$presvote,
     ylab="Incumbent Voter Share", 
     xlab="Presidental Vote Share", 
     main="Incumbent vs Challenger Spending")

abline(lm(incumbent$voteshare ~ incumbent$presvote), col = "Blue")

#Part3 
incumbent$Resids3 <- residuals(lm3)
incumbent$Resids3


#Part4
coef(lm3) # I use this to get the slope of 0.388

#Q4 Part 1

lm(incumbent$Resids ~ incumbent$Resids2, data = incumbent)

#Part2
plot(incumbent$Resids, incumbent$Resids2,
     ylab="Vote Share / Spending", 
     xlab="Presidental Vote Share / Spending", 
     main="Incumbent vs Challenger Spending")
abline(lm(incumbent$Resids ~ incumbent$Resids2), col = "Green")

#Part3 
coef(lm(incumbent$Resids ~ incumbent$Resids2))

#Q5 Part 1 
lm(voteshare ~ difflog * presvote, data = incumbent)

