#Reading in Data

install.packages("car")
library("car")
data(Prestige)
help(Prestige)

str(Prestige)
summary(Prestige)

#Q1 Part A
Professional <- ifelse(Prestige$type == 'prof', 1, 0)
Blue_Collar <- ifelse(Prestige$type == 'bc', 0, 0) #to be sure I created both WC and BC as 0 values
White_Collar <- ifelse(Prestige$type == 'wc', 0, 0) #as it did not cause problems further on as WC/BC aren't called further on

Prestige_1 <- data.frame(education = Prestige$education,
                     income = Prestige$income,
                     women = Prestige$women,
                     prestige = Prestige$prestige,
                     census = Prestige$census, 
                     Professional =Professional, 
                     Blue_Collar = Blue_Collar, 
                     White_Collar = White_Collar)

#I am unsure what to do with the NAs as I assume they will not alter the findings and won't be counted. 

#Q1 Part B
lm_mod1 = lm(prestige ~ income + Professional, data = Prestige_1) #used the + function, per the ones I saw in class

summary(lm_mod1)

lm_mod1.2 = lm(prestige ~ income * Professional, data = Prestige_1) #the question said use x times, I did not use this in the end  

summary(lm_mod1.2)

#Q1 Part C
#Prediction Equation = 

#Prestige = 3.062 + 1.371*(Income) + 2.276*(Professional)  #this is for the + model lm_mod1

#Q1 Part D

#The output, b1=1.371 implies that prestiege will be expected to increase by 1.371 units for an every additional unit of income. In addition, the hypothesis that the prestige rating is linearly related to the income level with other predictors being constant.

#Ho : b1 is equal to 0 (no linear relationship)
#Ha : b1 is not equal to 0 (significant linear relationship)

#So, for the test statistic t = 5.348 and p-value for the test statistic(t=5.348) is less than 6.12*10^(-7). Which means that the probability of getting test statistic 5.348 by chance under the assumption of b1 =0 is extremely rare. So we reject the null hypothesis b1=0 and it shows the evidence of a positive linear relationship between Income and prestige rating level.

#Q1 Part E
#The output, b1=2.267 implies that prestiege will be expected to increase by 2.267 units if professional. In addition, the hypothesis that the prestige rating is linearly related to professional with other predictors being constant.

#Ho : b1 is equal to 0 (no linear relationship)
#Ha : b1 is not equal to 0 (significant linear relationship)

#So, for the test statistic t = 9.817 and p-value for the test statistic(t=9.817) is less than 4.07*10^(-16). Which means that the probability of getting test statistic 9.817 by chance under the assumption of b1 =0 is extremely rare. So we reject the null hypothesis b1=0 and it shows the evidence of a positive linear relationship between profession and prestige rating level.

#Q1 Part F

#Prestige = 3.062 + 1.371*(Income) + 2.276*(Professional)
#Prestige = 3.062 + 1.371*(1,000) + 2.276*(0)
#Prestige = 3.062 + 1,371 + 0
#Prestige = 1,374.62

#Q1 Part G 

lm_mod1.3 = lm(prestige ~ income + White_Collar, data = Prestige_1)

summary(lm_mod1.3)


#Q2 Part A

qt(p=.05/2, df=29, lower.tail=FALSE) # 2 sided as it can be positive or negative  

2*pt(q=1.699127, df=29, lower.tail =FALSE)

#H0:B1 = 0
#vs
#Ha:B1 =(not equal) 0

#As the P Vlaue = 0.01 and is less than or equal to our Alpha it indicates strong evidence against the null hypothesis, so we reject it.


#Q2 Part B

qt(p=.05/2, df=75, lower.tail=FALSE) # 2 sided as it can be positive or negative  

2*pt(q=1.9921, df=75, lower.tail =FALSE)

#H0:B2 = 0
#vs
#Ha:B2 =(not equal) 0

#As our P value is 0.05000024 and is slightly above our alpha of 0.05 it indicates weak evidence against the null hypothesis, so we fail to reject it.


#Q2 Part C

qt(p=.05/2, df=106, lower.tail=FALSE)
2*pt(q=1.982597, df=106, lower.tail =FALSE)

####################################################
#Prestige = 3.062 + 1.371*(Income) + 2.276*(Professional)
#Prestige = 3.062 + 1.371*(0) + 2.276*(6000)
#Prestige = 3.062 + 0 + 8226
#Prestige = 8229.062
