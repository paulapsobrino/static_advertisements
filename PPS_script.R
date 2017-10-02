##Paula P Sobrino
##13 June 2017
##Stats static adverts with 2 fig ops: simple and complex

setwd("~/Desktop/madness")
static=read.csv("static_adverts.csv")

#let's transform RT by using log

LogRT = log(static$RT)
LogRT

#****************************
#1. RT ~ FIGURATIVE COMPLEXITY1 * NFC 

#Descriptive results
table1=aggregate(RT~Complexity1, data=static, mean) #this is the average RT by Fig language type

# mixed effects model
#first load the function
library(lme4)
#Now perform the model with Complexity 1
mymodel1 = lmer(LogRT~Complexity1*NFC+
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel1)
fixef(mymodel1) #output from the table - estimated fixed effects

#for p values

mymodel1.null=lmer (LogRT~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel1.null, mymodel1, test="Chisq")#comare the null model agaisnt the full model


##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel1)

#Now perform the model with Complexity 2
mymodel1 = lmer(LogRT~Complexity2*NFC+
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel1)
fixef(mymodel1) #output from the table - estimated fixed effects

#for p values

mymodel1.null=lmer (LogRT~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel1.null, mymodel1, test="Chisq")#comare the null model agaisnt the full model


##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel1)


#**************************
#2. APPRECIATION ~ FIG COMPLEXITY1

#Descriptive results
table2=aggregate(Appreciation~Complexity1, data=static, mean) #this is the average RT by Fig language type
table2
# mixed effects model

#Now perform the model with Complexity 1
mymodel2 = lmer(Appreciation~Complexity1 +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel2)

#for p values

mymodel2.null=lmer (Appreciation~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel2.null, mymodel2, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel2)

#Now perform the model with Complexity 2
mymodel2 = lmer(Appreciation~Complexity2 +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel2)

#for p values

mymodel2.null=lmer (Appreciation~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel2.null, mymodel2, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel2)

#*************************
#3. RT ~ APPRECIATION

#Descriptive results
table3=aggregate(RT~Appreciation, data=static, mean) #this is the average RT by Fig language type
table3
# mixed effects model

#Now perform the model
mymodel3 = lmer(LogRT)~Appreciation +
  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel3)

#for p values

mymodel3.null=lmer (log(RT)~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel3.null, mymodel3, test="Chisq")#comare the null model agaisnt the full model


##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel3)

cor(static$Appreciation, static$RT, method="spearman")
#*************************

#4. EFFECTIVENESS ~ FIGURATIVE COMPLEXITY1

#Descriptive results
table4=aggregate(Effectiveness~Complexity1, data=static, mean) #this is the average RT by Fig language type
table4
# mixed effects model

#Now perform the model with Complexity 1
mymodel4 = lmer(Effectiveness~Complexity1 +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel4)

#for p values

mymodel4.null=lmer (Effectiveness~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel4.null, mymodel4, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel4)

# mixed effects model

#Now perform the modelwith COmplexity 2
mymodel4 = lmer(Effectiveness~Complexity2*NFC +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel4)

#for p values

mymodel4.null=lmer (Effectiveness~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel4.null, mymodel4, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel4)

#*************************
#5.  RT ~ EFFECTIVENESS

#Descriptive results
table5=aggregate(RT~Effectiveness, data=static, mean) #this is the average RT by Fig language type
table5
# mixed effects model

#Now perform the model
mymodel5 = lmer(LogRT~Effectiveness +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel5)

#for p values

mymodel5.null=lmer (LogRT~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel5.null, mymodel5, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel5)

# check if these two variables are correlated:
cor(static$Effectiveness, static$LogRT, method="spearman")


#****************
#6.  EFFECTIVENESS~APPRECIATION

#Descriptive results
table6=aggregate(Effectiveness~Appreciation, data=static, mean) #this is the average RT by Fig language type
table6
# mixed effects model

#Now perform the model
mymodel6 = lmer(Effectiveness~Appreciation +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel6)

#for p values

mymodel6.null=lmer (RT~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel6.null, mymodel6, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel6)

# check if these two variables are correlated:
cor(static$Appreciation, static$Effectiveness)


#**************************************
#7.  RT~NATIONALITY

#Descriptive results
table7=aggregate(RT~Nationality_Participant, data=static, mean) #this is the average RT by Fig language type
table7
# mixed effects model

#Now perform the model
mymodel7 = lmer(LogRT~Nationality_Participant +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel7)

#for p values

mymodel7.null=lmer (LogRT~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel7.null, mymodel7, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel7)

#****************************************
#****************************************
#8.  RT~NATIONALITY*FIGURATIVE COMPLEXITY1

#Descriptive results
table8=aggregate(RT~Nationality_Participant*Complexity1, data=static, mean) #this is the average RT by Fig language type
table8
# mixed effects model

#Now perform the model with Complexity 1
mymodel8 = lmer(LogRT~Nationality_Participant*Complexity1*NFC +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel8)

#for p values

mymodel8.null=lmer (LogRT~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel8.null, mymodel8, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel8)

#Now perform the model with Complexity 2
mymodel8 = lmer(LogRT~Nationality_Participant*Complexity2*NFC +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel8)

#for p values

mymodel8.null=lmer (LogRT~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel8.null, mymodel8, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel8)

#**************************************

#**************************************
#9.  APPRECIATION~NATIONALITY

#Descriptive results
table9=aggregate(Appreciation~Nationality_Participant, data=static, mean) #this is the average RT by Fig language type
table9
# mixed effects model

#Now perform the model
mymodel9 = lmer(Appreciation~Nationality_Participant +
                  (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel9)

#for p values

mymodel9.null=lmer (Appreciation~1 +
                      (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel9.null, mymodel9, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel9)

#****************************************
#10.  APPRECIATION~NATIONALITY*FIGURATIVE COMPLEXITY1

#Descriptive results
table10=aggregate(Appreciation~Nationality_Participant*Complexity1, data=static, mean) #this is the average RT by Fig language type
table10
# mixed effects model

#Now perform the model with Complexity 1
mymodel10 = lmer(Appreciation~Nationality_Participant*Complexity1 +
                   (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel10)

#for p values

mymodel10.null=lmer (Appreciation~1 +
                       (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel10.null, mymodel10, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel10)

#Now perform the model with Complexity 2
mymodel10 = lmer(Appreciation~Nationality_Participant*Complexity2 +
                   (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel10)

#for p values

mymodel10.null=lmer (Appreciation~1 +
                       (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel10.null, mymodel10, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel10)

#****************************************
#11.  APPRECIATION~NATIONALITY*RT 

#Descriptive results
table11=aggregate(Appreciation~Nationality_Participant*LogRT, data=static, mean) #this is the average RT by Fig language type
table11
# mixed effects model

#Now perform the model
mymodel11 = lmer(Appreciation~Nationality_Participant*LogRT +
                   (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel11)


#for p values

mymodel11.null=lmer (Appreciation~1 +
                       (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel11.null, mymodel11, test="Chisq")#compare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel11)

#*******************
#12.  EFFECTIVENESS~NATIONALITY

#Descriptive results
table12=aggregate(Appreciation~Nationality_Participant, data=static, mean) #this is the average RT by Fig language type
table12
# mixed effects model

#Now perform the model
mymodel12 = lmer(Effectiveness~Nationality_Participant +
                   (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel12)

#for p values

mymodel12.null=lmer (Effectiveness~1 +
                       (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel12.null, mymodel12, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel12)

#****************************************
#13. EFFECTIVENESS~NATIONALITY*FIGURAITVE COMPLEXITY1

#Descriptive results
table13=aggregate(Effectiveness~Nationality_Participant*Complexity1*NFC, data=static, mean) #this is the average RT by Fig language type
table13
# mixed effects model

#Now perform the model with Complexity 1
mymodel13 = lmer(Effectiveness~Nationality_Participant*Complexity1*NFC +
                   (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel13)

#for p values

mymodel13.null=lmer (Effectiveness~1 +
                       (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel13.null, mymodel13, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel13)

#Now perform the model with Complexity 2
mymodel13 = lmer(Effectiveness~Nationality_Participant*Complexity2*NFC +
                   (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel13)

#for p values

mymodel13.null=lmer (Effectiveness~1 +
                       (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel13.null, mymodel13, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel13)

#*************************************
#14. EFFECTIVENESS~NATIONALITY*RT

#Descriptive results
table14=aggregate(Effectiveness~Nationality_Participant*LogRT, data=static, mean) #this is the average RT by Fig language type
table14
# mixed effects model

#Now perform the model
mymodel14 = lmer(Effectiveness~Nationality_Participant*LogRT +
                   (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel13)

#for p values

mymodel14.null=lmer (Effectiveness~1 +
                       (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel14.null, mymodel13, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel14)

#****************************************
#15.  EFFECITVENESS~NATIONALITY*APPRECIATION COMPLEXITY1

#Descriptive results
table15=aggregate(Effectiveness~Nationality_Participant*Appreciation, data=static, mean) #this is the average RT by Fig language type
table15
# mixed effects model

#Now perform the model
mymodel15 = lmer(Effectiveness~Nationality_Participant*Appreciation +
                   (1|Subject) + (1|Ad_Trial), data = static)
#Now have a look
summary(mymodel15)

#for p values

mymodel15.null=lmer (Effectiveness~1 +
                       (1|Subject) + (1|Ad_Trial), data = static) #create a null model
anova(mymodel15.null, mymodel14, test="Chisq")#comare the null model agaisnt the full model

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(mymodel15)


#*************
