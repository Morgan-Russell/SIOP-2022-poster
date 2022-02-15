
#PASTED CURRENT VERSION OF 'MORGAN JOB TITLE HOURS WORKED'
#KEEP UP TO DATE
# Morgan factorizes the "What is the title of the job you were thinking about while responding to this survey?" column
# As well as the "How many hours do you typically work per week in this job?" column

# An object lesson in why you should NEVER HAVE FREE TEXT ENTRY IN YOUR SURVEY


# Packages
library(tidyverse)
library(DT)
library(labourR)
library(apaTables)


temp <- read.csv("qualtrics_pilot_data.csv", header=FALSE, na.strings="")

x <- temp[2,]
data <- temp[-c(1:3),]
colnames(data) <- x

num <- nrow(data)

## getting conditions into one large file below - 12/9/20

data$Cond1 <- rowSums(is.na(data[18:53]))
data$Cond2 <- rowSums(is.na(data[54:89]))
data$Cond3 <- rowSums(is.na(data[90:125]))
data$Cond4 <- rowSums(is.na(data[126:161]))

data$Condition[data$Cond1 < 36] <- 1
data$Condition[data$Cond2 < 36] <- 2
data$Condition[data$Cond3 < 36] <- 3
data$Condition[data$Cond4 < 36] <- 4

cond1 <- data[ which(data$Condition==1), ]
cond2 <- data[ which(data$Condition==2), ]
cond3 <- data[ which(data$Condition==3), ]
cond4 <- data[ which(data$Condition==4), ]

cond1.red <- cond1[,c(6, 18:53, 162:165, 171)]  ## using Cond1 ordering
cond2.red <- cond2[,c(6, 62:65, 70:73, 82:85, 58:61, 74:77, 86:89, 66:69, 78:81, 54:57, 162:165, 171)]
cond3.red <- cond3[,c(6, 94:97, 106:109, 118:121, 98:101, 110:113, 122:125, 102:105, 114:117, 90:93, 162:165, 171)]
cond4.red <- cond4[,c(6, 138:161, 130:137, 126:129, 162:165, 171)]        ## 171 versus 172 because testing script has extra "hours" variable

names(cond1.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond1.red))      ## Getting rid of condition markers so rbind will work
names(cond2.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond2.red))  
names(cond3.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond3.red))  
names(cond4.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond4.red))  

together <- rbind(cond1.red, cond2.red, cond3.red, cond4.red)        ## we'll be using this object for analyses

num_valid <- nrow(together)

i <- c(1:37)                                          ## Changing item responses to numerics
together[ , i] <- apply(together[ , i], 2,            # Specify own function within apply
                        function(x) as.numeric(as.character(x)))


##########################################################
##########################################################
##########################################################
############# RECODES

together$`Most days, I feel happiest when the workday is soon to be complete.` <- 7 - together$`Most days, I feel happiest when the workday is soon to be complete.`
together$`This job drains my energy.` <- 7 - together$`This job drains my energy.`

## BEHAVIORAL (NONE):

## COGNITIVE: 

together$`Thinking about work saps my energy.` <- 7 - together$`Thinking about work saps my energy.`
together$`I often think about finding another job.` <- 7 - together$`I often think about finding another job.`


library(lavaan)
library(sem)
library(semPlot)
library(dplyr)


CFAdata<-together[,2:37]

CFAdata<-CFAdata%>%rename(
  Item_1=`Iâ€™m able to concentrate on my work without distractions.`,
  Item_2=`I have a hard time detaching mentally from my work.`,
  Item_3=`Time passes quickly while Iâ€™m working.`,
  Item_4=`I find it difficult to mentally disconnect from work.`,
  Item_5=`I enjoy thinking about work even when Iâ€™m not at work.`,
  Item_6=`Most days, I feel happiest when the workday is soon to be complete.`,
  Item_7=`I am happiest when I am immersed in a project.`,
  Item_8=`I love starting my workday.`,
  Item_9=`I devote more time than is expected of me.`,
  Item_10=`I have to be reminded to take breaks while Iâ€™m at work.`,
  Item_11=`I never miss a work deadline.`,
  Item_12=`I never allow distractions to interfere with my work.`,
  Item_13=`I devote my full attention to my work tasks throughout the day.`,
  Item_14=`Thinking about work saps my energy.`,
  Item_15=`I would rather direct my focus toward a work task than a personal task.`,
  Item_16=`Iâ€™m able to maintain good levels of energy throughout the workday.`,
  Item_17=`I enjoy spending time completing my job tasks.`,
  Item_18=`Most days I feel enthusiastic about starting my work day.`,
  Item_19=`I feel motivated to go beyond what is asked of me.`,
  Item_20=`This job drains my energy.`,
  Item_21=`When work is slow I find ways to be productive.`,
  Item_22=`I express enthusiasm for my job while at work.`,
  Item_23=`I try my best to perform well at work.`,
  Item_24=`If I notice my energy level is low, I take corrective steps to re-energize.`,
  Item_25=`I plan my future with this company.`,
  Item_26=`I believe this company cares about my career goals.`,
  Item_27=`I often think about finding another job.`,
  Item_28=`This organization challenges me to work at my full potential.`,
  Item_29=`I am proud to be a member of this organization.`,
  Item_30=`I feel supported by my supervisor when I fail at a task.`,
  Item_31=`I feel proud of my accomplishments within this organization.`,
  Item_32=`My job makes me feel like Iâ€™m part of something meaningful.`,
  Item_33=`I make valued contributions to the organization.`,
  Item_34=`I embrace challenging situations at work.`,
  Item_35=`I speak positively about this organization to others.`,
  Item_36=`This organization provides the resources necessary for me to successfully perform my job.`
)

modified2 <-'
Absorption = ~Item_1 + Item_3  + Item_5  + Item_8  + Item_10 + Item_11
Vigor      = ~Item_14 + Item_16 + Item_17 + Item_19 + Item_21 + Item_22
Dedication = ~ Item_26 + Item_28 + Item_31 + Item_32 + Item_34 + Item_35
Cognitive  = ~Item_1  + Item_3  + Item_14 + Item_16 + Item_26 + Item_28
Affective  = ~Item_5 +  Item_8  + Item_17 + Item_19 + Item_31 + Item_32
Behavioral = ~Item_10 + Item_11 + Item_21 + Item_22 + Item_34 + Item_35
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor      ~~ 0*Affective
Vigor      ~~ 0*Behavioral
Vigor      ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
'

Fit.mod2 <- lavaan::cfa(modified2, data = CFAdata, missing = "ML", estimator = 'MLR')

# semPlot::semPaths(Fit.mod2, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3",
#                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, pastel=FALSE)

semPlot::semPaths(Fit.mod2, bifactor = c("Cognitive", "Affective", "Behavioral"), style="lisrel", "std", layout = "tree3", sizeLat=10, rotation = 2, sizeMan=4.5,edge.label.cex=0.75, edge.color="black", asize=2)



fit1.1 <- as.data.frame(fitMeasures(Fit1.1))
fit1.1$`fitMeasures(Fit1.1)` <- round(fit1.1$`fitMeasures(Fit1.1)`, 2)

fit1.2 <- as.data.frame(fitMeasures(Fit1.2))
fit1.2$`fitMeasures(Fit1.2)` <- round(fit1.2$`fitMeasures(Fit1.2)`, 2)

fit.mod2 <- as.data.frame(fitMeasures(Fit.mod2))
fit.mod2$`fitMeasures(Fit.mod2)` <- round(fit.mod2$`fitMeasures(Fit.mod2)`, 2)

indices_att <- as.data.frame(t(fit1.1[c(3,4,23,29,9,10,19),]))        ## Chi-sq, df, RMSEA, SRMR, CFI, TLI, AIC
indices_sub <- as.data.frame(t(fit1.2[c(3,4,23,29,9,10,19),]))
indices_bi <- as.data.frame(t(fit.mod2[c(3,4,44,58,17,18,38),]))

fits <- rbind(indices_att, indices_sub, indices_bi)
names(fits) <- c("Chi Square", "df", "RMSEA", "SRMR", "CFI", "TLI", "AIC")
model <- c("Attitudinal", "Substantive", "bifactor")
fits2 <- cbind(model, fits)

apa_table(fits2, caption = "Summary fit statistics (18 item final proposed scale definitions)")






mod <- data.frame(read.csv('att.csv', row.names = NULL)[1:7, 2:5])
mod2 <- mod[,-2]
mod2$Notes <- c("Candidate for deletion due to construct duplication (both are Absorption/Cognition indicators)", "", "Candidate for retention due to substantive construct association (Dedication)", "", "", "", "")

colnames(mod2) <- c('Element 1', 'Element 2', "Modification Index", "Notes") #Δχ2 isn't showing up properly in knit output. Fix.

#mod <- apa_table(mod2, caption = "Attitudinal structure modification indices (36 item analysis)")
#mod
