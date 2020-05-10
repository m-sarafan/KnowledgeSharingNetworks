###-----LIBRARIES-----###
library(foreign)
library(pastecs)
library(psych)
library(car)
library(lmtest)
library(Hmisc)
library(stargazer)
library(lavaan)
library(multcomp)
library(lsmeans)
library(emmeans)
library(MVN)

###-----DIRECTORIES-----###
workdirectory <- setwd("~/Google Drive/Postdoc/Research/Knowledge sharing/Experiments/All experiments combined (Prolific)")

#--------Experminet 1 (Governance, Climate)--------#

###-----DATA IMPORT/EXPORT-----###
Exp1data <- read.spss("GovernanceClimate.sav", use.value.labels = T, to.data.frame = T)
head(Exp1data)

#Making factor variables numeric
for(i in 1:ncol(Exp1data)) {
     if(is.factor(Exp1data[,i]) == TRUE) {
          Exp1data[paste('N', colnames(Exp1data[i]), sep="")] <- sapply(Exp1data[,i], as.numeric)
     }
}


###-----Attention check-----###

subset(Exp1data, Exp1data$NQ9.3_5 != 5)
subset(Exp1data, Exp1data$NQ10.2_7 != 3)
subset(Exp1data, Exp1data$NQ11.7_5 != 2)
subset(Exp1data, Exp1data$NQ12.7_3 != 5)
subset(Exp1data, Exp1data$NQ12.7_9 != 2)
subset(Exp1data, Exp1data$NQ11.6_5 != 6)

Exp1data2 <- subset(Exp1data, Exp1data$NQ9.3_5 == 5 & Exp1data$NQ10.2_7 == 3 & Exp1data$NQ11.7_5 == 2 &  Exp1data$NQ12.7_3 == 5 & Exp1data$NQ12.7_9 == 2 & Exp1data$NQ11.6_5 == 6)

###-----Manipulations-----### 
###  (P = PMO governance/0; S = Shared governance/1) and (N = No climate/0; M = Mastery climate/2; P = Performance climate/1)

for(i in 1:ncol(Exp1data2)){
     Exp1data2[i][is.na(Exp1data2[i])] <- 0
}

Exp1data2$Governance <- ifelse(Exp1data2$Scenario_NS ==1 | Exp1data2$Scenario_MS ==1  | Exp1data2$Scenario_PS ==1 , 1, 0)
Exp1data2$Climate <- ifelse(Exp1data2$Scenario_PS ==1 | Exp1data2$Scenario_PP ==1 , 1, ifelse(Exp1data2$Scenario_MS ==1 | Exp1data2$Scenario_MP ==1 , 2, 0))

Exp1data2$PerformanceClimate <- ifelse(Exp1data2$Scenario_PS ==1 | Exp1data2$Scenario_PP ==1 , 1, 0)
Exp1data2$MasteryClimate <- ifelse(Exp1data2$Scenario_MS ==1 | Exp1data2$Scenario_MP ==1, 1, 0)

###-----Manipulation checks-----###
###
# shared = 1, NAO = 0
Exp1data2$NQ11.3_recode <- 6 - Exp1data2$NQ11.3
Governancevariable <- Exp1data2[,c("NQ11.4", "NQ11.3_recode")] 
Exp1data2$GovernanceManipulation <- apply(Governancevariable, 1, FUN= "mean", na.rm= TRUE)

PerformanceCvariable <- Exp1data2[,c("NQ11.6_1", "NQ11.6_2", "NQ11.6_3", "NQ11.6_4", "NQ11.6_6", "NQ11.6_8", "NQ11.7_3", "NQ11.7_8")] 
Exp1data2$PerformanceC <- apply(PerformanceCvariable, 1, FUN= "mean", na.rm= TRUE)

MasteryCvariable <- Exp1data2[,c("NQ11.6_7", "NQ11.7_1", "NQ11.7_2", "NQ11.7_4", "NQ11.7_6", "NQ11.7_7")] 
Exp1data2$MasteryC <- apply(MasteryCvariable, 1, FUN= "mean", na.rm= TRUE)

###-----Variables-----###
#Section 1: Knowledge (tacit sharing, explicit sharing, rationalised hiding, evasive hiding)

Exp1data2$NQ9.2_1_recode <- 8 - Exp1data2$NQ9.2_1
KS_Motivationvariable <- Exp1data2[,c("NQ9.2_1_recode","NQ9.2_2","NQ9.2_3","NQ9.2_4")] 
Exp1data2$KS_Motivation <- apply(KS_Motivationvariable, 1, FUN= "mean", na.rm= TRUE)

KH_Dumbvariable <- Exp1data2[,c("NQ9.3_1","NQ9.3_3","NQ9.4_1","NQ9.4_3")]
Exp1data2$KH_Dumb <- apply(KH_Dumbvariable, 1, FUN= "mean", na.rm= TRUE)

KH_Evasivevariable <- Exp1data2[,c("NQ9.3_4","NQ9.3_6","NQ9.4_2","NQ9.4_5")]
Exp1data2$KH_Evasive <- apply(KH_Evasivevariable, 1, FUN= "mean", na.rm= TRUE)

KH_Rationalisedvariable <- Exp1data2[,c("NQ9.3_2","NQ9.4_4", "NQ9.4_6")]
Exp1data2$KH_Rationalised <- apply(KH_Rationalisedvariable, 1, FUN= "mean", na.rm= TRUE)

KHvariable <- Exp1data2[,c("KH_Rationalised","KH_Evasive", "KH_Dumb")] 
Exp1data2$KH <- apply(KHvariable, 1, FUN= "mean", na.rm= TRUE)

#Section 2: Mediation (need-support, need-satisfaction, realism) and feeling

NSupp_Autvariable <- Exp1data2[,c("NQ10.2_2", "NQ10.2_8","NQ10.2_10","NQ10.2_13")] 
Exp1data2$NSupp_Aut <- apply(NSupp_Autvariable, 1, FUN= "mean", na.rm= TRUE)

NSupp_Comvariable <- Exp1data2[,c("NQ10.2_4","NQ10.2_5", "NQ10.2_6", "NQ10.2_11")] 
Exp1data2$NSupp_Com <- apply(NSupp_Comvariable, 1, FUN= "mean", na.rm= TRUE)

NSupp_Relvariable <- Exp1data2[,c("NQ10.2_1","NQ10.2_3", "NQ10.2_9", "NQ10.2_12")] 
Exp1data2$NSupp_Rel <- apply(NSupp_Relvariable, 1, FUN= "mean", na.rm= TRUE)

NSuppvariable <- Exp1data2[,c("NSupp_Aut","NSupp_Com", "NSupp_Rel")] 
Exp1data2$NSupp <- apply(NSuppvariable, 1, FUN= "mean", na.rm= TRUE)


Exp1data2$NQ12.3_recode <- 8 - Exp1data2$NQ12.3
Feelingvariable <- Exp1data2[,c("NQ12.2","NQ12.3_recode")] 
Exp1data2$Feeling <- apply(Feelingvariable, 1, FUN= "mean", na.rm= TRUE)

#Section 3: Individual level factors (personality and orientation)

Exp1data2$NQ12.6_1_recode <- 8 - Exp1data2$NQ12.6_1
Exp1data2$NQ12.6_3_recode <- 8 - Exp1data2$NQ12.6_3
Exp1data2$NQ12.6_7_recode <- 8 - Exp1data2$NQ12.6_7
Opennessvariable <- Exp1data2[,c("NQ12.6_1_recode","NQ12.6_3_recode", "NQ12.6_7_recode", "NQ12.6_8")] 
Exp1data2$Openness <- apply(Opennessvariable, 1, FUN= "mean", na.rm= TRUE)

Exp1data2$NQ12.6_4_recode <- 8 - Exp1data2$NQ12.6_4
Exp1data2$NQ12.6_6_recode <- 8 - Exp1data2$NQ12.6_6 
Conscienvariable <- Exp1data2[,c("NQ12.6_2","NQ12.6_4_recode", "NQ12.6_6_recode", "NQ12.6_5")] 
Exp1data2$Conscien <- apply(Conscienvariable, 1, FUN= "mean", na.rm= TRUE)

Mastery_Ovariable <- Exp1data2[,c("NQ12.5_1","NQ12.5_3", "NQ12.5_4", "NQ12.5_5", "NQ12.7_2", "NQ12.7_5", "NQ12.7_6", "NQ12.7_11")] 
Exp1data2$Mastery_O <- apply(Mastery_Ovariable, 1, FUN= "mean", na.rm= TRUE)

Performance_Ovariable <- Exp1data2[,c("NQ12.5_2","NQ12.5_6", "NQ12.5_7", "NQ12.7_1", "NQ12.7_4", "NQ12.7_7", "NQ12.7_8", "NQ12.7_10")] 
Exp1data2$Performance_O <- apply(Performance_Ovariable, 1, FUN= "mean", na.rm= TRUE)

#Section 4: Control (age, gender, education, culture, work experience, realism) 

Exp1data2$Gender <- Exp1data2$Q12.9

Exp1data2$Age <- Exp1data2$NQ12.8_1
Exp1data2$Age <- as.numeric(Exp1data$Age)
Exp1data2$Age <- Exp1data2$Age + 17

Exp1data2$WE <- Exp1data2$NQ12.11_1
Exp1data2$WE <- as.numeric(Exp1data2$WE)
Exp1data2$WE <- Exp1data2$WE - 1

Exp1data2$WE_PM <- Exp1data2$NQ12.12
Exp1data2$WE_IOR <- Exp1data2$NQ12.13

Exp1data2$Education <- Exp1data2$NQ12.10

Exp1data2$Nationality <- Exp1data2$Q12.15

Exp1data2$Residence <- Exp1data2$Q12.14

Realismvariable <- Exp1data2[,c("NQ12.1_1","NQ12.1_2", "NQ12.1_3")] 
Exp1data2$Realism <- apply(Realismvariable, 1, FUN= "mean", na.rm= TRUE)

#Z-standardizing and M-centring all variables
for(i in 1:ncol(Exp1data2)) {
     if(is.numeric(Exp1data2[,i]) == TRUE) {
          Exp1data2[paste('Z', colnames(Exp1data2[i]), sep="")] <- scale(Exp1data2[i],center=TRUE,scale=TRUE)
     }
}

for(i in 1:ncol(Exp1data2)) {
     if(is.numeric(Exp1data2[,i]) == TRUE) {
          Exp1data2[paste('M', colnames(Exp1data2[i]), sep="")] <- scale(Exp1data2[i],center=TRUE,scale=FALSE)
     }
}

###-----Factors-----###

Exp1data2$F_Governance <- as.factor(Exp1data2$Governance)
contrasts(Exp1data2$F_Governance) <- contr.treatment(2, base = 1)

Exp1data2$F_Climate <- as.factor(Exp1data2$Climate)
contrasts(Exp1data2$F_Climate) <- contr.treatment(3, base = 1)

summary(aov(GovernanceManipulation ~ Governance, data = Exp1data2))
summary(aov(PerformanceC ~ F_Climate, data = Exp1data2))

Realismvariable <- Exp1data2[,c("NQ12.1_1","NQ12.1_2", "NQ12.1_3")] 
Exp1data2$Realism <- apply(Realismvariable, 1, FUN= "mean", na.rm= TRUE)
mean(Exp1data2$Realism)

###-----manipulation check-----###
summary(lm(NQ11.3 ~ Governance, data = Exp1data2))
summary(lm(NQ11.4 ~ Governance, data = Exp1data2))

summary(aov(PerformanceC ~ PerformanceClimate, data = Exp1data2))
summary(aov(PerformanceC ~ MasteryClimate, data = Exp1data2))

summary(aov(MasteryC ~ MasteryClimate, data = Exp1data2))
summary(aov(MasteryC ~ PerformanceClimate, data = Exp1data2))

#--------Experminet 2 (Governance, Status)--------#

###-----DATA IMPORT/EXPORT-----###
Exp2data <- read.spss("GovernanceStatus.sav", use.value.labels = T, to.data.frame = T)
head(Exp2data)

#Making factor variables numeric
for(i in 1:ncol(Exp2data)) {
     if(is.factor(Exp2data[,i]) == TRUE) {
          Exp2data[paste('N', colnames(Exp2data[i]), sep="")] <- sapply(Exp2data[,i], as.numeric)
     }
}

###-----Attention check-----###

subset(Exp2data, Exp2data$NQ14.3_9 != 2)
subset(Exp2data, Exp2data$NQ14.3_3 != 5)
subset(Exp2data, Exp2data$NQ10.2_7 != 3)

Exp2data2 <- subset(Exp2data, Exp2data$NQ14.3_3 == 5 & Exp2data$NQ14.3_9 == 2 & Exp2data$NQ10.2_7 == 3)

###-----Manipulations-----### 
###  (P = PMO governance/0; S = Shared governance/1) 
for(i in 1:ncol(Exp2data2)){
     Exp2data2[i][is.na(Exp2data2[i])] <- 0
}

Exp2data2$Governance <- ifelse(Exp2data2$Scenario_SL ==1 | Exp2data2$Scenario_SH ==1  | Exp2data2$Scenario_SN ==1 , 1, 0)
Exp2data2$Status <- ifelse(Exp2data2$Scenario_SL ==1 | Exp2data2$Scenario_NL ==1 , -1, ifelse(Exp2data2$Scenario_SH == 1 | Exp2data2$Scenario_NH ==1 , 1, 0))


Exp2data2$LowStatus <- ifelse(Exp2data2$Scenario_SL ==1 | Exp2data2$Scenario_NL ==1 , 1, 0)
Exp2data2$HighStatus <- ifelse(Exp2data2$Scenario_SH == 1 | Exp2data2$Scenario_NH ==1, 1, 0)

###-----Manipulation checks-----###
###
# shared = 1, NAO = 0
Exp2data2$NQ11.2_recode <- 6 - Exp2data2$NQ11.2
Exp2data2$NQ11.3
Governancevariable <- Exp2data2[,c("NQ11.3", "NQ11.2_recode")] 
Exp2data2$GovernanceManipulation <- apply(Governancevariable, 1, FUN= "mean", na.rm= TRUE)

Statusvariable <- Exp2data2[,c("NQ11.4", "NQ11.5")] 
Exp2data2$StatusManipulation <- apply(Statusvariable, 1, FUN= "mean", na.rm= TRUE)

###-----Variables-----###
#Section 1: Knowledge 

Exp2data2$NQ9.2_1_recode <- 8 - Exp2data2$NQ9.2_1
KS_Motivationvariable <- Exp2data2[,c("NQ9.2_1_recode","NQ9.2_2","NQ9.2_3","NQ9.2_4")] 
Exp2data2$KS_Motivation <- apply(KS_Motivationvariable, 1, FUN= "mean", na.rm= TRUE)


#Section 2: Mediation (need-support, need-satisfaction, realism) and feeling

NSupp_Autvariable <- Exp2data2[,c("NQ10.2_2", "NQ10.2_8","NQ10.2_10","NQ10.2_13")] 
Exp2data2$NSupp_Aut <- apply(NSupp_Autvariable, 1, FUN= "mean", na.rm= TRUE)

NSupp_Comvariable <- Exp2data2[,c("NQ10.2_4","NQ10.2_5", "NQ10.2_6", "NQ10.2_11")] 
Exp2data2$NSupp_Com <- apply(NSupp_Comvariable, 1, FUN= "mean", na.rm= TRUE)

NSupp_Relvariable <- Exp2data2[,c("NQ10.2_1","NQ10.2_3", "NQ10.2_9", "NQ10.2_12")] 
Exp2data2$NSupp_Rel <- apply(NSupp_Relvariable, 1, FUN= "mean", na.rm= TRUE)

NSuppvariable <- Exp2data2[,c("NSupp_Aut","NSupp_Com", "NSupp_Rel")] 
Exp2data2$NSupp <- apply(NSuppvariable, 1, FUN= "mean", na.rm= TRUE)


Exp2data2$NQ12.3_recode <- 8 - Exp2data2$NQ12.3
Feelingvariable <- Exp2data2[,c("NQ12.2","NQ12.3_recode")] 
Exp2data2$Feeling <- apply(Feelingvariable, 1, FUN= "mean", na.rm= TRUE)

#Section 3: Individual level factors (personality and orientation)

Mastery_Ovariable <- Exp2data2[,c("NQ14.2_1","NQ14.2_3", "NQ14.2_4", "NQ14.2_5", "NQ14.3_2", "NQ14.3_5", "NQ14.3_6", "NQ14.3_11")] 
Exp2data2$Mastery_O <- apply(Mastery_Ovariable, 1, FUN= "mean", na.rm= TRUE)

Performance_Ovariable <- Exp2data2[,c("NQ14.2_2","NQ14.2_6", "NQ14.2_7", "NQ14.3_1", "NQ14.3_4", "NQ14.3_7", "NQ14.3_8", "NQ14.3_10")] 
Exp2data2$Performance_O <- apply(Performance_Ovariable, 1, FUN= "mean", na.rm= TRUE)

#Section 4: Control (age, gender, education, culture, work experience, realism) 

Exp2data2$Gender <- Exp2data2$Q14.5

Exp2data2$Age <- Exp2data2$NQ14.4_1
Exp2data2$Age <- as.numeric(Exp2data2$Age)
Exp2data2$Age <- Exp2data2$Age + 17

Exp2data2$WE <- Exp2data2$NQ14.7_1
Exp2data2$WE <- as.numeric(Exp2data2$WE)
Exp2data2$WE <- Exp2data2$WE - 1

Exp2data2$WE_PM <- Exp2data2$NQ14.8
Exp2data2$WE_IOR <- Exp2data2$NQ14.9

Exp2data2$Education <- Exp2data2$NQ14.6

Exp2data2$CurrentPlace <- Exp2data2$Q14.10

Exp2data2$Nationality <- Exp2data2$Q14.11

Exp2data2$Residence <- Exp2data2$Q14.10

Realismvariable <- Exp2data2[,c("NQ12.1_1","NQ12.1_2", "NQ12.1_3")] 
Exp2data2$Realism <- apply(Realismvariable, 1, FUN= "mean", na.rm= TRUE)


Exp2data2$Prosocial <- 0 + ifelse(Exp2data2$NQ13.2 == 3, 1, 0) + ifelse(Exp2data2$NQ13.3 == 2, 1, 0) + ifelse(Exp2data2$NQ13.4 == 1, 1, 0) + ifelse(Exp2data2$NQ13.5 == 3, 1, 0) + ifelse(Exp2data2$NQ13.6 == 2, 1, 0) + ifelse(Exp2data2$NQ13.7 == 1, 1, 0) + ifelse(Exp2data2$NQ13.8 == 1, 1, 0) + ifelse(Exp2data2$NQ13.9 == 3, 1, 0) + ifelse(Exp2data2$NQ13.10 == 2, 1, 0) 
Exp2data2$IND <- 0 + ifelse(Exp2data2$NQ13.2 == 2, 1, 0) + ifelse(Exp2data2$NQ13.3 == 1, 1, 0) + ifelse(Exp2data2$NQ13.4 == 3, 1, 0) + ifelse(Exp2data2$NQ13.5 == 2, 1, 0) + ifelse(Exp2data2$NQ13.6 == 1, 1, 0) + ifelse(Exp2data2$NQ13.7 == 3, 1, 0) + ifelse(Exp2data2$NQ13.8 == 2, 1, 0) + ifelse(Exp2data2$NQ13.9 == 1, 1, 0) + ifelse(Exp2data2$NQ13.10 == 3, 1, 0) 
Exp2data2$Competitive <- 0 + ifelse(Exp2data2$NQ13.2 == 1, 1, 0) + ifelse(Exp2data2$NQ13.3 == 3, 1, 0) + ifelse(Exp2data2$NQ13.4 == 2, 1, 0) + ifelse(Exp2data2$NQ13.5 == 1, 1, 0) + ifelse(Exp2data2$NQ13.6 == 3, 1, 0) + ifelse(Exp2data2$NQ13.7 == 2, 1, 0) + ifelse(Exp2data2$NQ13.8 == 3, 1, 0) + ifelse(Exp2data2$NQ13.9 == 2, 1, 0) + ifelse(Exp2data2$NQ13.10 == 1, 1, 0) 

Exp2data2$SVO <- ifelse(Exp2data2$Prosocial >= 6, 2, ifelse(Exp2data2$IND >= 6, 1, 0))

#Z-standardizing and M-centring all variables
for(i in 1:ncol(Exp2data2)) {
     if(is.numeric(Exp2data2[,i]) == TRUE) {
          Exp2data2[paste('Z', colnames(Exp2data2[i]), sep="")] <- scale(Exp2data2[i],center=TRUE,scale=TRUE)
     }
}

for(i in 1:ncol(Exp2data2)) {
     if(is.numeric(Exp2data2[,i]) == TRUE) {
          Exp2data2[paste('M', colnames(Exp2data2[i]), sep="")] <- scale(Exp2data2[i],center=TRUE,scale=FALSE)
     }
}

###-----Factors-----###

Exp2data2$F_Governance <- as.factor(Exp2data2$Governance)
contrasts(Exp2data2$F_Governance) <- contr.treatment(2, base = 1)

Exp2data2$F_Status <- as.factor(Exp2data2$Status)
contrasts(Exp2data2$F_Status) <- contr.treatment(3, base = 1)

Exp2data2$F_SVO <- as.factor(Exp2data2$SVO)
contrasts(Exp2data2$F_SVO) <- contr.treatment(3)

Exp2data2$F_CurrentPlace <- as.factor(Exp2data2$CurrentPlace)
contrasts(Exp2data2$F_CurrentPlace) <- contr.helmert(3)

Exp2data2$F_Gender <- as.factor(Exp2data2$Gender)
contrasts(Exp2data2$F_Gender) <- contr.helmert(3)

summary(aov(GovernanceManipulation ~ Governance, data = Exp2data2))
summary(aov(StatusManipulation ~ F_Status, data = Exp2data2))



#--------Experminet 3 (Governance, Power)--------#


###-----DATA IMPORT/EXPORT-----###
Exp3data <- read.spss("GovernancePower.sav", use.value.labels = T, to.data.frame = T)
head(Exp3data)

#Making factor variables numeric
for(i in 1:ncol(Exp3data)) {
     if(is.factor(Exp3data[,i]) == TRUE) {
          Exp3data[paste('N', colnames(Exp3data[i]), sep="")] <- sapply(Exp3data[,i], as.numeric)
     }
}


###-----Attention check-----###


subset(Exp3data, Exp3data$NQ15.2_7 != 3)
subset(Exp3data, Exp3data$NQ16.1_4 != 6)

Exp3data2 <- subset(Exp3data, Exp3data$NQ15.2_7 == 3 & Exp3data$NQ16.1_4 == 6)


###-----Manipulations-----### 
###  (P = PMO governance/0; S = Shared governance/1) 
for(i in 1:ncol(Exp3data2)){
     Exp3data2[i][is.na(Exp3data2[i])] <- 0
}

Exp3data2$Governance <- ifelse(Exp3data2$Scenario_SC ==1 | Exp3data2$Scenario_SL ==1  | Exp3data2$Scenario_SH ==1 , 1, 0)
Exp3data2$Power <- ifelse(Exp3data2$Scenario_SH ==1 | Exp3data2$Scenario_NH ==1 , 2, ifelse(Exp3data2$Scenario_SL ==1 | Exp3data2$Scenario_NL ==1 , 1, 0))

Exp3data2$HighPower <- ifelse(Exp3data2$Scenario_SH ==1 | Exp3data2$Scenario_NH ==1 , 1, 0)
Exp3data2$LowPower <- ifelse(Exp3data2$Scenario_SL ==1 | Exp3data2$Scenario_NL ==1, 1, 0)



###-----Manipulation checks-----###
###-------Quick
# shared = 1, NAO = 0
Exp3data2$NQ9.1_1recode <- 6 - Exp3data2$NQ9.1_1
Exp3data2$NQ9.1_1recode
Governancevariable1 <- Exp3data2[,c("NQ9.1_2", "NQ9.1_1recode")] 
Exp3data2$GovernanceManipulation1 <- apply(Governancevariable1, 1, FUN= "mean", na.rm= TRUE)

#power
Exp3data2$NQ9.2

###-------Check
# shared = 1, NAO = 0
Exp3data2$NQ14.1_1recode <- 6 - Exp3data2$NQ14.1_1
Exp3data2$NQ14.1_1recode
Governancevariable2 <- Exp3data2[,c("NQ14.1_2", "NQ14.1_1recode")] 
Exp3data2$GovernanceManipulation2 <- apply(Governancevariable2, 1, FUN= "mean", na.rm= TRUE)

PowerManipulation <- Exp3data2[,c("NQ14.2_1","NQ14.2_2","NQ14.2_3")] 
Exp3data2$PowerManipulation <- apply(Uni_Pubvariable , 1, FUN= "mean", na.rm= TRUE)


Realismvariable <- Exp3data2[,c("NQ15.3_1","NQ15.3_2", "NQ15.3_3")] 
Exp3data2$Realism <- apply(Realismvariable, 1, FUN= "mean", na.rm= TRUE)
mean(Exp3data2$Realism)


###-----Variables-----###
#Section 1: Knowledge 

Exp3data2$NQ13.1_1 <- (-1 * Exp3data2$NQ13.1_1 + 2)
Exp3data2$NQ13.1_2 <- (-1 * Exp3data2$NQ13.1_2 + 2)
Exp3data2$NQ13.1_3 <- (-1 * Exp3data2$NQ13.1_3 + 2)
Exp3data2$NQ13.1_4 <- (-1 * Exp3data2$NQ13.1_4 + 2)
Exp3data2$NQ13.1_5 <- (-1 * Exp3data2$NQ13.1_5 + 2)
Exp3data2$NQ13.1_6 <- (-1 * Exp3data2$NQ13.1_6 + 2)
Exp3data2$NQ13.1_7 <- (-1 * Exp3data2$NQ13.1_7 + 2)
Exp3data2$NQ13.1_8 <- (-1 * Exp3data2$NQ13.1_8 + 2)
Exp3data2$NQ13.1_9 <- (-1 * Exp3data2$NQ13.1_9 + 2)
Exp3data2$NQ13.1_10 <- (-1 * Exp3data2$NQ13.1_10 + 2)
Exp3data2$NQ13.1_11 <- (-1 * Exp3data2$NQ13.1_11 + 2)
Exp3data2$NQ13.1_12 <- (-1 * Exp3data2$NQ13.1_12 + 2)


Imp_Privariable <- Exp3data2[,c("NQ13.1_8","NQ13.1_9","NQ13.1_12")] 
Exp3data2$Imp_Pri <- apply(Imp_Privariable, 1, FUN= "sum", na.rm= TRUE)

Imp_Pubvariable <- Exp3data2[,c("NQ13.1_1","NQ13.1_4","NQ13.1_5")] 
Exp3data2$Imp_Pub <- apply(Imp_Pubvariable, 1, FUN= "sum", na.rm= TRUE)

Uni_Privariable <- Exp3data2[,c("NQ13.1_7","NQ13.1_10","NQ13.1_11")] 
Exp3data2$Uni_Pri <- apply(Uni_Privariable, 1, FUN= "sum", na.rm= TRUE)

Uni_Pubvariable <- Exp3data2[,c("NQ13.1_2","NQ13.1_3","NQ13.1_6")] 
Exp3data2$Uni_Pub <- apply(Uni_Pubvariable , 1, FUN= "sum", na.rm= TRUE)

KS_Quantityvariable <- Exp3data2[,c("NQ13.1_2","NQ13.1_3","NQ13.1_6", "NQ13.1_8","NQ13.1_9","NQ13.1_12", "NQ13.1_1","NQ13.1_4","NQ13.1_5", "NQ13.1_7","NQ13.1_10","NQ13.1_11")] 
Exp3data2$KS_Quantity <- apply(KS_Quantityvariable, 1, FUN= "sum", na.rm= TRUE)

KS_Impvariable <- Exp3data2[,c("NQ13.1_1","NQ13.1_4","NQ13.1_5","NQ13.1_8","NQ13.1_9","NQ13.1_12")] 
Exp3data2$KS_Imp <- apply(KS_Impvariable, 1, FUN= "sum", na.rm= TRUE)    

KS_Privariable <- Exp3data2[,c("NQ13.1_7","NQ13.1_8","NQ13.1_9","NQ13.1_10","NQ13.1_11","NQ13.1_12")] 
Exp3data2$KS_Pri <- apply(KS_Privariable, 1, FUN= "sum", na.rm= TRUE)

KSvariable <- Exp3data2[,c("NQ13.1_8","NQ13.1_9","NQ13.1_12", "NQ13.1_1","NQ13.1_4","NQ13.1_5", "NQ13.1_7","NQ13.1_10","NQ13.1_11", "NQ13.1_2","NQ13.1_3","NQ13.1_6")] 
Exp3data2$KS <- apply(KSvariable, 1, FUN= "sum", na.rm= TRUE)

#Section 2: Mediation (need-support, need-satisfaction, realism) and feeling

NSupp_Autvariable <- Exp3data2[,c("NQ15.2_2", "NQ15.2_8","NQ15.2_10","NQ15.2_13")] 
Exp3data2$NSupp_Aut <- apply(NSupp_Autvariable, 1, FUN= "mean", na.rm= TRUE)

NSupp_Comvariable <- Exp3data2[,c("NQ15.2_4","NQ15.2_5", "NQ15.2_6", "NQ15.2_11")] 
Exp3data2$NSupp_Com <- apply(NSupp_Comvariable, 1, FUN= "mean", na.rm= TRUE)

NSupp_Relvariable <- Exp3data2[,c("NQ15.2_1","NQ15.2_3", "NQ15.2_9", "NQ15.2_12")] 
Exp3data2$NSupp_Rel <- apply(NSupp_Relvariable, 1, FUN= "mean", na.rm= TRUE)

NSuppvariable <- Exp3data2[,c("NSupp_Aut","NSupp_Com", "NSupp_Rel")] 
Exp3data2$NSupp <- apply(NSuppvariable, 1, FUN= "mean", na.rm= TRUE)

#----- alternative Mediations
#
CooperationGoalsvariable <- Exp3data2[,c("NQ16.1_1", "NQ16.1_3", "NQ16.1_3", "NQ16.1_8")] 
Exp3data2$CooperationGoals <- apply(CooperationGoalsvariable, 1, FUN= "mean", na.rm= TRUE)

CompetitionGoalsvariable <- Exp3data2[,c("NQ16.1_5", "NQ16.1_9", "NQ16.2_1", "NQ16.2_2", "NQ16.3_2")] 
Exp3data2$CompetitionGoals <- apply(CompetitionGoalsvariable, 1, FUN= "mean", na.rm= TRUE)

IndependentGoalsvariable <- Exp3data2[,c("NQ16.1_6", "NQ16.1_7", "NQ16.2_3", "NQ16.2_4", "NQ16.3_1")] 
Exp3data2$IndependentGoals <- apply(IndependentGoalsvariable, 1, FUN= "mean", na.rm= TRUE)

#Section 4: Control (age, gender, education, culture, work experience, realism) 

#-- INDIVIDUAL
Exp3data2$Gender <- Exp3data2$Q17.3

Exp3data2$Age <- Exp3data2$NQ17.2_1
Exp3data2$Age <- as.numeric(Exp3data2$Age)
Exp3data2$Age <- Exp3data2$Age + 17

Exp3data2$WE <- Exp3data2$NQ17.5_1
Exp3data2$WE <- as.numeric(Exp3data2$WE)
Exp3data2$WE <- Exp3data2$WE - 1

Exp3data2$WE_PM <- Exp3data2$NQ17.6
Exp3data2$WE_IOR <- Exp3data2$NQ17.7

Exp3data2$Education <- Exp3data2$NQ17.4
Exp3data2$Nationality <- Exp3data2$Q17.13
Exp3data2$Residence <- Exp3data2$Q17.12

#-- FIRM

Exp3data2$FirmSize <- Exp3data2$Q17.9

Exp3data2$JobFunction <- Exp3data2$Q17.8

Exp3data2$Industry <- Exp3data2$Q17.10

#Z-standardizing and M-centring all variables
for(i in 1:ncol(Exp3data2)) {
     if(is.numeric(Exp3data2[,i]) == TRUE) {
          Exp3data2[paste('Z', colnames(Exp3data2[i]), sep="")] <- scale(Exp3data2[i],center=TRUE,scale=TRUE)
     }
}

for(i in 1:ncol(Exp3data2)) {
     if(is.numeric(Exp3data2[,i]) == TRUE) {
          Exp3data2[paste('M', colnames(Exp3data2[i]), sep="")] <- scale(Exp3data2[i],center=TRUE,scale=FALSE)
     }
}

###-----Factors-----###

Exp3data2$F_Governance <- as.factor(Exp3data2$Governance)
contrasts(Exp3data2$F_Governance) <- contr.helmert(2)

Exp3data2$F_Power <- as.factor(Exp3data2$Power)
contrasts(Exp3data2$F_Power) <- contr.treatment(3, base = 1)

Exp3data2$F_HighPower <- as.factor(Exp3data2$HighPower)
contrasts(Exp3data2$F_HighPower) <- contr.treatment(2)

Exp3data2$F_LowPower <- as.factor(Exp3data2$LowPower)
contrasts(Exp3data2$F_LowPower) <- contr.treatment(2)

Exp3data2$F_Gender <- as.factor(Exp3data2$Gender)
contrasts(Exp3data2$F_Gender) <- contr.helmert(3)

###-----manipulation check-----###

summary(aov(NQ9.2 ~ F_Power, data = Exp3data2))
summary(aov(GovernanceManipulation2 ~ F_Governance, data = Exp3data2))


#--------Experminet 4 (Governance, Information)--------#

###-----DATA IMPORT/EXPORT-----###
Exp4data <- read.spss("GovernanceInformation.sav", use.value.labels = T, to.data.frame = T)
head(Exp4data)

#Making factor variables numeric
for(i in 1:ncol(Exp4data)) {
     if(is.factor(Exp4data[,i]) == TRUE) {
          Exp4data[paste('N', colnames(Exp4data[i]), sep="")] <- sapply(Exp4data[,i], as.numeric)
     }
}

###-----Attention check-----###

subset(Exp4data, Exp4data$NQ17.1_4 != 6)
subset(Exp4data, Exp4data$NQ16.2_7 != 3)
Exp4data2 <- subset(Exp4data, Exp4data$NQ17.1_4 == 6 & Exp4data$NQ16.2_7 == 3)


###-----Manipulations-----### 
###  (P = PMO governance/0; S = Shared governance/1) 
for(i in 1:ncol(Exp4data2)){
     Exp4data2[i][is.na(Exp4data2[i])] <- 0
}

Exp4data2$Governance <- ifelse(Exp4data2$Scenario_SC ==1 | Exp4data2$Scenario_SL ==1  | Exp4data2$Scenario_SH ==1 , 1, 0)
Exp4data2$Information <- ifelse(Exp4data2$Scenario_SH ==1 | Exp4data2$Scenario_NH ==1 , 2, ifelse(Exp4data2$Scenario_SL ==1 | Exp4data2$Scenario_NL ==1 , 1, 0))

Exp4data2$HighInformation <- ifelse(Exp4data2$Scenario_SH ==1 | Exp4data2$Scenario_NH ==1 , 1, 0)
Exp4data2$LowInformation <- ifelse(Exp4data2$Scenario_SL ==1 | Exp4data2$Scenario_NL ==1, 1, 0)

###-----Manipulation checks-----###
###-------Quick
# shared = 1, NAO = 0
# 
Exp4data2$NQ4.1_1recode <- 6 - Exp4data2$NQ4.1_1
Exp4data2$NQ4.1_1recode
Governancevariable1 <- Exp4data2[,c("NQ4.1_2", "NQ4.1_1recode")] 
Exp4data2$GovernanceManipulation1 <- apply(Governancevariable1, 1, FUN= "mean", na.rm= TRUE)

#information
Exp4data2$NQ15.2

###-------Check
# shared = 1, NAO = 0
Exp4data2$NQ15.1_1recode <- 6 - Exp4data2$NQ15.1_1
Exp4data2$NQ15.1_1recode
Governancevariable2 <- Exp4data2[,c("NQ15.1_2", "NQ15.1_1recode")] 
Exp4data2$GovernanceManipulation2 <- apply(Governancevariable2, 1, FUN= "mean", na.rm= TRUE)

InformationManipulation <- Exp4data2$NQ16.4


Realismvariable <- Exp4data2[,c("NQ16.3_1","NQ16.3_2","NQ16.3_3")] 
Exp4data2$Realism <- apply(Realismvariable, 1, FUN= "mean", na.rm= TRUE)
mean(Exp4data2$Realism)


###-----Variables-----###
#Section 1: Knowledge 

Exp4data2$NQ12.1_1 <- -1 + Exp4data2$NQ12.1_1
Exp4data2$NQ12.1_2 <- (-1 + Exp4data2$NQ12.1_2 )
Exp4data2$NQ12.1_3 <- (-1 + Exp4data2$NQ12.1_3 )
Exp4data2$NQ12.1_4 <- (-1 + Exp4data2$NQ12.1_4 )
Exp4data2$NQ12.1_5 <- (-1 + Exp4data2$NQ12.1_5 )
Exp4data2$NQ12.1_6 <- (-1 + Exp4data2$NQ12.1_6 )
Exp4data2$NQ12.1_7 <- (-1 + Exp4data2$NQ12.1_7 )
Exp4data2$NQ12.1_8 <- (-1 + Exp4data2$NQ12.1_8 )
Exp4data2$NQ12.1_9 <- (-1 + Exp4data2$NQ12.1_9 )
Exp4data2$NQ12.1_10 <- (-1 + Exp4data2$NQ12.1_10 )
Exp4data2$NQ12.1_11 <- (-1 + Exp4data2$NQ12.1_11 )
Exp4data2$NQ12.1_12 <- (-1 + Exp4data2$NQ12.1_12 )


Exp4data2$NQ13.1_1 <- (-1 + Exp4data2$NQ13.1_1 )
Exp4data2$NQ13.1_2 <- (-1 + Exp4data2$NQ13.1_2 )
Exp4data2$NQ13.1_3 <- (-1 + Exp4data2$NQ13.1_3 )
Exp4data2$NQ13.1_4 <- (-1 + Exp4data2$NQ13.1_4 )
Exp4data2$NQ13.1_5 <- (-1 + Exp4data2$NQ13.1_5 )
Exp4data2$NQ13.1_6 <- (-1 + Exp4data2$NQ13.1_6 )
Exp4data2$NQ13.1_7 <- (-1 + Exp4data2$NQ13.1_7 )
Exp4data2$NQ13.1_8 <- (-1 + Exp4data2$NQ13.1_8 )


Exp4data2$NQ14.1_1 <- (-1 + Exp4data2$NQ14.1_1 )
Exp4data2$NQ14.1_2 <- (-1 + Exp4data2$NQ14.1_2 )
Exp4data2$NQ14.1_3 <- (-1 + Exp4data2$NQ14.1_3 )
Exp4data2$NQ14.1_4 <- (-1 + Exp4data2$NQ14.1_4 )
Exp4data2$NQ14.1_5 <- (-1 + Exp4data2$NQ14.1_5 )
Exp4data2$NQ14.1_6 <- (-1 + Exp4data2$NQ14.1_6 )
Exp4data2$NQ14.1_7 <- (-1 + Exp4data2$NQ14.1_7 )
Exp4data2$NQ14.1_8 <- (-1 + Exp4data2$NQ14.1_8 )
Exp4data2$NQ14.1_9 <- (-1 + Exp4data2$NQ14.1_9 )
Exp4data2$NQ14.1_10 <- (-1 + Exp4data2$NQ14.1_10 )
Exp4data2$NQ14.1_11 <- (-1 + Exp4data2$NQ14.1_11 )
Exp4data2$NQ14.1_12 <- (-1 + Exp4data2$NQ14.1_12 )
Exp4data2$NQ14.1_13 <- (-1 + Exp4data2$NQ14.1_13 )
Exp4data2$NQ14.1_14 <- (-1 + Exp4data2$NQ14.1_14 )
Exp4data2$NQ14.1_15 <- (-1 + Exp4data2$NQ14.1_15 )
Exp4data2$NQ14.1_16 <- (-1 + Exp4data2$NQ14.1_16 )

#Control
Imp_Privariable <- Exp4data2[,c("NQ12.1_1","NQ12.1_8","NQ12.1_9","NQ12.1_12","NQ13.1_6","NQ13.1_7","NQ14.1_1","NQ14.1_8","NQ14.1_9","NQ14.1_12","NQ14.1_13","NQ14.1_5")] 
Exp4data2$Imp_Pri <- apply(Imp_Privariable, 1, FUN= "sum", na.rm= TRUE)

Imp_Pubvariable <- Exp4data2[,c("NQ12.1_4","NQ12.1_5","NQ13.1_3","NQ13.1_4","NQ14.1_4","NQ14.1_5")] 
Exp4data2$Imp_Pub <- apply(Imp_Pubvariable, 1, FUN= "sum", na.rm= TRUE)

Uni_Privariable <- Exp4data2[,c("NQ12.1_6", "NQ12.1_7","NQ12.1_10","NQ12.1_11","NQ13.1_5","NQ13.1_8","NQ14.1_6","NQ14.1_7","NQ14.1_10","NQ14.1_11","NQ14.1_14","NQ14.1_16")] 
Exp4data2$Uni_Pri <- apply(Uni_Privariable, 1, FUN= "sum", na.rm= TRUE)

Uni_Pubvariable <- Exp4data2[,c("NQ12.1_2","NQ12.1_3", "NQ13.1_1","NQ13.1_2", "NQ14.1_2","NQ14.1_3")] 
Exp4data2$Uni_Pub <- apply(Uni_Pubvariable , 1, FUN= "sum", na.rm= TRUE)

KSvariable <- Exp4data2[,c("NQ12.1_1","NQ12.1_8","NQ12.1_9","NQ12.1_12","NQ13.1_6","NQ13.1_7","NQ14.1_1","NQ14.1_8","NQ14.1_9","NQ14.1_12","NQ14.1_13","NQ14.1_5", "NQ12.1_4","NQ12.1_5","NQ13.1_3","NQ13.1_4","NQ14.1_4","NQ14.1_5", "NQ12.1_6", "NQ12.1_7","NQ12.1_10","NQ12.1_11","NQ13.1_5","NQ13.1_8","NQ14.1_6","NQ14.1_7","NQ14.1_10","NQ14.1_11","NQ14.1_14","NQ14.1_16", "NQ12.1_2","NQ12.1_3", "NQ13.1_1","NQ13.1_2", "NQ14.1_2","NQ14.1_3")] 
Exp4data2$KS <- apply(KSvariable, 1, FUN= "sum", na.rm= TRUE)

#Section 2: Mediation (need-support, need-satisfaction, realism) and feeling

NSupp_Autvariable <- Exp4data2[,c("NQ16.2_2", "NQ16.2_8","NQ16.2_10","NQ16.2_13")] 
Exp4data2$NSupp_Aut <- apply(NSupp_Autvariable, 1, FUN= "mean", na.rm= TRUE)

NSupp_Comvariable <- Exp4data2[,c("NQ16.2_4","NQ16.2_5", "NQ16.2_6", "NQ16.2_11")] 
Exp4data2$NSupp_Com <- apply(NSupp_Comvariable, 1, FUN= "mean", na.rm= TRUE)

NSupp_Relvariable <- Exp4data2[,c("NQ16.2_1","NQ16.2_3", "NQ16.2_9", "NQ16.2_12")] 
Exp4data2$NSupp_Rel <- apply(NSupp_Relvariable, 1, FUN= "mean", na.rm= TRUE)

NSuppvariable <- Exp4data2[,c("NSupp_Aut","NSupp_Com", "NSupp_Rel")] 
Exp4data2$NSupp <- apply(NSuppvariable, 1, FUN= "mean", na.rm= TRUE)

#----- alternative Mediations
#
CooperationGoalsvariable <- Exp4data2[,c("NQ17.1_1", "NQ17.1_3", "NQ17.1_3", "NQ17.1_8")] 
Exp4data2$CooperationGoals <- apply(CooperationGoalsvariable, 1, FUN= "mean", na.rm= TRUE)

CompetitionGoalsvariable <- Exp4data2[,c("NQ17.1_5", "NQ17.1_9", "NQ17.2_1", "NQ17.2_2", "NQ17.3_2")] 
Exp4data2$CompetitionGoals <- apply(CompetitionGoalsvariable, 1, FUN= "mean", na.rm= TRUE)

IndependentGoalsvariable <- Exp4data2[,c("NQ17.1_6", "NQ17.1_7", "NQ17.2_3", "NQ17.2_4", "NQ17.3_1")] 
Exp4data2$IndependentGoals <- apply(IndependentGoalsvariable, 1, FUN= "mean", na.rm= TRUE)

#Section 4: Control (age, gender, education, culture, work experience, realism) 

#-- INDIVIDUAL
Exp4data2$Gender <- Exp4data2$Q18.3

Exp4data2$Age <- Exp4data2$NQ18.2_1
Exp4data2$Age <- as.numeric(Exp4data2$Age)
Exp4data2$Age <- Exp4data2$Age + 17

Exp4data2$WE <- Exp4data2$NQ18.5_1
Exp4data2$WE <- as.numeric(Exp4data2$WE)
Exp4data2$WE <- Exp4data2$WE - 1

Exp4data2$WE_PM <- Exp4data2$NQ18.6
Exp4data2$WE_IOR <- Exp4data2$NQ17.7

Exp4data2$Education <- Exp4data2$NQ18.4
Exp4data2$Nationality <- Exp4data2$Q18.13
Exp4data2$Residence <- Exp4data2$Q18.12

#-- FIRM

Exp4data2$FirmSize <- Exp4data2$Q18.9

Exp4data2$JobFunction <- Exp4data2$Q18.8

Exp4data2$Industry <- Exp4data2$Q18.10

#Z-standardizing and M-centring all variables
for(i in 1:ncol(Exp4data2)) {
     if(is.numeric(Exp4data2[,i]) == TRUE) {
          Exp4data2[paste('Z', colnames(Exp4data2[i]), sep="")] <- scale(Exp4data2[i],center=TRUE,scale=TRUE)
     }
}

for(i in 1:ncol(Exp4data2)) {
     if(is.numeric(Exp4data2[,i]) == TRUE) {
          Exp4data2[paste('M', colnames(Exp4data2[i]), sep="")] <- scale(Exp4data2[i],center=TRUE,scale=FALSE)
     }
}

###-----Factors-----###

Exp4data2$F_Governance <- as.factor(Exp4data2$Governance)
contrasts(Exp4data2$F_Governance) <- contr.helmert(2)

Exp4data2$F_Information <- as.factor(Exp4data2$Information)
contrasts(Exp4data2$F_Information) <- contr.treatment(3, base = 1)

Exp4data2$F_HighInformation <- as.factor(Exp4data2$HighInformation)
contrasts(Exp4data2$F_HighInformation) <- contr.treatment(2)

Exp4data2$F_LowInformation <- as.factor(Exp4data2$LowInformation)
contrasts(Exp4data2$F_LowInformation) <- contr.treatment(2, base = 2)

Exp4data2$F_Gender <- as.factor(Exp4data2$Gender)
contrasts(Exp4data2$F_Gender) <- contr.helmert(3)

###-----manipulation check-----###

summary(aov(NQ16.4 ~ F_Information, data = Exp4data2))
summary(aov(GovernanceManipulation2 ~ F_Governance, data = Exp4data2))


# Expdata2$Imp_Pri <- ifelse(Expdata2$Scenario_SC ==1 | Expdata2$Scenario_NC ==1, Expdata2$Imp_Pri/4, ifelse(Expdata2$Scenario_SL ==1 | Expdata2$Scenario_NL ==1, Expdata2$Imp_Pri/2, ifelse(Expdata2$Scenario_SH ==1 | Expdata2$Scenario_NH ==1, Expdata2$Imp_Pri/6, NA)))

# Creating DVs relative to the nimber of pieces of infor they had -> 
# Exp4data2$Imp_Pri <- ifelse(Exp4data2$Scenario_SC ==1 | Exp4data2$Scenario_NC ==1, Exp4data2$Uni_Pri/4, ifelse(Exp4data2$Scenario_SL ==1 | Exp4data2$Scenario_NL ==1, Exp4data2$Uni_Pri/2, ifelse(Exp4data2$Scenario_SH ==1 | Exp4data2$Scenario_NH ==1, Exp4data2$Imp_Pri/6, NA)))
# Exp4data2$Uni_Pri <- ifelse(Exp4data2$Scenario_SC ==1 | Exp4data2$Scenario_NC ==1, Exp4data2$Uni_Pri/4, ifelse(Exp4data2$Scenario_SL ==1 | Exp4data2$Scenario_NL ==1, Exp4data2$Uni_Pri/2, ifelse(Exp4data2$Scenario_SH ==1 | Exp4data2$Scenario_NH ==1, Exp4data2$Uni_Pri/6, NA)))
# Exp4data2$Uni_Pub <- Exp4data2$Uni_Pub/2
# Exp4data2$Imp_Pub <- Exp4data2$Imp_Pub/2
# Exp4data2$KS <- ifelse(Exp4data2$Scenario_SC ==1 | Exp4data2$Scenario_NC ==1, Exp4data2$KS/12, ifelse(Exp4data2$Scenario_SL ==1 | Exp4data2$Scenario_NL ==1, Exp4data2$KS/8, ifelse(Exp4data2$Scenario_SH ==1 | Exp4data2$Scenario_NH ==1, Exp4data2$KS/16, NA)))

#------------------------------------------------------DATA SUBSETS------------------------------------------------------------#

# DURATION
Exp1data3 <- subset(Exp1data2, Exp1data2$Duration__in_seconds_ < 1200)
Exp2data3 <- subset(Exp2data2, Exp2data2$Duration__in_seconds_ < 1200)
Exp3data3 <- subset(Exp3data2, Exp3data2$Duration__in_seconds_ < 1200)
Exp4data3 <- subset(Exp4data2, Exp4data2$Duration__in_seconds_ < 1200)

# NO CONTROL
Exp1data3 <- subset(Exp1data2, Exp1data2$F_Climate != 0)
Exp2data3 <- subset(Exp2data2, Exp2data2$F_Status != 0)
Exp3data3 <- subset(Exp3data2, Exp3data2$F_Power != 0)
Exp4data3 <- subset(Exp4data2, Exp4data2$F_Information != 0)

Exp1data3$F_Climate <- as.factor(Exp1data3$Climate)
contrasts(Exp1data3$F_Climate) <- contr.treatment(2)

Exp2data3$F_Status <- as.factor(Exp2data3$Status)
contrasts(Exp2data3$F_Status) <- contr.treatment(2)

Exp3data3$F_Power <- as.factor(Exp3data3$Power)
contrasts(Exp3data3$F_Power) <- contr.treatment(2)

Exp4data3$F_Information <- as.factor(Exp4data3$Information)
contrasts(Exp4data3$F_Information) <- contr.treatment(2)

#------------------------------------------------------PART 1: DESCRIPTIVES------------------------------------------------------------#


Experimentalconditions1 <- list(Exp1data2$Governance, Exp1data2$Climate)
Experimentalconditions2 <- list(Exp2data2$Governance, Exp2data2$Status)
Experimentalconditions3 <- list(Exp3data2$Governance, Exp3data2$Power)
Experimentalconditions4 <- list(Exp4data2$Governance, Exp4data2$Information)

# descriptives for overall KS
stat.desc(Exp1data2$KS_Motivation, basic=FALSE, norm=TRUE)
stat.desc(Exp2data2$KS_Motivation, basic=FALSE, norm=TRUE)
stat.desc(Exp3data2$KS, basic=FALSE, norm=TRUE)
stat.desc(Exp4data2$KS, basic=FALSE, norm=TRUE)

# descriptives for overall KS (IMP_PRI)

by(Exp3data2$Imp_Pri, Experimentalconditions3, stat.desc, basic = F, norm = T)
by(Exp4data2$Imp_Pri, Experimentalconditions4, stat.desc, basic = F, norm = T)



# descriptives for needs satisfaction

stat.desc(Exp1data2$NSupp, basic=FALSE, norm=TRUE)
stat.desc(Exp2data2$NSupp, basic=FALSE, norm=TRUE)
stat.desc(Exp3data2$NSupp, basic=FALSE, norm=TRUE)
stat.desc(Exp4data2$NSupp, basic=FALSE, norm=TRUE)

# descriptives for demographics

#AGE
stat.desc(Exp1data2$Age, basic=FALSE, norm=TRUE)
stat.desc(Exp2data2$Age, basic=FALSE, norm=TRUE)
stat.desc(Exp3data2$Age, basic=FALSE, norm=TRUE)
stat.desc(Exp4data2$Age, basic=FALSE, norm=TRUE)

#WORK EXPERIENCE
stat.desc(Exp1data2$WE, basic=FALSE, norm=TRUE)
stat.desc(Exp2data2$WE, basic=FALSE, norm=TRUE)
stat.desc(Exp3data2$WE, basic=FALSE, norm=TRUE)
stat.desc(Exp4data2$WE, basic=FALSE, norm=TRUE)

#WORK EXPERIENCE: PM
stat.desc(Exp1data2$WE_PM, basic=FALSE, norm=TRUE)
stat.desc(Exp2data2$WE_PM, basic=FALSE, norm=TRUE)
stat.desc(Exp3data2$WE_PM, basic=FALSE, norm=TRUE)
stat.desc(Exp4data2$WE_PM, basic=FALSE, norm=TRUE)

#GENDER
table(Exp1data2$Gender)
table(Exp2data2$Gender)
table(Exp3data2$Gender)
table(Exp4data2$Gender)

#INDUSTRY
table(Exp3data2$Industry)
table(Exp4data2$Industry)

#EDUCATION
table(Exp1data2$Education)
table(Exp2data2$Education)
table(Exp3data2$Education)
table(Exp4data2$Education)


#------------------------------------------------------PART 2: GRAPHS------------------------------------------------------------#


ggplot(Exp1data2) + geom_bar(aes(x = F_Governance, fill = F_Climate))
ggplot(Exp1data2) + geom_bar(aes(x = F_Governance, fill = F_Climate), position = position_dodge(preserve = 'single'))
ggplot(Exp1data2) + geom_bar(aes(x = F_Governance, fill = F_Climate, y = KS_Motivation), stat = 'identity', width = 10 , position = position_dodge(preserve = 'single'))


ggplot(Exp2data2) + geom_bar(aes(x = F_Governance, fill = F_Status, y = KS_Motivation), width =30, stat = 'identity' , position = position_dodge(preserve = 'single')) + coord_flip()
ggplot(Exp2data2) + geom_bar(aes(x = F_Governance, fill = F_Status, y = KS_Motivation), width =90, stat = 'identity' , position = position_dodge(preserve = 'single', width = 20)) 

ggplot(Exp3data2) + geom_bar(aes(x = F_Governance, fill = F_Power, y = Imp_Pri), width = 0.8, stat = 'identity' , position = position_dodge(preserve = 'single', width = 0.9)) 



# Interaction between Governance and asymmetries

# OVERALL KNOWLEDGE
interaction.plot(Exp1data2$F_Governance, Exp1data2$F_Climate, Exp1data2$KS_Motivation)
interaction.plot(Exp2data2$F_Governance, Exp2data2$F_Status, Exp2data2$KS_Motivation)
interaction.plot(Exp3data2$F_Governance, Exp3data2$F_Power, Exp3data2$KS)
interaction.plot(Exp4data2$F_Governance, Exp4data2$F_Information, Exp4data2$KS)

# IMP_PRI KNOWLEDGE
interaction.plot(Exp3data2$F_Governance, Exp3data2$F_Power, Exp3data2$Imp_Pri)
interaction.plot(Exp4data2$F_Governance, Exp4data2$F_Information, Exp4data2$Imp_Pri)

# OVERALL NEEDS 
interaction.plot(Exp1data2$F_Governance, Exp1data2$F_Climate, Exp1data2$NSupp)
interaction.plot(Exp2data2$F_Governance, Exp2data2$F_Status, Exp2data2$NSupp)
interaction.plot(Exp3data2$F_Governance, Exp3data2$F_Power, Exp3data2$NSupp)
interaction.plot(Exp4data2$F_Governance, Exp4data2$F_Information, Exp4data2$NSupp)


# Ploting interaction effect using anova model as an input

library(phia)
plot(interactionMeans(Model3))  
testInteractions(Model3)

plot(interactionMeans(Model3))  
testInteractions(Model3)
#------------------------------------------------------PART 3: ANALYSES------------------------------------------------------------#

# Running the first part of the model: the effect of governance and asymmetry on needs satisfaction

Model1 <- manova(cbind(NSupp_Aut, NSupp_Com, NSupp_Rel) ~ F_Governance*F_Climate + ZEducation + ZWE_PM + ZAge + ZNQ12.1_3, data = Exp1data2)
Model2 <- manova(cbind(NSupp_Aut, NSupp_Com, NSupp_Rel) ~ F_Governance*F_Status + ZEducation + ZWE_PM + ZAge + ZNQ12.1_3, data = Exp2data2)
Model3 <- manova(cbind(NSupp_Aut, NSupp_Com, NSupp_Rel) ~ F_Governance*F_Power + ZEducation + ZWE_PM + ZAge + ZNQ15.3_3, data = Exp3data2)
Model4 <- manova(cbind(NSupp_Aut, NSupp_Com, NSupp_Rel) ~ F_Governance*F_Information + ZEducation + ZWE_PM + ZAge + ZNQ16.3_3, data = Exp4data2)


summary(Model1, test="Pillai")
summary(Model2, test="Pillai")
summary(Model3, test="Pillai")
summary(Model4, test="Pillai")

summary.aov(Model1)
summary.aov(Model2)
summary.aov(Model3)
summary.aov(Model4)

# Running the first part of the model: the effect of governance and asymmetry on needs, individually

Model1 <- aov(NSupp ~ F_Climate + F_Governance + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp1data2)
Model2 <- aov(NSupp ~ F_Status + F_Governance + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp2data2)
Model3 <- aov(NSupp ~ F_Power + F_Governance + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)
Model4 <- aov(NSupp ~ F_Information + F_Governance + ZEducation + ZWE_PM + ZAge + NQ16.3_3, data = Exp4data2)

Anova(Model1, type= "III")
Anova(Model2, type= "III")
Anova(Model3, type= "III")
Anova(Model4, type= "III")

summary.lm(Model1)
summary.lm(Model2)
summary.lm(Model3)
summary.lm(Model4)


# Running the second part of the model: the effect of needs and asymmetry on knowledge provision

Model3 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~ F_Power*F_Governance + ZEducation + ZWE + ZAge + FirmSize, data = Exp3data2)
Model4 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~  F_Information*F_Governance  + ZEducation + ZWE + ZAge + FirmSize, data = Exp4data2)

summary(Model3, test="Pillai")
summary(Model4, test="Pillai")

summary.aov(Model3)
summary.aov(Model4)

# Running the second part of the model:the effect of needs and asymmetry on Quantity

Model1 <- aov(KS_Motivation ~ F_Climate * (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp1data2)
Model2 <- aov(KS_Motivation ~ F_Status * (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp2data2)
Model3 <- aov(KS ~ F_Power * (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)
Model4 <- aov(KS ~ F_Information * (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ16.3_3, data = Exp4data2)

Anova(Model1, type= "III")
Anova(Model2, type= "III")
Anova(Model3, type= "III")
Anova(Model4, type= "III")

# Running the second part of the model:the effect of needs on Quantity

Model1 <- aov(KS_Motivation ~ F_Climate + (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp1data2)
Model2 <- aov(KS_Motivation ~ F_Status + (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp2data2)
Model3 <- aov(KS ~ F_Power + (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)
Model4 <- aov(KS ~ F_Information + (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ16.3_3, data = Exp4data2)

Anova(Model1, type= "III")
Anova(Model2, type= "III")
Anova(Model3, type= "III")
Anova(Model4, type= "III")

# Running the second part of the model:the effect of needs on Imp_Pri

Model1 <- aov(KS_Motivation ~ F_Climate + (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp1data2)
Model2 <- aov(KS_Motivation ~ F_Status + (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp2data2)
Model3 <- aov(Imp_Pri ~ F_Power*(NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)
Model4 <- aov(Imp_Pri ~ F_Information*(NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ16.3_3, data = Exp4data2)

Anova(Model1, type= "III")
Anova(Model2, type= "III")
Anova(Model3, type= "III")
Anova(Model4, type= "III")

# Running the full model without mediation

Model1 <- aov(KS_Motivation ~ F_Climate * F_Governance + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp1data2)
Model2 <- aov(KS_Motivation ~ F_Status * F_Governance + ZEducation + ZWE_PM + ZAge + NQ12.1_3, data = Exp2data2)
Model3 <- aov(Imp_Pri ~ F_Power * F_Governance + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)
Model4 <- aov(Imp_Pri ~ F_Information * F_Governance + ZEducation + ZWE_PM + ZAge + NQ16.3_3, data = Exp4data2)

Anova(Model1, type= "III")
Anova(Model2, type= "III")
Anova(Model3, type= "III")
Anova(Model4, type= "III")

summary.lm(Model1)
summary.lm(Model2)
summary.lm(Model3)
summary.lm(Model4)

# Running the first part of the model: the effect of governance and asymmetry on mutual goals

Model3 <- manova(cbind(CooperationGoals, CompetitionGoals, IndependentGoals) ~ F_Governance*F_Power + ZEducation + ZWE_PM + ZAge + ZNQ15.3_3, data = Exp3data2)
Model4 <- manova(cbind(CooperationGoals, CompetitionGoals, IndependentGoals) ~ F_Governance*F_Information + ZEducation + ZWE_PM + ZAge + ZNQ16.3_3, data = Exp4data2)

summary(Model3, test="Pillai")
summary(Model4, test="Pillai")

summary.aov(Model3)
summary.aov(Model4)

# Running the second part of the model: the effect of goals on ks

Model3 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~ (CooperationGoals + CompetitionGoals + IndependentGoals)*F_Power + ZEducation + ZWE + ZAge + NQ15.3_3, data = Exp3data2)
Model4 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~ (CooperationGoals + CompetitionGoals + IndependentGoals)*F_Information + ZEducation + ZWE + ZAge + NQ16.3_3, data = Exp4data2)

summary(Model3, test="Pillai")
summary(Model4, test="Pillai")

summary.aov(Model3)
summary.aov(Model4)

# Running the second part of the model: the effect of needs on ks

Model3 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~ ZNSupp_Aut + ZNSupp_Com + ZNSupp_Rel + ZEducation + ZWE + ZAge + NQ15.3_3, data = Exp3data2)
Model4 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~ ZNSupp_Aut + ZNSupp_Com + ZNSupp_Rel + ZEducation + ZWE + ZAge + NQ16.3_3, data = Exp4data2)

summary(Model3, test="Pillai")
summary(Model4, test="Pillai")

summary.aov(Model3)
summary.aov(Model4)


# Running the a model for the amount of information pooled (total knowledge: wide data)

Model3 <- aov(KS ~ F_Power*F_Governance*Quality + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data3)
Model4 <- aov(KS ~ F_Information*F_Governance*Quality  + ZEducation + ZWE_PM + ZAge + NQ16.3_3, data = Exp4data3)

Anova(Model3, type= "III")
Anova(Model4, type= "III")

summary.lm(Model3)
summary.lm(Model4)

# Running the a model for the amount of information pooled (critical knowledge)
contrasts(Exp3data2$F_Power) <- c(-1,0,1)
contrasts(Exp4data2$F_Information) <- c(0,-1,1)

Model3 <- aov(Imp_Pri ~ F_Power*F_Governance + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)
Model4 <- aov(Imp_Pri ~ F_Information*F_Governance  + ZEducation + ZWE_PM + ZAge + NQ16.3_3, data = Exp4data2)

Anova(Model3, type= "III")
Anova(Model4, type= "III")

summary.lm(Model3)
summary.lm(Model4)

#------------------------------------------------------PART 4: WITHIN_SUBJECT------------------------------------------------------------#

library(tidyverse)
library(ggpubr)
library(rstatix)
library(lme4)
library(nlme)

#adding an id columns
Exp3data3 <- mutate(Exp3data2, id = rownames(Exp3data2))
Exp3data3$id

Exp4data3 <- mutate(Exp4data2, id = rownames(Exp4data2))
Exp4data3$id

Exp4data3$Imp_Pri
#adding a categorical KS variable (similar to t1, t2, t3)


data_long3 <- gather(Exp3data3, Quality, KnowledgeSharing, Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub, factor_key=TRUE)
data_long3[, c("id", "Quality", "KnowledgeSharing")]

data_long4 <- gather(Exp4data3, Quality, KnowledgeSharing, Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub, factor_key=TRUE)
data_long4[, c("id", "Quality", "KnowledgeSharing")]

data_long3$F_Quality <- as.factor(data_long3$Quality)
contrasts(data_long3$F_Quality) <- contr.Helmert(4)

data_long4$F_Quality <- as.factor(data_long4$Quality)
contrasts(data_long4$F_Quality) <- contr.Helmert(4)

#adding a Sharedness and Importance as two within-subject variables

data_long3$Private <- ifelse(data_long3$Quality == "Imp_Pub" | data_long3$Quality == "Uni_Pub" , 0, 1)
data_long3$F_Private <- as.factor(data_long3$Private)
contrasts(data_long3$F_Private) <- contr.treatment(2)

data_long3$Importance <- ifelse(data_long3$Quality == "Imp_Pri" | data_long3$Quality == "Imp_Pub" , 1, 0)
data_long3$F_Importance <- as.factor(data_long3$Importance)
contrasts(data_long3$F_Importance) <- contr.treatment(2)


data_long4$Private <- ifelse(data_long4$Quality == "Imp_Pub" | data_long4$Quality == "Uni_Pub" , 0, 1)
data_long4$F_Private <- as.factor(data_long4$Private)
contrasts(data_long4$F_Private) <- contr.treatment(2)

data_long4$Importance <- ifelse(data_long4$Quality == "Imp_Pri" | data_long4$Quality == "Imp_Pub" , 1, 0)
data_long4$F_Importance <- as.factor(data_long4$Importance)
contrasts(data_long4$F_Importance) <- contr.treatment(2)

#analysing data using normal ANOVA model with (error) - KS as DV and Quality as IV

Model3 <- aov(KnowledgeSharing ~ F_Quality*F_Governance*F_Power + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + Error(id/F_Quality) + F_Governance*F_Power, data = data_long3)
Model4 <- aov(KnowledgeSharing ~ F_Quality*F_Governance*F_Information + ZEducation + ZWE_PM + ZAge + NQ16.3_3 + Error(id/F_Quality) + F_Governance*F_Information, data = data_long4)

summary(Model3)
summary(Model4)

model.tables(Model3, "means")
model.tables(Model4, "means")

#analysing data using normal ANOVA model with (error) - KS as DV and Private and Importance as IV

Model3 <- aov(KnowledgeSharing ~ (F_Private + F_Importance)*F_Power*F_Governance + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + Error(id/F_Private*F_Importance), data = data_long3)
Model4 <- aov(KnowledgeSharing ~ (F_Private + F_Importance)*F_Information*F_Governance + ZEducation + ZWE_PM + ZAge + NQ16.3_3 + Error(id/F_Private*F_Importance), data = data_long4)

summary(Model3)
summary(Model4)

model.tables(Model3, "means")
model.tables(Model4, "means")

#analysing data using normal ANOVA model with (error) - KS as DV and needs as mediators

Model3 <- aov(KnowledgeSharing ~ (F_Private+F_Importance)*(NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + Error(id/F_Private*F_Importance), data = data_long3)
Model4 <- aov(KnowledgeSharing ~ (F_Private+F_Importance)*(NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ16.3_3 + Error(id/F_Private*F_Importance), data = data_long4)

summary(Model3)
summary(Model4)

model.tables(Model3, "means")
model.tables(Model4, "means")

#analysing data using normal ANOVA model with (error) - Needs as DV 
Model3 <- aov(NSupp ~  F_Governance*F_Power + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + Error(id/F_Private*F_Importance), data = data_long3)
Model4 <- aov(NSupp ~ F_Governance*F_Information + ZEducation + ZWE_PM + ZAge + NQ16.3_3 + Error(id/F_Private*F_Importance), data = data_long4)

summary(Model3)
summary(Model4)

model.tables(Model3, "means")
model.tables(Model4, "means")

#analysing data using LME4  - KS as DV 

Model3 <- lmer(KnowledgeSharing ~ (F_Private + F_Importance)*F_Power*F_Governance + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + (F_Importance | id) + (F_Private | id), data = data_long3, REML = FALSE)
Model4 <- lmer(KnowledgeSharing ~ (F_Private)*F_Information*F_Governance + ZEducation + ZWE_PM + ZAge + NQ16.3_3 + (1| id), data = data_long4, REML = FALSE)

Anova(Model3)
Anova(Model4, type = "III")

summary(Model3)
summary(Model4)

#analysing data using LMER  - KS as DV and needs as mediators

Model3 <- lmer(KnowledgeSharing ~ F_Private*F_Importance*(NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + (1 | id), data = data_long3, REML = FALSE)
Model4 <- lmer(KnowledgeSharing  ~ F_Private*F_Importance*(NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ16.3_3 + (1 | id), data = data_long4, REML = FALSE)

Anova(Model3, type = "III")
Anova(Model4, type = "III")

#analysing data using LME  - KS as DV 

Model3 <- lme(KnowledgeSharing ~ F_Power*F_Governance*(F_Private + F_Importance) + ZEducation + ZWE_PM + ZAge + NQ15.3_3 , random= ~F_Private*F_Importance|id, data=data_long3) 
Model4 <- lme(KnowledgeSharing ~ F_Information*F_Governance*(F_Private+F_Importance) + ZEducation + ZWE_PM + ZAge + NQ16.3_3 , random= ~F_Private*F_Importance|id, data=data_long4) 

Anova(Model3)
Anova(Model4)

#analysing data uwith GLM model assuming for poisson distribution

mod10 = glm(blight ~ precip.m, data=dat, family="poisson")  
mod11 = glmer(KnowledgeSharing ~ F_Private*F_Importance*F_Governance*F_Information + ZEducation + ZWE_PM + ZAge + NQ16.3_3 + (F_Private | id) + (F_Importance | id), data=data_long4, family=poisson) 

summary(mod11)


#--Posthoc tests for interaction and plots---- #----------https://cran.r-project.org/web/packages/afex/vignettes/afex_anova_example.html#anova---------#

library(afex)     # needed for ANOVA functions.
library(emmeans)  # emmeans must now be loaded explicitly for follow-up tests.
library(multcomp) # for advanced control for multiple testing/Type 1 errors.
library(ggplot2)  # for customizing plots.
afex_options(emmeans_model = "multivariate") # use multivariate model for all follow-up tests.


a1 <- aov_ez("id", "KnowledgeSharing", data_long4, between = c("F_Governance", "F_Information"), within = c("F_Private", "F_Importance"))

knitr::kable(nice(a1))
print(xtable::xtable(anova(a1), digits = c(rep(2, 5), 3, 4)), type = "latex")

#Bellow functions are the equivalent calls (i.e., producing exactly the same output) of the other two ANOVA functions aov_car or aov4 is shown below.
aov_car(KnowledgeSharing ~ F_Governance*F_Information + Error(id/F_Private*F_Importance), data_long4)
aov_4(KnowledgeSharing ~ F_Governance*F_Information + (F_Private*F_Importance|id), data_long4)
#-----

m1 <- emmeans(a1, ~ F_Private*F_Importance)
#This object can now also be used to compare whether or not there are differences between the levels of the factor:
pairs(m1)
#To obtain more powerful p-value adjustments, we can furthermore pass it to multcomp (Bretz, Hothorn, & Westfall, 2011):
summary(as.glht(pairs(m1)), test=adjusted("free"))


m2 <- emmeans(a1, "F_Private", by = "F_Governance")
## equal: emmeans(a1, ~ F_Private|F_Governance)
pairs(m2)

#Basic Plots
afex_plot(a1, x = "F_Private", trace = "F_Information", panel = "F_Importance")
#------------------------------------------------------PART 5: CFA -----------------------------------------------------------#

# MODEL3: Direct effects

cfa3 <- '
NSupp_Aut =~ NQ15.2_2 + NQ15.2_8 + NQ15.2_10 + NQ15.2_13
NSupp_Com =~ NQ15.2_4 + NQ15.2_5 + NQ15.2_6 + NQ15.2_11
NSupp_Rel =~ NQ15.2_1 + NQ15.2_3 + NQ15.2_9 + NQ15.2_12


NSupp_Aut ~ a1*Governance + x1*HighPower + y1*LowPower + Education + WE_PM + Age + NQ15.3_3
NSupp_Com ~ a2*Governance + x2*HighPower + y2*LowPower + Education + WE_PM + Age+ NQ15.3_3
NSupp_Rel ~ a3*Governance + x3*HighPower + y3*LowPower + Education + WE_PM + Age + NQ15.3_3


Imp_Pri ~ c*Governance + z*HighPower + l*LowPower + b1*NSupp_Aut + b2*NSupp_Com + b3*NSupp_Rel + Education + WE_PM + Age + NQ15.3_3

#indirect effects
gov1aut_indirect := a1*b1
gov2com_indirect := a2*b2
gov3rel_indirect := a3*b3

HighPower1aut_indirect := x1*b1
HighPower2com_indirect := x2*b2
HighPower3rel_indirect := x3*b3


LowPower1aut_indirect := y1*b1
LowPower2com_indirect := y2*b2
LowPower3rel_indirect := y3*b3


#total effects
gov_total := c + a1*b1 + a2*b2 + a3*b3
HighPower_total := z + x1*b1 + x2*b2 + x3*b3
LowPower_total := l + y1*b1 + y2*b2 + y3*b3
'

fit.cfa3 <- sem(cfa3, estimator = "MLR", data=Exp3data2)
summary(fit.cfa3, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


# MODEL3: Direct effects with the quantity of knowledge

cfa3 <- '
NSupp_Aut =~ NQ15.2_2 + NQ15.2_8 + NQ15.2_10 + NQ15.2_13
NSupp_Com =~ NQ15.2_4 + NQ15.2_5 + NQ15.2_6 + NQ15.2_11
NSupp_Rel =~ NQ15.2_1 + NQ15.2_3 + NQ15.2_9 + NQ15.2_12


NSupp_Aut ~ a1*Governance + HighPower + LowPower + Education + WE_PM + Age + NQ15.3_3
NSupp_Com ~ a2*Governance + HighPower + LowPower + Education + WE_PM + Age+ NQ15.3_3
NSupp_Rel ~ a3*Governance + HighPower + LowPower + Education + WE_PM + Age + NQ15.3_3


KS ~ c*Governance + HighPower + LowPower + b1*NSupp_Aut + b2*NSupp_Com + b3*NSupp_Rel + Education + WE_PM + Age + NQ15.3_3

#indirect effects
gov1aut_indirect := a1*b1
gov2com_indirect := a2*b2
gov3rel_indirect := a3*b3

#total effects
gov_total := c + a1*b1 + a2*b2 + a3*b3

'

fit.cfa3 <- sem(cfa3, estimator = "MLR", data=Exp3data2)
summary(fit.cfa3, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


# MODEL3: Direct effects with tall types of knowledge

cfa3 <- '
NSupp_Aut =~ NQ15.2_2 + NQ15.2_8 + NQ15.2_10 + NQ15.2_13
NSupp_Com =~ NQ15.2_4 + NQ15.2_5 + NQ15.2_6 + NQ15.2_11
NSupp_Rel =~ NQ15.2_1 + NQ15.2_3 + NQ15.2_9 + NQ15.2_12


NSupp_Aut ~ a1*Governance + HighPower + LowPower + Education + WE_PM + Age + NQ15.3_3
NSupp_Com ~ a2*Governance + HighPower + LowPower + Education + WE_PM + Age+ NQ15.3_3
NSupp_Rel ~ a3*Governance + HighPower + LowPower + Education + WE_PM + Age + NQ15.3_3


Imp_Pri ~ c1*Governance + HighPower + LowPower + b1*NSupp_Aut + b2*NSupp_Com + b3*NSupp_Rel + Education + WE_PM + Age + NQ15.3_3
Imp_Pub ~ c2*Governance + HighPower + LowPower + l1*NSupp_Aut + l2*NSupp_Com + l3*NSupp_Rel + Education + WE_PM + Age + NQ15.3_3
Uni_Pri ~ c3*Governance + HighPower + LowPower + y1*NSupp_Aut + y2*NSupp_Com + y3*NSupp_Rel + Education + WE_PM + Age + NQ15.3_3
Uni_Pub ~ c4*Governance + HighPower + LowPower + z1*NSupp_Aut + z2*NSupp_Com + z3*NSupp_Rel + Education + WE_PM + Age + NQ15.3_3


#indirect effects
govIPaut_indirect := a1*b1
govIPcom_indirect := a2*b2
govIPrel_indirect := a3*b3

govIPUaut_indirect := a1*l1
govIPUcom_indirect := a2*l2
govIPUrel_indirect := a3*l3

govUPaut_indirect := a1*y1
govUPcom_indirect := a2*y2
govUPrel_indirect := a3*y3

govUPUaut_indirect := a1*z1
govUPUcom_indirect := a2*z2
govUPUrel_indirect := a3*z3


#total effects
govIP_total := c1 + a1*b1 + a2*b2 + a3*b3
govIPU_total := c2 + a1*l1 + a2*l2 + a3*l3
govUP_total := c3 + a1*y1 + a2*y2 + a3*y3
govUPU_total := c4 + a1*z1 + a2*z2 + a3*z3

'

fit.cfa3 <- sem(cfa3, estimator = "MLR", data=Exp3data2)
summary(fit.cfa3, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


# MODEL3: Direct effects with moderation

Exp3data2$LowInteraction <- Exp3data2$LowPower * Exp3data2$Governance
Exp3data2$HighInteraction <- Exp3data2$HighPower * Exp3data2$Governance

cfa3 <- '
NSupp_Aut =~ NQ15.2_2 + NQ15.2_8 + NQ15.2_10 + NQ15.2_13
NSupp_Com =~ NQ15.2_4 + NQ15.2_5 + NQ15.2_6 + NQ15.2_11
NSupp_Rel =~ NQ15.2_1 + NQ15.2_3 + NQ15.2_9 + NQ15.2_12


NSupp_Aut ~ a1*Governance + x1*HighPower + y1*LowPower + x1w*HighInteraction + y1w*LowInteraction + Education + WE_PM + Age + NQ15.3_3
NSupp_Com ~ a2*Governance + x2*HighPower + y2*LowPower + x2w*HighInteraction + y2w*LowInteraction + Education + WE_PM + Age+ NQ15.3_3
NSupp_Rel ~ a3*Governance + x3*HighPower + y3*LowPower +  x3w*HighInteraction + y3w*LowInteraction + Education + WE_PM + Age + NQ15.3_3


Imp_Pri ~ c*Governance + z*HighPower + l*LowPower + z1w*HighInteraction + l1w*LowInteraction + z2w*HighInteraction + l2w*LowInteraction + z3w*HighInteraction + l3w*LowInteraction + b1*NSupp_Aut + b2*NSupp_Com + b3*NSupp_Rel + Education + WE_PM + Age + NQ15.3_3

#(control)
#indirect effects 
gov1aut_indirect0 := a1*b1
gov2com_indirect0 := a2*b2
gov3rel_indirect0 := a3*b3

#total effects
gov_total0 := c + a1*b1 + a2*b2 + a3*b3

#(Low power)
#indirect effects 
gov1aut_indirect1 := a1*b1 + y1w
gov2com_indirect1 := a2*b2 + y2w
gov3rel_indirect1 := a3*b3 + y3w

#total effects
gov_total1 := c + a1*b1 + a2*b2 + a3*b3 + y1w*b1 + y2w*b2 + y3w*b3


#(High power)
#indirect effects 
gov1aut_indirect2 := a1*b1 + x1w
gov2com_indirect2 := a2*b2 + x2w
gov3rel_indirect2 := a3*b3 + x3w

#total effects
gov_total2 := c + a1*b1 + a2*b2 + a3*b3 + x1w*b1 + x2w*b2 + x3w*b3

'

fit.cfa3 <- sem(cfa3, estimator = "MLR", data=Exp3data2)
summary(fit.cfa3, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


# MODEL4: Direct effects

cfa4 <- '
NSupp_Aut =~ NQ16.2_2 + NQ16.2_8 + NQ16.2_10 + NQ16.2_13
NSupp_Com =~ NQ16.2_4 + NQ16.2_5 + NQ16.2_6 + NQ16.2_11
NSupp_Rel =~ NQ16.2_1 + NQ16.2_3 + NQ16.2_9 + NQ16.2_12


NSupp_Aut ~ a1*Governance + x1*HighInformation + y1*LowInformation + Education + WE_PM + Age + NQ16.3_3
NSupp_Com ~ a2*Governance + x2*HighInformation + y2*LowInformation + Education + WE_PM + Age+ NQ16.3_3
NSupp_Rel ~ a3*Governance + x3*HighInformation + y3*LowInformation + Education + WE_PM + Age + NQ16.3_3


Imp_Pri ~ c*Governance + z*HighInformation + l*LowInformation + b1*NSupp_Aut + b2*NSupp_Com + b3*NSupp_Rel + Education + WE_PM + Age + NQ16.3_3

#indirect effects
gov1aut_indirect := a1*b1
gov2com_indirect := a2*b2
gov3rel_indirect := a3*b3

HighInformation1aut_indirect := x1*b1
HighInformation2com_indirect := x2*b2
HighInformation3rel_indirect := x3*b3


LowInformation1aut_indirect := y1*b1
LowInformation2com_indirect := y2*b2
LowInformation3rel_indirect := y3*b3


#total effects
gov_total := c + a1*b1 + a2*b2 + a3*b3
HighInformation_total := z + x1*b1 + x2*b2 + x3*b3
LowInformation_total := l + y1*b1 + y2*b2 + y3*b3
'

fit.cfa4 <- sem(cfa4, estimator = "MLR", data=Exp4data2)
summary(fit.cfa4, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)

# MODEL4: Direct effects with all types of knowledge

cfa4 <- '
NSupp_Aut =~ NQ16.2_2 + NQ16.2_8 + NQ16.2_10 + NQ16.2_13
NSupp_Com =~ NQ16.2_4 + NQ16.2_5 + NQ16.2_6 + NQ16.2_11
NSupp_Rel =~ NQ16.2_1 + NQ16.2_3 + NQ16.2_9 + NQ16.2_12


NSupp_Aut ~ a1*Governance + HighInformation + LowInformation + Education + WE_PM + Age + NQ16.3_3
NSupp_Com ~ a2*Governance + HighInformation + LowInformation + Education + WE_PM + Age+ NQ16.3_3
NSupp_Rel ~ a3*Governance + HighInformation + LowInformation + Education + WE_PM + Age + NQ16.3_3


Imp_Pri ~ c1*Governance + HighInformation + LowInformation + b1*NSupp_Aut + b2*NSupp_Com + b3*NSupp_Rel + Education + WE_PM + Age + NQ16.3_3
Imp_Pub ~ c2*Governance + HighInformation + LowInformation + l1*NSupp_Aut + l2*NSupp_Com + l3*NSupp_Rel + Education + WE_PM + Age + NQ16.3_3
Uni_Pri ~ c3*Governance + HighInformation + LowInformation + y1*NSupp_Aut + y2*NSupp_Com + y3*NSupp_Rel + Education + WE_PM + Age + NQ16.3_3
Uni_Pub ~ c4*Governance + HighInformation + LowInformation + z1*NSupp_Aut + z2*NSupp_Com + z3*NSupp_Rel + Education + WE_PM + Age + NQ16.3_3



#indirect effects
govIPaut_indirect := a1*b1
govIPcom_indirect := a2*b2
govIPrel_indirect := a3*b3

govIPUaut_indirect := a1*l1
govIPUcom_indirect := a2*l2
govIPUrel_indirect := a3*l3

govUPaut_indirect := a1*y1
govUPcom_indirect := a2*y2
govUPrel_indirect := a3*y3

govUPUaut_indirect := a1*z1
govUPUcom_indirect := a2*z2
govUPUrel_indirect := a3*z3


#total effects
govIP_total := c1 + a1*b1 + a2*b2 + a3*b3
govIPU_total := c2 + a1*l1 + a2*l2 + a3*l3
govUP_total := c3 + a1*y1 + a2*y2 + a3*y3
govUPU_total := c4 + a1*z1 + a2*z2 + a3*z3
'

fit.cfa4 <- sem(cfa4, estimator = "MLR", data=Exp4data2)
summary(fit.cfa4, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)

# MODEL4: Direct effects with quantity of knowledge

cfa4 <- '
NSupp_Aut =~ NQ16.2_2 + NQ16.2_8 + NQ16.2_10 + NQ16.2_13
NSupp_Com =~ NQ16.2_4 + NQ16.2_5 + NQ16.2_6 + NQ16.2_11
NSupp_Rel =~ NQ16.2_1 + NQ16.2_3 + NQ16.2_9 + NQ16.2_12


NSupp_Aut ~ a1*Governance + HighInformation + LowInformation + Education + WE_PM + Age + NQ16.3_3
NSupp_Com ~ a2*Governance + HighInformation + LowInformation + Education + WE_PM + Age+ NQ16.3_3
NSupp_Rel ~ a3*Governance + HighInformation + LowInformation + Education + WE_PM + Age + NQ16.3_3


KS ~ c1*Governance + HighInformation + LowInformation + b1*NSupp_Aut + b2*NSupp_Com + b3*NSupp_Rel + Education + WE_PM + Age + NQ16.3_3


#indirect effects
govaut_indirect := a1*b1
govcom_indirect := a2*b2
govrel_indirect := a3*b3



#total effects
gov_total := c1 + a1*b1 + a2*b2 + a3*b3

'

fit.cfa4 <- sem(cfa4, estimator = "MLR", data=Exp4data2)
summary(fit.cfa4, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


# MODEL4: Direct effects with moderation

Exp4data2$LowInteraction <- Exp4data2$LowInformation * Exp4data2$Governance
Exp4data2$HighInteraction <- Exp4data2$HighInformation * Exp4data2$Governance

cfa4 <- '
NSupp_Aut =~ NQ16.2_2 + NQ16.2_8 + NQ16.2_10 + NQ16.2_13
NSupp_Com =~ NQ16.2_4 + NQ16.2_5 + NQ16.2_6 + NQ16.2_11
NSupp_Rel =~ NQ16.2_1 + NQ16.2_3 + NQ16.2_9 + NQ16.2_12


NSupp_Aut ~ a1*Governance + x1*HighInformation + y1*LowInformation + x1w*HighInteraction + y1w*LowInteraction + Education + WE_PM + Age + NQ16.3_3
NSupp_Com ~ a2*Governance + x2*HighInformation + y2*LowInformation + x2w*HighInteraction + y2w*LowInteraction + Education + WE_PM + Age+ NQ16.3_3
NSupp_Rel ~ a3*Governance + x3*HighInformation + y3*LowInformation +  x3w*HighInteraction + y3w*LowInteraction + Education + WE_PM + Age + NQ16.3_3


Imp_Pri ~ c*Governance + z*HighInformation + l*LowInformation + z1w*HighInteraction + l1w*LowInteraction + z2w*HighInteraction + l2w*LowInteraction + z3w*HighInteraction + l3w*LowInteraction + b1*NSupp_Aut + b2*NSupp_Com + b3*NSupp_Rel + Education + WE_PM + Age + NQ16.3_3

#(control)
#indirect effects 
gov1aut_indirect0 := a1*b1
gov2com_indirect0 := a2*b2
gov3rel_indirect0 := a3*b3

#total effects
gov_total0 := c + a1*b1 + a2*b2 + a3*b3

#(Low Information)
#indirect effects 
gov1aut_indirect1 := a1*b1 + y1w
gov2com_indirect1 := a2*b2 + y2w
gov3rel_indirect1 := a3*b3 + y3w

#total effects
gov_total1 := c + a1*b1 + a2*b2 + a3*b3 + y1w*b1 + y2w*b2 + y3w*b3


#(High Information)
#indirect effects 
gov1aut_indirect2 := a1*b1 + x1w
gov2com_indirect2 := a2*b2 + x2w
gov3rel_indirect2 := a3*b3 + x3w

#total effects
gov_total2 := c + a1*b1 + a2*b2 + a3*b3 + x1w*b1 + x2w*b2 + x3w*b3

'

fit.cfa4 <- sem(cfa4, estimator = "MLR", data=Exp4data2)
summary(fit.cfa4, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)


