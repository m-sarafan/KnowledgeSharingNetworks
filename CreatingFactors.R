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

###-----Manipulations-----### 
###  (P = PMO governance/0; S = Shared governance/1) 
for(i in 1:ncol(Exp3data2)){
     Exp3data2[i][is.na(Exp3data2[i])] <- 0
}

Exp3data2$Governance <- ifelse(Exp3data2$Scenario_SC ==1 | Exp3data2$Scenario_SL ==1  | Exp3data2$Scenario_SH ==1 , 1, 0)
Exp3data2$Power <- ifelse(Exp3data2$Scenario_SH ==1 | Exp3data2$Scenario_NH ==1 , 2, ifelse(Exp3data2$Scenario_SL ==1 | Exp3data2$Scenario_NL ==1 , 1, 0))

Exp3data2$HighPower <- ifelse(Exp3data2$Scenario_SH ==1 | Exp3data2$Scenario_NH ==1 , 1, 0)
Exp3data2$LowPower <- ifelse(Exp3data2$Scenario_SL ==1 | Exp3data2$Scenario_NL ==1, 1, 0)


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

