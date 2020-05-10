#------------------------------------------------------PART 3: ANALYSES------------------------------------------------------------#

# Running the first part of the model: the effect of governance and asymmetry on needs satisfaction

Model3 <- manova(cbind(NSupp_Aut, NSupp_Com, NSupp_Rel) ~ F_Governance*F_Power + ZEducation + ZWE_PM + ZAge + ZNQ15.3_3, data = Exp3data2)

# Running the first part of the model: the effect of governance and asymmetry on needs, individually

Model3 <- aov(NSupp ~ F_Power + F_Governance + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)

# Running the second part of the model: the effect of needs and asymmetry on knowledge provision

Model3 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~ F_Power*F_Governance + ZEducation + ZWE + ZAge + FirmSize, data = Exp3data2)

# Running the second part of the model:the effect of needs and asymmetry on Quantity

Model3 <- aov(KS ~ F_Power * (NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)

# Running the second part of the model:the effect of needs on Imp_Pri

Model3 <- aov(Imp_Pri ~ F_Power*(NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ15.3_3, data = Exp3data2)

# Running the first part of the model: the effect of governance and asymmetry on mutual goals

Model3 <- manova(cbind(CooperationGoals, CompetitionGoals, IndependentGoals) ~ F_Governance*F_Power + ZEducation + ZWE_PM + ZAge + ZNQ15.3_3, data = Exp3data2)

# Running the second part of the model: the effect of goals on ks

Model3 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~ (CooperationGoals + CompetitionGoals + IndependentGoals)*F_Power + ZEducation + ZWE + ZAge + NQ15.3_3, data = Exp3data2)

# Running the second part of the model: the effect of needs on ks

Model3 <- manova(cbind(Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub) ~ ZNSupp_Aut + ZNSupp_Com + ZNSupp_Rel + ZEducation + ZWE + ZAge + NQ15.3_3, data = Exp3data2)

summary(Model1, test="Pillai")
summary.aov(Model1)