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
