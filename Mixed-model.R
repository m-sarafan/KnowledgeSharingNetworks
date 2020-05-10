#------------------------------------------------------PART 4: WITHIN_SUBJECT------------------------------------------------------------#

library(tidyverse)
library(ggpubr)
library(rstatix)
library(lme4)
library(nlme)

#adding an id columns
Exp3data3 <- mutate(Exp3data2, id = rownames(Exp3data2))
Exp3data3$id

#adding a categorical KS variable (similar to t1, t2, t3)

data_long3 <- gather(Exp3data3, Quality, KnowledgeSharing, Imp_Pri, Imp_Pub, Uni_Pri, Uni_Pub, factor_key=TRUE)
data_long3[, c("id", "Quality", "KnowledgeSharing")]

data_long3$F_Quality <- as.factor(data_long3$Quality)
contrasts(data_long3$F_Quality) <- contr.Helmert(4)

#adding a Sharedness and Importance as two within-subject variables

data_long3$Private <- ifelse(data_long3$Quality == "Imp_Pub" | data_long3$Quality == "Uni_Pub" , 0, 1)
data_long3$F_Private <- as.factor(data_long3$Private)
contrasts(data_long3$F_Private) <- contr.treatment(2)

data_long3$Importance <- ifelse(data_long3$Quality == "Imp_Pri" | data_long3$Quality == "Imp_Pub" , 1, 0)
data_long3$F_Importance <- as.factor(data_long3$Importance)
contrasts(data_long3$F_Importance) <- contr.treatment(2)

#analysing data using normal ANOVA model with (error) - KS as DV and Quality as IV

Model3 <- aov(KnowledgeSharing ~ F_Quality*F_Governance*F_Power + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + Error(id/F_Quality) + F_Governance*F_Power, data = data_long3)

#analysing data using normal ANOVA model with (error) - KS as DV and Private and Importance as IV

Model3 <- aov(KnowledgeSharing ~ (F_Private + F_Importance)*F_Power*F_Governance + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + Error(id/F_Private*F_Importance), data = data_long3)

#analysing data using normal ANOVA model with (error) - KS as DV and needs as mediators

Model3 <- aov(KnowledgeSharing ~ (F_Private+F_Importance)*(NSupp_Aut + NSupp_Com + NSupp_Rel) + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + Error(id/F_Private*F_Importance), data = data_long3)

#analysing data using normal ANOVA model with (error) - Needs as DV 
Model3 <- aov(NSupp ~  F_Governance*F_Power + ZEducation + ZWE_PM + ZAge + NQ15.3_3 + Error(id/F_Private*F_Importance), data = data_long3)