###-----DATA IMPORT/EXPORT-----###
Exp3data <- read.spss("GovernancePower.sav", use.value.labels = T, to.data.frame = T)
head(Exp3data)

#Making factor variables numeric
for(i in 1:ncol(Exp3data)) {
     if(is.factor(Exp3data[,i]) == TRUE) {
          Exp3data[paste('N', colnames(Exp3data[i]), sep="")] <- sapply(Exp3data[,i], as.numeric)
     }
}