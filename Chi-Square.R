pima_indians_diabetes <- read.csv(file.choose())
pima_indians_diabetes
pima_indians_diabetes = pima_indians_diabetes[1:768,1:9]
pima_indians_diabetes
library(stats)
library(dplyr)
pima_indians_diabetes[,1:8] <- replace(pima_indians_diabetes[,1:8],pima_indians_diabetes[,1:8]==0,NA)
pima_indians_diabetes
pima_indians_diabetes <- data.frame(
  sapply(
    pima_indians_diabetes,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))
pima_indians_diabetes
chisq.test(pima_indians_diabetes)
table(pima_indians_diabetes$NumberOfTimesPregnant, pima_indians_diabetes$ClassVariable)
chisq.test(pima_indians_diabetes$NumberOfTimesPregnant, pima_indians_diabetes$ClassVariable, correct=FALSE)
table(pima_indians_diabetes$PlasmaGlucoseConcentration, pima_indians_diabetes$ClassVariable)
chisq.test(pima_indians_diabetes$PlasmaGlucoseConcentration, pima_indians_diabetes$ClassVariable, correct=FALSE)
table(pima_indians_diabetes$DiastolicBloodPressure, pima_indians_diabetes$ClassVariable)
chisq.test(pima_indians_diabetes$DiastolicBloodPressure, pima_indians_diabetes$ClassVariable, correct=FALSE)
table(pima_indians_diabetes$TricepsSkinFoldThickness, pima_indians_diabetes$ClassVariable)
chisq.test(pima_indians_diabetes$TricepsSkinFoldThickness, pima_indians_diabetes$ClassVariable, correct=FALSE)
table(pima_indians_diabetes$SerumInsulin, pima_indians_diabetes$ClassVariable)
chisq.test(pima_indians_diabetes$SerumInsulin, pima_indians_diabetes$ClassVariable, correct=FALSE)
table(pima_indians_diabetes$BodyMassIndex, pima_indians_diabetes$ClassVariable)
chisq.test(pima_indians_diabetes$BodyMassIndex, pima_indians_diabetes$ClassVariable, correct=FALSE)
table(pima_indians_diabetes$DiabetesPedigreeFunction, pima_indians_diabetes$ClassVariable)
chisq.test(pima_indians_diabetes$DiabetesPedigreeFunction, pima_indians_diabetes$ClassVariable, correct=FALSE)
table(pima_indians_diabetes$Age, pima_indians_diabetes$ClassVariable)
chisq.test(pima_indians_diabetes$Age, pima_indians_diabetes$ClassVariable, correct=FALSE)


