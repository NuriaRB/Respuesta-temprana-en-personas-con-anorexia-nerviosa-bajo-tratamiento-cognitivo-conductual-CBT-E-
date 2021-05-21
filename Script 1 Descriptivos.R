#base de datos en: https://osf.io/esgzb/ 
load("R TFG/dat.rda")
bob<-dat
#Carga de paquetes
library(psych)

#Estandarización de variables######################
#BMI
bmiMn <- mean(bob$bmi_0, na.rm = TRUE) 
bmiSd <- sd(bob$bmi_0, na.rm = TRUE)

bmivars <- names(bob)[grepl("bmi", names(dat))] 
newbmivars <- paste0(bmivars, "_std") 

bob[newbmivars] <- (bob[bmivars] - bmiMn)/bmiSd 

#EDEQ
edeqMn <- mean(bob$edeq_0, na.rm = TRUE) 
edeqSd <- sd(bob$edeq_0, na.rm = TRUE)

edeqvars <- paste0("edeq_", c(0:4)) 
newedeqvars <- paste0(edeqvars, "_std") 

bob[newedeqvars] <- (bob[edeqvars] - edeqMn)/edeqSd 

#Descriptivos y correlaciones#####################
summary(bob[c("age","sex")])
describe(bob$age)

describe(bob[c("edeq_0_std","edeq_1_std", "edeq_2_std")]) [c("n", "mean", "sd","skew")]
describe(bob[c("bmi_0_std","bmi_5_std", "bmi_10_std")]) [c("n", "mean", "sd","skew")]

cor.test(bob$bmi_0_std, bob$edeq_0_std, method="pearson", conf.level = 0.95)
cor.test(bob$bmi_5_std, bob$edeq_1_std, method="pearson", conf.level = 0.95)
cor.test(bob$bmi_10_std, bob$edeq_2_std, method="pearson", conf.level = 0.95)
