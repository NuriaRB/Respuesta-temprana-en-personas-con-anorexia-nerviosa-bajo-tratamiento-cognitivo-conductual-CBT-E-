#base de datos en: https://osf.io/esgzb/ 
load("R TFG/dat.rda")
bob<-dat
#Carga de paquetes
library(psych)

#Descriptivos y correlaciones
summary(bob[c("age","sex")])
describe(bob$age)
describe(bob[c(paste0("edeq_", c(0:2)),paste0("bmi_", c(0, 5, 10)))]) [c("n", "mean", "sd","skew")]
cor(bob[c(paste0("edeq_", c(0:2)),paste0("bmi_", c(0, 5, 10)))], use="pairwise.complete.obs")
cor.test(bob$bmi_0, bob$edeq_0, method="pearson", conf.level = 0.95)
cor.test(bob$bmi_5, bob$edeq_1, method="pearson", conf.level = 0.95)
cor.test(bob$bmi_10, bob$edeq_2, method="pearson", conf.level = 0.95)