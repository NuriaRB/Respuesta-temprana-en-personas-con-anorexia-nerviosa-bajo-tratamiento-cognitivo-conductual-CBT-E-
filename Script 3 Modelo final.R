#base de datos en: https://osf.io/esgzb/ 
load("R TFG/dat.rda")
bob<-dat
library(lavaan)
library(semTools)
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

rm(bmiMn,bmiSd,bmivars,edeqMn,edeqSd,edeqvars)

#Cambio de nombre para el código ##################

names(bob)[names(bob) == 'edeq_0_std'] <- 'oy1'
names(bob)[names(bob) == 'edeq_1_std'] <- 'oy2'
names(bob)[names(bob) == 'edeq_2_std'] <- 'oy3'

names(bob)[names(bob) == 'bmi_0_std'] <- 'ox1'
names(bob)[names(bob) == 'bmi_5_std'] <- 'ox2'
names(bob)[names(bob) == 'bmi_10_std'] <- 'ox3'

#Modelo 5 #########################################
LCSM5 = "
# Declaring latent level
y1	=~ 1* oy1
y2	=~ 1* oy2
y3	=~ 1* oy3

x1	=~ 1* ox1
x2	=~ 1* ox2
x3	=~ 1* ox3

# Auto-regression
y2	~ 1* y1
y3	~ 1* y2

x2	~ 1* x1
x3	~ 1* x2

# Define latent change
dy2	 =~ 1* y2
dy3	 =~ 1* y3

dx2	 =~ 1* x2
dx3	 =~ 1* x3

# Auto-proportions
dy2	 ~ b_y * y1
dy3	 ~ b_y * y2

dx2	 ~ b_x * x1
dx3	 ~ b_x * x2

# Couplings
dy2	 ~ g_y * x1
dy3	 ~ g_y * x2

dx2	 ~ g_x * y1
dx3	 ~ g_x * y2

#Phi (autorregresion del cambio) 
dy3	 ~ p_y * dy2

dx3	 ~ p_x * dx2

#Ksi (coupling del cambio)
dy3	 ~ k_y * dx2

dx3	 ~ k_x * dy2

# Latent intercepts and slopes
yInt =~ 1 * y1
ySlp =~ 1*dy2  + 1*dy3 

xInt =~ 1 * x1
xSlp =~ 1*dx2  + 1*dx3 

# Latent means
yInt ~ yInMn * 1
ySlp ~ ySlMn * 1

xInt ~ xInMn * 1
xSlp ~ xSlMn * 1

# Observed means fixed at 0
oy1 ~ 0 * 1
oy2 ~ 0 * 1
oy3 ~ 0 * 1

ox1 ~ 0 * 1
ox2 ~ 0 * 1
ox3 ~ 0 * 1

#M5 liberación de varianzas y covarianzas xy del componente aditivo
# Latent variances and covariances
yInt ~~ yInV   * yInt #varianza intersección Y
ySlp ~~ ySlV   * ySlp #varianza pendiente Y
yInt ~~ 0* ySlp #covarianza int slp y

xInt ~~ xInV   * xInt #varianza intersección X
xSlp ~~ xSlpV * xSlp #varianza pendiente X
xInt ~~ 0 * xSlp #covarianza int slp x

yInt ~~ xyIntCv * xInt
ySlp ~~ xySlpCv * xSlp
yInt ~~ 0 * xSlp
xInt ~~ 0 * ySlp

#Errores (varianzas) de latentes
y1  ~~ 0 * y1
y2  ~~ 0 * y2
y3  ~~ 0 * y3

x1  ~~ 0 * x1
x2  ~~ 0 * x2
x3  ~~ 0 * x3

# Dynamic errors
dy2  ~~ 0 * dy2
dy3  ~~ 0 * dy3
dx2  ~~ 0 * dx2
dx3  ~~ 0 * dx3
# Covariance between dynamic errors
dy2 ~~ 0 * dx2
dy3 ~~ 0 * dx3

#Error de medida observada (MerX, MerY)
oy1  ~~ MerY * oy1
oy2  ~~ MerY * oy2
oy3  ~~ MerY * oy3

ox1  ~~ MerX * ox1
ox2  ~~ MerX * ox2
ox3  ~~ MerX * ox3

#Covarianza de errores de medida observada (MerXY)
oy1  ~~ MerXY * ox1
oy2  ~~ MerXY * ox2
oy3  ~~ MerXY * ox3

"
# Fit model
LCSM5.fit = lavaan(LCSM5, data=bob, missing="ML")
LCSM5.sum <- summary(LCSM5.fit, fit.measures=T) # Obtain summary
LCSM5.pars <- LCSM5.sum$PE
LCSM5.pars <- LCSM5.pars[!is.na(LCSM5.pars$z),]
LCSM5.pars <- LCSM5.pars[!duplicated(round(LCSM5.pars$est,5)),]
LCSM5.pars
fitmeasures(LCSM5.fit)

cbind(LCSM5.pars[c("lhs", "op", "rhs", "label")],
      round(LCSM5.pars[c("est", "se", "z", "pvalue")], 3))
#Modelo 5 Optimizado (M5B) #########################
LCSM5B = "
# Declaring latent level
y1	=~ 1* oy1
y2	=~ 1* oy2
y3	=~ 1* oy3

x1	=~ 1* ox1
x2	=~ 1* ox2
x3	=~ 1* ox3

# Auto-regression
y2	~ 1* y1
y3	~ 1* y2

x2	~ 1* x1
x3	~ 1* x2

# Define latent change
dy2	 =~ 1* y2
dy3	 =~ 1* y3

dx2	 =~ 1* x2
dx3	 =~ 1* x3

# Auto-proportions
dy2	 ~ b_y * y1
dy3	 ~ b_y * y2

dx2	 ~ b_x * x1
dx3	 ~ b_x * x2

# Couplings RESTRINGIDOS
dy2	 ~ 0 * x1
dy3	 ~ 0 * x2

dx2	 ~ 0 * y1
dx3	 ~ 0 * y2

#Phi (autorregresion del cambio) 
dy3	 ~ p_y * dy2

dx3	 ~ p_x * dx2

#RESTRICCION
#Ksi (coupling del cambio)
dy3	 ~ 0 * dx2

dx3	 ~ 0 * dy2

# Latent intercepts and slopes
yInt =~ 1 * y1
ySlp =~ 1*dy2  + 1*dy3 

xInt =~ 1 * x1
xSlp =~ 1*dx2  + 1*dx3 

# Latent means
yInt ~ yInMn * 1
ySlp ~ ySlMn * 1

xInt ~ xInMn * 1
xSlp ~ xSlMn * 1

# Observed means fixed at 0
oy1 ~ 0 * 1
oy2 ~ 0 * 1
oy3 ~ 0 * 1

ox1 ~ 0 * 1
ox2 ~ 0 * 1
ox3 ~ 0 * 1

#M5 liberación de varianzas y covarianzas xy del componente aditivo
# Latent variances and covariances
yInt ~~ yInV   * yInt #varianza intersección Y
ySlp ~~ ySlV   * ySlp #varianza pendiente Y
yInt ~~ 0* ySlp #covarianza int slp y

xInt ~~ xInV   * xInt #varianza intersección X
xSlp ~~ xSlpV * xSlp #varianza pendiente X
xInt ~~ 0 * xSlp #covarianza int slp x

yInt ~~ xyIntCv * xInt
ySlp ~~ xySlpCv * xSlp
yInt ~~ 0 * xSlp
xInt ~~ 0 * ySlp

#Errores (varianzas) de latentes
y1  ~~ 0 * y1
y2  ~~ 0 * y2
y3  ~~ 0 * y3

x1  ~~ 0 * x1
x2  ~~ 0 * x2
x3  ~~ 0 * x3

# Dynamic errors
dy2  ~~ 0 * dy2
dy3  ~~ 0 * dy3
dx2  ~~ 0 * dx2
dx3  ~~ 0 * dx3
# Covariance between dynamic errors
dy2 ~~ 0 * dx2
dy3 ~~ 0 * dx3

#Error de medida observada (MerX, MerY)
oy1  ~~ MerY * oy1
oy2  ~~ MerY * oy2
oy3  ~~ MerY * oy3

ox1  ~~ MerX * ox1
ox2  ~~ MerX * ox2
ox3  ~~ MerX * ox3

#Covarianza de errores de medida observada (MerXY)
oy1  ~~ MerXY * ox1
oy2  ~~ MerXY * ox2
oy3  ~~ MerXY * ox3

"
# Fit model
LCSM5B.fit = lavaan(LCSM5B, data=bob, missing="ML")
LCSM5B.sum <- summary(LCSM5B.fit, fit.measures=T) # Obtain summary
LCSM5B.pars <- LCSM5B.sum$PE
LCSM5B.pars <- LCSM5B.pars[!is.na(LCSM5B.pars$z),]
LCSM5B.pars <- LCSM5B.pars[!duplicated(round(LCSM5B.pars$est,5)),]
LCSM5B.pars
fitmeasures(LCSM5B.fit)

cbind(LCSM5B.pars[c("lhs", "op", "rhs", "label")],
      round(LCSM5B.pars[c("est", "se", "z", "pvalue")], 3))

estimacionM5B <- cbind(LCSM5B.pars[c("lhs", "op", "rhs", "label")],
                       round(LCSM5B.pars[c("est", "se", "z", "pvalue")], 3))
#Comparación M5 y M5B ################
modelL5 <- list()
modelL5$m5 <- LCSM5
modelL5$m5B <- LCSM5B
fitL5 <- list()
fitL5$m5 <- lavaan(data=bob, modelL5$m5, missing="ML")
fitL5$m5B <- lavaan(data=bob, modelL5$m5B, missing="ML")
summ_list <- list()
fitms <- c("npar","logl","aic",
           "chisq", "df", "pvalue",
           "rmsea", "cfi", "srmr")
fitmeasL5 <- list()
fitmeasL5$m5 <- fitmeasures(fitL5$m5, fit.measures = fitms)
fitmeasL5$m5B <- fitmeasures(fitL5$m5B, fit.measures = fitms)

sum55b <- cbind(round(fitmeasL5$m5,3), round(fitmeasL5$m5B,3))
sum55b 
comp55b <- compareFit(fitL5$m5, fitL5$m5B, nested=TRUE) 
comp55b

#------ Resumen de resultados de todos los analisis -------
#__________________________________________________________

#indices de ajuste de modelo 5 y 5 optimizado
sum55b
#comparación de modelos 5 y 5 optimizado
comp55b
#estimación de parámetros modelo 5 optimizado
estimacionM5B