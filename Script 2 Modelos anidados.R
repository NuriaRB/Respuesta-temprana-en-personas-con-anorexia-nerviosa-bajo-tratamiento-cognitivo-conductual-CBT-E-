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

#Modelo 0/Base ####################################
LCSM0 = "
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
dy2	 ~ 0 * y1
dy3	 ~ 0 * y2

dx2	 ~ 0 * x1
dx3	 ~ 0 * x2

# Couplings
dy2	 ~ 0 * x1
dy3	 ~ 0 * x2

dx2	 ~ 0 * y1
dx3	 ~ 0 * y2

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

# Latent variances and covariances
yInt ~~ yInV * yInt #varianza intersección Y
ySlp ~~ 0 * ySlp #varianza pendiente Y
yInt ~~ 0 * ySlp #covarianza int slp y

xInt ~~ xInV * xInt #varianza intersección X
xSlp ~~ 0 * xSlp #varianza pendiente X
xInt ~~ 0 * xSlp #covarianza int slp x

yInt ~~ xyIntCv * xInt
ySlp ~~ 0 * xSlp
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
LCSM0.fit = lavaan(LCSM0, data=bob, missing="ML")
LCSM0.sum <- summary(LCSM0.fit, fit.measures=T) # Obtain summary
LCSM0.pars <- LCSM0.sum$PE
LCSM0.pars <- LCSM0.pars[!is.na(LCSM0.pars$z),]
LCSM0.pars <- LCSM0.pars[!duplicated(round(LCSM0.pars$est,5)),]
LCSM0.pars
fitmeasures(LCSM0.fit)

cbind(LCSM0.pars[c("lhs", "op", "rhs", "label")],
      round(LCSM0.pars[c("est", "se", "z", "pvalue")], 3))

#Modelo 1 #########################################
LCSM1 = "
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

#M1 Estimación de parámetro de autorregresion nivel-cambio
# Auto-proportions
dy2	 ~ b_y * y1
dy3	 ~ b_y * y2

dx2	 ~ b_x * x1
dx3	 ~ b_x * x2

# Couplings
dy2	 ~ 0 * x1
dy3	 ~ 0 * x2

dx2	 ~ 0 * y1
dx3	 ~ 0 * y2

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

# Latent variances and covariances
yInt ~~ yInV * yInt #varianza intersección Y
ySlp ~~ 0 * ySlp #varianza pendiente Y
yInt ~~ 0 * ySlp #covarianza int slp y

xInt ~~ xInV * xInt #varianza intersección X
xSlp ~~ 0 * xSlp #varianza pendiente X
xInt ~~ 0 * xSlp #covarianza int slp x

yInt ~~ xyIntCv * xInt
ySlp ~~ 0 * xSlp
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
LCSM1.fit = lavaan(LCSM1, data=bob, missing="ML")
LCSM1.sum <- summary(LCSM1.fit, fit.measures=T) # Obtain summary
LCSM1.pars <- LCSM1.sum$PE
LCSM1.pars <- LCSM1.pars[!is.na(LCSM1.pars$z),]
LCSM1.pars <- LCSM1.pars[!duplicated(round(LCSM1.pars$est,5)),]
LCSM1.pars
fitmeasures(LCSM1.fit)

cbind(LCSM1.pars[c("lhs", "op", "rhs", "label")],
      round(LCSM1.pars[c("est", "se", "z", "pvalue")], 3))

#Modelo 2 #########################################
LCSM2 = "
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

#M2 Estimación de parámetro de emparejamiento nivel-cambio
# Couplings
dy2	 ~ g_y * x1
dy3	 ~ g_y * x2

dx2	 ~ g_x * y1
dx3	 ~ g_x * y2

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

# Latent variances and covariances
yInt ~~ yInV * yInt #varianza intersección Y
ySlp ~~ 0 * ySlp #varianza pendiente Y
yInt ~~ 0 * ySlp #covarianza int slp y

xInt ~~ xInV * xInt #varianza intersección X
xSlp ~~ 0 * xSlp #varianza pendiente X
xInt ~~ 0 * xSlp #covarianza int slp x

yInt ~~ xyIntCv * xInt
ySlp ~~ 0 * xSlp
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
LCSM2.fit = lavaan(LCSM2, data=bob, missing="ML")
LCSM2.sum <- summary(LCSM2.fit, fit.measures=T) # Obtain summary
LCSM2.pars <- LCSM2.sum$PE
LCSM2.pars <- LCSM2.pars[!is.na(LCSM2.pars$z),]
LCSM2.pars <- LCSM2.pars[!duplicated(round(LCSM2.pars$est,5)),]
LCSM2.pars
fitmeasures(LCSM2.fit)

cbind(LCSM2.pars[c("lhs", "op", "rhs", "label")],
      round(LCSM2.pars[c("est", "se", "z", "pvalue")], 3))

#Modelo 3 #########################################
LCSM3 = "
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

#M3 estimación de parámetro de autorregresion del cambio
#Phi (autorregresion del cambio) 
dy3	 ~ p_y * dy2

dx3	 ~ p_x * dx2

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

# Latent variances and covariances
yInt ~~ yInV * yInt #varianza intersección Y
ySlp ~~ 0 * ySlp #varianza pendiente Y
yInt ~~ 0 * ySlp #covarianza int slp y

xInt ~~ xInV * xInt #varianza intersección X
xSlp ~~ 0 * xSlp #varianza pendiente X
xInt ~~ 0 * xSlp #covarianza int slp x

yInt ~~ xyIntCv * xInt
ySlp ~~ 0 * xSlp
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
LCSM3.fit = lavaan(LCSM3, data=bob, missing="ML")
LCSM3.sum <- summary(LCSM3.fit, fit.measures=T) # Obtain summary
LCSM3.pars <- LCSM3.sum$PE
LCSM3.pars <- LCSM3.pars[!is.na(LCSM3.pars$z),]
LCSM3.pars <- LCSM3.pars[!duplicated(round(LCSM3.pars$est,5)),]
LCSM3.pars
fitmeasures(LCSM3.fit)

cbind(LCSM3.pars[c("lhs", "op", "rhs", "label")],
      round(LCSM3.pars[c("est", "se", "z", "pvalue")], 3))

#Modelo 4 #########################################
LCSM4 = "
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

#M4 estimación de parámetro de emperejamiento cambio-cambio
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

# Latent variances and covariances
yInt ~~ yInV * yInt #varianza intersección Y
ySlp ~~ 0 * ySlp #varianza pendiente Y
yInt ~~ 0 * ySlp #covarianza int slp y

xInt ~~ xInV * xInt #varianza intersección X
xSlp ~~ 0 * xSlp #varianza pendiente X
xInt ~~ 0 * xSlp #covarianza int slp x

yInt ~~ xyIntCv * xInt
ySlp ~~ 0 * xSlp
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
LCSM4.fit = lavaan(LCSM4, data=bob, missing="ML")
LCSM4.sum <- summary(LCSM4.fit, fit.measures=T) # Obtain summary
LCSM4.pars <- LCSM4.sum$PE
LCSM4.pars <- LCSM4.pars[!is.na(LCSM4.pars$z),]
LCSM4.pars <- LCSM4.pars[!duplicated(round(LCSM4.pars$est,5)),]
LCSM4.pars
fitmeasures(LCSM4.fit)

cbind(LCSM4.pars[c("lhs", "op", "rhs", "label")],
      round(LCSM4.pars[c("est", "se", "z", "pvalue")], 3))

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

#Modelo 6 #########################################
LCSM6 = "
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

# Latent variances and covariances
yInt ~~ yInV   * yInt #varianza intersección Y
ySlp ~~ ySlV   * ySlp #varianza pendiente Y
yInt ~~ yInSlCv* ySlp #covarianza int slp y

xInt ~~ xInV   * xInt #varianza intersección X
xSlp ~~ xSlpV * xSlp #varianza pendiente X
xInt ~~ xInSlCv * xSlp #covarianza int slp x

#M6 Liberación de covarianzas:
yInt ~~ yxInCv   * xInt
ySlp ~~ xySlCv * xSlp
yInt ~~ yIntxSlCv * xSlp
xInt ~~ xInySlCv   * ySlp

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
LCSM6.fit = lavaan(LCSM6, data=bob, missing="ML")
LCSM6.sum <- summary(LCSM6.fit, fit.measures=T) # Obtain summary
LCSM6.pars <- LCSM6.sum$PE
LCSM6.pars <- LCSM6.pars[!is.na(LCSM6.pars$z),]
LCSM6.pars <- LCSM6.pars[!duplicated(round(LCSM6.pars$est,5)),]
LCSM6.pars
fitmeasures(LCSM6.fit)

cbind(LCSM6.pars[c("lhs", "op", "rhs", "label")],
      round(LCSM6.pars[c("est", "se", "z", "pvalue")], 3))

#lavaan WARNING: The variance-covariance matrix of the estimated parameters (vcov)
#does not appear to be positive definite

#Agrupación de resultados. Comparación de modelos #########################

modelL <- list()
modelL$m0 <- LCSM0
modelL$m1 <- LCSM1
modelL$m2 <- LCSM2
modelL$m3 <- LCSM3
modelL$m4 <- LCSM4
modelL$m5 <- LCSM5
modelL$m6 <- LCSM6

fitL <- list()
fitL$m0 <- lavaan(data=bob, modelL$m0, missing="ML")
fitL$m1 <- lavaan(data=bob, modelL$m1, missing="ML")
fitL$m2 <- lavaan(data=bob, modelL$m2, missing="ML")
fitL$m3 <- lavaan(data=bob, modelL$m3, missing="ML")
fitL$m4 <- lavaan(data=bob, modelL$m4, missing="ML")
fitL$m5 <- lavaan(data=bob, modelL$m5, missing="ML")
fitL$m6 <- lavaan(data=bob, modelL$m6, missing="ML")

summ_list <- list()

fitms <- c("npar","logl","aic",
           "chisq", "df", "pvalue",
           "rmsea", "cfi", "srmr")
fitmeasL <- list()
fitmeasL$m0 <- fitmeasures(fitL$m0, fit.measures = fitms)
fitmeasL$m1 <- fitmeasures(fitL$m1, fit.measures = fitms)
fitmeasL$m2 <- fitmeasures(fitL$m2, fit.measures = fitms)
fitmeasL$m3 <- fitmeasures(fitL$m3, fit.measures = fitms)
fitmeasL$m4 <- fitmeasures(fitL$m4, fit.measures = fitms)
fitmeasL$m5 <- fitmeasures(fitL$m5, fit.measures = fitms)
fitmeasL$m6 <- fitmeasures(fitL$m6, fit.measures = fitms)

summodels <- sapply(fitmeasL, function(f) {round(f,3)})
summodels

compmodels <- compareFit(fitL$m0, fitL$m1, fitL$m2, fitL$m3,
                         fitL$m4, fitL$m5,
                         nested=TRUE) 
compmodels

#------ Resumen de resultados de todos los analisis -------
#__________________________________________________________

#indices de ajuste de los modelos anidados
summodels
#comparación de indices de ajustes (a excepcion de modelo 6)
compmodels