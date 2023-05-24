library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(knitr)
library(tidyverse)
library(vroom)
library(viridis)
library(pwr)
require(GGally)
require(CCA)
require(CCP)
library(scales)
library(car)
library(plotrix)
library(lubridate)


X=read.csv(file="SDI_PD.csv", header=T)

summary(X)

dim(X)


PdDB=as.data.frame(X)

# Anotar las variables categóricas como factores

PdDB$Sex <-as.factor(PdDB$Sex)

PdDB$SDI_stratum <-as.factor(PdDB$SDI_stratum)

PdDB$SDI_level <-as.factor(PdDB$SDI_level)

PdDB$living_rank <-as.factor(PdDB$Living_rank)

PdDB$health_security_rank <-as.factor(PdDB$Health_security_rank)

PdDB$edu_lag_rank <-as.factor(PdDB$Edu_lag_rank)

PdDB$durable_goods_rank <-as.factor(PdDB$Durable_goods_rank)

PdDB$sanitary_rank <-as.factor(PdDB$Sanitary_rank)

PdDB$energetic_rank <-as.factor(PdDB$Energetic_rank)

PdDB$Education_level <-as.factor(PdDB$Education_level)

PdDB$Physical_activity <-as.factor(PdDB$Physical_activity)

PdDB$Sleep_quality <-as.factor(PdDB$Sleep_quality)

PdDB$smoked <-as.factor(PdDB$Smoked)

PdDB$cigarettes <-as.factor(PdDB$Cigarettes)

PdDB$smokes <-as.factor(PdDB$Smokes)

PdDB$Smoking<-as.factor(PdDB$Smoking)

PdDB$SDI_stratum <-as.factor(PdDB$SDI_stratum)

PdDB$Brush_your_teeth <-as.factor(PdDB$Brush_your_teeth)

PdDB$Brush_before_bed <-as.factor(PdDB$Brush_before_bed)

PdDB$Floss_use_after <-as.factor(PdDB$Floss_use_after)

PdDB$Method_clean <-as.factor(PdDB$Method_clean)

PdDB$Use_dental_prostheses <-as.factor(PdDB$Use_dental_prostheses)

PdDB$Dental_care_12 <-as.factor(PdDB$Dental_care_12)

PdDB$Implant_placed <-as.factor(PdDB$Implant_placed)

PdDB$Gum_treatment <-as.factor(PdDB$Gum_treatment)

PdDB$Lost_bone <-as.factor(PdDB$Lost_bone)

PdDB$Lost_tooth <-as.factor(PdDB$Lost_tooth)

PdDB$Gum_health <-as.factor(PdDB$Gum_health)

PdDB$Tooth_does_not_look_right <-as.factor(PdDB$Tooth_does_not_look_right)

PdDB$Loose_tooth <-as.factor(PdDB$Loose_tooth)

PdDB$Loose_tooth_later <-as.factor(PdDB$Loose_tooth_later)

PdDB$Have_gum_disease <-as.factor(PdDB$Have_gum_disease)

PdDB$Had_bleeding_gums <-as.factor(PdDB$Had_bleeding_gums)

PdDB$Gum_infected <-as.factor(PdDB$Gum_infected)

PdDB$Bad_breath<-as.factor(PdDB$Bad_breath)

PdDB$Abscessed_teeth <-as.factor(PdDB$Abscessed_teeth)

PdDB$Gum_surgery <-as.factor(PdDB$Gum_surgery)

PdDB$Reason_surgery <-as.factor(PdDB$Reason_surgery)

PdDB$Satisfaction_level <-as.factor(PdDB$Satisfaction_level)

PdDB$COVID19 <-as.factor(PdDB$COVID19)

PdDB$MetS <-as.factor(PdDB$MetS)

PdDB$Severe_periodontitis <- as.factor(PdDB$Severe_periodontitis)

PdDB$Gingivitis <- as.factor(PdDB$Gingivitis)

PdDB$Periodontal_care <-as.factor(PdDB$Periodontal_care)

summary(PdDB)



# PdDB$Oral_hygiene <-as.factor(PdDB$Oral_hygiene)
#PdDB$Regular_hygiene <-as.factor(PdDB$Regular_hygiene)

Age.test <- shapiro.test(Age)
print(Age.test)

PdDB$Height

#Weight.test <- shapiro.test(Weight)
#print(Weight.test)

#Height.test <- shapiro.test(Height)
#print(Height.test)

#prueba de normalidad shapiro
#p > 0.05 una distribución normal en p < 0.05 una distribución que no es normal.

SDI_1=filter(PdDB, SDI_stratum=="1")
summary(SDI_1)

SDI_2=filter(PdDB, SDI_stratum=="2")
summary(SDI_2)

SDI_3=filter(PdDB, SDI_stratum=="3")
summary(SDI_3)

SDI_4=filter(PdDB, SDI_stratum=="4")
summary(SDI_4)


summary(SDI_1$Anxiety_trait) 
shapiro.test(SDI_1$Anxiety_trait)

summary(SDI_2$Anxiety_trait) 
shapiro.test(SDI_2$Anxiety_trait)

summary(SDI_3$Anxiety_trait) 
shapiro.test(SDI_3$Anxiety_trait)

summary(SDI_4$Anxiety_trait) 
shapiro.test(SDI_4$Anxiety_trait)

######


data_hist_bucal_semana <-data.frame(PdDB$Times_mouthwash_use, PdDB$SDI_stratum)

data_hist_dia_cepilla <-data.frame(PdDB$Times_brush_teeth, PdDB$SDI_stratum)

data_hist_hilo_semana <-data.frame(PdDB$Times_floss_use, PdDB$SDI_stratum)

data_hist_implantes <-data.frame(PdDB$Num_implant, PdDB$SDI_stratum)

data_hist_edad <-data.frame(PdDB$Age, PdDB$SDI_stratum)

library(ggplot2)

ggplot(data_hist_bucal_semana, aes(x=PdDB$Times_mouthwash_use, fill=PdDB$SDI_stratum))+geom_histogram(position="identity",alpha=0.2, bins=10)

ggplot(data_hist_dia_cepilla, aes(x=PdDB$Times_brush_teeth, fill=PdDB$SDI_stratum))+geom_histogram(position="identity",alpha=0.2, bins=30)

ggplot(data_hist_hilo_semana, aes(x=PdDB$Times_floss_use, fill=PdDB$SDI_stratum))+geom_histogram(position="identity",alpha=0.2, bins=10)

ggplot(data_hist_implantes, aes(x=PdDB$Num_implant, fill=PdDB$SDI_stratum))+geom_histogram(position="identity",alpha=0.2, bins=10)

ggplot(data_hist_edad, aes(x=PdDB$Age, fill=PdDB$SDI_stratum))+geom_histogram(position="identity",alpha=0.2, bins=10)


#Calculos de edades

library(lubridate)

#' Calculate Age
#' 
#' By default, calculates the typical "Age in years", with a
#' \code{floor} applied so that you are, e.g., 5 years old from
#' 5th birthday through the day before your 6th birthday. Set
#' \code{floor = FALSE} to return decimal ages, and change \code{units}
#' for units other than years.
#' @param dob date-of-birth, the day to start calculating Age.
#' @param Age.day the date on which Age is to be calculated.
#' @param units unit to measure Age in. Defaults to \code{"years"}. Passed to \link{\code{duration}}.
#' @param floor boolean for whether or not to floor the result. Defaults to \code{TRUE}.
#' @return Age in \code{units}. Will be an integer if \code{floor = TRUE}.
#' @examples
#' my.dob <- as.Date('1983-10-20')
#' age(my.dob)
#' age(my.dob, units = "minutes")
#' age(my.dob, floor = FALSE)
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

head(PdDB$FECHANACIMIENTO)

# Poner años en 4 digitos (agregas 19 al principio)

PdDB$nacim <- as.Date(format(as.Date(PdDB$FECHANACIMIENTO,format="%d/%m/%y"), "19%y%m%d"), "%Y%m%d")

head(PdDB$nacim)

# Calculamos age con el codigo que puse arriba 'Calculate Age'

PdDB$Age <-Age(PdDB$nacim)

PdDB$Age

age.test <- shapiro.test(Age)
print(Age.test)

#prueba de normalidad shapiro
#p > 0.05 una distribución normal en p < 0.05 una distribución que no es normal.

# si quieres la age con fracciones de año
#PdDB$age_fraction <-age(PdDB$nacim, floor=FALSE)
#PdDB$age_fraction

# write.csv(PdDB, file="Base_limpia_periodontitis_Abril_2022.csv")
 
#para prueba de hipóteisis entre proporciones
sexf<-c(83,299,199,262)
sexm<-c(51,163,106,131)
ene<-c(134,462,305,393)

prop.test(sexf,ene)

prop.test(sexm,ene)

smoke<-c(80,269,179,240)
prop.test(smoke,ene)

phy_act_low<-c(22,91,59,67)
prop.test(phy_act_low,ene)

phy_act_med<-c(67,213,137,192)
prop.test(phy_act_med,ene)

phy_act_high<-c(45,158,109,134)
prop.test(phy_act_high,ene)

coviddx<-c(29,115,58,55)
prop.test(coviddx,ene)

metsdx<-c(9,20,11,18)
prop.test(metsdx,ene)

sleep_bad<-c(82,247,170,205)
prop.test(sleep_bad,ene)

lost_tooth<-c(60,193,124,161)
prop.test(lost_tooth,ene)

notlookright<-c(38.06,31.60,33.44,30.28)
prop.test(notlookright,ene)

loose_tooth<-c(15,42,31,40)
prop.test(loose_tooth,ene)

loose_tooth_lost<-c(11,26,20,27)
prop.test(loose_tooth_lost,ene)

gum_disease<-c(19.40,25.54, 31.80,25.45)
prop.test(gum_disease,ene)

gum_bleeding<-c(48.51,52.38,50.82,47.84)
prop.test(gum_bleeding,ene)

gum_infected<-c(16.42,20.56,19.67,22.14)
prop.test(gum_infected,ene)

bad_breath<-c(23.13,25.97,27.54,21.88)
prop.test(bad_breath,ene)

abscessed<-c(14.93,15.80,12.13,12.47)
prop.test(abscessed,ene)

gum_surgery<-c(15.67,14.72,15.41,20.10)
prop.test(gum_surgery,ene)

gingiv_dx<-c(7,37,21,38)
prop.test(gingiv_dx,ene)

perio_dx_1<-c(5,8,8,13)
prop.test(perio_dx_1,ene)

perio_dx_0<-c(129,454,297,380)
prop.test(perio_dx_0,ene)

perio_tx_1<-c(1,4,3,9)
prop.test(perio_tx_1,ene)

perio_tx_0<-c(133,458,302,384)
prop.test(perio_tx_0,ene)

dental_tx_0<-c(61,242,153,213)
prop.test(dental_tx_0,ene)

Implant_placed_si<-c(19,53,46,51)
prop.test(Implant_placed_si,ene)

gum_tx_1<-c(17.16,22.73,25.25,21.88)
prop.test(gum_tx_1,ene)

lost_bone_yes<-c(12,42,41,57)
prop.test(lost_bone_yes,ene)

brush_teeth<-c(132,451,297,385)
prop.test(brush_teeth,ene)

brush_bef_sleep<-c(102,361,245,339)
prop.test(brush_bef_sleep,ene)

floos_aft_brus<-c(6.72,16.23,20.33,20.61)
prop.test(floos_aft_brus,ene)

dental_prostheses<-c(14.18,11.26,12.46,12.47)
prop.test(dental_prostheses,ene)

satisfaction_very_unsat<-c(2.24,1.30,1.97,0.76)
prop.test(satisfaction_very_unsat,ene)

satisfaction_unsat<-c(8.96,9.96,9.51,8.40)
prop.test(satisfaction_unsat,ene)

satisfaction_satisfied<-c(55.97,55.19,47.87,57)
prop.test(satisfaction_satisfied,ene)

satisfaction_very_satisfied<-c(32.84,33.55,40.66,33.84)
prop.test(satisfaction_very_satisfied,ene)

gum_excellent<-c(6.72,9.31,6.23,11.70)
prop.test(gum_excellent,ene)

gum_verygood<-c(25.37,24.46,28.85,22.90)
prop.test(gum_verygood,ene)

gum_good<-c(45.52,44.37,42.95,41.73)
prop.test(gum_good,ene)

gum_regular<-c(21.64,18.18,19.02,20.87)
prop.test(gum_regular,ene)

gum_bad<-c(0.75,3.68,2.95,2.80)
prop.test(gum_bad,ene)

Elementary<-c(0.74,0.22,0.66,0.25)
prop.test(Elementary,ene)

Middle<-c(11.94,5.19,4.92,2.54 )
prop.test(Middle,ene)

High<-c(40.30,36.80,29.18,21.12)
prop.test(High,ene)

College<-c(38.81,47.62,50.82,52.42)
prop.test(College,ene)

Postgraduate<-c(8.20,10.17,14.42,23.66)
prop.test(Postgraduate,ene)

# Codigo para hacer correlogramas 

summary(PdDB)

Oral_Health_IDS <- PdDB[c("Gingivitis","Had_bleeding_gums", "Severe_periodontitis", "Times_mouthwash_use",
            "Times_brush_teeth","SDI_value")]

theme_set(theme_minimal())

opts <- options()

options(ggplot2.continuous.colour="viridis")

options(ggplot2.discrete.colour="viridis")

options(ggplot2.continuous.fill = "viridis")

options(ggplot2.discrete.fill = "viridis")

ggpairs(Oral_Health_IDS, title="Correlograms", upper = list(continuous = "smooth_loess", combo = "box_no_facet"), lower = list(continuous = "density", combo = "facethist"), diag = list(continuous = "barDiag", discrete = "barDiag", na = "naDiag"), ggplot2::aes(color=Times_brush_teeth))

# ccc <- filter(PdDB, MetS=="1")
# summary(ccc)
# 
# ddd <- filter(PdDB, Gingivitis=="1")
# 
# summary(ddd)
# 
# eee <- filter(PdDB, Severe_periodontitis=="1")
# 
# summary(eee)
# 

#Lista Completa de variables para correr los modelos

# Periodon <- glm(Severe_periodontitis ~ Sex	+ Weight + Height	+ Body_mass_index +	Waist	
#                  + Systolic_average	+ Diastolic_average	+ SDI_value	
#                  + SDI_stratum	+ SDI_level	+ Living_value	+ Health_security_value	
#                  + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value	+ Energetic_value	
#                  + Living_rank	+ Health_security_rank	+ Edu_lag_rank	+ Durable_goods_rank	
#                  + Sanitary_rank	+ Energetic_rank	+ Education_level	+ Physical_activity	
#                  + Anxiety_trait	+ Anxiety_trait	+ Sleep_quality	+ Smoked
#                  + Cigarettes	+ Smokes + Smoking + Brush_your_teeth	+ Times_brush_teeth	
#                  + Brush_before_bed	+ Floss_use_after	+ Times_floss_use
#                  + Times_mouthwash_use	+ Method_clean	+ Use_dental_prostheses	
#                  + Dental_care_12	+ Implant_placed	+ Num_implant	
#                  + Gum_treatment	+ Lost_bone	+ Lost_tooth	
#                  + Gum_health	+ Tooth_does_not_look_right	+ Loose_tooth	
#                  + Num_loose_teeth	+ Loose_tooth_later	+ Have_gum_disease
#                  + Had_bleeding_gums	+ Gum_infected	+ Bad_breath	
#                  + Abscessed_teeth	+ Gum_surgery	+ Reason_surgery	
#                  + Satisfaction_level	+ COVID19	+ MetS	
#                  + Gingivitis	+ Periodontal_care,
#                            data = PdDB, family = "binomial")
# 
# summary(Periodon)

# Conteo de número de valores en un factor
#sapply(lapply(PdDB, unique), length)

# Variables selectas generales

# Periodon <- glm( Severe_periodontitis ~ Sex	+ SDI_value	
#                  + Living_value	+ Health_security_value	
#                  + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value		
#                  + Education_level	+ Sleep_quality	+ Smoking
#                  + Brush_your_teeth	+ Times_brush_teeth	
#                  + Brush_before_bed	+ Floss_use_after	+ Times_floss_use
#                  + Times_mouthwash_use	+  Use_dental_prostheses	
#                  + Dental_care_12	+ Implant_placed	+ Num_implant	
#                  + Gum_treatment	+ Lost_bone	+ Lost_tooth	
#                  + Gum_health	+ Tooth_does_not_look_right	+ Loose_tooth	
#                  + Num_loose_teeth	+ Loose_tooth_later	+ Have_gum_disease
#                  + Had_bleeding_gums	+ Gum_infected	+ Bad_breath	
#                  + Abscessed_teeth	+ Gum_surgery	
#                  + Satisfaction_level	+ COVID19	+ MetS	
#                  + Gingivitis,
#                  data = PdDB, family = "binomial")
# 
# summary(Periodon)

# Variables selectas generales no redundantes

#Salud_Peri <- glm(Severe_periodontitis ~ Sex + Living_value	+ Physical_activity	
              #+ Anxiety_trait	+ Health_security_value	
               #  + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value + Education_level	
                # + Sleep_quality + Smoking + Brush_your_teeth+ Times_brush_teeth+ Brush_before_bed	
                 #+ Floss_use_after	+ Times_floss_use + Times_mouthwash_use
                 #+ Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant +Gum_treatment 
                 #+ Gum_health + Tooth_does_not_look_right  
                  #+ Bad_breath	+ Abscessed_teeth + Gum_surgery + Satisfaction_level
                 #+ COVID19	+ MetS + Gingivitis,
                 #data = PdDB, family = "binomial")

#summary(Periodon)

# Variables selectas generales no relacionadas directamente con el padecimiento

# + Education_level	

#Periodon2 <-  glm(Severe_periodontitis ~ Sex + Living_value	+ Health_security_value	
 #                  + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value 
  #                 + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed	
   #                + Floss_use_after+ Times_floss_use + Times_mouthwash_use
  #                 + Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant +Gum_treatment 
   #                + Gum_health + Tooth_does_not_look_right  
  #                 + Bad_breath	+ Abscessed_teeth + Gum_surgery + Satisfaction_level
   #                + COVID19	+ MetS + Gingivitis,
  #                 data = PdDB, family = "binomial")

#summary(Periodon2)

#step(Periodon2, test="LRT")

# Modelo final segun LRT 

#Periodon_final<-  glm(formula = Severe_periodontitis ~ Sex + Living_value + Edu_lag_value + 
 #                       Education_level + Times_brush_teeth + Brush_before_bed + Use_dental_prostheses + 
  #                      Gum_health + Tooth_does_not_look_right + Abscessed_teeth + 
   #                     Gum_surgery + Satisfaction_level + COVID19, 
    #                  family = "binomial", data = PdDB)
  
#summary(Periodon_final)

# Intervalos de confianza

# confint(Periodon_final)

# Odds ratios 

# lreg.or <-exp(cbind(OR = coef(Periodon_final)))

##############################################################
#No se incluyeron: Weight + Height +	Waist + SDI_stratum	+ SDI_level	+ Living_rank	+ Health_security_rank	+ Edu_lag_rank	+ Durable_goods_rank	
#+ Sanitary_rank	+ Energetic_rank + Num_implant	
#+ Education_level + Anxiety_trait	

#se quitaron por ser las que construyeron la variable de gingivitis  + Have_gum_disease 

#se queja de:+ Reason_surgery+ Gum_surgery + Abscessed_teeth + Gum_infected + Cigarettes + Smokes + Method_clean	+ Gum_health	+Had_bleeding_gums

############################Gingivitis1 es el modelo con SDI global#################

Gingivitis1<- glm(formula = Gingivitis ~  Sex
                  + Body_mass_index 	
                  + Systolic_average	
                  + Diastolic_average	
                  + Smoked
                  + SDI_value		
                  + Sleep_quality 
                  + Physical_activity
                  + Smoking 
                  + Brush_your_teeth
                  + Times_brush_teeth	
                  + Brush_before_bed	
                  + Floss_use_after	
                  + Times_floss_use
                  + Times_mouthwash_use	
                  + Use_dental_prostheses	
                  + Dental_care_12	
                  + Implant_placed	
                  + Gum_treatment	
                  + Lost_bone	
                  + Lost_tooth	
                  + Tooth_does_not_look_right	
                  + Loose_tooth	
                  + Num_loose_teeth
                  + Loose_tooth_later	
                  + Bad_breath	 
                  + Periodontal_care
                  + Satisfaction_level
                  + COVID19	
                  + MetS	
                  + Periodontal_care
              , family = "binomial", 
                        data = PdDB)




step(Gingivitis1, test="LRT")

# Modelo final segun LRT 

Gingivitis1_final<- glm(formula = Gingivitis ~ SDI_value + Brush_before_bed + Lost_bone + 
                          Tooth_does_not_look_right + Loose_tooth_later + Bad_breath + 
                          Satisfaction_level + COVID19, family = "binomial", data = PdDB)

summary(Gingivitis1_final)

vif(Gingivitis1_final)

# Intervalos de confianza

confint(Gingivitis1_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Gingivitis1_final)))

round(lreg.or, digits=4)

# Pruebas de sensibilidad/especificidad del modelo por Curvas ROC

library(pROC)
test_prob = predict(Gingivitis1_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Gingivitis ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

library(LogisticDx)

GoodFit<-gof(Gingivitis1_final, plotROC=TRUE,g=8)

# Pas0 10: Pruebas de bondad de ajuste

# Medidas pseudo-R-cuadrada para la bondad de ajuste por máxima verosimilitud

library(pscl)

#pR2(Gingivitis1_final)

pR2(Gingivitis1_final)

# Prueba de Hosmer Lemeshow


library(generalhoslem)

logitgof(PdDB$Gingivitis, fitted(Gingivitis1_final), g = 10)

# Prueba de Stukel

# Definimos la función stukel.test, 
#una vez definida se puede comentar el código que la define

################ Una vez definida stukel.test se puede comentar desde aquí

stukel.test = function(obj) {
  # first, check to see if we fed in the right kind of object
  stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
  high.prob <- (obj$fitted.values >= 0.5) 
  logit2 <- obj$linear.predictors^2
  z1 = 0.5*logit2*high.prob
  z2 = 0.5*logit2*(1-high.prob)
  mf <- obj$model
  trials = rep(1, times = nrow(mf))
  if(any(colnames(mf) == "(weights)")) 
    trials <- mf[[ncol(mf)]]
  prop = mf[[1]]
  # the double bracket (above) gets the index of items within an object
  if (is.factor(prop)) 
    prop = (as.numeric(prop) == 2)  # Converts 1-2 factor levels to logical 0/1 values
  pi.hat = obj$fitted.values 
  y <- trials*prop
  exclude <- which(colnames(mf) == "(weights)")
  vars <- data.frame(z1, z2, y, mf[,-c(1,exclude)])
  full <- glm(formula = y/trials ~ ., family = binomial(link = logit), weights = trials, data = vars)
  null <- glm(formula = y/trials ~ ., family = binomial(link = logit), weights = trials, data = vars[,-c(1,2)])
  LRT <- anova(null,full)
  p.value <- 1 - pchisq(LRT$Deviance[[2]], LRT$Df[[2]])
  cat("Stukel Test Stat = ", LRT$Deviance[[2]], "with p-value = ", p.value, "\n")
}

#### Fin de la definición de la función. 
# Hasta aquí se puede comentar después de la primera vez que se use

# The results are similar: if p-value is above the threshold of 0.05, 
# there is no statistical evidence of the poor fit of our model 
# based on the difference in the occurence of event between 
# the fitted and the observed values.

stukel.test(Gingivitis1_final)

# Prueba de Osius-Rojek 

# Definimos la función o.r.test, 
#una vez definida se puede comentar el código que la define

################ Una vez definida stukel.test se puede comentar desde aquí

o.r.test = function(obj) {
  # first, check to see if we fed in the right kind of object
  stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
  mf <- obj$model
  trials = rep(1, times = nrow(mf))
  if(any(colnames(mf) == "(weights)")) 
    trials <- mf[[ncol(mf)]]
  prop = mf[[1]]
  # the double bracket (above) gets the index of items within an object
  if (is.factor(prop)) 
    prop = as.numeric(prop) == 2  # Converts 1-2 factor levels to logical 0/1 values
  pi.hat = obj$fitted.values 
  y <- trials*prop
  yhat <- trials*pi.hat
  nu <- yhat*(1-pi.hat)
  pearson <- sum((y - yhat)^2/nu)
  c = (1 - 2*pi.hat)/nu
  exclude <- c(1,which(colnames(mf) == "(weights)"))
  vars <- data.frame(c,mf[,-exclude]) 
  wlr <- lm(formula = c ~ ., weights = nu, data = vars)
  rss <- sum(nu*residuals(wlr)^2 )
  J <- nrow(mf)
  A <- 2*(J - sum(1/trials))
  z <- (pearson - (J - ncol(vars) - 1))/sqrt(A + rss)
  p.value <- 2*(1 - pnorm(abs(z)))
  cat("z = ", z, "with p-value = ", p.value, "\n")
}

#### Fin de la definición de la función. 
# Hasta aquí se puede comentar después de la primera vez que se use

o.r.test(Gingivitis1_final)

# Paso 11: Pruebas de multicolinealidad y residuos

library(car)

car::residualPlots(Gingivitis1_final)

car::marginalModelPlots(Gingivitis1_final)

vif(Gingivitis1_final)

outlierTest(Gingivitis1_final)

influenceIndexPlot(Gingivitis1_final, id.n. = 3)

influencePlot(Gingivitis1_final, col = "red", id.n. = 3)


# Paso 12: Forest plot comparando coeficientes
# https://strengejacke.github.io/sjPlot/

library(sjPlot)
library(glmmTMB)

plot_model(Gingivitis1_finalbis, type="std2",sort.est=TRUE,vline.color="grey80",
           show.values=TRUE,value.offset=.3,show.p=TRUE,title="Comparacion de coeficientes")


plot_model(Gingivitis1_finalbis, type="resid",sort.est=TRUE,vline.color="grey80",
           show.values=TRUE,value.offset=.3,show.p=TRUE,title="Comparacion de residuos")

plot_model(Gingivitis1_finalbis, type="eff",sort.est=TRUE,vline.color="grey80",
           show.values=TRUE,value.offset=.3,show.p=TRUE,title="Efectos en modelo univariado")


plot_model(Gingivitis1_finalbis, type="std2",sort.est=TRUE,vline.color="grey80",
           show.values=TRUE,value.offset=.3,show.p=TRUE,title="Comparacion de coeficientes",transform = NULL)

plot_model(bis, type="std2",sort.est=TRUE,vline.color="grey80",
           show.values=TRUE,value.offset=.3,show.p=TRUE,title="Comparacion de coeficientes",transform = "plogis")

#######################################Gingivitis2 es el modelo con los componentes del SDI
Gingivitis2<- glm(formula = Gingivitis ~  Sex
                  + Body_mass_index 	
                  + Systolic_average	
                  + Diastolic_average	
                  + Edu_lag_value
                  + Living_value	
                  + Health_security_value	
                  + Durable_goods_value	
                  + Energetic_value
                  + Sanitary_value
                  + Sleep_quality 
                  + Physical_activity
                  + Smoking 
                  + Brush_your_teeth
                  + Times_brush_teeth	
                  + Brush_before_bed	
                  + Floss_use_after	
                  + Times_floss_use
                  + Times_mouthwash_use	
                  + Use_dental_prostheses	
                  + Dental_care_12	
                  + Implant_placed	
                  + Gum_treatment	
                  + Lost_bone	
                  + Lost_tooth	
                  + Tooth_does_not_look_right	
                  + Loose_tooth	
                  + Num_loose_teeth
                  + Loose_tooth_later	
                  + Bad_breath	 
                  + Periodontal_care
                  + Satisfaction_level
                  + COVID19	
                  + MetS	
                  + Periodontal_care
                  , family = "binomial", 
                  data = PdDB)


step(Gingivitis2, test="LRT")

# Modelo final segun LRT 

Gingivitis2_final<- glm(formula = Gingivitis ~ Sanitary_value + Brush_before_bed + 
                          Lost_bone + Tooth_does_not_look_right + Loose_tooth_later + 
                          Bad_breath + Satisfaction_level + COVID19, family = "binomial", 
                        data = PdDB)



summary(Gingivitis2_final)

vif(Gingivitis2_final)

# Intervalos de confianza

confint(Gingivitis2_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Gingivitis2_final)))

round(lreg.or, digits=4)

# Pruebas de sensibilidad/especificidad del modelo por Curvas ROC

library(pROC)
test_prob = predict(Gingivitis2_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Gingivitis ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)



################################Lost_tooth_later1##############################
#se eliminaron del modelo + Lost_tooth + Bad_breath	+ Loose_tooth	+ Num_loose_teeth + Education_level + Anxiety_trait + Num_implant + Gum_health
#+ Living_value	+ Health_security_value	+ Abscessed_teeth + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value +Energetic_value + Use_dental_prostheses+ Implant_placed	 + Gum_treatment	+ Lost_bone+ Tooth_does_not_look_right	+Have_gum_disease+ Had_bleeding_gums	+ Gum_infected

Loose_tooth_later1 <- glm(Loose_tooth_later ~ Sex 
                          + Body_mass_index 	
                          + Systolic_average	
                          + Diastolic_average	
                          + Physical_activity 
                          + SDI_value	+ Sleep_quality
                          + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed	
                          + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                           + Dental_care_12 	
                          + Satisfaction_level	+ COVID19	+ MetS, data = PdDB, family = "binomial")
summary(Loose_tooth_later1)            



step(Loose_tooth_later1, test="LRT")

# Modelo final segun LRT 

Loose_tooth_later1_final<- glm(formula = Loose_tooth_later ~ Sex + Body_mass_index + Floss_use_after + 
                                 Satisfaction_level + COVID19, family = "binomial", data = PdDB)

summary(Loose_tooth_later1_final)

vif(Loose_tooth_later1_final)


# Intervalos de confianza

confint(Loose_tooth_later1_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Loose_tooth_later1_final)))

round(lreg.or, digits=4)

test_prob = predict(Loose_tooth_later1_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Loose_tooth_later ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################################Lost_tooth_later2##############################
#se eliminaron del modelo + Bad_breath	+Lost_tooth +  Loose_tooth	+ Num_loose_teeth + Education_level + Anxiety_trait   + Use_dental_prostheses   + Gum_health

Loose_tooth_later2 <- glm(Loose_tooth_later ~ Sex 
                         + Body_mass_index 	+ Living_value	+ Health_security_value	+ Energetic_value 
                         + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value
                         + Systolic_average	
                         + Diastolic_average	
                         + Physical_activity 
                         + SDI_value	+ Sleep_quality
                         + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed	
                         + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                         + Dental_care_12 	
                         + Satisfaction_level	+ COVID19	+ MetS, data = PdDB, family = "binomial")
summary(Loose_tooth_later2)            



step(Loose_tooth_later2, test="LRT")

# Modelo final segun LRT 

Loose_tooth_later2_final<- glm(formula = Loose_tooth_later ~ Sex + Body_mass_index + Floss_use_after + 
                                 Satisfaction_level + COVID19, family = "binomial", data = PdDB)

summary(Loose_tooth_later2_final)

vif(Loose_tooth_later2_final)


# Intervalos de confianza

confint(Loose_tooth_later2_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Loose_tooth_later2_final)))

round(lreg.or, digits=4)

test_prob = predict(Loose_tooth_later2_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Loose_tooth_later ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

###############################Bleeding1##############################
#mod 1 sin componentes mod 2 con componentes del SDI, se eliminaron: + Bad_breath	+ Use_dental_prostheses + Num_implant   + Gum_health
#+ Implant_placed	 + Gum_treatment	+ Lost_bone+ Tooth_does_not_look_right	+ Have_gum_disease+ Lost_tooth +  Loose_tooth	+ Num_loose_teeth 	+ Gum_infected		+ Abscessed_teeth	


Had_bleeding_gums_mod1 <- glm(Had_bleeding_gums ~ Sex 
                              + Body_mass_index 	
                              + Systolic_average	
                              + Diastolic_average
                              + SDI_value
                              + Physical_activity
                              + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed	
                              + Floss_use_after+ Times_floss_use + Times_mouthwash_use 
                               + Dental_care_12 
                              + Satisfaction_level	+ COVID19	+ MetS, data = PdDB, family = "binomial")
summary(Had_bleeding_gums_mod1)            



step(Had_bleeding_gums_mod1, test="LRT")

# Modelo final segun LRT 

Had_bleeding_gums_mod1_final<- glm(formula = Had_bleeding_gums ~ Brush_your_teeth + Times_brush_teeth + 
                                     Brush_before_bed + Floss_use_after + Times_mouthwash_use, 
                                   family = "binomial", data = PdDB)

summary(Had_bleeding_gums_mod1_final)

vif(Had_bleeding_gums_mod1_final)

# Intervalos de confianza

confint(Had_bleeding_gums_mod1_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Had_bleeding_gums_mod1_final)))

round(lreg.or, digits=4)

test_prob = predict(Had_bleeding_gums_mod1_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Had_bleeding_gums ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)



###############################Blending2##############################
#se eliminaron + Bad_breath	+ Num_implant + Use_dental_prostheses + Gum_health + Implant_placed	 + Gum_treatment	+ Lost_bone+ Tooth_does_not_look_right	+ Have_gum_disease+ Lost_tooth +  Loose_tooth	+ Num_loose_teeth 	+ Gum_infected		+ Abscessed_teeth	

Had_bleeding_gums_mod2 <- glm(Had_bleeding_gums ~ Sex 
                             + Body_mass_index 	
                             + Systolic_average	
                             + Diastolic_average
                          + Living_value	+ Health_security_value	+Energetic_value
                          + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value 
                          + Physical_activity
                          + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed	
                          + Floss_use_after+ Times_floss_use + Times_mouthwash_use 
                           + Dental_care_12 
                          + Satisfaction_level	+ COVID19	+ MetS, data = PdDB, family = "binomial")
summary(Had_bleeding_gums_mod2)            



step(Had_bleeding_gums_mod2, test="LRT")

# Modelo final segun LRT 

Had_bleeding_gums_mod2_final<- glm(formula = Had_bleeding_gums ~ Brush_your_teeth + Times_brush_teeth + 
                                     Brush_before_bed + Floss_use_after + Times_mouthwash_use, 
                                   family = "binomial", data = PdDB)

summary(Had_bleeding_gums_mod2_final)

vif(Had_bleeding_gums_mod2_final)

# Intervalos de confianza

confint(Had_bleeding_gums_mod2_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Had_bleeding_gums_mod2_final)))

round(lreg.or, digits=4)

test_prob = predict(Had_bleeding_gums_mod2_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Had_bleeding_gums ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

############Brush_before_bed#################################################
#No se incluyó este modelo
#se eliminaron del modelo: Education_level + Brush_your_teeth 	+ MetS

Brush_before_bed_mod <- glm(Brush_before_bed ~ Sex + Body_mass_index
                                      + Living_value	+ Health_security_value	+ Had_bleeding_gums
                                      + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value +Energetic_value
                                      + Physical_activity + Times_brush_teeth 
                                      + Sleep_quality + Smoking  
                                      + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                                      + Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant  
                                      + Gum_treatment	+ Lost_bone
                                      + Gum_health	+ Tooth_does_not_look_right	+ Have_gum_disease
                                      + Lost_tooth +  Loose_tooth	+ Num_loose_teeth 	+ Gum_infected	+ Bad_breath	
                                      + Abscessed_teeth	
                                      + Satisfaction_level	+ COVID19, data = PdDB, family = "binomial")
summary(Brush_before_bed_mod)            



step(Brush_before_bed_mod, test="LRT")

# Modelo final segun LRT 

Brush_before_bed_mod_final<- glm(formula = Brush_before_bed ~ Body_mass_index + Living_value + Had_bleeding_gums + 
                                 Physical_activity + Anxiety_trait + Times_brush_teeth + Times_floss_use + 
                                 Loose_tooth + Bad_breath, family = "binomial", data = PdDB)

summary(Brush_before_bed_mod_final)

vif(Brush_before_bed_mod_final)

# Intervalos de confianza

confint(Brush_before_bed_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Brush_before_bed_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Brush_before_bed_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Brush_before_bed ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

############Lost_bone_1###########################################

#se eliminaron del modelo: Education_level + Gum_health	 + Edu_lag_value + Periodontal_care 
# + Edu_lag_value + Use_dental_prostheses	+ Num_implant + Implant_placed + Lost_tooth	+ Loose_tooth	+ Num_loose_teeth+ Loose_tooth_later	+ Have_gum_disease  + Gum_treatment
# + Living_value	+ Tooth_does_not_look_right	
# + Health_security_value	+ Bad_breath	
# + Durable_goods_value	
# + Education_level	

perdido_hueso_mod1 <- glm(Lost_bone ~  Sex
                         + Body_mass_index 	
                         + Systolic_average	
                         + Diastolic_average	
                         + SDI_value	
                         + Sleep_quality 
                         + Physical_activity
                         + Smoking 
                         + Brush_your_teeth
                         + Times_brush_teeth	
                         + Brush_before_bed	
                         + Floss_use_after	
                         + Times_floss_use
                         + Times_mouthwash_use	
                         + Dental_care_12	 
                         + Satisfaction_level
                         + COVID19	
                         + MetS, data = PdDB, family = "binomial")
                                    
            
summary(perdido_hueso_mod1)            

step(perdido_hueso_mod1, test="LRT")

# Modelo final segun LRT 

Lost_bone_mod1_final<- glm(formula = Lost_bone ~ SDI_value + Physical_activity + Times_floss_use + 
                             Satisfaction_level, family = "binomial", data = PdDB)

summary(Lost_bone_mod1_final)

vif(Lost_bone_mod1_final)

# Intervalos de confianza

confint(Lost_bone_mod1_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Lost_bone_mod1_final)))

round(lreg.or, digits=4)

# Pruebas de sensibilidad/especificidad del modelo por Curvas ROC

library(pROC)
test_prob = predict(Lost_bone_mod1_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Lost_bone ~ test_prob, plot = TRUE, print.auc = TRUE)
test_roc = roc(PdDB$Lost_bone ~ test_prob, plot = FALSE, print.auc = FALSE)

as.numeric(test_roc$auc)


##########################Lost_bone_2###########################
##el modelo 2 es con los componentes
#se eliminaron del modelo: + Bad_breath	 Education_level + Gum_health	 + Edu_lag_value + Periodontal_care + Use_dental_prostheses	  + Gum_treatment	+ Tooth_does_not_look_right
#+ Num_implant	+ Have_gum_disease   + Lost_tooth	 + Loose_tooth	+ Num_loose_teeth + Loose_tooth_later	+ Implant_placed

perdido_hueso_mod2 <- glm(Lost_bone ~  Sex
                          + Body_mass_index 	
                          + Systolic_average	
                          + Diastolic_average	
                          + Living_value	+ Health_security_value	+Energetic_value
                        	+ Durable_goods_value	+ Sanitary_value 
                          + Sleep_quality 
                          + Physical_activity
                          + Smoking 
                          + Brush_your_teeth
                          + Times_brush_teeth	
                          + Brush_before_bed	
                          + Floss_use_after	
                          + Times_floss_use
                          + Times_mouthwash_use	
                          + Dental_care_12	
                          + Satisfaction_level
                          + COVID19	
                          + MetS, data = PdDB, family = "binomial")


summary(perdido_hueso_mod2)            

step(perdido_hueso_mod2, test="LRT")

# Modelo final segun LRT 

Lost_bone_mod2_final<- glm(formula = Lost_bone ~ Living_value + Physical_activity + 
                             Times_floss_use + Satisfaction_level, family = "binomial", 
                           data = PdDB)

summary(Lost_bone_mod2_final)

vif(Lost_bone_mod2_final)

# Intervalos de confianza

confint(Lost_bone_mod2_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Lost_bone_mod2_final)))

round(lreg.or, digits=4)

# Pruebas de sensibilidad/especificidad del modelo por Curvas ROC

library(pROC)
test_prob = predict(Lost_bone_mod2_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Lost_bone ~ test_prob, plot = TRUE, print.auc = TRUE)
test_roc = roc(PdDB$Lost_bone ~ test_prob, plot = FALSE, print.auc = FALSE)

as.numeric(test_roc$auc)


#####Tooth_does_not_look_right##################################################
####################Este modelo no se incluyó
# se eliminó: Education_level 	

Tooth_does_not_look_right_mod <- glm(Tooth_does_not_look_right ~ Sex + Energetic_value
                         + Living_value	+ Health_security_value + Physical_activity 
                         + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value
                         + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                         + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                         + Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant
                         + Gum_treatment	+ Had_bleeding_gums 
                         + Lost_bone	+ Have_gum_disease
                         + Lost_tooth +  Loose_tooth	+ Num_loose_teeth 	+ Gum_infected	+ Bad_breath
                         + Abscessed_teeth + Gum_health	
                         + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Tooth_does_not_look_right_mod)            

step(Tooth_does_not_look_right_mod, test="LRT")

# Modelo final segun LRT 

Tooth_does_not_look_right_mod_final<- glm(formula = Tooth_does_not_look_right ~  Health_security_value + 
                                Durable_goods_value + Sleep_quality + Brush_before_bed + Floss_use_after + 
                                Dental_care_12 + Implant_placed + Num_implant + 
                                Have_gum_disease + Lost_tooth + Loose_tooth + 
                                Gum_infected + Bad_breath+ Gum_health + Satisfaction_level, 
                              family = "binomial", data = PdDB)

summary(Tooth_does_not_look_right_mod_final)

vif(Tooth_does_not_look_right_mod_final)

# Intervalos de confianza

confint(Tooth_does_not_look_right_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Tooth_does_not_look_right_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Tooth_does_not_look_right_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Tooth_does_not_look_right ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

###################Loose_tooth_1####################################
# se eliminó: Education_level  + Num_loose_teeth + Num_implant + Use_dental_prostheses + Gum_health	+ Implant_placed	 + Gum_treatment	+ Had_bleeding_gums + Lost_bone	+ Have_gum_disease + Lost_tooth +  Tooth_does_not_look_right + Gum_infected	+ Bad_breath + Abscessed_teeth 

Loose_tooth_mod1 <- glm(Loose_tooth ~ Sex + Body_mass_index 	
                       + Systolic_average	
                       + Diastolic_average
                   + Physical_activity 	
                   + SDI_value + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                                     + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                                      + Dental_care_12 
                                     + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Loose_tooth_mod1)            

step(Loose_tooth_mod1, test="LRT")

# Modelo final segun LRT 

Loose_tooth_mod1_final<- glm(formula = Loose_tooth ~ Sex + Body_mass_index + Systolic_average + 
                               Times_brush_teeth + Brush_before_bed + Satisfaction_level + 
                               COVID19, family = "binomial", data = PdDB)

summary(Loose_tooth_mod1_final)

vif(Loose_tooth_mod1_final)

# Intervalos de confianza

confint(Loose_tooth_mod1_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Loose_tooth_mod1_final)))

round(lreg.or, digits=4)

test_prob = predict(Loose_tooth_mod1_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Loose_tooth ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

#######################################################

###############Loose_tooth_2######################################
##modelo 2 con componentes del SDI
# se eliminó: Education_level + Implant_placed	+ Gum_treatment	+ Had_bleeding_gums + Lost_bone	+ Have_gum_disease + Lost_tooth +  Tooth_does_not_look_right + Gum_infected	+ Bad_breath + Abscessed_teeth + Num_loose_teeth  + Use_dental_prostheses + Num_implant  + Gum_health + Energetic_value

Loose_tooth_mod2 <- glm(Loose_tooth ~ Sex + Body_mass_index 	
                        + Systolic_average	
                        + Diastolic_average
                        + Living_value	+ Health_security_value	+ Energetic_value
                        + Durable_goods_value	+ Sanitary_value 
                        + Physical_activity  + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                        + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                       + Dental_care_12 
                        + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Loose_tooth_mod2)            

step(Loose_tooth_mod2, test="LRT")

# Modelo final segun LRT 

Loose_tooth_mod2_final<- glm(formula = Loose_tooth ~ Sex + Body_mass_index + Systolic_average + 
                               Energetic_value + Sanitary_value + Sleep_quality + Brush_your_teeth + 
                               Times_brush_teeth + Brush_before_bed + Satisfaction_level + 
                               COVID19, family = "binomial", data = PdDB)

summary(Loose_tooth_mod2_final)

vif(Loose_tooth_mod2_final)

# Intervalos de confianza

confint(Loose_tooth_mod2_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Loose_tooth_mod2_final)))

round(lreg.or, digits=4)

test_prob = predict(Loose_tooth_mod2_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Loose_tooth ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

####Lost_tooth_1NI###################################################
#se eliminó: Education_level  + Edu_lag_value	este modelo no se incluyó

Lost_tooth_mod1 <- glm(Lost_tooth ~ Sex + Body_mass_index 	
                      + Systolic_average	
                      + Diastolic_average
                      + Physical_activity 	
                      + SDI_value 
                        + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                        + Floss_use_after+ Times_floss_use + Times_mouthwash_use 
                        + Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant
                        + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth
                        + Lost_bone	+ Have_gum_disease
                        + Tooth_does_not_look_right + Gum_infected	+ Bad_breath
                        + Abscessed_teeth + Gum_health	
                        + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Lost_tooth_mod1)            



step(Lost_tooth_mod1, test="LRT")

# Modelo final segun LRT 

Lost_tooth_mod1_final<- glm(formula = Lost_tooth ~ Body_mass_index + Use_dental_prostheses + 
                              Dental_care_12 + Implant_placed + Gum_treatment + Had_bleeding_gums + 
                              Num_loose_teeth + Tooth_does_not_look_right + Satisfaction_level, 
                            family = "binomial", data = PdDB)

summary(Lost_tooth_mod1_final)

vif(Lost_tooth_mod1_final)
# Intervalos de confianza

confint(Lost_tooth_mod1_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Lost_tooth_mod1_final)))

round(lreg.or, digits=4)

test_prob = predict(Lost_tooth_mod1_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Lost_tooth ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)


###Lost_tooth_2NI###################################################
#se eliminó: Education_level  + Edu_lag_value,	este modelo no se incluyó

Lost_tooth_mod2 <- glm(Lost_tooth ~ Sex + Body_mass_index 	
                       + Systolic_average	
                       + Diastolic_average
                       + Physical_activity 	
                       + Living_value	+ Health_security_value		
                       + Durable_goods_value	+ Sanitary_value + Energetic_value
                       + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                       + Floss_use_after+ Times_floss_use + Times_mouthwash_use 
                       + Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant
                       + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth
                       + Lost_bone	+ Have_gum_disease
                       +  Tooth_does_not_look_right + Gum_infected	+ Bad_breath
                       + Abscessed_teeth + Gum_health	
                       + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Lost_tooth_mod2)            



step(Lost_tooth_mod2, test="LRT")

# Modelo final segun LRT 

Lost_tooth_mod2_final<- glm(formula = Lost_tooth ~ Body_mass_index + Living_value + Health_security_value + 
                              Use_dental_prostheses + Dental_care_12 + Implant_placed + 
                              Gum_treatment + Had_bleeding_gums + Num_loose_teeth + Tooth_does_not_look_right + 
                              Satisfaction_level, family = "binomial", data = PdDB)

summary(Lost_tooth_mod2_final)

vif(Lost_tooth_mod2_final)
# Intervalos de confianza

confint(Lost_tooth_mod2_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Lost_tooth_mod2_final)))

round(lreg.or, digits=4)

test_prob = predict(Lost_tooth_mod2_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Lost_tooth ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)


########################Have_gum_disease1#########
##se quitó:   + Use_dental_prostheses + Num_implant + Gum_health
##este modelo no se incluyó

Have_gum_disease1_mod <- glm(Have_gum_disease ~ Sex + Body_mass_index 	
                             + Systolic_average	
                             + Diastolic_average
                             + Physical_activity 	
                             + SDI_value 
                          + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                          + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                         + Dental_care_12 	
                          + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Have_gum_disease1_mod)            


step(Have_gum_disease1_mod, test="LRT")

# Modelo final segun LRT 

Have_gum_disease1_mod_final<- glm(formula = Have_gum_disease ~ Sex + Diastolic_average + Brush_before_bed + 
                                    Satisfaction_level, family = "binomial", data = PdDB)

summary(Have_gum_disease1_mod_final)

vif(Have_gum_disease1_mod_final)

# Intervalos de confianza

confint(Have_gum_disease1_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Have_gum_disease1_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Have_gum_disease1_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Have_gum_disease ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)


#########################Have_gum_disease2############################
##este modelo no se incluyó + Implant_placed	+ Num_implant + Gum_treatment	+ + Bad_breath + Use_dental_prostheses + Had_bleeding_gums + Num_loose_teeth + Lost_bone	+ Lost_tooth +  Tooth_does_not_look_right + Gum_infected	+ Abscessed_teeth + Gum_health	

Have_gum_disease2_mod <- glm(Have_gum_disease ~ Sex + Body_mass_index 	
                             + Systolic_average	
                             + Diastolic_average+ Living_value	+ Health_security_value + Physical_activity 
                             + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value + Energetic_value
                             + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                             + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                             + Dental_care_12 
                             + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Have_gum_disease2_mod)            


step(Have_gum_disease2_mod, test="LRT")

# Modelo final segun LRT 

Have_gum_disease2_mod_final<- glm(formula = Have_gum_disease ~ Sex + Diastolic_average + Health_security_value + 
                                    Brush_before_bed + Satisfaction_level, family = "binomial", 
                                  data = PdDB)

summary(Have_gum_disease2_mod_final)

vif(Have_gum_disease2_mod_final)

# Intervalos de confianza

confint(Have_gum_disease2_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Have_gum_disease2_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Have_gum_disease2_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Have_gum_disease ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

#######################Gum_infected_1##########

#se eliminó: Education_level + + Bad_breath + Anxiety_trait + Use_dental_prostheses + Implant_placed	+ Num_implant + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth + Lost_bone	+ Lost_tooth + Tooth_does_not_look_right + Have_gum_disease	 + Abscessed_teeth	+ Gum_surgery + Gum_health

Gum_infected1_mod <- glm(Gum_infected ~ Sex  
                         +SDI_value
                         + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                         + Floss_use_after + Times_floss_use + Times_mouthwash_use + Bad_breath
                         + Dental_care_12 	
                         + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Gum_infected1_mod)            

step(Gum_infected1_mod, test="LRT")

# Modelo final segun LRT 

Gum_infected1_mod_final<- glm(formula = Gum_infected ~ Sex + Smoking + Bad_breath + Dental_care_12 + 
                                Satisfaction_level + MetS, family = "binomial", data = PdDB)

summary(Gum_infected1_mod_final)

vif(Gum_infected1_mod_final)

# Intervalos de confianza

confint(Gum_infected1_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Gum_infected1_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Gum_infected1_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Gum_infected ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)
#######################Gum_infected_2################################

Gum_infected2_mod <- glm(Gum_infected ~ Sex  
                                    + Living_value	+ Health_security_value + Physical_activity 
                                    + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value + Energetic_value
                                    + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                                    + Floss_use_after + Times_floss_use + Times_mouthwash_use 
                                     + Dental_care_12 	
                                    + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Gum_infected2_mod)            



step(Gum_infected2_mod, test="LRT")

# Modelo final segun LRT 

Gum_infected2_mod_final<- glm(formula = Gum_infected ~ Sex + Smoking + Brush_before_bed + 
                                Times_mouthwash_use + Dental_care_12 + Satisfaction_level + 
                                MetS, family = "binomial", data = PdDB)

summary(Gum_infected2_mod_final)

vif(Gum_infected2_mod_final)

# Intervalos de confianza

confint(Gum_infected2_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Gum_infected2_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Gum_infected2_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Gum_infected ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)
######################Bad_breath_1#################################
#se eliminó: Education_level + Anxiety_trait + Implant_placed	 + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth + Lost_bone	+ Lost_tooth + Tooth_does_not_look_right + Have_gum_disease	+ Gum_infected + Abscessed_teeth	+ Gum_surgery 	

mal_aliento1_mod <- glm(Bad_breath~ Sex + Body_mass_index 	
                        + Systolic_average	
                        + Diastolic_average
                        + SDI_value	
                        + Physical_activity 
                        + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                        + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                        + Dental_care_12 
                        + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(mal_aliento1_mod)            



step(mal_aliento1_mod, test="LRT")

# Modelo final segun LRT 

mal_aliento1_mod_final<- glm(formula = Bad_breath ~ Body_mass_index + Times_brush_teeth + 
                               Brush_before_bed + Times_mouthwash_use + Dental_care_12 + 
                               Implant_placed + Had_bleeding_gums + Lost_bone + Tooth_does_not_look_right + 
                               Have_gum_disease + Abscessed_teeth + Satisfaction_level, 
                             family = "binomial", data = PdDB)

summary(mal_aliento1_mod_final)

vif(mal_aliento1_mod_final)

# Intervalos de confianza

confint(mal_aliento1_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(mal_aliento1_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(mal_aliento1_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$mal_aliento ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

######################Bad_breath_2#################################
#se eliminó: Education_level + Anxiety_trait + Implant_placed	 + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth + Lost_bone	+ Lost_tooth + Tooth_does_not_look_right + Have_gum_disease	+ Gum_infected + Abscessed_teeth	+ Gum_surgery 

mal_aliento2_mod <- glm(Bad_breath~ Sex + Body_mass_index 	
                           + Systolic_average	
                           + Diastolic_average + Energetic_value
                           + Living_value	+ Health_security_value + Physical_activity 
                           + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value 
                           + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                           + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                           + Dental_care_12 
                           + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(mal_aliento2_mod)            



step(mal_aliento2_mod, test="LRT")

# Modelo final segun LRT 

mal_aliento2_mod_final<- glm(formula = Bad_breath ~ Body_mass_index + Energetic_value + 
                               Health_security_value + Times_brush_teeth + Brush_before_bed + 
                               Floss_use_after + Dental_care_12 + Satisfaction_level, family = "binomial", 
                             data = PdDB)

summary(mal_aliento2_mod_final)

vif(mal_aliento2_mod_final)

# Intervalos de confianza

confint(mal_aliento2_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(mal_aliento2_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(mal_aliento2_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$mal_aliento ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Abscessed_teeth######################################
#se eliminó: Education_level

Abscessed_teeth_mod <- glm(Abscessed_teeth ~ Sex + Energetic_value
                       + Living_value	+ Health_security_value + Physical_activity 
                       + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value
                       + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                       + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                       + Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant
                       + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth
                       + Lost_bone	+ Lost_tooth
                       + Tooth_does_not_look_right + Have_gum_disease	+ Gum_infected
                       + Bad_breath	 + Gum_health	
                       + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Abscessed_teeth_mod)            


step(Abscessed_teeth_mod, test="LRT")

# Modelo final segun LRT 

Abscessed_teeth_mod_final<- glm(formula = Abscessed_teeth ~ Sex + Living_value + Brush_your_teeth + 
                                   Floss_use_after + Dental_care_12 + Tooth_does_not_look_right + 
                                   Have_gum_disease + Gum_infected + Bad_breath+ 
                                   Gum_health + Satisfaction_level, family = "binomial", 
                                 data = PdDB)

summary(Abscessed_teeth_mod_final)

vif(Abscessed_teeth_mod_final)

# Intervalos de confianza

confint(Abscessed_teeth_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Abscessed_teeth_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Abscessed_teeth_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Abscessed_teeth ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Gum_surgery#######################################

#se eliminó: Education_level

Gum_surgery_mod <- glm(Gum_surgery~ Sex 
                            + Living_value	+ Health_security_value + Physical_activity 
                            + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value + Energetic_value
                            + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth + Brush_before_bed
                            + Floss_use_after+ Times_floss_use + Times_mouthwash_use
                            + Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant
                            + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth
                            + Lost_bone	+ Lost_tooth
                            + Tooth_does_not_look_right + Have_gum_disease	+ Gum_infected
                            + Bad_breath	+ Abscessed_teeth + Gum_health	
                            + Satisfaction_level + COVID19	+ MetS, data = PdDB, family = "binomial")


summary(Gum_surgery_mod)            

step(Gum_surgery_mod, test="LRT")

# Modelo final segun LRT 

Gum_surgery_mod_final<- glm(formula = Gum_surgery ~ Edu_lag_value + Smoking + Brush_your_teeth + 
                                 Use_dental_prostheses + Num_implant + Gum_treatment + Lost_bone + 
                                 Lost_tooth + Have_gum_disease, family = "binomial", 
                               data = PdDB)

summary(Gum_surgery_mod_final)

vif(Gum_surgery_mod_final)


# Intervalos de confianza

confint(Gum_surgery_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Gum_surgery_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Gum_surgery_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Gum_surgery ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)
################Periodontal_care#######################################

#se eliminó: Education_level + Lost_bone	+ Lost_tooth + Gum_health + COVID19 + Satisfaction_level 

Periodontal_care_mod <- glm(Periodontal_care~ Sex
                       + Living_value	+ Health_security_value + Physical_activity 
                       + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value + Energetic_value
                       + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth 
                       + Brush_before_bed+ Floss_use_after+ Times_floss_use + Times_mouthwash_use
                       + Use_dental_prostheses + Dental_care_12 + Implant_placed	+ Num_implant
                       + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth 
                       + Tooth_does_not_look_right + Have_gum_disease	+ Gum_infected
                       + Bad_breath	+ Abscessed_teeth + MetS 
                      	+ Gum_surgery, data = PdDB, family = "binomial")


summary(Periodontal_care_mod)            

step(Periodontal_care_mod, test="LRT")

# Modelo final segun LRT 

Periodontal_care_mod_final<- glm(formula = Periodontal_care ~ Durable_goods_value + Sanitary_value + Use_dental_prostheses + 
                              Implant_placed + Num_loose_teeth + Have_gum_disease + Gum_surgery, 
                            family = "binomial", data = PdDB)

summary(Periodontal_care_mod_final)

vif(Periodontal_care_mod_final)

# Intervalos de confianza

confint(Periodontal_care_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Periodontal_care_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Periodontal_care_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Periodontal_care ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Dental_care_1#######################################

Dental_care1_12_mod <- glm(Dental_care_12 ~ Sex + Physical_activity 
                          + Body_mass_index 	
                          + Systolic_average	
                          + Diastolic_average
                          + SDI_value	 + Satisfaction_level 
                       + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth 
                       + Brush_before_bed+ Floss_use_after+ Times_floss_use + Times_mouthwash_use
                      + Implant_placed + COVID19
                     	+ Had_bleeding_gums + Num_loose_teeth 
                       + Tooth_does_not_look_right + Have_gum_disease	+ Gum_infected
                       + Bad_breath	+ Abscessed_teeth + MetS + Lost_tooth + Lost_bone
                       , data = PdDB, family = "binomial")


summary(Dental_care1_12_mod)            

step(Dental_care1_12_mod, test="LRT")

# Modelo final segun LRT 

Dental_care1_12_mod_final<- glm(formula = Dental_care_12 ~ Sex + Sleep_quality + Times_brush_teeth + 
                                  Floss_use_after + Times_floss_use + Times_mouthwash_use + 
                                  Implant_placed + Num_loose_teeth + Tooth_does_not_look_right + 
                                  Gum_infected + Bad_breath + Abscessed_teeth + MetS + Lost_tooth, 
                                family = "binomial", data = PdDB)

summary(Dental_care1_12_mod_final)

vif(Dental_care1_12_mod_final)

# Intervalos de confianza

confint(Dental_care1_12_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Dental_care1_12_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Dental_care1_12_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Dental_care_12 ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Dental_care2#######################################
Dental_care2_12_mod <- glm(Dental_care_12 ~ Sex + Physical_activity 
                          + Body_mass_index + Energetic_value
                          + Living_value	+ Health_security_value 
                          + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value 
                          + Systolic_average	
                          + Diastolic_average
                          + SDI_value	 + Satisfaction_level 
                          + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth 
                          + Brush_before_bed+ Floss_use_after+ Times_floss_use + Times_mouthwash_use
                          + Implant_placed + COVID19
                          + Had_bleeding_gums + Num_loose_teeth 
                          + Tooth_does_not_look_right + Have_gum_disease	+ Gum_infected
                          + Bad_breath	+ Abscessed_teeth + MetS + Lost_tooth + Lost_bone
                          , data = PdDB, family = "binomial")


summary(Dental_care2_12_mod)            

step(Dental_care2_12_mod, test="LRT")

# Modelo final segun LRT 

Dental_care2_12_mod_final<- glm(formula = Dental_care_12 ~ Sex + Sleep_quality + Times_brush_teeth + 
                                  Floss_use_after + Times_floss_use + Times_mouthwash_use + 
                                  Implant_placed + Num_loose_teeth + Tooth_does_not_look_right + 
                                  Gum_infected + Bad_breath + Abscessed_teeth + MetS + Lost_tooth, 
                                family = "binomial", data = PdDB)

summary(Dental_care2_12_mod_final)

vif(Dental_care2_12_mod_final)

# Intervalos de confianza

confint(Dental_care2_12_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Dental_care2_12_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Dental_care2_12_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Dental_care_12 ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Implant_placed#######################################
# se eliminó: Tooth_does_not_look_right + Education_level + Num_implant

Implant_placed_mod <- glm(Implant_placed ~ Sex  + Energetic_value
                                    + Living_value	+ Health_security_value + Physical_activity + Gum_health + Dental_care_12
                                    + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value + Satisfaction_level 
                                    + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth 
                                    + Brush_before_bed+ Floss_use_after+ Times_floss_use + Times_mouthwash_use
                                    + Use_dental_prostheses   + COVID19 + Periodontal_care
                                    + Gum_treatment	+ Had_bleeding_gums + Num_loose_teeth 
                                   + Have_gum_disease	+ Gum_infected   
                                    + Bad_breath	+ Abscessed_teeth + MetS + Lost_tooth + Lost_bone + Gum_surgery
                                    , data = PdDB, family = "binomial")


summary(Implant_placed_mod)            

step(Implant_placed_mod, test="LRT")

# Modelo final segun LRT 

Implant_placed_mod_final<- glm(formula = Implant_placed ~ Sex + Gum_health + Dental_care_12 + 
                           Smoking + Times_brush_teeth + Times_mouthwash_use + 
                           Use_dental_prostheses + Periodontal_care + Num_loose_teeth + Lost_tooth + 
                           Gum_surgery, family = "binomial", data = PdDB)

summary(Implant_placed_mod_final)

vif(Implant_placed_mod_final)

# Intervalos de confianza

confint(Implant_placed_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Implant_placed_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Implant_placed_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Implant_placed ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Gum_treatment_1##################
# se eliminó: Education_level 

Gum_treatment1_mod <- glm(Gum_treatment ~ Sex + Body_mass_index 	
                          + Systolic_average	
                          + Diastolic_average
                          + SDI_value	+ Physical_activity + 
                   + Satisfaction_level 
                    + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth 
                    + Brush_before_bed+ Floss_use_after+ Times_floss_use + Times_mouthwash_use
                    + COVID19 + Periodontal_care + Tooth_does_not_look_right 
                    + Implant_placed + Had_bleeding_gums + Num_loose_teeth 
                    + Have_gum_disease	+ Gum_infected   
                    + Bad_breath	+ Abscessed_teeth + MetS + Lost_tooth + Lost_bone 
                    , data = PdDB, family = "binomial")


summary(Gum_treatment1_mod)            

step(Gum_treatment1_mod, test="LRT")

# Modelo final segun LRT 

Gum_treatment1_mod_final<- glm(formula = Gum_treatment ~ Diastolic_average + Smoking + Times_brush_teeth + 
                                 Implant_placed + Had_bleeding_gums + Bad_breath + Lost_tooth + 
                                 Lost_bone, family = "binomial", data = PdDB)

summary(Gum_treatment1_mod_final)

vif(Gum_treatment1_mod_final)

# Intervalos de confianza

confint(Gum_treatment1_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Gum_treatment1_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Gum_treatment1_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Gum_treatment ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Gum_treatment_2#######################################

Gum_treatment2_mod <- glm(Gum_treatment ~ Sex + Body_mass_index 	
                         + Systolic_average	+ Living_value	+ Health_security_value 
                         + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value
                         + Diastolic_average
                         + SDI_value	+ Physical_activity 
                           + Satisfaction_level 
                         + Sleep_quality + Smoking + Brush_your_teeth + Times_brush_teeth 
                         + Brush_before_bed+ Floss_use_after+ Times_floss_use + Times_mouthwash_use
                         + COVID19 + Periodontal_care + Tooth_does_not_look_right 
                         + Implant_placed + Had_bleeding_gums + Num_loose_teeth 
                         + Have_gum_disease	+ Gum_infected   
                         + Bad_breath	+ Abscessed_teeth + MetS + Lost_tooth + Lost_bone 
                         , data = PdDB, family = "binomial")


summary(Gum_treatment2_mod)            

step(Gum_treatment2_mod, test="LRT")

# Modelo final segun LRT 

Gum_treatment2_mod_final<- glm(formula = Gum_treatment ~ Diastolic_average + Smoking + Times_brush_teeth + 
                                 Implant_placed + Had_bleeding_gums + Bad_breath + Lost_tooth + 
                                 Lost_bone, family = "binomial", data = PdDB)

summary(Gum_treatment2_mod_final)

vif(Gum_treatment2_mod_final)

# Intervalos de confianza

confint(Gum_treatment2_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Gum_treatment2_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Gum_treatment2_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Gum_treatment ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Brush_your_teeth#######################################

# se eliminó: Education_level +  Times_brush_teeth + Times_floss_use + Brush_before_bed+ Floss_use_after + Times_mouthwash_use

Brush_your_teeth_mod <- glm(Brush_your_teeth ~ Sex + Energetic_value
                              + Living_value	+ Health_security_value + Physical_activity + Gum_health + Dental_care_12
                              + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value + Satisfaction_level 
                              + Sleep_quality + Smoking + Use_dental_prostheses + COVID19 + Periodontal_care + Tooth_does_not_look_right 
                              + Implant_placed + Had_bleeding_gums + Num_loose_teeth + Num_implant
                              + Have_gum_disease + Gum_infected + Gum_treatment
                              + Bad_breath	+ Abscessed_teeth + MetS + Lost_tooth + Lost_bone + Gum_surgery
                              , data = PdDB, family = "binomial")


summary(Brush_your_teeth_mod)            

step(Brush_your_teeth_mod, test="LRT")

# Modelo final segun LRT 

Brush_your_teeth_mod_final<- glm(formula = Brush_your_teeth ~ Sex + Health_security_value + 
                                  Gum_health + Durable_goods_value + Sanitary_value + Satisfaction_level + 
                                  Smoking + Use_dental_prostheses + Had_bleeding_gums + Have_gum_disease + 
                                  Gum_infected + Bad_breath+ Abscessed_teeth, family = "binomial", 
                                data = PdDB)

summary(Brush_your_teeth_mod_final)

vif(Brush_your_teeth_mod_final)

# Intervalos de confianza

confint(Brush_your_teeth_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Brush_your_teeth_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Brush_your_teeth_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Brush_your_teeth ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

################Floss_use_after#######################################

# se eliminó: Education_level +  Times_brush_teeth + Times_floss_use 

Floss_use_after_mod <- glm(Floss_use_after ~ Sex + Energetic_value
                               + Living_value	+ Health_security_value + Physical_activity + Gum_health + Dental_care_12
                               + Edu_lag_value	+ Durable_goods_value	+ Sanitary_value + Satisfaction_level 
                               + Sleep_quality + Smoking + Use_dental_prostheses + COVID19 + Periodontal_care + Tooth_does_not_look_right 
                               + Implant_placed + Had_bleeding_gums + Num_loose_teeth + Num_implant + Brush_before_bed + Times_mouthwash_use
                               + Have_gum_disease + Gum_infected + Gum_treatment + Brush_your_teeth
                               + Bad_breath	+ Abscessed_teeth + MetS + Lost_tooth + Lost_bone + Gum_surgery
                               , data = PdDB, family = "binomial")


summary(Floss_use_after_mod)            

step(Floss_use_after_mod, test="LRT")

# Modelo final segun LRT 

Floss_use_after_mod_final<- glm(formula = Floss_use_after ~ Anxiety_trait + Dental_care_12 + 
                              Sanitary_value + Smoking + COVID19 + Tooth_does_not_look_right + 
                              Had_bleeding_gums + Brush_before_bed + Times_mouthwash_use + 
                              Gum_infected + Brush_your_teeth + Abscessed_teeth + 
                              Lost_bone, family = "binomial", data = PdDB)

summary(Floss_use_after_mod_final)

vif(Floss_use_after_mod_final)

# Intervalos de confianza

confint(Floss_use_after_mod_final)

# Odds ratios Cálculo de razones de momios

lreg.or <-exp(cbind(OR = coef(Floss_use_after_mod_final)))

round(lreg.or, digits=4)

test_prob = predict(Floss_use_after_mod_final, newdata = PdDB, type = "response")
test_roc = roc(PdDB$Floss_use_after ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)

##############################################################


# family poisson es para conteos, si la media es diferente de la varianza (casi siempre) entonces usar quasipoisson

Veces_dia_cepilla_ids <- glm(formula = Times_brush_teeth ~ Sex + Body_mass_index + Education_level + Physical_activity + 
                            Anxiety_trait + COVID19, family = "quasipoisson", 
                          data = PdDB)

summary(Veces_dia_cepilla_ids)

optimo_perio <- glm(formula = Severe_periodontitis ~ Sex + Education_level + Times_floss_use + 
      Lost_bone + Tooth_does_not_look_right + Num_loose_teeth + 
      Abscessed_teeth + COVID19, family = "binomial", 
    data = PdDB)

summary(optimo_perio)



###Correlacion cuando la variable a predecir es numérica####################

Living_value<-lm(Living_value ~ Education_level
            	 + Smoking + Brush_your_teeth + Brush_before_bed
             + Times_floss_use 
               + Implant_placed	+ Num_implant
            	 + Num_loose_teeth
             + Lost_bone	
              + Gum_infected	+ Bad_breath
             + Abscessed_teeth	 + COVID19 + Sex + Anxiety_trait + MetS
              + Gum_health + Satisfaction_level + Gum_surgery + Use_dental_prostheses  + Gum_treatment +  Tooth_does_not_look_right + Floss_use_after + Times_brush_teeth + Times_mouthwash_use + Lost_tooth + Dental_care_12 + Have_gum_disease + Had_bleeding_gums
             , data = PdDB)
summary(Living_value)

step(Living_value)

optimo_Vivienda_num <- glm(formula = Living_value ~ Education_level + Smoking + Brush_your_teeth + 
                             Brush_before_bed + Times_floss_use + Lost_bone + 
                             Gum_infected + Abscessed_teeth + COVID19, data = PdDB)

summary(optimo_Vivienda_num)
