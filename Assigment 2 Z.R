setwd("~/Desktop/PSYP13")
rm(list = ls())
library(lsr)
library(psych)
library(sciplot)
library(tidyverse)
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(lm.beta)

coef_table = function(model){
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))	
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))	
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"	
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)	
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
  mod_sum_table["(Intercept)","Std.Beta"] = "0"	
  return(mod_sum_table)
}


source("GraphPlot.R")

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")
who(TRUE)
describe(data_sample_1)
summary(data_sample_1) #kolla på värdena, något som sticker ut??

str(data_sample_1)
plot(pain~age, data=data_sample_1)

#clean the data as last one
clean_data <- data_sample_1 %>% 	
  mutate(STAI_trait = as.numeric(replace(STAI_trait, STAI_trait == "3.5", 35)), 
         household_income = as.numeric(replace(household_income, household_income == "-4562", 45620)))	

full_model <- lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum+mindfulness+weight+household_income+IQ, data=clean_data)
print(full_model)
summary(full_model)

#cooks distance 
full_model %>% 	
  plot(which = 4) #same that can be interepreted as outliers as in the model of the first assignment
full_model %>% 	
  plot(which = 5)
cooks.distance(full_model) #higer than 4/N?? 

#Check if remove outliers might effect the linearity?
data_nooutliers = clean_data %>% 	
  slice(-c(55, 74, 88))

full_model_nooutliers <- lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum+mindfulness+weight+household_income+IQ, data=data_nooutliers)


#Normality
full_model %>% 	
  plot(which = 2)
residuals(full_model)
describe(residuals(full_model))
residualPlot(full_model) #skew and kurtosis is fine 
plot(full_model)
residuals_full_model = enframe(residuals(full_model))	
residuals_full_model %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram() #inte bell shaped!

residuals_full_model %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()

#linearty 
openGraph()
full_model %>% 	
  residualPlots()
#cat pain is sig, needs to be corrected 

#test
openGraph()
full_model_nooutliers %>% #test utan outliers får inte upp tabell 
  residualPlots()
residuals_full_model_nooutliers = enframe(residuals(full_model_nooutliers))	
residuals_full_model_nooutliers %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()

full_model_higherorder <- lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum++mindfulness+weight+household_income+IQ+I(age^2)+I(sex^2)+I(STAI_trait^2)+I(pain_cat^2)+I(cortisol_serum^2)+I(mindfulness^2)+I(weight^2)+I(household_income^2)+I(IQ^2), data=clean_data) #funkar inte!
full_model_higherorder <- lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum++mindfulness+weight+household_income+IQ+I(pain_cat^2), data=clean_data)
summary(full_model_higherorder)
summary(full_model)

AIC(full_model)
AIC(full_model_higherorder) #lägre värde än tdigare vilket innebär att modellen passar bättre in. Gjort den mer flexibel men inte för flexibel!

full_model %>% 	
  residualPlots()

#homoscedacisty
full_model_higherorder %>% 	
  plot(which = 3)

full_model %>% 	
  plot(which = 3)

full_model_higherorder %>% 	
  ncvTest() #ej sig

full_model %>% 	
  ncvTest() #ej sig

full_model_higherorder %>% 	
  bptest() #ej sig men nära!

full_model %>% 	
  bptest()

#multicollinearity 
full_model_higherorder %>% 	
  vif()	#all values under 3 before changing to the new model. When entering the higher order 

full_model %>%
  vif()


#Backward regression 

step(object = full_model, direction = "backward") 
      # allow it remove predictors but not add them +)

backward.modell <- lm(pain ~ age+sex+pain_cat+cortisol_serum+mindfulness+weight, data=clean_data)
#Best modell with lowest AIC = 59.52

mod.no.serum = lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_saliva, data= clean_data)
#model from assignment 1

AIC(backward.modell)
AIC(mod.no.serum)

anova(backward.modell, mod.no.serum)
anova(mod.no.serum, backward.modell)

backward.modell
summary(backward.modell)

####data 2
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")

summary(data_sample_2)
describe(data_sample_2)


#test
age = c(20,30,40,50,60)
STAI_trait = c(20,30,40,50,60)
pain_cat = c(20,30,40,50,60)
cortisol_saliva = c(2,4,5,6,7)
mindfulness = c(1,3,5,7)
try_to_predict = as.data.frame(cbind(age,STAI_trait,pain_cat,cortisol_saliva,mindfulness))

predicted_try = predict(mod.no.serum, newdata = try_to_predict)


newdata_try1<-data.frame(age=42, STAI_trait=40, pain_cat=30,cortisol_saliva=4, mindfulness=3)
predict(mod.no.serum, newdata_try1)

#kanske
predict(mod.no.serum, data_sample_2)
plot(predict(mod.no.serum, data_sample_2))

#tabell
sm_table = coef_table(mod.no.serum) 
sm_table

sm_table2 = coef_table(backward.modell)
sm_table2







