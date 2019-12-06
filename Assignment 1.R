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

clean_data <- data_sample_1 %>% 	
  mutate(STAI_trait = as.numeric(replace(STAI_trait, STAI_trait == "3.5", 35)), 
         household_income = as.numeric(replace(household_income, household_income == "-4562", 45620)))	#gör om från 3,5 till 35

summary(clean_data)
describe(clean_data)           
 	
ggplot(data=clean_data, aes(x=pain, y=age)) + geom_point()
ggplot(data=clean_data, aes(x=pain, y=sex)) + geom_point()

mod.1 <- lm(pain ~ age + sex, data = clean_data )
print(mod.1)

mod.3 <- lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness, data=clean_data)   #forsätt
print(mod.3)

lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness, data=clean_data, subset = -123) #checked the three clear outliers and their affect on the model
lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness, data=clean_data, subset = -88)
lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness, data=clean_data, subset = -74)
summary(mod.3)

###########################assumption of regression
mod.3 %>% 	
  plot(which = 4)
mod.3 %>% 	
  plot(which = 5)

cooks.distance(model=mod.1)
cooks.distance(model=mod.3)

###################checking for normality of the residuals 
mod.3 %>% 	
  plot(which = 2)

hist( x = residuals( mod.3 ), xlab = "Value of residual", main = "",breaks = 20)

residuals_mod.3 = enframe(residuals(mod.3))	
residuals_mod.3 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram() 

describe(residuals(mod.3))

residuals(object = mod.1)
residuals(object = mod.3)

openGraph()
plot(cooks.distance(model = mod.1))
plot(cooks.distance(model = mod.3))
plot(x=mod.1, which = 4)
plot(x=mod.3, which = 4)
plot(x=mod.3, which = 5)

data_nooutliers = clean_data %>% 	
  slice(-c(74, 88)) #try remove outliers
print(data_nooutliers)
describe(data_nooutliers)

mod.4 <- lm(pain ~ age+sex+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness, data=data_nooutliers) 

summary(mod.3)
summary(mod.4)

#linearty 
openGraph()
mod.3 %>% 	
  residualPlots()

#homoscedacisty
mod.3 %>% 	
  plot(which = 3)

mod.3 %>% 	
  ncvTest()

mod.3 %>% 	
  bptest()

#multicollinearity 
mod.3 %>% 	
  vif()	

#Try to fix the multicollinearity
#2
clean_data %>% 	
  select(pain, age, sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness) %>% 	
  pairs.panels(col = "red", lm = T)	
#1
mod.withcortisol = lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum*cortisol_saliva, clean_data)
mod.withcortisol %>% 	
  vif()	

clean_data_test = clean_data %>% 	
  mutate(corstisol_serum_clean = cortisol_serum - mean(cortisol_serum),	
         cortisol_saliva_clean = cortisol_saliva - mean(cortisol_saliva))

mod.withcortisol_clean = lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+corstisol_serum_clean*cortisol_saliva_clean, data= clean_data_test)

mod.withcortisol_clean %>% 	
  vif()	

summary(mod.withcortisol_clean)
summary(mod.3)

mod.no.serum = lm(pain ~ age+sex+STAI_trait+pain_cat+mindfulness+cortisol_saliva, data= clean_data)
mod.no.serum %>% 	
  vif()	

# Rerun all the tests with the new model 
#Normality:
mod.no.serum %>% 	
  plot(which = 2)

describe(residuals(mod.no.serum)) #skew and kurtosis still holds TRUE

residuals(object = mod.no.serum)

plot(cooks.distance(model = mod.no.serum))
plot(x=mod.no.serum, which = 4)
plot(x=mod.no.serum, which = 5)

#Linearity  
mod.no.serum %>% 	
  residualPlots()

#homoscedacisty
mod.no.serum %>% 	
  plot(which = 3)

mod.no.serum %>% 	
  ncvTest()

mod.no.serum%>% 	
  bptest()

#ANOVA
anova(mod.1,mod.3)
anova(mod.1, mod.no.serum)

#Summary
summary(mod.1)
summary(mod.no.serum)

#regression coeffiscient 
sm = summary(mod.no.serum)

confint(mod.no.serum)	
lm.beta(mod.no.serum)	

sm_table = coef_table(mod.no.serum) 
sm_table

sm_table2 = coef_table(mod.1)
sm_table2

#AIC
AIC(mod.1)
AIC(mod.no.serum)






  














