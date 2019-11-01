###CODE FOR FIGURE 2####
library(tidyverse)
library(MASS)
library(gridExtra)
set.seed(1390)

setwd("Urban_Institute_500cities_Noise/Data")
load("final_noise_outcome_apr19.RData")
final_dta <-  dplyr::select(final_dta, NO2, pm25, population_density, prop_homeowner, prop_over_75yo, female_p, prop_white, prop_black, obesity, smoke, hbp, sleep7, mental, dnl_mean, hhinc,prop_below_poverty )
final_dta <- na.omit(final_dta)

#Rescale population density
final_dta$population_density <- final_dta$population_density*1000*1000

###PREDICTIONS###
#Sleep
#q = sleep, unadjusted
q1 <- glm(sleep7 ~ dnl_mean , data = final_dta, family = binomial())
#Predict after running regression
newdatq1 <- with(final_dta, data.frame(dnl_mean = seq(40,70, by = 1)))
newdatq2 <- cbind(newdatq1, predict(q1, newdata = newdatq1, type = "link",
                                    se = TRUE))
newdatq2 <- within(newdatq2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

tiff(filename = "Sleep_Unadjusted", type = "cairo" ,  res=300, height = 4, width = 4, units = "in")
ggplot(newdatq2, aes(x = dnl_mean, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) + 
  scale_y_continuous("Predicted Proportion Poor Sleep", breaks = c(.15, .2 ,.25, .3, .35, .4, .45), limits=c(0.175, 0.475)) +
  geom_line(aes(), size = 1) + 
  theme_minimal(base_size = 14) +
  xlab("Average day-night noise level (dBA)") 
dev.off()

#2 Model adjusted for confounding variables but not variables included in the auxillary model
#Predict after running regression
p2 <- glm(sleep7 ~ dnl_mean + population_density + NO2 + pm25 + prop_homeowner , data = final_dta, family = binomial(), na.action = na.omit)
summary(p2)
#Predict after running regression
sleep_newdata1b <- with(final_dta, data.frame(dnl_mean = seq(40,70, by = 1), NO2 = mean(NO2), pm25 = mean(pm25),
                                              population_density = mean(population_density), prop_homeowner = mean(prop_homeowner) 
))
sleep_newdata2b <- cbind(sleep_newdata1b, predict(p2, newdata = sleep_newdata1b, 
                                                  type = "link", se = TRUE))
sleep_newdata2b <- within(sleep_newdata2b, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

tiff(filename = "Sleep_Adj1.tiff", type = "cairo" ,  res=300, height = 4, width = 4, units = "in")
ggplot(sleep_newdata2b, aes(x = dnl_mean, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) + 
  geom_line(aes(), size = 1) +
  scale_y_continuous("Predicted Proportion Poor Sleep", breaks = c(.15, .2 ,.25, .3, .35, .4, .45), limits=c(0.175, 0.475)) +
  theme_minimal(base_size = 14) +
  xlab("Average day-night noise level (dBA)") 
dev.off()

#3 Model adjusted for confounding variables and variables included in auxiliary model
#Predict after running regression
p3 <- glm(sleep7 ~ dnl_mean + population_density + NO2 + pm25 + prop_homeowner + prop_white +  prop_black + prop_over_75yo + female_p +  prop_below_poverty, data = final_dta, family = binomial(), na.action = na.omit)
summary(p3)
#Predict after running regression
sleep_newdata1b <- with(final_dta, data.frame(dnl_mean = seq(40,70, by = 1), NO2 = mean(NO2), pm25 = mean(pm25),
                                              population_density = mean(population_density), prop_homeowner = mean(prop_homeowner),
                                              prop_white = mean(prop_white), prop_below_poverty = mean(prop_below_poverty),
                                              prop_black = mean(prop_black), prop_over_75yo = mean(prop_over_75yo), female_p = mean(female_p)))
sleep_newdata2b <- cbind(sleep_newdata1b, predict(p3, newdata = sleep_newdata1b, 
                                                  type = "link", se = TRUE))
sleep_newdata2b <- within(sleep_newdata2b, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

tiff(filename = "Sleep_Adj2.tiff", type = "cairo" ,  res=300, height = 4, width = 4, units = "in")
ggplot(sleep_newdata2b, aes(x = dnl_mean, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) + 
  geom_line(aes(), size = 1) +
  scale_y_continuous("Predicted Proportion Poor Sleep", breaks = c(.15, .2 ,.25, .3, .35, .4, .45), limits=c(0.175, 0.475)) +
  theme_minimal(base_size = 14) +
  xlab("Average day-night noise level (dBA)") 
dev.off()