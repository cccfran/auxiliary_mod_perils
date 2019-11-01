library(tidyverse)
expit <- function(x) { exp(x)/(1+exp(x)) }

set.seed(1) #this seed gives good contrast between Ypred and Y

##########################################################################

# First setting:
# When there is no association between X and Y there can be an apparent association between X and Ypred

##########################################################################
	
# Generate auxiliary data 

	N<-10000

	#Generate exposure 
		
		Exposure<-runif(N,40,70)

	# Generate non-collider covariates that will predict Y
	
		Homeowner<-rbinom(N,1,.52)
		Population<-rnorm(N,3976,6765)
		NO2<-rnorm(N,10.1,4.5)
		PM25<-rnorm(N,9.1,1.9)	
		
	# Generate collider covariates to go into Ypred model
		# Poverty is strong collider between Homeowner and Exposure

		PovertyPredictor<-(1-Homeowner)*Exposure+.1*(Homeowner)*Exposure
		Poverty<-rbinom(N,1,PovertyPredictor/max(PovertyPredictor))
		
		Race<-rmultinom(N,1,prob=c(.2,.45,1-.65))
   		White<-Race[2,]
		Black<-Race[1,]
		Female<-rbinom(N,1,.45)
		Old<-rbinom(N,1,.05)

	#Generate Y based on Homeowner but not Poverty so that the path between Exposure and Y is collided
             
    	   	Y<-rbinom(N,1,(1-Homeowner)*.7+Homeowner*.3)
    
	#Save data
    
   	 	auxiliarydata<-as.data.frame(cbind(Y,Exposure,Population,NO2,PM25,Poverty,Black,White,Female, Old))
    
    
#Overwrite this to generate analysis data 

	N<-1000
	
	#Generate exposure 
	
		Exposure<-runif(N)

	# Generate non-collider covariates that will predict Y
	
		Homeowner<-rbinom(N,1,.52)
		Population<-rnorm(N,3976,6765)
		NO2<-rnorm(N,10.1,4.5)
		PM25<-rnorm(N,9.1,1.9)	
		
	# Generate collider covariates to go into Ypred model
		# Poverty is strong collider between Homeowner and Exposure

		PovertyPredictor<-(1-Homeowner)*Exposure+.1*(Homeowner)*Exposure
		Poverty<-rbinom(N,1,PovertyPredictor/max(PovertyPredictor))
		
		Race<-rmultinom(N,1,prob=c(.2,.45,1-.65))
   		White<-Race[2,]
		Black<-Race[1,]
		Female<-rbinom(N,1,.45)
		Old<-rbinom(N,1,.05)

	#Generate Y based on Homeowner but not Poverty so that the path between Exposure and Y is collided
             
    	   	Y<-rbinom(N,1,(1-Homeowner)*.7+Homeowner*.3)

    
	#Save data
    
   	 	analysisdata<-as.data.frame(cbind(Y,Exposure,Population,NO2,PM25,Poverty,Black,White,Female, Old))
    
  

# Create analysis data outcome variable

	#Predict Y using auxiliary data

		Model1<-glm(Y~Poverty+Black+White+Female+Old, family=binomial, data=auxiliarydata)
		
	#Get fitted values for analysis data
		
		Ypred<-predict(Model1,newdata=analysisdata,type="response")
			

#Analyze data

	# Unadjusted model shows strong association
	
	p1<-glm(Ypred~Exposure, data=analysisdata, family=binomial())		
	summary(p1)
	
	# Using Y instead of Ypred, the true association is null
	
	p1true<-glm(Y~Exposure, data=analysisdata, family=binomial())		
	summary(p1true)


# Make Figure using Joan's code
# Joan will layer the picture for p1 over the picture for p1true

# Figure using Ypred

	hyp_newdata1b <- with(analysisdata, data.frame(Exposure))
                                            
	hyp_newdata2b <- cbind(hyp_newdata1b, predict(p1, newdata = hyp_newdata1b, 
                                              type = "link", se = TRUE))
	hyp_newdata2b <- within(hyp_newdata2b, {
			  PredictedProb <- plogis(fit)
			  LL <- plogis(fit - (1.96 * se.fit))
			  UL <- plogis(fit + (1.96 * se.fit))
			})


		ggplot(hyp_newdata2b, aes(x = Exposure, y = PredictedProb)) + 
		geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) + 
 		geom_line(aes(), size = 1) +
		scale_y_continuous("Predicted Proportion Poor Sleep", breaks = c(.3,.35, .4 ,.45, .5, .55,.6), limits=c(.3, .6)) +
		theme_minimal(base_size = 14) +
		scale_x_continuous("Average day-night noise level (dBA)",breaks=c(0,0.33,0.67,1),labels=c("40","50","60","70"))
		
		
# Figure using the true Y 

	hyp_newdata1c <- with(analysisdata, data.frame(Exposure))
                                            
	hyp_newdata2c <- cbind(hyp_newdata1c, predict(p1true, newdata = hyp_newdata1c, 
                                              type = "link", se = TRUE))
	hyp_newdata2c <- within(hyp_newdata2c, {
			  PredictedProb <- plogis(fit)
			  LL <- plogis(fit - (1.96 * se.fit))
			  UL <- plogis(fit + (1.96 * se.fit))
			})

		ggplot(hyp_newdata2c, aes(x = Exposure, y = PredictedProb)) + 
		geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) + 
 		geom_line(aes(), size = 1) +
		scale_y_continuous("Predicted Proportion Poor Sleep", breaks = c(.3,.35, .4 ,.45, .5, .55,.6), limits=c(.3, .6)) +
		theme_minimal(base_size = 14) +
		scale_x_continuous("Average day-night noise level (dBA)",breaks=c(0,0.33,0.67,1),labels=c("40","50","60","70"))
	
		
#Together truth and predicted for scenario 1
hyp_newdata2c$TrueY <- 1
hyp_newdata2b$TrueY <- 0
hyp_newdata2d <- rbind(hyp_newdata2b,hyp_newdata2c)

#Plot
a <-ggplot(hyp_newdata2d, aes(x = Exposure, y = PredictedProb, 
                          group = factor(TrueY), color = factor(TrueY), fill = factor(TrueY))) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(TrueY), color = NA), alpha = 0.1) + 
  geom_line(aes(color = factor(TrueY)), size = 1.5) +
  scale_color_brewer("Model", palette="Set1", labels = c("Predicted Y","True Y")) +
  scale_fill_brewer("Model", palette="Set1", labels = c("Predicted Y","True Y"))+
  scale_y_continuous("Predicted Proportion Poor Sleep", breaks = c(.3,.35, .4 ,.45, .5, .55,.6), limits=c(.3, .6)) +
  theme_minimal(base_size = 14) +
  scale_x_continuous("Average simulated day-night noise level (dBA)",breaks=c(0,0.33,0.67,1),labels=c("40","50","60","70"))

#Save plot
tiff(filename = "/Users/joancasey/Documents/Columbia/RMF/Urban_Institute/Scenario1_Pred_True_Y.tiff", type = "cairo" ,  res=300, height = 5, width = 6, units = "in")
		  ggplot(hyp_newdata2d, aes(x = Exposure, y = PredictedProb, 
		                            group = factor(TrueY), color = factor(TrueY), fill = factor(TrueY))) + 
		  geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(TrueY), color = NA), alpha = 0.1) + 
		  geom_line(aes(color = factor(TrueY)), size = 1.5) +
		  scale_color_brewer("Model", palette="Set1", labels = c("Predicted Y","True Y")) +
		  scale_fill_brewer("Model", palette="Set1", labels = c("Predicted Y","True Y"))+
		  scale_y_continuous("Predicted Proportion Poor Sleep", breaks = c(.3,.35, .4 ,.45, .5, .55,.6), limits=c(.3, .6)) +
		  theme_minimal(base_size = 14) +
		  scale_x_continuous("Average simulated day-night noise level (dBA)",breaks=c(0,0.33,0.67,1),labels=c("40","50","60","70"))
dev.off()


		

