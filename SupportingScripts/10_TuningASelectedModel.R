#
##
### This script imports stocka assessment models from stockassessment.org, and modifies their configuration in attempts to tune the model
### This script can run independently, so long as the model exists, is committed and is made public on the website.
###
### Authored by Elliot Brown, based on scripts provided by Casper W. Berg.
##
#

#===
# Data/model and dependencies ----
#===
library(stockassessment)

fit = fitfromweb("ple.27.21-32_WKBPLAICE_2024_5ysw")
#===

#===
# Model with maximum freedom ----
#===
## Configure coupling of variation and corellations
### Copy Configuration
conf_free = fit$conf

### Coupling of the fishing mortality states.                            
conf_free$keyLogFsta[1,]=c(0:6)
# conf_free$keyLogFsta[2,]=c(7:13) # Only for multi-fleet

### Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
conf_free$corFlag=0

####Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above.
# NA's indicate where correlation parameters can be specified (-1 where they cannot).
# values represent age pairs 1-2 2-3 3-4 4-5 5-6 6-7                        
conf_Fcor$keyCorObs[1,]= c(rep("NA",6))
conf_Fcor$keyCorObs[2,]= c(rep("NA",6))
conf_Fcor$keyCorObs[3,]= c(rep("NA",6))

### Coupling of the survey catchability parameters.                            
conf_free$keyLogFpar[2,]=0:6    #Change row selected if more than one fishing fleet
conf_free$keyLogFpar[3,]=7:13   #Change row selected if more than one fishing fleet

### Configure 
conf_free$keyVarObs[1,]=c(0:6)
conf_free$keyVarObs[2,]=c(7:13)
conf_free$keyVarObs[3,]=c(14:20)

# conf_free$keyVarObs[1,]=c(0,rep(1,6))
# conf_free$keyVarObs[2,]=c(2,rep(3,6))
# conf_free$keyVarObs[3,]=rep(4,7)
# conf_free$keyVarObs[4,]=rep(5,7)   #Only if extra fleets

## Refit model with new config
fit_free = stockassessment:::refit(fit,newConf=conf_free)
#===

#===
# Model with F correlation across ages (minimal restrictions) ----
#===
## Configure coupling of variation and corellations
### Copy Configuration
conf_Fcor = fit$conf

### Coupling of the fishing mortality states.                            
conf_Fcor$keyLogFsta[1,]=c(0:6)
# conf_Fcor$keyLogFsta[2,]=c(7:13) # Only for multi-fleet

### Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
conf_Fcor$corFlag=2

####Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above.
# NA's indicate where correlation parameters can be specified (-1 where they cannot).
# values represent age pairs 1-2 2-3 3-4 4-5 5-6 6-7                        
conf_Fcor$keyCorObs[1,]= c(rep("NA",6))
conf_Fcor$keyCorObs[2,]= c(rep("NA",6))
conf_Fcor$keyCorObs[3,]= c(rep("NA",6))

### Coupling of the survey catchability parameters.                            
conf_Fcor$keyLogFpar[2,]=0:6    #Change row selected if more than one fishing fleet
conf_Fcor$keyLogFpar[3,]=7:13   #Change row selected if more than one fishing fleet

### Configure 
conf_Fcor$keyVarObs[1,]=c(0:6)
conf_Fcor$keyVarObs[2,]=c(7:13)
conf_Fcor$keyVarObs[3,]=c(14:20)

# conf_Fcor$keyVarObs[1,]=c(0,rep(1,6))
# conf_Fcor$keyVarObs[2,]=c(2,rep(3,6))
# conf_Fcor$keyVarObs[3,]=rep(4,7)
# conf_Fcor$keyVarObs[4,]=rep(5,7)   #Only if extra fleets

## Refit model with new config
fit_free = stockassessment:::refit(fit,newConf=conf_Fcor)
#===

#===
# Model with F correlation across ages (minimal restrictions) ----
#===
## Configure coupling of variation and corellations
### Copy Configuration
conf_Fcor = fit$conf

### Coupling of the fishing mortality states.                            
# conf_Fcor$keyLogFsta[1,]=c(0:6)
# conf_Fcor$keyLogFsta[2,]=c(7:13) # Only for multi-fleet

### Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# conf_Fcor$corFlag=2

####Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above.
# NA's indicate where correlation parameters can be specified (-1 where they cannot).
# values represent age pairs 1-2 2-3 3-4 4-5 5-6 6-7                        
conf_Fcor$keyCorObs[1,]= c(rep("NA",6))
conf_Fcor$keyCorObs[2,]= c(rep("NA",6))
conf_Fcor$keyCorObs[3,]= c(rep("NA",6))

### Coupling of the survey catchability parameters.                            
conf_Fcor$keyLogFpar[2,]=0:6    #Change row selected if more than one fishing fleet
conf_Fcor$keyLogFpar[3,]=7:13   #Change row selected if more than one fishing fleet

### Configure 
conf_Fcor$keyVarObs[1,]=c(0:6)
conf_Fcor$keyVarObs[2,]=c(7:13)
conf_Fcor$keyVarObs[3,]=c(14:20)

# conf_Fcor$keyVarObs[1,]=c(0,rep(1,6))
# conf_Fcor$keyVarObs[2,]=c(2,rep(3,6))
# conf_Fcor$keyVarObs[3,]=rep(4,7)
# conf_Fcor$keyVarObs[4,]=rep(5,7)   #Only if extra fleets

## Refit model with new config
fit_free = stockassessment:::refit(fit,newConf=conf_Fcor)
#===



#===
# Interrogate Model(s) ----
#===
## Base
resid = residuals(fit)
plot(fit)
retr = retro(fit,5)
mohn(retr)
plot(retr)


## Model of Interest
fit2 <- fit_free
resid = residuals(fit2)
plot(fit_free)
retr_2 = retro(fit2,5)
mohn(retr_2)
plot(retr_2)

## Compare models
plot(c(fit,fit2))
#===
