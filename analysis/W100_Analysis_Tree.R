## This example uses boosted regression trees to examine the relative
## importance of model edges (linkages) in determining the response
## of [A NODE] to an increase/decrease in [A DIFFERENT NODE] for simulations
## of a given Dia network. Code modified from Melbourne-Thomas et al 2012
## (https://doi.org/10.1890/12-0207.1)

# Load required packages
library(QPress)
library(tcltk2)
library(XML)
library(here) #folder management
library(dismo)
library(gbm)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

#setwd("/Users/caitlinmagel/Documents/UW-PSI Postdoc/UW-EarthLab Postdoc/QNM") #Redundant with using 'here'


##### Read model specification ##### 
edges <- QPress::model.dia("./model/SolutionModel_25Oct2022_forR.dia")
edges <- enforce.limitation(edges)

## Examine unweighted adjacency matrix
A <- QPress::adjacency.matrix(edges, labels=TRUE)
A
adjacency.image(edges)
write.csv(A, file = "Model_AdjMatrix.csv", row.names = TRUE)

## Function to generate the community matrix
s <- QPress::community.sampler(edges)


##### Tree Analysis: RESIDENTIAL ##### 

## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Source control"=1, "Residential"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
    tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {

  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()

  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1

  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns
#write.csv(d, "Edgeweights_forTreeModel_26Aug2022.csv")

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                 gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
				learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
dev.new()
opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence
dev.new()
gbm.plot(fit_physhealth, n.plots=28, write.title = FALSE) #fitted function for each variable - this doesn't seem to work


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns
#write.csv(d2, "Biodiv_Edgeweights_forTreeModel.csv")

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                 gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                 learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
dev.new()
opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Resi_Source_Comparison_13Sept2022.csv", row.names = F)

## different perturbation
## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Gray infrastructure"=1, "Residential"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
  tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1
  
  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns
#write.csv(d, "Edgeweights_forTreeModel_26Aug2022.csv")

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                           gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                           learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
dev.new()
opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns
#write.csv(d2, "Biodiv_Edgeweights_forTreeModel.csv")

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                       gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                       learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
dev.new()
opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Resi_Gray_Comparison_13Sept2022.csv", row.names = F)


### different perturbation
## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Green infrastructure"=1, "Residential"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
  tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1
  
  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns
#write.csv(d, "Edgeweights_forTreeModel_26Aug2022.csv")

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                           gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                           learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns
#write.csv(d2, "Biodiv_Edgeweights_forTreeModel.csv")

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                       gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                       learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Resi_Green_Comparison_13Sept2022.csv", row.names = F)

##### Tree Analysis: INDUSTRIAL ##### 

## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Source control"=1, "Industrial"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
  tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1
  
  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                           gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                           learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
dev.new()
opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence
dev.new()
gbm.plot(fit_physhealth, n.plots=28, write.title = FALSE) #fitted function for each variable - this doesn't seem to work


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                       gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                       learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
dev.new()
opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Indus_Source_Comparison_30Dec2022.csv", row.names = F)

## different perturbation
## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Gray infrastructure"=1, "Industrial"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
  tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1
  
  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                           gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                           learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
dev.new()
opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                       gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                       learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
dev.new()
opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Indus_Gray_Comparison_30Dec2022.csv", row.names = F)


### different perturbation
## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Green infrastructure"=1, "Industrial"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
  tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1
  
  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                           gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                           learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                       gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                       learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Indus_Green_Comparison_30Dec2022.csv", row.names = F)

##### Tree Analysis: TRANSPORTATION ##### 

## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Source control"=1, "Transportation"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
  tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1
  
  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                           gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                           learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence
#dev.new()
#gbm.plot(fit_physhealth, n.plots=28, write.title = FALSE) #fitted function for each variable - this doesn't seem to work


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                       gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                       learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Trans_Source_Comparison_30Dec2022.csv", row.names = F)

## different perturbation
## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Gray infrastructure"=1, "Transportation"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
  tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1
  
  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                           gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                           learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                       gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                       learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Trans_Gray_Comparison_30Dec2022.csv", row.names = F)


### different perturbation
## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Green infrastructure"=1, "Transportation"=1)) #node name and direction of perturbation

## Keep track of edge weights in data frame
Wlog <- matrix(nrow=1,ncol=length(edges$From))
Wlog <- as.data.frame(Wlog)
whichW <- which(A!=0)
tempedgenames <- matrix("",nrow=dim(A)[1],ncol=dim(A)[2])
for (i in 1:dim(edges)[1]) {
  tempedgenames[edges$To[i],edges$From[i]] <- paste(edges$From[i],edges$To[i],sep='_')
}
colnames(Wlog) <- tempedgenames[whichW]

## Use 1000 simulations
n.sims <- 1000
n.stable <- 0
allimp <- matrix(0,n.sims,length(levels(edges$From)))
while(n.stable < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  n.stable <- n.stable+1
  
  ## Monitor impact post press
  imp <- impact(W)
  allimp[n.stable,] <- imp
  Wlog[n.stable,] <- W[whichW]
}

## Fit classification tree model (with SPECIFY NODE as the response)
temp <- sign(allimp)
colnames(temp) <- levels(edges$From)
temp <- cbind(temp,Wlog)
d <- cbind(temp["Physical health"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d) #number of columns

fit_physhealth <- gbm.step(data=d, gbm.x=c(2:135), #specify which columns to use as predictors
                           gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                           learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_physhealth <- summary(fit_physhealth, las=2) #Relative influence


##Compare to another node

## Fit classification tree model (with SPECIFY NODE as the response)
d2 <- cbind(temp["Biodiversity"],temp[names(Wlog)]) #SPECIFY NODE HERE
ncol(d2) #number of total columns

fit_biodiv <- gbm.step(data=d2, gbm.x=c(2:135),
                       gbm.y=1, family="gaussian", tree.complexity=5, #gbm.x = indices or names of predictor variables in 'data', gbm.y = response variables in 'data'
                       learning.rate = 0.005, bag.fraction = 0.5) #Harvey et al used lr = 0.001 to get >1,000 trees
#dev.new()
#opar <- par(mar=c(5,10,1,1)+0.1)
rel_inf_biodiv <- summary(fit_biodiv, las=2) #Relative influence

rel_inf_comp <- full_join(rel_inf_biodiv, rel_inf_physhealth, by = "var", suffix = c(".Biodiv", ".PhysHealth"))
write.csv(rel_inf_comp, "Influence_Trans_Green_Comparison_30Dec2022.csv", row.names = F)

##### Relative Influence results and compare: RESIDENTIAL ##### 

#first, load the key for the proper link names
link_key_sept <- read.csv("KEY_LinkName_forRelInf_12Sept.csv", header = TRUE) 
#load the results
resi_green_relinf <- read.csv("Influence_Resi_Green_Comparison_13Sept2022.csv", header = TRUE) %>% full_join(link_key_sept, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
resi_green_relinf$Solution <- "green"
resi_green_relinf$LandUse <- "Residential"

resi_gray_relinf <- read.csv("Influence_Resi_Gray_Comparison_13Sept2022.csv", header = TRUE) %>% full_join(link_key_sept, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
resi_gray_relinf$Solution <- "gray"
resi_gray_relinf$LandUse <- "Residential"

resi_source_relinf <- read.csv("Influence_Resi_Source_Comparison_13Sept2022.csv", header = TRUE) %>% full_join(link_key_sept, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
resi_source_relinf$Solution <- "source"
resi_source_relinf$LandUse <- "Residential"

#combine the three datasets
resi_relinf <- rbind(resi_green_relinf, resi_gray_relinf, resi_source_relinf)


sol.labs <- c("Source Control", "Gray Infrastructure", "Green Infrastructure")
names(sol.labs) <- c("source", "gray", "green")

resi_relinf_bySol <- resi_relinf %>% filter(relative_influence >0.999) %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = relative_influence, y = link, color = Node, shape = Node)) + 
  geom_point(size = 5,position = position_jitter(width = 0.03, height = 0.13)) +
  facet_wrap(~Solution, labeller = as_labeller(sol.labs)) + 
  theme_bw() + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(color = "gray96")) +
  scale_colour_viridis_d() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  labs(x = "Relative influence", y = "Link")

resi_relinf_byNode <- resi_relinf %>% filter(relative_influence >0.999) %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = relative_influence, y = link, color = Solution, shape = Solution)) + 
  geom_point(size = 5) +
  facet_wrap(~Node) + 
  theme_bw() + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(color = "gray96"),
                     panel.grid.major.y = element_line(color = "gray96")) +
  scale_colour_viridis_d() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  labs(x = "Relative influence", y = "Link")


##### Relative Influence results and compare: INDUSTRIAL ##### 

#first, load the key for the proper link names
link_key <- read.csv("KEY_LinkName_forRelInf_30Dec.csv", header = TRUE) 
#load the results
indus_green_relinf <- read.csv("Influence_Indus_Green_Comparison_30Dec2022.csv", header = TRUE) %>% full_join(link_key, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
indus_green_relinf$Solution <- "green"
indus_green_relinf$LandUse <- "Industrial"

indus_gray_relinf <- read.csv("Influence_Indus_Gray_Comparison_30Dec2022.csv", header = TRUE) %>% full_join(link_key, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
indus_gray_relinf$Solution <- "gray"
indus_gray_relinf$LandUse <- "Industrial"

indus_source_relinf <- read.csv("Influence_Indus_Source_Comparison_30Dec2022.csv", header = TRUE) %>% full_join(link_key, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
indus_source_relinf$Solution <- "source"
indus_source_relinf$LandUse <- "Industrial"

#combine the three datasets
indus_relinf <- rbind(indus_green_relinf, indus_gray_relinf, indus_source_relinf)

#plot the relative influence values across solution

sol.labs <- c("Source Control", "Gray Infrastructure", "Green Infrastructure")
names(sol.labs) <- c("source", "gray", "green")

indus_relinf_byNode <- indus_relinf %>% filter(relative_influence >0.999) %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = relative_influence, y = link, color = Solution, shape = Solution)) + 
  geom_point(size = 5) +
  facet_wrap(~Node) + 
  theme_bw() + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(color = "gray96"),
                     panel.grid.major.y = element_line(color = "gray96")) +
  scale_colour_viridis_d() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  labs(x = "Relative influence", y = "Link")
           


##### Relative Influence results and compare: TRANSPORTATION ##### 

#first, load the key for the proper link names
link_key <- read.csv("KEY_LinkName_forRelInf_30Dec.csv", header = TRUE) 
#load the results
trans_green_relinf <- read.csv("Influence_Trans_Green_Comparison_30Dec2022.csv", header = TRUE) %>% full_join(link_key, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
trans_green_relinf$Solution <- "green"
trans_green_relinf$LandUse <- "Transportation"

trans_gray_relinf <- read.csv("Influence_Trans_Gray_Comparison_30Dec2022.csv", header = TRUE) %>% full_join(link_key, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
trans_gray_relinf$Solution <- "gray"
trans_gray_relinf$LandUse <- "Transportation"

trans_source_relinf <- read.csv("Influence_Trans_Source_Comparison_30Dec2022.csv", header = TRUE) %>% full_join(link_key, by = "var") %>%
  gather(key="Node", value="relative_influence", 2:3) %>% mutate(Node = replace(Node, Node == 'rel.inf.PhysHealth', 'Physical health')) %>% 
  mutate(Node = replace(Node, Node == 'rel.inf.Biodiv', 'Biodiversity')) %>% dplyr::select(!(var))
trans_source_relinf$Solution <- "source"
trans_source_relinf$LandUse <- "Transportation"

#combine the three datasets
trans_relinf <- rbind(trans_green_relinf, trans_gray_relinf, trans_source_relinf)

#plot the relative influence values across solution
trans_relinf_byNode <- trans_relinf %>% filter(relative_influence >0.999) %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = relative_influence, y = link, color = Solution, shape = Solution)) + 
  geom_point(size = 5) +
  facet_wrap(~Node) + 
  theme_bw() + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(color = "gray96"),
                     panel.grid.major.y = element_line(color = "gray96")) +
  scale_colour_viridis_d() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  labs(x = "Relative influence", y = "Link")



##### Combine and rank the relinf datasets for all three land uses ##### 

relinf_all <- rbind(trans_relinf, indus_relinf, resi_relinf)

#calculate rank and cumulative sum of relative influence for each LandUse+Solution+Node combo
relinf_all$rank <- NA
relinf_all <- relinf_all %>% group_by(Solution, Node, LandUse) %>% mutate(rank = dense_rank(desc(relative_influence))) %>% arrange(LandUse, Solution, Node, rank)
relinf_all <- relinf_all %>% group_by(Solution, Node, LandUse) %>% mutate(cum_sum = cumsum(relative_influence))

write.csv(relinf_all, "RelativeInfluence_BiodivPhysHealth.csv", row.names = FALSE)

##### plot the relative influence values across land use and solution #####
relinf_all <- read.csv("./results/RelativeInfluence_BiodivPhysHealth.csv")
#merge with link_type
link_key <- read.csv("./model/KEY_LinkName_forRelInf_30Dec.csv", header = TRUE) 
relinf_all <- relinf_all %>% full_join(link_key, by = "link")
relinf_all <- select(relinf_all, !var)


relinf_cumulative <- relinf_all %>% 
  ggplot(aes(y = cum_sum, x = rank, color = Solution, linetype = Solution)) + 
  geom_step(size = 0.9) +
  facet_wrap(~LandUse+Node, ncol = 2) +  
  theme_bw() + 
  scale_colour_viridis_d() +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  theme(panel.grid.major.y = element_line(color = "gray96")) +
  labs(y = "Relative influence (cumulative sum)", x = "Link")
ggsave("./figs/RelInf_CumulativeCurves.png", width = 11, height = 8, device='png', dpi=400)


#plot all the link relinf across land uses and sources 
plot_relinf_tiles <- relinf_all %>% 
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = link, fill = relative_influence)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) + coord_fixed() + theme_bw(base_size = 9) +
  theme(panel.grid.major = element_blank(), aspect.ratio=4/1) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(0, 30), breaks = c(0, 10, 20, 30),
                                                      colours=c("#FFFFFF", "#FFA590", "#FF6242", "#FB3B1E"),
                                                      labels = c("0", "10", "20", "30")) +
  facet_wrap(~Node+LandUse, ncol = 6) + 
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Link", fill = element_blank(), size = 12)
ggsave("./figs/RelInf_Tiles_byLandUse.png", width = 13, height = 9, device='png', dpi=700)

#plot just the inter-node links
plot_relinf_tiles_inter <- relinf_all %>% filter(link_type == "inter") %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = link, fill = relative_influence)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) + coord_fixed() + theme_bw(base_size = 10) +
  theme(panel.grid.major = element_blank(), aspect.ratio=4/1) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(0, 30), breaks = c(0, 10, 20, 30),
                                                      colours=c("#FFFFFF", "#FFA590", "#FF6242", "#FB3B1E"),
                                                      labels = c("0", "10", "20", "30")) +
  facet_wrap(~Node+LandUse, ncol = 6) + 
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Inter-Node Link", fill = element_blank(), size = 12)
ggsave("./figs/RelInf_Tiles_byLandUse_interlinks.png", width = 13, height = 9, device='png', dpi=700)


#inter-node links
plot_relinf_tiles_inter <- relinf_all %>% filter(link_type == "inter") %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = link, fill = relative_influence)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) + coord_fixed() + theme_bw(base_size = 10) +
  theme(panel.grid.major = element_blank(), aspect.ratio=4/1) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(0, 30), breaks = c(0, 10, 20, 30),
                                                      colours=c("#FFFFFF", "#FFA590", "#FF6242", "#FB3B1E"),
                                                      labels = c("0", "10", "20", "30")) +
  facet_wrap(~Node+LandUse, ncol = 6) + 
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Inter-Node Link", fill = element_blank(), size = 12)
ggsave("./figs/RelInf_Tiles_byLandUse_interlinks.png", width = 13, height = 9, device='png', dpi=700)


#plot just the self-effects links
plot_relinf_tiles_self <- relinf_all %>% filter(link_type == "self") %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = link, fill = relative_influence)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) + coord_fixed() + theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), aspect.ratio=3/1) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(0, 30), breaks = c(0, 10, 20, 30),
                                                      colours=c("#FFFFFF", "#FFA590", "#FF6242", "#FB3B1E"),
                                                      labels = c("0", "10", "20", "30")) +
  facet_wrap(~Node+LandUse, ncol = 3) + 
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Self-effect Link", fill = element_blank(), size = 12)
ggsave("./figs/RelInf_Tiles_byLandUse_selflinks.png", width = 13, height = 9, device='png', dpi=700)


#plot the links with the highest (top 10) relative influence across solutions for each Land Use
# RESIDENTIAL
relinf_top10links_res <- relinf_all %>% filter(LandUse == "Residential" & rank < 6) %>% ungroup() %>% select(link) %>% distinct()
relinf_top10_res <- relinf_all %>% ungroup() %>% filter(LandUse == "Residential") %>% inner_join(relinf_top10links_res, by = "link")

relinf_top_resi <- relinf_top10_res %>% mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = relative_influence, group = link, color = link, shape = link)) + 
  geom_line(size = 0.8) + geom_point(size = 4) +
  theme_bw() + ylim(0,30) +
  scale_colour_viridis_d() +
  scale_shape_manual(values = c(0, 15, 1, 16, 2, 17, 5, 19, 6, 8, 18)) +
  facet_wrap(~Node, ncol = 1) +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  theme(panel.grid.major.y = element_line(color = "gray96")) +
  labs(y = "Relative influence", x = "Solution") #+ annotate("text", x = 0.65, y = 25, label = "Residential", size = 6)
ggsave("figures/RelInf_TopLinks_Residential.png", width = 11, height = 8, device='png', dpi=400)

# INDUSTRIAL
relinf_top10links_indus <- relinf_all %>% filter(LandUse == "Industrial" & rank < 6) %>% ungroup() %>% select(link) %>% distinct()
relinf_top10_indus <- relinf_all %>% ungroup() %>% filter(LandUse == "Industrial") %>% inner_join(relinf_top10links_indus, by = "link")

relinf_top_indus <- relinf_top10_indus %>% mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = relative_influence, group = link, color = link, shape = link)) + 
  geom_line(size = 0.8) + geom_point(size = 4) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_shape_manual(values = c(0, 15, 1, 16, 2, 17, 5, 19, 6, 8, 18)) +
  facet_wrap(~Node, ncol = 1) +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  theme(panel.grid.major.y = element_line(color = "gray96")) +
  labs(y = "Relative influence", x = "Solution") #+ annotate("text", x = 0.65, y = 25, label = "Industrial", size = 6)
ggsave("figures/RelInf_TopLinks_Industrial.png", width = 11, height = 8, device='png', dpi=400)

# TRANSPORTATION
relinf_top10links_trans <- relinf_all %>% filter(LandUse == "Transportation" & rank < 6) %>% ungroup() %>% select(link) %>% distinct()
relinf_top10_trans <- relinf_all %>% ungroup() %>% filter(LandUse == "Transportation") %>% inner_join(relinf_top10links_trans, by = "link")

relinf_top_trans <- relinf_top10_trans %>% mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = relative_influence, group = link, color = link, shape = link)) + 
  geom_line(size = 0.8) + geom_point(size = 4) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_shape_manual(values = c(0, 15, 1, 16, 2, 17, 5, 19, 6, 8, 18)) +
  facet_wrap(~Node, ncol = 1) +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  theme(panel.grid.major.y = element_line(color = "gray96")) +
  labs(y = "Relative influence", x = "Solution") #+ annotate("text", x = 0.65, y = 25, label = "Transportation", size = 6)
ggsave("figures/RelInf_TopLinks_Transportation.png", width = 11, height = 8, device='png', dpi=400)


#join all the top links from each land use
top_links <- rbind(relinf_top10links_res, relinf_top10links_trans, relinf_top10links_indus) %>% select(link) %>% distinct()

#plot all the link relinf across land uses and sources 
plot_relinf_tiles_top <- relinf_all %>% inner_join(top_links, by = "link") %>% 
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = link, fill = relative_influence)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) + coord_fixed() + theme_bw(base_size = 9) +
  theme(panel.grid.major = element_blank(), aspect.ratio=4/1) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(0, 30), breaks = c(0, 10, 20, 30),
                                                      colours=c("#FFFFFF", "#FFA590", "#FF6242", "#FB3B1E"),
                                                      labels = c("0", "10", "20", "30")) +
  facet_wrap(~Node+LandUse, ncol = 3) + 
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Link", fill = element_blank(), size = 12)
ggsave("./figs/RelInf_Tiles_byLandUse_Top.png", width = 13, height = 9, device='png', dpi=700)

#plot all the link relinf across land uses and sources 
plot_relinf_tiles_top_self <- relinf_all %>% inner_join(top_links, by = "link") %>% filter(link_type == "self") %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = link, fill = relative_influence)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) + coord_fixed() + theme_bw(base_size = 9) +
  theme(panel.grid.major = element_blank(), aspect.ratio=4/1) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(0, 30), breaks = c(0, 10, 20, 30),
                                                      colours=c("#FFFFFF", "#FFA590", "#FF6242", "#FB3B1E"),
                                                      labels = c("0", "10", "20", "30")) +
  facet_wrap(~Node+LandUse, ncol = 3) + 
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Link", fill = element_blank(), size = 12)
ggsave("./figs/RelInf_Tiles_byLandUse_Top_Self.png", width = 9, height = 9, device='png', dpi=700)

plot_relinf_tiles_top_inter <- relinf_all %>% inner_join(top_links, by = "link") %>% filter(link_type == "inter") %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = link, fill = relative_influence)) +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) + coord_fixed() + theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), aspect.ratio=3/1) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(0, 21), breaks = c(0, 7, 14, 21),
                                                      colours=c("#FFFFFF", "#FFA590", "#FF6242", "#FB3B1E"),
                                                      labels = c("0", "7", "14", "21")) +
  facet_wrap(~Node+LandUse, ncol = 3) + 
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Inter-Node Link", fill = element_blank(), size = 12)
ggsave("./figs/RelInf_Tiles_byLandUse_Top_Inter.png", width = 9, height = 9, device='png', dpi=700)
