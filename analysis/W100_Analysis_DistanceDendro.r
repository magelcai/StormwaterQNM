## This example demonstrates the calculation of distance measures
## (and associated clustering) for node responses to a perturbation
## scenario (suppression of rabbits, rats and mice) for Raymond et al.'s 
## (2011, Journal of Applied Ecology ) Subantarctic Macquarie Island model 
## ("macquarie.dia"). Code modified from Melbourne-Thomas et al 2012
## (https://doi.org/10.1890/12-0207.1)

# Load required packages
library(QPress)
library(tcltk2)
library(XML)
library(here) #folder management

#source("dia.r")
#source("community.r")

## Read model specification
edges <- QPress::model.dia("./model/SolutionModel_25Oct2022_forR.dia")
edges <- enforce.limitation(edges)

## Examine unweighted adjacency matrix
A <- adjacency.matrix(edges)
A

## Function to generate the community matrix
s <- QPress::community.sampler(edges)

## Function to check the validation condition
#press <- press.validate(edges,
#                         perturb=c("Rabbits"=1),
#                         monitor=c("Rabbits"=1,"Tall tussock vegetation"=-1))


##### Source Control + Residential #####
## Function to define the perturbation scenario
impact_ResSou <- QPress::press.impact(edges,perturb=c("Source control"=1, "Residential"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_ResSou <- list(0,0,0)
i <- 0
while(i < n.sims) {


  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()

  ## Check stability
  if(!stable.community(W)) next

  ## Monitor impact post press
  imp <- sign(impact_ResSou(W))

  ## Compute distances
  dist_ResSou[[1]] <- dist_ResSou[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_ResSou[[2]] <- dist_ResSou[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_ResSou[[3]] <- dist_ResSou[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}

dist.scale <- list(0,0,0)
for(i in 1:3){
	rownames(dist[[i]]) <- levels(edges$From)
	colnames(dist[[i]]) <- levels(edges$From)
	dist.scale[[i]] <- dist[[i]]/max(dist[[i]])
}

## Use clustering to show similarity in model variable responses, including land uses and solutions
fit1 <- hclust(as.dist(dist.scale[[1]]))
fit2 <- hclust(as.dist(dist.scale[[2]]))
fit3 <- hclust(as.dist(dist.scale[[3]]))
plot(fit1,xlab='',ylab='Distance (d1)')
#dev.new()
plot(fit2,xlab='',ylab='Distance (d2)')
#dev.new()
plot(fit3,xlab='',ylab='Distance (d3)')


#remove the solution and land use nodes from the distance list
to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale.1 <- dist.scale[[1]] 
dist.scale.1 <- dist.scale.1[!(rownames(dist.scale.1) %in% to_remove),]
dist.scale.1 <- dist.scale.1[,!(colnames(dist.scale.1) %in% to_remove)]

dist.scale.2 <- dist.scale[[2]] 
dist.scale.2 <- dist.scale.2[!(rownames(dist.scale.2) %in% to_remove),]
dist.scale.2 <- dist.scale.2[,!(colnames(dist.scale.2) %in% to_remove)]

dist.scale.3 <- dist.scale[[3]] 
dist.scale.3 <- dist.scale.3[!(rownames(dist.scale.3) %in% to_remove),]
dist.scale.3 <- dist.scale.3[,!(colnames(dist.scale.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1 <- hclust(as.dist(dist.scale.1))
fit2 <- hclust(as.dist(dist.scale.2))
fit3 <- hclust(as.dist(dist.scale.3))

png("figs/DistanceDendro_x3_ResSou.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1,xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2,xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3,xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()


##### Gray Infrastructure + Residential #####
## Function to define the perturbation scenario
impact_ResGra <- QPress::press.impact(edges,perturb=c("Gray infrastructure"=1, "Residential"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_ResGra <- list(0,0,0)
i <- 0
while(i < n.sims) {
  
  
  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  
  ## Monitor impact post press
  imp <- sign(impact_ResGra(W))
  
  ## Compute distances
  dist_ResGra[[1]] <- dist_ResGra[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_ResGra[[2]] <- dist_ResGra[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_ResGra[[3]] <- dist_ResGra[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}

dist.scale <- list(0,0,0)
for(i in 1:3){
  rownames(dist[[i]]) <- levels(edges$From)
  colnames(dist[[i]]) <- levels(edges$From)
  dist.scale[[i]] <- dist[[i]]/max(dist[[i]])
}

## Use clustering to show similarity in model variable responses, including land uses and solutions
fit1 <- hclust(as.dist(dist.scale[[1]]))
fit2 <- hclust(as.dist(dist.scale[[2]]))
fit3 <- hclust(as.dist(dist.scale[[3]]))
plot(fit1,xlab='',ylab='Distance (d1)')
#dev.new()
plot(fit2,xlab='',ylab='Distance (d2)')
#dev.new()
plot(fit3,xlab='',ylab='Distance (d3)')


#remove the solution and land use nodes from the distance list
to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale.1 <- dist.scale[[1]] 
dist.scale.1 <- dist.scale.1[!(rownames(dist.scale.1) %in% to_remove),]
dist.scale.1 <- dist.scale.1[,!(colnames(dist.scale.1) %in% to_remove)]

dist.scale.2 <- dist.scale[[2]] 
dist.scale.2 <- dist.scale.2[!(rownames(dist.scale.2) %in% to_remove),]
dist.scale.2 <- dist.scale.2[,!(colnames(dist.scale.2) %in% to_remove)]

dist.scale.3 <- dist.scale[[3]] 
dist.scale.3 <- dist.scale.3[!(rownames(dist.scale.3) %in% to_remove),]
dist.scale.3 <- dist.scale.3[,!(colnames(dist.scale.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1 <- hclust(as.dist(dist.scale.1))
fit2 <- hclust(as.dist(dist.scale.2))
fit3 <- hclust(as.dist(dist.scale.3))

png("figs/DistanceDendro_x3_ResGra.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1,xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2,xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3,xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()


##### Green Infrastructure + Residential #####
## Function to define the perturbation scenario
impact_ResGre <- QPress::press.impact(edges,perturb=c("Green infrastructure"=1, "Residential"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_ResGre <- list(0,0,0)
i <- 0
while(i < n.sims) {
  
  
  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  
  ## Monitor impact post press
  imp <- sign(impact_ResGre(W))
  
  ## Compute distances
  dist_ResGre[[1]] <- dist_ResGre[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_ResGre[[2]] <- dist_ResGre[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_ResGre[[3]] <- dist_ResGre[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}

dist.scale <- list(0,0,0)
for(i in 1:3){
  rownames(dist[[i]]) <- levels(edges$From)
  colnames(dist[[i]]) <- levels(edges$From)
  dist.scale[[i]] <- dist[[i]]/max(dist[[i]])
}

## Use clustering to show similarity in model variable responses, including land uses and solutions
fit1 <- hclust(as.dist(dist.scale[[1]]))
fit2 <- hclust(as.dist(dist.scale[[2]]))
fit3 <- hclust(as.dist(dist.scale[[3]]))
plot(fit1,xlab='',ylab='Distance (d1)')
#dev.new()
plot(fit2,xlab='',ylab='Distance (d2)')
#dev.new()
plot(fit3,xlab='',ylab='Distance (d3)')


#remove the solution and land use nodes from the distance list
to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale.1 <- dist.scale[[1]] 
dist.scale.1 <- dist.scale.1[!(rownames(dist.scale.1) %in% to_remove),]
dist.scale.1 <- dist.scale.1[,!(colnames(dist.scale.1) %in% to_remove)]

dist.scale.2 <- dist.scale[[2]] 
dist.scale.2 <- dist.scale.2[!(rownames(dist.scale.2) %in% to_remove),]
dist.scale.2 <- dist.scale.2[,!(colnames(dist.scale.2) %in% to_remove)]

dist.scale.3 <- dist.scale[[3]] 
dist.scale.3 <- dist.scale.3[!(rownames(dist.scale.3) %in% to_remove),]
dist.scale.3 <- dist.scale.3[,!(colnames(dist.scale.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1 <- hclust(as.dist(dist.scale.1))
fit2 <- hclust(as.dist(dist.scale.2))
fit3 <- hclust(as.dist(dist.scale.3))

png("figs/DistanceDendro_x3_ResGre.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1, xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2, xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3, xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()


##### Source Control + Industrial #####
## Function to define the perturbation scenario
impact_IndSou <- QPress::press.impact(edges,perturb=c("Source control"=1, "Industrial"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_IndSou <- list(0,0,0)
i <- 0
while(i < n.sims) {
  
  
  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  
  ## Monitor impact post press
  imp <- sign(impact_IndSou(W))
  
  ## Compute distances
  dist_IndSou[[1]] <- dist_IndSou[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_IndSou[[2]] <- dist_IndSou[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_IndSou[[3]] <- dist_IndSou[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}

##### Gray Infrastructure + Industrial #####
## Function to define the perturbation scenario
impact_IndGra <- QPress::press.impact(edges,perturb=c("Gray infrastructure"=1, "Industrial"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_IndGra <- list(0,0,0)
i <- 0
while(i < n.sims) {
  
  
  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  
  ## Monitor impact post press
  imp <- sign(impact_IndGra(W))
  
  ## Compute distances
  dist_IndGra[[1]] <- dist_IndGra[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_IndGra[[2]] <- dist_IndGra[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_IndGra[[3]] <- dist_IndGra[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}


##### Green Infrastructure + Industrial #####
## Function to define the perturbation scenario
impact_IndGre <- QPress::press.impact(edges,perturb=c("Green infrastructure"=1, "Industrial"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_IndGre <- list(0,0,0)
i <- 0
while(i < n.sims) {
  
  
  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  
  ## Monitor impact post press
  imp <- sign(impact_IndGre(W))
  
  ## Compute distances
  dist_IndGre[[1]] <- dist_IndGre[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_IndGre[[2]] <- dist_IndGre[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_IndGre[[3]] <- dist_IndGre[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}


##### Source Control + Transportation #####
## Function to define the perturbation scenario
impact_TranSou <- QPress::press.impact(edges,perturb=c("Source control"=1, "Transportation"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_TranSou <- list(0,0,0)
i <- 0
while(i < n.sims) {
  
  
  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  
  ## Monitor impact post press
  imp <- sign(impact_TranSou(W))
  
  ## Compute distances
  dist_TranSou[[1]] <- dist_TranSou[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_TranSou[[2]] <- dist_TranSou[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_TranSou[[3]] <- dist_TranSou[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}



##### Gray Infrastructure + Transportation #####
## Function to define the perturbation scenario
impact_TranGra <- QPress::press.impact(edges,perturb=c("Gray infrastructure"=1, "Transportation"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_TranGra <- list(0,0,0)
i <- 0
while(i < n.sims) {
  
  
  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  
  ## Monitor impact post press
  imp <- sign(impact_TranGra(W))
  
  ## Compute distances
  dist_TranGra[[1]] <- dist_TranGra[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_TranGra[[2]] <- dist_TranGra[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_TranGra[[3]] <- dist_TranGra[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}


##### Green Infrastructure + Transportation #####
## Function to define the perturbation scenario
impact_TranGre <- QPress::press.impact(edges,perturb=c("Green infrastructure"=1, "Transportation"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist_TranGre <- list(0,0,0)
i <- 0
while(i < n.sims) {
  
  
  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!stable.community(W)) next
  
  ## Monitor impact post press
  imp <- sign(impact_TranGre(W))
  
  ## Compute distances
  dist_TranGre[[1]] <- dist_TranGre[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist_TranGre[[2]] <- dist_TranGre[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist_TranGre[[3]] <- dist_TranGre[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}


##### BY LAND USE: add all distances together and analyze distances #####
#RESIDENTIAL LAND USE
#add together each distance for each perturbation
dist_res_d1 <- dist_ResSou[[1]] + dist_ResGra[[1]] + dist_ResGre[[1]]
dist_res_d2 <- dist_ResSou[[2]] + dist_ResGra[[2]] + dist_ResGre[[2]]
dist_res_d3 <- dist_ResSou[[3]] + dist_ResGra[[3]] + dist_ResGre[[3]]
#combine the matrices into a list
dist_res_list <- list(dist_res_d1, dist_res_d2, dist_res_d3)

#scale the results
dist.scale_res_sum <- list(0,0,0)
for(i in 1:3){
  rownames(dist_res_list[[i]]) <- levels(edges$From)
  colnames(dist_res_list[[i]]) <- levels(edges$From)
  dist.scale_res_sum[[i]] <- dist_res_list[[i]]/max(dist_res_list[[i]])
}


#remove the solution and land use nodes from the distance list
to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale_res_sum.1 <- dist.scale_res_sum[[1]] 
dist.scale_res_sum.1 <- dist.scale_res_sum.1[!(rownames(dist.scale_res_sum.1) %in% to_remove),]
dist.scale_res_sum.1 <- dist.scale_res_sum.1[,!(colnames(dist.scale_res_sum.1) %in% to_remove)]

dist.scale_res_sum.2 <- dist.scale_res_sum[[2]] 
dist.scale_res_sum.2 <- dist.scale_res_sum.2[!(rownames(dist.scale_res_sum.2) %in% to_remove),]
dist.scale_res_sum.2 <- dist.scale_res_sum.2[,!(colnames(dist.scale_res_sum.2) %in% to_remove)]

dist.scale_res_sum.3 <- dist.scale_res_sum[[3]] 
dist.scale_res_sum.3 <- dist.scale_res_sum.3[!(rownames(dist.scale_res_sum.3) %in% to_remove),]
dist.scale_res_sum.3 <- dist.scale_res_sum.3[,!(colnames(dist.scale_res_sum.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1_res <- hclust(as.dist(dist.scale_res_sum.1))
fit2_res <- hclust(as.dist(dist.scale_res_sum.2))
fit3_res <- hclust(as.dist(dist.scale_res_sum.3))

png("./figs/DistanceDendro_x3_Res_sum.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1_res, xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2_res, xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3_res, xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()


#INDUSTRIAL LAND USE
#add together each distance for each perturbation
dist_ind_d1 <- dist_IndSou[[1]] + dist_IndGra[[1]] + dist_IndGre[[1]]
dist_ind_d2 <- dist_IndSou[[2]] + dist_IndGra[[2]] + dist_IndGre[[2]]
dist_ind_d3 <- dist_IndSou[[3]] + dist_IndGra[[3]] + dist_IndGre[[3]]
#combine the matrices into a list
dist_ind_list <- list(dist_ind_d1, dist_ind_d2, dist_ind_d3)

#scale the results
dist.scale_ind_sum <- list(0,0,0)
for(i in 1:3){
  rownames(dist_ind_list[[i]]) <- levels(edges$From)
  colnames(dist_ind_list[[i]]) <- levels(edges$From)
  dist.scale_ind_sum[[i]] <- dist_ind_list[[i]]/max(dist_ind_list[[i]])
}


#remove the solution and land use nodes from the distance list
to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale_ind_sum.1 <- dist.scale_ind_sum[[1]] 
dist.scale_ind_sum.1 <- dist.scale_ind_sum.1[!(rownames(dist.scale_ind_sum.1) %in% to_remove),]
dist.scale_ind_sum.1 <- dist.scale_ind_sum.1[,!(colnames(dist.scale_ind_sum.1) %in% to_remove)]

dist.scale_ind_sum.2 <- dist.scale_ind_sum[[2]] 
dist.scale_ind_sum.2 <- dist.scale_ind_sum.2[!(rownames(dist.scale_ind_sum.2) %in% to_remove),]
dist.scale_ind_sum.2 <- dist.scale_ind_sum.2[,!(colnames(dist.scale_ind_sum.2) %in% to_remove)]

dist.scale_ind_sum.3 <- dist.scale_ind_sum[[3]] 
dist.scale_ind_sum.3 <- dist.scale_ind_sum.3[!(rownames(dist.scale_ind_sum.3) %in% to_remove),]
dist.scale_ind_sum.3 <- dist.scale_ind_sum.3[,!(colnames(dist.scale_ind_sum.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1_ind <- hclust(as.dist(dist.scale_ind_sum.1))
fit2_ind <- hclust(as.dist(dist.scale_ind_sum.2))
fit3_ind <- hclust(as.dist(dist.scale_ind_sum.3))

png("./figs/DistanceDendro_x3_Indus_sum.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1_ind, xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2_ind, xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3_ind, xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()


#TRANSPORTATION LAND USE
#add together each distance for each perturbation
dist_tran_d1 <- dist_TranSou[[1]] + dist_TranGra[[1]] + dist_TranGre[[1]]
dist_tran_d2 <- dist_TranSou[[2]] + dist_TranGra[[2]] + dist_TranGre[[2]]
dist_tran_d3 <- dist_TranSou[[3]] + dist_TranGra[[3]] + dist_TranGre[[3]]
#combine the matrices into a list
dist_tran_list <- list(dist_tran_d1, dist_tran_d2, dist_tran_d3)

#scale the results
dist.scale_tran_sum <- list(0,0,0)
for(i in 1:3){
  rownames(dist_tran_list[[i]]) <- levels(edges$From)
  colnames(dist_tran_list[[i]]) <- levels(edges$From)
  dist.scale_tran_sum[[i]] <- dist_tran_list[[i]]/max(dist_tran_list[[i]])
}

#remove the solution and land use nodes from the distance list
to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale_tran_sum.1 <- dist.scale_tran_sum[[1]] 
dist.scale_tran_sum.1 <- dist.scale_tran_sum.1[!(rownames(dist.scale_tran_sum.1) %in% to_remove),]
dist.scale_tran_sum.1 <- dist.scale_tran_sum.1[,!(colnames(dist.scale_tran_sum.1) %in% to_remove)]

dist.scale_tran_sum.2 <- dist.scale_tran_sum[[2]] 
dist.scale_tran_sum.2 <- dist.scale_tran_sum.2[!(rownames(dist.scale_tran_sum.2) %in% to_remove),]
dist.scale_tran_sum.2 <- dist.scale_tran_sum.2[,!(colnames(dist.scale_tran_sum.2) %in% to_remove)]

dist.scale_tran_sum.3 <- dist.scale_tran_sum[[3]] 
dist.scale_tran_sum.3 <- dist.scale_tran_sum.3[!(rownames(dist.scale_tran_sum.3) %in% to_remove),]
dist.scale_tran_sum.3 <- dist.scale_tran_sum.3[,!(colnames(dist.scale_tran_sum.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1_tran <- hclust(as.dist(dist.scale_tran_sum.1))
fit2_tran <- hclust(as.dist(dist.scale_tran_sum.2))
fit3_tran <- hclust(as.dist(dist.scale_tran_sum.3))

png("./figs/DistanceDendro_x3_Trans_sum.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1_tran, xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2_tran, xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3_tran, xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()



##### BY INFRASTRUCTURE: add all distances together and analyze distances #####
#Source Control
#add together each distance for each perturbation
dist_sou_d1 <- dist_ResSou[[1]] + dist_IndSou[[1]] + dist_TranSou[[1]]
dist_sou_d2 <- dist_ResSou[[2]] + dist_IndSou[[2]] + dist_TranSou[[2]]
dist_sou_d3 <- dist_ResSou[[3]] + dist_IndSou[[3]] + dist_TranSou[[3]]
#combine the matrices into a list
dist_sou_list <- list(dist_sou_d1, dist_sou_d2, dist_sou_d3)

#scale the results
dist.scale_sou_sum <- list(0,0,0)
for(i in 1:3){
  rownames(dist_sou_list[[i]]) <- levels(edges$From)
  colnames(dist_sou_list[[i]]) <- levels(edges$From)
  dist.scale_sou_sum[[i]] <- dist_sou_list[[i]]/max(dist_sou_list[[i]])
}

#remove the solution and land use nodes from the distance list
#to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale_sou_sum.1 <- dist.scale_sou_sum[[1]] 
dist.scale_sou_sum.1 <- dist.scale_sou_sum.1[!(rownames(dist.scale_sou_sum.1) %in% to_remove),]
dist.scale_sou_sum.1 <- dist.scale_sou_sum.1[,!(colnames(dist.scale_sou_sum.1) %in% to_remove)]

dist.scale_sou_sum.2 <- dist.scale_sou_sum[[2]] 
dist.scale_sou_sum.2 <- dist.scale_sou_sum.2[!(rownames(dist.scale_sou_sum.2) %in% to_remove),]
dist.scale_sou_sum.2 <- dist.scale_sou_sum.2[,!(colnames(dist.scale_sou_sum.2) %in% to_remove)]

dist.scale_sou_sum.3 <- dist.scale_sou_sum[[3]] 
dist.scale_sou_sum.3 <- dist.scale_sou_sum.3[!(rownames(dist.scale_sou_sum.3) %in% to_remove),]
dist.scale_sou_sum.3 <- dist.scale_sou_sum.3[,!(colnames(dist.scale_sou_sum.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1_sou <- hclust(as.dist(dist.scale_sou_sum.1))
fit2_sou <- hclust(as.dist(dist.scale_sou_sum.2))
fit3_sou <- hclust(as.dist(dist.scale_sou_sum.3))

png("./figs/DistanceDendro_x3_Source_sum.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1_sou, xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2_sou, xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3_sou, xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()

#Gray Infrastructure
#add together each distance for each perturbation
dist_gra_d1 <- dist_ResGra[[1]] + dist_IndGra[[1]] + dist_TranGra[[1]]
dist_gra_d2 <- dist_ResGra[[2]] + dist_IndGra[[2]] + dist_TranGra[[2]]
dist_gra_d3 <- dist_ResGra[[3]] + dist_IndGra[[3]] + dist_TranGra[[3]]
#combine the matrices into a list
dist_gra_list <- list(dist_gra_d1, dist_gra_d2, dist_gra_d3)

#scale the results
dist.scale_gra_sum <- list(0,0,0)
for(i in 1:3){
  rownames(dist_gra_list[[i]]) <- levels(edges$From)
  colnames(dist_gra_list[[i]]) <- levels(edges$From)
  dist.scale_gra_sum[[i]] <- dist_gra_list[[i]]/max(dist_gra_list[[i]])
}

#remove the solution and land use nodes from the distance list
#to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale_gra_sum.1 <- dist.scale_gra_sum[[1]] 
dist.scale_gra_sum.1 <- dist.scale_gra_sum.1[!(rownames(dist.scale_gra_sum.1) %in% to_remove),]
dist.scale_gra_sum.1 <- dist.scale_gra_sum.1[,!(colnames(dist.scale_gra_sum.1) %in% to_remove)]

dist.scale_gra_sum.2 <- dist.scale_gra_sum[[2]] 
dist.scale_gra_sum.2 <- dist.scale_gra_sum.2[!(rownames(dist.scale_gra_sum.2) %in% to_remove),]
dist.scale_gra_sum.2 <- dist.scale_gra_sum.2[,!(colnames(dist.scale_gra_sum.2) %in% to_remove)]

dist.scale_gra_sum.3 <- dist.scale_gra_sum[[3]] 
dist.scale_gra_sum.3 <- dist.scale_gra_sum.3[!(rownames(dist.scale_gra_sum.3) %in% to_remove),]
dist.scale_gra_sum.3 <- dist.scale_gra_sum.3[,!(colnames(dist.scale_gra_sum.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1_gra <- hclust(as.dist(dist.scale_gra_sum.1))
fit2_gra <- hclust(as.dist(dist.scale_gra_sum.2))
fit3_gra <- hclust(as.dist(dist.scale_gra_sum.3))

png("./figs/DistanceDendro_x3_Gray_sum.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1_gra, xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2_gra, xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3_gra, xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()


#Green Infrastructure
#add together each distance for each perturbation
dist_gre_d1 <- dist_ResGre[[1]] + dist_IndGre[[1]] + dist_TranGre[[1]]
dist_gre_d2 <- dist_ResGre[[2]] + dist_IndGre[[2]] + dist_TranGre[[2]]
dist_gre_d3 <- dist_ResGre[[3]] + dist_IndGre[[3]] + dist_TranGre[[3]]
#combine the matrices into a list
dist_gre_list <- list(dist_gre_d1, dist_gre_d2, dist_gre_d3)

#scale the results
dist.scale_gre_sum <- list(0,0,0)
for(i in 1:3){
  rownames(dist_gre_list[[i]]) <- levels(edges$From)
  colnames(dist_gre_list[[i]]) <- levels(edges$From)
  dist.scale_gre_sum[[i]] <- dist_gre_list[[i]]/max(dist_gre_list[[i]])
}

#remove the solution and land use nodes from the distance list
#to_remove <- c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")

dist.scale_gre_sum.1 <- dist.scale_gre_sum[[1]] 
dist.scale_gre_sum.1 <- dist.scale_gre_sum.1[!(rownames(dist.scale_gre_sum.1) %in% to_remove),]
dist.scale_gre_sum.1 <- dist.scale_gre_sum.1[,!(colnames(dist.scale_gre_sum.1) %in% to_remove)]

dist.scale_gre_sum.2 <- dist.scale_gre_sum[[2]] 
dist.scale_gre_sum.2 <- dist.scale_gre_sum.2[!(rownames(dist.scale_gre_sum.2) %in% to_remove),]
dist.scale_gre_sum.2 <- dist.scale_gre_sum.2[,!(colnames(dist.scale_gre_sum.2) %in% to_remove)]

dist.scale_gre_sum.3 <- dist.scale_gre_sum[[3]] 
dist.scale_gre_sum.3 <- dist.scale_gre_sum.3[!(rownames(dist.scale_gre_sum.3) %in% to_remove),]
dist.scale_gre_sum.3 <- dist.scale_gre_sum.3[,!(colnames(dist.scale_gre_sum.3) %in% to_remove)]

## Use clustering to show similarity in model variable responses, EXCLUDING land uses and solutions
fit1_gre <- hclust(as.dist(dist.scale_gre_sum.1))
fit2_gre <- hclust(as.dist(dist.scale_gre_sum.2))
fit3_gre <- hclust(as.dist(dist.scale_gre_sum.3))

png("./figs/DistanceDendro_x3_Green_sum.png", width = 13, height = 7, units = 'in', res = 600)
par(mfrow = c(1,3))
plot(fit1_gre, xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2_gre, xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3_gre, xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()

png("./figs/DistanceDendro_d1_allSolutions_sum.png", width = 14, height = 7, units = 'in', res = 600)
par(mfrow = c(1,3), mar=c(4.1,0.5,2.1,7))         #mar values bottom, left, top, right
plot(as.dendrogram(fit1_sou), xlab=expression("Distance " (italic(d1))), ylab='', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5, horiz = T)
mtext("a) source control", side=3, adj = 0)
plot(as.dendrogram(fit1_gra), xlab=expression("Distance " (italic(d1))), ylab='', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5, horiz = T)
mtext("b) gray infrastructure", side=3, adj = 0)
plot(as.dendrogram(fit1_gre), xlab=expression("Distance " (italic(d1))), ylab='', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5, horiz = T)
mtext("c) green infrastructure", side=3, adj = 0)
dev.off()

cutree(fit1_sou, k = 13)
cutree(fit1_gra, k = 13)
cutree(fit1_gre, k = 9)

library(dendextend)

png("./figs/DistanceDendro_d1_allSolutions_withboxes.png", width = 14, height = 7, units = 'in', res = 600)
par(mfrow = c(1,3), mar=c(4,1,2,8))     #mar values bottom, left, top, right
#source
plot(as.dendrogram(fit1_sou), xlab=expression("Distance " (italic(d1))), ylab='', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=2, horiz = TRUE)
mtext("a) source control", side=3, adj = 0)
rect.dendrogram(as.dendrogram(fit1_sou), k=12, lty = 5, lwd = 0,
                col=rgb(0.1, 0.2, 0.4, 0.1), horiz = TRUE, which = c(4,12))
rect.dendrogram(as.dendrogram(fit1_sou), k=7, lty = 5, lwd = 1, border = "darkgray",
                col=NA, horiz = TRUE, which = c(1))
#gray
plot(as.dendrogram(fit1_gra), xlab=expression("Distance " (italic(d1))), ylab='', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=2, horiz = T)
mtext("b) gray infrastructure", side=3, adj = 0)
rect.dendrogram(as.dendrogram(fit1_gra), k=13, lty = 5, lwd = 0,
                col=rgb(0.1, 0.2, 0.4, 0.1), horiz = TRUE, which = c(9,10))
rect.dendrogram(as.dendrogram(fit1_gra), k=6, lty = 5, lwd = 1, border = "darkgray",
                col=NA, horiz = TRUE, which = c(1))
rect.dendrogram(as.dendrogram(fit1_gra), k=5, lty = 3, lwd = 1.5, border = "darkgray",
                col=NA, horiz = TRUE, which = c(2))
#green
plot(as.dendrogram(fit1_gre), xlab=expression("Distance " (italic(d1))), ylab='', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=2, horiz = T)
mtext("c) green infrastructure", side=3, adj = 0)
rect.dendrogram(as.dendrogram(fit1_gre), k=9, lty = 5, lwd = 0,
                col=rgb(0.1, 0.2, 0.4, 0.1), horiz = TRUE, which = c(4,5))
rect.dendrogram(as.dendrogram(fit1_gre), k=4, lty = 3, lwd = 1.5, border = "darkgray",
                col=NA, horiz = TRUE, which = c(4))
dev.off()

##### unused #####
#plot using dendextend instead
#library(dendextend)

#dend_d1 <- as.dist(dist.scale[[1]])  %>% # take the d1 data
#  dist %>% # calculate a distance matrix, 
#  hclust(method = "average") %>% # on it compute hierarchical clustering using the "average" method, 
#  as.dendrogram # and lastly, turn that object into a dendrogram.
#dend_d1 %>% dendextend::prune(c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")) %>%
#  plot(ylab = "Distance, d1")

#dend_d2 <- as.dist(dist.scale[[2]])  %>% # take the d2 data
#  dist %>% # calculate a distance matrix, 
#  hclust(method = "average") %>% # on it compute hierarchical clustering using the "average" method, 
#  as.dendrogram # and lastly, turn that object into a dendrogram.
#dend_d2 %>% dendextend::prune(c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")) %>%
#  plot(ylab = "Distance, d2")

#dend_d3 <- as.dist(dist.scale[[3]])  %>% # take the d2 data
#  dist %>% # calculate a distance matrix, 
#  hclust(method = "average") %>% # on it compute hierarchical clustering using the "average" method, 
#  as.dendrogram # and lastly, turn that object into a dendrogram.
#dend_d3 %>% dendextend::prune(c("Green infrastructure", "Gray infrastructure", "Source control", "Residential", "Transportation", "Industrial")) %>%
#  plot(ylab = "Distance, d3")
