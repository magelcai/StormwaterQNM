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

## Function to define the perturbation scenario
impact <- QPress::press.impact(edges,perturb=c("Source control"=1, "Residential"=1)) #node name and direction of perturbation

## Use 1000 simulations
n.sims <- 1000
dist <- list(0,0,0)
i <- 0
while(i < n.sims) {


  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()

  ## Check stability
  if(!stable.community(W)) next

  ## Monitor impact post press
  imp <- sign(impact(W))

  ## Compute distances
  dist[[1]] <- dist[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist[[2]] <- dist[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist[[3]] <- dist[[3]] + outer(imp,imp,function(x,y) x!=y)
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

png("figs/DistanceDendro_x3.png", width = 13, height = 7, units = 'in', res = 300)
par(mfrow = c(1,3))
plot(fit1,xlab="", ylab='Distance (d1)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit2,xlab='', ylab='Distance (d2)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
#dev.new()
plot(fit3,xlab='', ylab='Distance (d3)', main = "", cex.lab = 1.5, cex.axis = 1.5, cex=1.5)
dev.off()


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
