#####
# Network Analysis for Magel, Levin, Francis
# Water 100 stormwater solutions

#QPress package is based on the analysis from Melbourne-Thomas et al 2012
#https://wiley.figshare.com/articles/dataset/Supplement_1_Example_R_code_and_models_/3568107/1
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/12-0207.1?casa_token=VHTR6o_DvQ8AAAAA%3A_vmeRY9Q0b_fiUK3YpRBnKSq5_q-armBNM4ik05kN15SkrRXWQL-uUfmJyENGHCm_gN0-VY8rh-amw
#Some modifications were made to the original QPress code - they are contained in QPressFunctions.R

# Load required packages
library(QPress)
library(tcltk2)
library(XML)
library(here) #folder management


##### Load the Dia models #####

mod <- QPress::model.dia("./DiaModels/SolutionModel_25Oct2022_forR.dia")
write.digraph(mod, "TNCEarthLab_FullModel_25Oct2022.txt")  #write list of links (excluding the self-limiting ones)

## Enforce limitation (This is redundant with the self-limiting edges in the Dia model, if they are already included)

mod <- QPress::enforce.limitation(mod)


## Examine unweighted adjacency matrices
A_mod <- adjacency.matrix(mod, labels=TRUE)
A_mod
write.csv(A_mod, file = "Model_AdjMatrix_25Oct2022.csv", row.names = TRUE) #save the matrix, if needed
adjacency.image(sims_mod$edges) #this is wrong

##### Simulations #####
#If model simulations already exist, load them

sims_mod <- readRDS("Sims_Mod_10000_2022-08-26.rds")


#If model simulation does not exist, simulate and save!
n_sims <- 10000 #number of accepted simulations requested

sims_mod <- QPress::system.simulate(n_sims, mod)
sims_mod$total #1096042


##### Save Simulations #####

saveRDS(sims_mod, file = paste("Sims_", "Mod_", n_sims, "_", Sys.Date(), ".rds", sep = ""))


##### Perturbations #####
#using custom exploratory widget & print results from QPressFunctions.R
#Save the pos/neg sims by copying/pasting last output into Excel

impact.barplot(sim = sims_mod)

impact_mod <- data.frame(impact.table(sim = sims_mod))
write.csv(impact_mod, file = "ImpactTable_Mod_25Oct2022.csv")

#Examine weight values in the accepted model runs:
sims_mod$edges
mean(abs(sims_mod$w)) #0.4950787


#Display weights of valid and invalid matrices as density plots
weight.density.shiny(sim = sims_mod, epsilon = 1e-05, main = "")

##################################################################################################
#### Results Synthesis #####

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

#import the file containing results from perturbations
data <- read.csv("EarthLabTNC_QNM_Results_Oct2022.csv", header = TRUE)
data$LandUse <- as.factor(data$LandUse)
data$Solution <- as.factor(data$Solution)
data$Node<- as.factor(data$Node)
data$Type<- as.factor(data$Type)
summary(data)

#calculate % pos and neg responses
data$Positive_percent <- (data$positive/10000)*100
data$Negative_percent <- (data$negative/10000)*100
#calculate (positive-negative)/10000
data$proportion <- (data$positive-data$negative)/10000

data <- data %>% dplyr::mutate(across(Solution, factor, levels=c("none","source","gray","green")))

#plot the benefits
benefits <- subset(data, Type == "human" | Type == "nature") %>% 
  mutate(across(Node, factor, levels=c("Biodiversity", "Climate mitigation", "Salmon", "Water quality", "Access to nature", "Air quality",
                                       "Local foods", "Mental health", "Physical health", "Social cohesion", "Walkability", "Affordability", "Economic wellbeing",
                                       "Local jobs", "Property value"))) %>% group_by(LandUse, Solution)

allbene_facet <- ggplot(benefits, aes(x = Solution, y = Node, fill = proportion)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + coord_fixed() + theme_bw() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  scale_y_discrete(limits=rev) +
  scale_fill_gradient2(limits = c(-1, 1), breaks = c(1, 0, -1), labels = c("Positive", "Equivocal", "Negative"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000") + 
  facet_wrap(~LandUse) +
  labs(x = "Stormwater Solution", y = "Benefit", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 30)) #size of the legend bar

#plot the stressors
stressors <- subset(data, Type == "stressor") %>% group_by(LandUse, Solution)

allstress_facet <- ggplot(stressors, aes(x = Solution, y = Node, fill = proportion)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + coord_fixed() + theme_bw() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  scale_y_discrete(limits=rev) +
  scale_fill_gradient2(limits = c(-1, 1), breaks = c(1, 0, -1), labels = c("Positive", "Equivocal", "Negative"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000") + 
  facet_wrap(~LandUse) +
  labs(x = "Stormwater Solution", y = "Land Use Effect", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 8)) #size of the legend bar

#calculate the difference between responses w/ and w/out solutions
#import the file containing results from perturbations
data2 <- read.csv("EarthLabTNC_QNM_Results_Oct2022.csv", header = TRUE)
data2$LandUse <- as.factor(data2$LandUse)
data2$Solution <- as.factor(data2$Solution)
data2$Node<- as.factor(data2$Node)
data2$Type<- as.factor(data2$Type)
summary(data2)

data2 <- data2 %>% select(c("Node", "Type", "LandUse", "Solution", "positive")) #pick just positive simulations

data_wide <- data2 %>% spread(Solution, positive) #spread format so we can take difference

#Calculate the difference between positive sims w/ and w/out solutions
data_wide$sourcediff <- data_wide$source - data_wide$none
data_wide$graydiff <- data_wide$gray - data_wide$none
data_wide$greendiff <- data_wide$green - data_wide$none

#reformat the df for plotting
data_diff <- data_wide %>% select(c("Node", "Type", "LandUse", "sourcediff", "graydiff", "greendiff")) %>% rename(source = sourcediff, gray = graydiff, green = greendiff) %>%
  gather(Solution, Difference, -c(Node, Type, LandUse))
data_diff$Solution <- as.factor(data_diff$Solution)
data_diff <- data_diff %>% dplyr::mutate(across(Solution, factor, levels=c("source","gray","green")))

data_diff$proportion <- data_diff$Difference/10000

#plot the difference in benefits
benefits_diff <- subset(data_diff, Type == "human" | Type == "nature") %>% 
  mutate(across(Node, factor, levels=c("Biodiversity", "Climate mitigation", "Salmon", "Water quality", "Access to nature", "Air quality", 
                                       "Local foods", "Mental health", "Physical health", "Social cohesion", "Walkability", "Affordability", "Economic wellbeing",
                                       "Local jobs", "Property value"))) %>% group_by(LandUse, Solution)

allbenediff_facet <- ggplot(benefits_diff, aes(x = Solution, y = Node, fill = proportion)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + coord_fixed() + theme_bw() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  scale_y_discrete(limits=rev) +
  scale_fill_gradient2(limits = c(0, 1), breaks = c(0, 1), labels = c("0: no change", "1: strong increase"),
                       low = "#f5f5f5",
                       high = "#FF0000") + 
  facet_wrap(~LandUse) +
  labs(x = "Stormwater Solution", y = "Benefit", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 15)) #size of the legend bar
ggsave("figures/Benefits_proportion_tiles.png", width = 11, height = 11, device='png', dpi=400)


#plot the proportion of benefits for each land use on the same plot

sol.labs <- c("Source Control", "Gray Infrastructure", "Green Infrastructure")
names(sol.labs) <- c("source", "gray", "green")

allbenediff_facetsol <- ggplot(benefits_diff, aes(x = proportion, y = Node, color = LandUse, shape = LandUse)) + 
  geom_point(size = 5,position = position_jitter(width = 0.03, height = 0.13)) +
  facet_wrap(~Solution, labeller = as_labeller(sol.labs)) + 
  theme_bw() + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(color = "gray96")) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0), labels = c(0,0.25,0.5,0.75,1)) +
  scale_colour_viridis_d() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  labs(x = "Proportional difference in positive simulations", y = "Benefit Node")
ggsave("figures/Benefits_proportion_pointsbySol.png", width = 11, height = 11, device='png', dpi=400)

allbenediff_facetuse <- ggplot(benefits_diff, aes(x = proportion, y = Node, color = Solution, shape = Solution)) + 
  geom_point(size = 5,position = position_jitter(width = 0.03, height = 0.13)) +
  facet_wrap(~LandUse) + theme_bw() + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_line(color = "gray96")) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0), labels = c(0,0.25,0.5,0.75,1)) +
  scale_color_manual(values = c("source" = "darksalmon",
                                "gray" = "gray",
                                "green" = "darkgreen")) +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  labs(x = "Proportional difference in positive simulations", y = "Benefit Node", fill = element_blank())
ggsave("figures/Benefits_proportion_pointsbyLandUse.png", width = 11, height = 11, device='png', dpi=400)


#simplified plot - biodiversity, climate, salmon, physical health, social cohesion, residential property value
benefits_diff2 <- benefits_diff %>% filter(Node == "Biodiversity" | Node == "Climate mitigation" | Node == "Salmon" |
                                             Node == "Physical health" | Node == "Social cohesion" | Node == "Residential property value")

allbenediff_facet_6 <- ggplot(benefits_diff2, aes(x = Solution, y = Node, fill = proportion)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + coord_fixed() + theme_bw() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  scale_y_discrete(limits=rev) +
  scale_fill_gradient2(limits = c(0, 1), breaks = c(0, 1), labels = c("0: no change", "1: strong increase"),
                       low = "#f5f5f5",
                       high = "#FF0000") + 
  facet_wrap(~LandUse) +
  labs(x = "Stormwater Solution", y = "Benefit", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10)) #size of the legend bar

#plot the difference in stressors
stressors_diff <- subset(data_diff, Type == "stressor") %>% group_by(LandUse, Solution)

allstressdiff_facet <- ggplot(stressors_diff, aes(x = Solution, y = Node, fill = proportion)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + coord_fixed() + theme_bw() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  scale_y_discrete(limits=rev) +
  scale_fill_gradient2(limits = c(-1,0), breaks = c(-1,0), labels = c("-1: Strong decrease", "0: No change"),
                       low = "#075AFF",
                       high = "#FF0000") + 
  facet_wrap(~LandUse) +
  labs(x = "Stormwater Solution", y = "Land Use Effect", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 10)) #size of the legend bar



##################################################################################################
##################################################################################################

# For generating plots that are not part of the QPress package, use code below
# Example code was provided by Ben Raymond, but this came from K. Sobocinski

# Extract the bits we need from the Status Quo Model
edges <- sims_mod$edges
write.csv(edges, file = "Model_EdgesList_26Aug2022.csv")
As <- sims_mod$A
nodes <- node.labels(edges)
write.csv(nodes, file = "Model_NodesList_26Aug2022.csv")

monitor <- c(rep(NA,length(nodes))) ## Don't enforce any required responses


#Call specific nodes of interest
#To show only a subset of node responses (e.g. 12, 13, 27, 4, 31), run this instead of standard plot:
myplot <- function(nodes,As,perturb,monitor,epsilon=1.0E-5,main="",cex.axis=1) {
  pal <- c("grey30", "gray80", "tomato2")
  results <- matrix(0,length(nodes),3)
  for(i in 1:length(As)) {
    impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
    if(all(monitor==impact,na.rm=T)) {
      results <- results + outer(impact,-1:1,'==')
    }
  }
  rownames(results) <- nodes
  nodes <- nodes[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)] #this is where you specify the nodes of interest
  results <- results[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29),] #this is where you specify the nodes of interest
  lwidth <- max(strwidth(nodes,units="inches",cex=cex.axis))
  opar <- par(mai=c(0.5,lwidth+0.15,0.15,0.15)+0.2)
  barplot(t(results),horiz=T,las=1,border=F,col=pal,
          xlab="Simulations",main=main,cex.axis=cex.axis)
  par(opar)
}

# Standard slider plot of all response nodes, perturbing each in turn given the vector of perturbations (press):
# windows()
# To output to PDF
currentDate <- Sys.Date()
Indiv_Perturb <- paste(currentDate,"_PerturbationPlots_pos",".pdf",sep="")
pdf(file = Indiv_Perturb)
# For function
#opar <- par
#par(mfrow=c(2,2)) #This can be invoked for a 2x2 layout (better for simple (reduced vars) plot)
for (i in 1:29) { #number of nodes in model
  #i=2
  #Set up desired directions of perturbations--based upon direction of press (-1,1)
  #For all presses (should have 1 per node)
  press = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

  #length(press)
  presses=diag(press, nrow=29, ncol=29)
  perturb=presses[i,]
  
  perturb2=ifelse(perturb==1,"(Increase)","(Decrease)")
  
  #If all press perturbs are positive, use this code:
  #perturb <- c(rep(0,length(nodes)))
  #perturb[i] <- 1 ## perturb the ith node, this is a positive perturbation
  
  #Choose: all nodes (impact.barplot.action) or a subset of nodes (myplot)--myplot code below
  #impact.barplot.action(nodes,As,perturb,monitor,main=c(nodes[i],perturb[i]))
  myplot(nodes,As,perturb,monitor,main=c(nodes[i],perturb2[i]))
}
#par(opar)
dev.off()

#NEGATIVES - this has not been changed for # nodes!
Indiv_Perturb <- paste(currentDate,"_PerturbationPlots_neg",".pdf",sep="")
pdf(file = Indiv_Perturb)
# For function
#opar <- par
#par(mfrow=c(2,2)) #This can be invoked for a 2x2 layout (better for simple (reduced vars) plot)
for (i in 1:33) { #number of nodes in model
  #i=2
  #Set up desired directions of perturbations--based upon direction of press (-1,1)
  #For all presses (should have 1 per node)
  press = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)

  #length(press)
  presses=diag(press, nrow=33, ncol=33)
  perturb=presses[i,]
  
  perturb2=ifelse(perturb==1,"(Increase)","(Decrease)")
  
  #If all press perturbs are positive, use this code:
  #perturb <- c(rep(0,length(nodes)))
  #perturb[i] <- 1 ## perturb the ith node, this is a positive perturbation
  
  #Choose: all nodes (impact.barplot.action) or a subset of nodes (myplot)--myplot code below
  #impact.barplot.action(nodes,As,perturb,monitor,main=c(nodes[i],perturb[i]))
  myplot(nodes,As,perturb,monitor,main=c(nodes[i],perturb2[i]))
}
par(opar)
dev.off()



##### Sensitivity Analysis ##### 
#Code adapted from K. Sobocinski 

library(reshape2)
library(ggplot2)
library(dplyr)

#write a function to plot the outlier edge weights for a given simulation
plot_outwts <- function (sims) 
{
  print("plotting outlier weights")
  weight <- as.data.frame(sims$w) #extract the weight values in the accepted model runs
  wts <- reshape2::melt(weight)
  colnames(wts)=c("Edge", "Value")
  
  edgemean <- as.data.frame(apply(sims$w, 2, mean)) #calculate weight mean for each edge
  edgestdev <- as.data.frame(apply(sims$w, 2, sd)) #calculate weight stdev for each edge
  lowerSD <- abs(edgemean[,1])-abs(edgestdev[,1])
  upperSD <- abs(edgemean[,1])+abs(edgestdev[,1])
  
  edgenames <- as.data.frame(levels(wts$Edge)) 
  edgesSD <- cbind(edgenames, abs(edgemean[,1]), lowerSD, upperSD)
  colnames(edgesSD)=c("Edge", "Mean", "LowerSD", "UpperSD")
  
  write.csv(edgesSD, "Edges_MeanSD.csv")
  
  #Pull out outliers (top/bottom 15 values) & reorder EdgesSD by mean
  bottom <- edgesSD %>% dplyr::slice_min(Mean, n = 15)
  top <- edgesSD %>% dplyr::slice_max(Mean, n = 15)
  outliers <- rbind(top, bottom)
  
  #Plot all outliers (top/bottom 15 values)
  ggplot(outliers, aes(x=Mean, y=reorder(Edge, Mean))) +
    geom_errorbarh(data=outliers, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
    ylab("Edge") +
    xlab("Weight (mean +/- SD)") +
    theme_bw() +
    geom_point() +
    geom_vline(xintercept=0, linetype="dotted") +
    geom_vline(xintercept=1.0, linetype="dotted") +
    geom_vline(xintercept=0.5, linetype="dotted") + theme(axis.text=element_text(size=12),
                                                          axis.title=element_text(size=12,face="bold"))
}

plot_outwts(sims = sims_mod)


##### Old sensitivity analysis code #####
#To extract the weight values in the accepted model runs:
sims_statquo$edges
head(sims_statquo$w)
tail(sims_statquo$w)

is.matrix(sims_statquo$w) #True
weight_statquo <- as.data.frame(sims_statquo$w)
head(weight_statquo)
#Check distributions of different nodes to see how variable they are
hist(weight_statquo[,10])

#To assess the sensitivity of the weights
#Extract edges and weights from simulations
wts_statquo <- reshape2::melt(weight_statquo)
colnames(wts_statquo)=c("Edge", "Value")
head(wts_statquo)

dim(sims_statquo$w) #10000 x 143 (143 linkages in the model)

#Get means for each edge
edgemean <- as.data.frame(apply(sims_statquo$w, 2, mean))
summary(edgemean)
edgemin <- as.data.frame(apply(sims_statquo$w, 2, min))
edgemax <- as.data.frame(apply(sims_statquo$w, 2, max))
edgestdev <- as.data.frame(apply(sims_statquo$w, 2, sd))
hist(abs(edgemean[,1]))
hist(abs(edgemin[,1]))
hist(abs(edgemax[,1]))
hist(abs(edgestdev[,1]))
lowerSD <- abs(edgemean[,1])-abs(edgestdev[,1])
upperSD <- abs(edgemean[,1])+abs(edgestdev[,1])

edgenames <- as.data.frame(levels(wts_statquo$Edge)) 
edge.vals <- cbind(edgenames, abs(edgemean[,1]), abs(edgemin[,1]), 
                 abs(edgemax[,1]))

#USe max and min values with mean
head(edge.vals)
dim(edge.vals) #143 linkages in the model x 4 columns
colnames(edge.vals)=c("Edge", "Mean", "Min", "Max")
str(edge.vals)

#Use SD
edgesSD <- cbind(edgenames, abs(edgemean[,1]), lowerSD, upperSD)
head(edgesSD)
colnames(edgesSD)=c("Edge", "Mean", "LowerSD", "UpperSD")

#Plot all edges and means, maxes, and mins
ggplot(edge.vals, aes(x=Mean, y=Edge)) +
  geom_errorbarh(data=edge.vals, aes(xmax=Max, xmin=Min), colour = "grey50") + 
  geom_point()

#Reorder so easier to see
ggplot(edgesSD, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=edgesSD, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  geom_point() + theme_bw() + xlab("Weight abs(mean +/- sd)") + ylab("Edge") +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_vline(xintercept=1.0, linetype="dotted") +
  geom_vline(xintercept=0.5, linetype="dotted") + theme(axis.text=element_text(size=6),
                                                         axis.title=element_text(size=10,face="bold"))

#Pull out outliers (top/bottom 15 values)
#Reorder EdgesSD by mean
bottom <- edgesSD %>% dplyr::slice_min(Mean, n = 15)
top <- edgesSD %>% dplyr::slice_max(Mean, n = 15)

outliers <- rbind(top, bottom)

#Plot all outliers (top/bottom 15 values)
ggplot(outliers, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=outliers, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  ylab("Edge") +
  xlab("Weight (mean +/- sd)") +
  theme_bw() +
  geom_point() +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_vline(xintercept=1.0, linetype="dotted") +
  geom_vline(xintercept=0.5, linetype="dotted") + theme(axis.text=element_text(size=12),
                                                        axis.title=element_text(size=12,face="bold"))

#To plot bottom (most sensitive edges only):
ggplot(bottom, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=bottom, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  ylab("Edge") +
  xlab("Mean Weight") +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"))+
  geom_point() +
  geom_vline(xintercept=0.5, linetype="dotted")


