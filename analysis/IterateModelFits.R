
#setwd("/Users/caitlinmagel/Documents/UW-PSI Postdoc/UW-EarthLab Postdoc/QNM/test")

## Trying different models - removing links one at a time
library(QPress)
library(tidyr)
library(dplyr)
library(tibble)
library(forcats)

n_sims <- 1000 #number of accepted simulations requested from each model simulation

#Read in the full model
mod <- read.digraph("TNCEarthLab_FullModel_25Oct2022.txt")
nrow(mod)   #should be 105 links (excluding self-limiting)
mod_lim <- QPress::enforce.limitation(mod)  #add self-limiting links
nrow(mod_lim)   #should be 134 links (including self-limiting)
sims_mod <- QPress::system.simulate(n_sims, mod_lim)  #simulate the model
saveRDS(sims_mod, file = paste("Sims_", "Mod_", "full_", n_sims, "_", Sys.Date(), ".rds", sep = ""))

#make a list of the links we want to leave off
mod_edges_pos <- mod %>% filter(Type == "P")
mod_edges_pos$Arrow <- "->"
mod_edges_neg <- mod %>% filter(Type == "N")
mod_edges_neg$Arrow <- "-*"
mod_links <- rbind(mod_edges_pos, mod_edges_neg)
mod_edges <- mod %>% full_join(mod_links, by = c("From", "To", "Group", "Type", "Pair")) %>%
  unite("Link", From, Arrow, To, remove = FALSE, sep = "") %>% select(From, To, Type, Link)
mod_edges$Number <- 1:105
mod_edges$Name <- "Sims_Mod_"
mod_edges_names <- mod_edges %>% unite("ModelName", Name, Number, remove = TRUE, sep = "")
#new row describing the full model
full_row <- c(NA, NA, NA, "Full", "Sims_Mod_full")
mod_edges_names <- rbind(mod_edges_names, full_row)


#Create models where you remove one link at a time (retain all self-limiting links), simulate, and save simulations
for (r in 1:nrow(mod)) {
  new_mod <- mod[-c(r),]
  new_mod_lim <- QPress::enforce.limitation(new_mod)
  sims_new_mod <- QPress::system.simulate(n_sims, new_mod_lim)
  saveRDS(sims_new_mod, file = paste("Sims_", "Mod_", r, "_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
}


#If simulations already exist, load all of them into a list that is named by the model name
temp <- list.files(pattern = "Sims_Mod_")
myfiles <- lapply(temp, readRDS)
list_names <- gsub("_1000_2022-10-26.rds", "", temp) #leave off the repeated part of the dataset names
#list_names <- gsub("Sims_Mod_", "", list_names)
names(myfiles) <- list_names


##### run RESIDENTIAL perturbations #####
#run a loop over all simulations that perturbs Residential and save the results either in a list
res <- list() #create an empty list of results
res
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Residential"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  res[[i]] <- pressimpact
}
names(res) <- names(myfiles)

#run a loop over all simulations that perturbs Source Control and Residential and save the results either in a list or indiv csv's
res_sou <- list() #create an empty list of results
res_sou
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Source control"=1, "Residential"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  res_sou[[i]] <- pressimpact
}
names(res_sou) <- names(myfiles)

#run a loop over all simulations that perturbs Gray Infrastructure and Residential and save the results either in a list or indiv csv's
res_gra <- list() #create an empty list of results
res_gra
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Gray infrastructure"=1, "Residential"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  res_gra[[i]] <- pressimpact
}
names(res_gra) <- names(myfiles)

#run a loop over all simulations that perturbs Green Infrastructure and Residential and save the results either in a list or indiv csv's
res_gre <- list() #create an empty list of results
res_gre
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Green infrastructure"=1, "Residential"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  res_gre[[i]] <- pressimpact
}
names(res_gre) <- names(myfiles)


##### run TRANSPORTATION perturbations #####
#run a loop over all simulations that perturbs Transportation and save the results either in a list
trans <- list() #create an empty list of results
trans
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Transportation"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  trans[[i]] <- pressimpact
}
names(trans) <- names(myfiles)

#run a loop over all simulations that perturbs Source Control and Transportation and save the results either in a list or indiv csv's
trans_sou <- list() #create an empty list of results
trans_sou
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Source control"=1, "Transportation"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  trans_sou[[i]] <- pressimpact
}
names(trans_sou) <- names(myfiles)

#run a loop over all simulations that perturbs Gray Infrastructure and Transportation and save the results either in a list or indiv csv's
trans_gra <- list() #create an empty list of results
trans_gra
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Gray infrastructure"=1, "Transportation"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  trans_gra[[i]] <- pressimpact
}
names(trans_gra) <- names(myfiles)

#run a loop over all simulations that perturbs Green Infrastructure and Transportation and save the results either in a list or indiv csv's
trans_gre <- list() #create an empty list of results
trans_gre
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Green infrastructure"=1, "Transportation"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  trans_gre[[i]] <- pressimpact
}
names(trans_gre) <- names(myfiles)



##### run INDUSTRIAL perturbations #####
#run a loop over all simulations that perturbs Industrial and save the results either in a list
indus <- list() #create an empty list of results
indus
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Industrial"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  indus[[i]] <- pressimpact
}
names(indus) <- names(myfiles)

#run a loop over all simulations that perturbs Source Control and Industrial and save the results either in a list or indiv csv's
indus_sou <- list() #create an empty list of results
indus_sou
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Source control"=1, "Industrial"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  indus_sou[[i]] <- pressimpact
}
names(indus_sou) <- names(myfiles)

#run a loop over all simulations that perturbs Gray Infrastructure and Industrial and save the results either in a list or indiv csv's
indus_gra <- list() #create an empty list of results
indus_gra
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Gray infrastructure"=1, "Industrial"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  indus_gra[[i]] <- pressimpact
}
names(indus_gra) <- names(myfiles)

#run a loop over all simulations that perturbs Green Infrastructure and Industrial and save the results either in a list or indiv csv's
indus_gre <- list() #create an empty list of results
indus_gre
# save files to list
for (i in 1:length(myfiles)) {
  pressimpact <- QPress::impact.barplot0(sim = myfiles[[i]], perturb=c("Green infrastructure"=1, "Industrial"=1))
  outcome_levels <- c("negative", "neutral", "positive")
  pressimpact <- setNames(as.data.frame(pressimpact), outcome_levels)
  indus_gre[[i]] <- pressimpact
}
names(indus_gre) <- names(myfiles)


##### OPTION 1: summarize perturbation results using n-1 model minus full model (raw simulations) #####
#Convert from List to Tibble - SOURCE
res_sou_diff1 <- list() 
for (i in 1:length(res_sou)) {
  as.data.frame(difference <- res_sou[[i]]$positive - res_sou[["Sims_Mod_full"]]$positive)
  nodes <- rownames(res_sou[[i]])
  difference <- setNames(difference, nodes)
  res_sou_diff1[[i]] <- difference
}
names(res_sou_diff1) <- names(myfiles)

res_sou_diff1_tib <- as_tibble(do.call(cbind, res_sou_diff1), rownames = NA) %>% rownames_to_column(var = "Node")
res_sou_diff1_tib$Node <- as.factor(res_sou_diff1_tib$Node)
#Reformat Tibble
res_sou_diff_tib_trans <- pivot_longer(res_sou_diff1_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_sou_diff1_links <- full_join(mod_edges_names, res_sou_diff_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_sou_diff1_links$LandUse <- "Residential" 
res_sou_diff1_links$LandUse <- as.factor(res_sou_diff1_links$LandUse)
res_sou_diff1_links$Solution <- "source" 
res_sou_diff1_links$Solution <- as.factor(res_sou_diff1_links$Solution)


#Convert from List to Tibble - GRAY
res_gra_diff1 <- list() 
for (i in 1:length(res_gra)) {
  as.data.frame(difference <- res_gra[[i]]$positive - res_gra[["Sims_Mod_full"]]$positive)
  nodes <- rownames(res_gra[[i]])
  difference <- setNames(difference, nodes)
  res_gra_diff1[[i]] <- difference
}
names(res_gra_diff1) <- names(myfiles)

res_gra_diff_tib <- as_tibble(do.call(cbind, res_gra_diff1), rownames = NA) %>% rownames_to_column(var = "Node")
res_gra_diff_tib$Node <- as.factor(res_gra_diff_tib$Node)
#Reformat Tibble
res_gra_diff_tib_trans <- pivot_longer(res_gra_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_gra_diff1_links <- full_join(mod_edges_names, res_gra_diff_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_gra_diff1_links$LandUse <- "Residential" 
res_gra_diff1_links$LandUse <- as.factor(res_gra_diff1_links$LandUse)
res_gra_diff1_links$Solution <- "gray" 
res_gra_diff1_links$Solution <- as.factor(res_gra_diff1_links$Solution)


#Convert from List to Tibble  - GREEN
res_gre_diff1 <- list() 
for (i in 1:length(res_gre)) {
  as.data.frame(difference <- res_gre[[i]]$positive - res_gre[["Sims_Mod_full"]]$positive)
  nodes <- rownames(res_gre[[i]])
  difference <- setNames(difference, nodes)
  res_gre_diff1[[i]] <- difference
}
names(res_gre_diff1) <- names(myfiles)

res_gre_diff_tib <- as_tibble(do.call(cbind, res_gre_diff1), rownames = NA) %>% rownames_to_column(var = "Node")
res_gre_diff_tib$Node <- as.factor(res_gre_diff_tib$Node)
#Reformat Tibble
res_gre_diff_tib_trans <- pivot_longer(res_gre_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_gre_diff1_links <- full_join(mod_edges_names, res_gre_diff_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_gre_diff1_links$LandUse <- "Residential" 
res_gre_diff1_links$LandUse <- as.factor(res_gre_diff1_links$LandUse)
res_gre_diff1_links$Solution <- "green" 
res_gre_diff1_links$Solution <- as.factor(res_gre_diff1_links$Solution)

#combine the results (differences) for all three solution types
diffresults_res_1 <- rbind(res_sou_diff1_links, res_gra_diff1_links, res_gre_diff1_links)
diffresults_res_1$Link <- as.factor(diffresults_res_1$Link)
diffresults_res_1$ModelName <- as.factor(diffresults_res_1$ModelName)

write.csv(diffresults_res_1, "IterateResults_Difference_Resi_Opt1_6Oct2022.csv", row.names = FALSE)

diffresults_res_1 <- read.csv("IterateResults_Difference_Resi_Opt1_6Oct2022.csv", header = T)


###### OPTION 1: plot the differences for green infrastructure & residential #####

diffresults_res_gre_1 <- filter(diffresults_res_1, Solution == "green")

diffresults_res_gre_1_plot <- gather(diffresults_res_gre_1, key="Node", value="difference", 3:31)
diffresults_res_gre_1_plot$difference_prop <- diffresults_res_gre_1_plot$difference/n_sims
diffresults_res_gre_1_plot$Node <- as.factor(diffresults_res_gre_1_plot$Node)


plot_physhealth_1 <- diffresults_res_gre_1_plot %>%
  filter(Node == "Physical.health") %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 0.6, linetype = 1) +
  theme_bw(base_size = 14) +
  scale_fill_gradient2(limits = c(-0.1, 0.1), breaks = c(-0.1, 0, 0.1), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Physical Health: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_1_plot, Node == "Physical.health")$difference_prop), y = "Residential->Bacteria",
           label = paste("Residential\n", "Green"))

plot_physhealth_top_1 <- diffresults_res_gre_1_plot %>%
  filter(Node == "Physical.health" & difference_prop >= 0) %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 2, linetype = 1) +
  theme_bw(base_size = 16) +
  scale_fill_gradient2(limits = c(0, 0.06), breaks = c(0, 0.03, 0.06), labels = c("No change", "3% increase", "6% increase"), midpoint = 0.03,
                       low = "#FFEFDD",
                       mid = "#FF998B",
                       high = "#A62C2B") +
  labs(x = "Physical Health: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_1_plot, Node == "Physical.health" & difference_prop >= 0)$difference_prop), 
           y = "Residential->Toxics", label = paste("Residential\n", "Green"), size = 5)


plot_physhealth_bottom_1 <- diffresults_res_gre_1_plot %>%
  filter(Node == "Physical.health" & difference_prop <= -0.025) %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 2, linetype = 1) +
  theme_bw(base_size = 16) +
  scale_fill_gradient2(limits = c(-0.1, -0.02), breaks = c(-0.1, -0.06, -0.02), labels = c("10% decrease", "6% decrease", "2% decrease"), midpoint = -0.06,
                       low = "#1065C0",
                       mid = "#41A7F5",
                       high = "#BBDFFB") +
  labs(x = "Physical Health: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_1_plot, Node == "Physical.health" & difference_prop <= -0.025)$difference_prop), 
           y = "Green infrastructure->Local jobs", label = paste("Residential\n", "Green"), size = 5)



plot_biodiv_1 <- diffresults_res_gre_1_plot %>%
  filter(Node == "Biodiversity") %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 0.6, linetype = 1) +
  theme_bw(base_size = 14) +
  scale_fill_gradient2(limits = c(-0.1, 0.1), breaks = c(-0.1, 0, 0.1), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Biodiversity: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_1_plot, Node == "Biodiversity")$difference_prop), y = "Nutrients-*Water quality",
           label = paste("Residential\n", "Green"))


plot_biodiv_top1 <- diffresults_res_gre_1_plot %>%
  filter(Node == "Biodiversity" & difference_prop >= 0) %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 2, linetype = 1) +
  theme_bw(base_size = 16) +
  scale_fill_gradient2(limits = c(0, 0.06), breaks = c(0, 0.03, 0.06), labels = c("No change", "3% increase", "6% increase"), midpoint = 0.03,
                       low = "#FFEFDD",
                       mid = "#FF998B",
                       high = "#A62C2B") +
  labs(x = "Biodiversity: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_1_plot, Node == "Biodiversity" & difference_prop >= 0)$difference_prop), y = "Residential->Toxics",
           label = paste("Residential\n", "Green"), size = 5) #+
# expand_limits(x=c(0.8, 0.9))


plot_biodiv_bottom_1 <- diffresults_res_gre_1_plot %>%
  filter(Node == "Biodiversity" & difference_prop <= -0.025) %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 2, linetype = 1) +
  theme_bw(base_size = 16) +
  scale_fill_gradient2(limits = c(-0.1, -0.02), breaks = c(-0.1, -0.06, -0.02), labels = c("10% decrease", "6% decrease", "2% decrease"), midpoint = -0.06,
                       low = "#1065C0",
                       mid = "#41A7F5",
                       high = "#BBDFFB") +
  labs(x = "Biodiversity: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_1_plot, Node == "Biodiversity" & difference_prop <= -0.025)$difference_prop), 
           y = "Transportation->Flooding", label = paste("Residential\n", "Green"), size = 5)



##### OPTION 2: summarize perturbation results using n-1 model minus full model (raw simulations) #####
### leaning towards this option!

#Calculate the proportion of positive simulations - RESI + SOURCE
res_sou_prop <- list() 
for (i in 1:length(res_sou)) {
  as.data.frame(prop_pos <- (res_sou[[i]]$positive - res_sou[[i]]$negative)/n_sims)
  nodes <- rownames(res_sou[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  res_sou_prop[[i]] <- prop_pos
}
names(res_sou_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
res_sou_prop_diff <- list() 
for (i in 1:length(res_sou_prop)) {
  as.data.frame(prop_pos <- (res_sou_prop[[i]] - res_sou_prop[["Sims_Mod_full"]]))
  nodes <- rownames(res_sou[[i]]) ##THIS ISN"T WORKING
  prop_pos <- setNames(prop_pos, nodes)
  res_sou_prop_diff[[i]] <- prop_pos
}
names(res_sou_prop_diff) <- names(myfiles)
#Convert from List to Tibble - SOURCE
res_sou_prop_diff_tib <- as_tibble(do.call(cbind, res_sou_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
res_sou_prop_diff_tib$Node <- as.factor(res_sou_prop_diff_tib$Node)
#Reformat Tibble
res_sou_prop_tib_trans <- pivot_longer(res_sou_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_sou_prop_diff_links <- full_join(mod_edges_names, res_sou_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_sou_prop_diff_links$LandUse <- "Residential" 
res_sou_prop_diff_links$LandUse <- as.factor(res_sou_prop_diff_links$LandUse)
res_sou_prop_diff_links$Solution <- "source" 
res_sou_prop_diff_links$Solution <- as.factor(res_sou_prop_diff_links$Solution)

#Calculate the proportion of positive simulations - RESI + GRAY
res_gra_prop <- list() 
for (i in 1:length(res_gra)) {
  as.data.frame(prop_pos <- (res_gra[[i]]$positive - res_gra[[i]]$negative)/n_sims)
  nodes <- rownames(res_gra[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  res_gra_prop[[i]] <- prop_pos
}
names(res_gra_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
res_gra_prop_diff <- list() 
for (i in 1:length(res_gra_prop)) {
  as.data.frame(prop_pos <- (res_gra_prop[[i]] - res_gra_prop[["Sims_Mod_full"]]))
  nodes <- rownames(res_gra[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  res_gra_prop_diff[[i]] <- prop_pos
}
names(res_gra_prop_diff) <- names(myfiles)
#Convert from List to Tibble - SOURCE
res_gra_prop_diff_tib <- as_tibble(do.call(cbind, res_gra_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
res_gra_prop_diff_tib$Node <- as.factor(res_gra_prop_diff_tib$Node)
#Reformat Tibble
res_gra_prop_tib_trans <- pivot_longer(res_gra_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_gra_prop_diff_links <- full_join(mod_edges_names, res_gra_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_gra_prop_diff_links$LandUse <- "Residential" 
res_gra_prop_diff_links$LandUse <- as.factor(res_gra_prop_diff_links$LandUse)
res_gra_prop_diff_links$Solution <- "gray" 
res_gra_prop_diff_links$Solution <- as.factor(res_gra_prop_diff_links$Solution)

#Calculate the proportion of positive simulations - RESI + GREEN
res_gre_prop <- list() 
for (i in 1:length(res_gre)) {
  as.data.frame(prop_pos <- (res_gre[[i]]$positive - res_gre[[i]]$negative)/n_sims)
  nodes <- rownames(res_gre[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  res_gre_prop[[i]] <- prop_pos
}
names(res_gre_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
res_gre_prop_diff <- list() 
for (i in 1:length(res_gre_prop)) {
  as.data.frame(prop_pos <- (res_gre_prop[[i]] - res_gre_prop[["Sims_Mod_full"]]))
  nodes <- rownames(res_gre[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  res_gre_prop_diff[[i]] <- prop_pos
}
names(res_gre_prop_diff) <- names(myfiles)
#Convert from List to Tibble - SOURCE
res_gre_prop_diff_tib <- as_tibble(do.call(cbind, res_gre_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
res_gre_prop_diff_tib$Node <- as.factor(res_gre_prop_diff_tib$Node)
#Reformat Tibble
res_gre_prop_tib_trans <- pivot_longer(res_gre_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_gre_prop_diff_links <- full_join(mod_edges_names, res_gre_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_gre_prop_diff_links$LandUse <- "Residential" 
res_gre_prop_diff_links$LandUse <- as.factor(res_gre_prop_diff_links$LandUse)
res_gre_prop_diff_links$Solution <- "green" 
res_gre_prop_diff_links$Solution <- as.factor(res_gre_prop_diff_links$Solution)

#combine the results (differences) for all three solution types
diffresults_res_2 <- rbind(res_sou_prop_diff_links, res_gra_prop_diff_links, res_gre_prop_diff_links)
diffresults_res_2$Link <- as.factor(diffresults_res_2$Link)
diffresults_res_2$ModelName <- as.factor(diffresults_res_2$ModelName)
#save the Residential land use file
write.csv(diffresults_res_2, "IterateResults_Difference_Resi_Opt2_27Oct2022.csv", row.names = FALSE)



#Calculate the proportion of positive simulations - TRANSPORTATION + SOURCE
trans_sou_prop <- list() 
for (i in 1:length(trans_sou)) {
  as.data.frame(prop_pos <- (trans_sou[[i]]$positive - trans_sou[[i]]$negative)/n_sims)
  nodes <- rownames(trans_sou[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  trans_sou_prop[[i]] <- prop_pos
}
names(trans_sou_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
trans_sou_prop_diff <- list() 
for (i in 1:length(trans_sou_prop)) {
  as.data.frame(prop_pos <- (trans_sou_prop[[i]] - trans_sou_prop[["Sims_Mod_full"]]))
  nodes <- rownames(trans_gra[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  trans_sou_prop_diff[[i]] <- prop_pos
}
names(trans_sou_prop_diff) <- names(myfiles)
#Convert from List to Tibble - GRAY
trans_sou_prop_diff_tib <- as_tibble(do.call(cbind, trans_sou_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
trans_sou_prop_diff_tib$Node <- as.factor(trans_sou_prop_diff_tib$Node)
#Reformat Tibble
trans_sou_prop_tib_trans <- pivot_longer(trans_sou_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
trans_sou_prop_diff_links <- full_join(mod_edges_names, trans_sou_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
trans_sou_prop_diff_links$LandUse <- "Transportation" 
trans_sou_prop_diff_links$LandUse <- as.factor(trans_sou_prop_diff_links$LandUse)
trans_sou_prop_diff_links$Solution <- "source" 
trans_sou_prop_diff_links$Solution <- as.factor(trans_sou_prop_diff_links$Solution)

#Calculate the proportion of positive simulations - TRANSPORTATION + GRAY
trans_gra_prop <- list() 
for (i in 1:length(trans_gra)) {
  as.data.frame(prop_pos <- (trans_gra[[i]]$positive - trans_gra[[i]]$negative)/n_sims)
  nodes <- rownames(trans_gra[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  trans_gra_prop[[i]] <- prop_pos
}
names(trans_gra_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
trans_gra_prop_diff <- list() 
for (i in 1:length(trans_gra_prop)) {
  as.data.frame(prop_pos <- (trans_gra_prop[[i]] - trans_gra_prop[["Sims_Mod_full"]]))
  nodes <- rownames(trans_gra[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  trans_gra_prop_diff[[i]] <- prop_pos
}
names(trans_gra_prop_diff) <- names(myfiles)
#Convert from List to Tibble - GRAY
trans_gra_prop_diff_tib <- as_tibble(do.call(cbind, trans_gra_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
trans_gra_prop_diff_tib$Node <- as.factor(trans_gra_prop_diff_tib$Node)
#Reformat Tibble
trans_gra_prop_tib_trans <- pivot_longer(trans_gra_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
trans_gra_prop_diff_links <- full_join(mod_edges_names, trans_gra_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
trans_gra_prop_diff_links$LandUse <- "Transportation" 
trans_gra_prop_diff_links$LandUse <- as.factor(trans_gra_prop_diff_links$LandUse)
trans_gra_prop_diff_links$Solution <- "gray" 
trans_gra_prop_diff_links$Solution <- as.factor(trans_gra_prop_diff_links$Solution)

#Calculate the proportion of positive simulations - TRANSPORTATION + GREEN
trans_gre_prop <- list() 
for (i in 1:length(trans_gre)) {
  as.data.frame(prop_pos <- (trans_gre[[i]]$positive - trans_gre[[i]]$negative)/n_sims)
  nodes <- rownames(trans_gre[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  trans_gre_prop[[i]] <- prop_pos
}
names(trans_gre_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
trans_gre_prop_diff <- list() 
for (i in 1:length(trans_gre_prop)) {
  as.data.frame(prop_pos <- (trans_gre_prop[[i]] - trans_gre_prop[["Sims_Mod_full"]]))
  nodes <- rownames(trans_gre[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  trans_gre_prop_diff[[i]] <- prop_pos
}
names(trans_gre_prop_diff) <- names(myfiles)
#Convert from List to Tibble - SOURCE
trans_gre_prop_diff_tib <- as_tibble(do.call(cbind, trans_gre_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
trans_gre_prop_diff_tib$Node <- as.factor(trans_gre_prop_diff_tib$Node)
#Reformat Tibble
trans_gre_prop_tib_trans <- pivot_longer(trans_gre_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
trans_gre_prop_diff_links <- full_join(mod_edges_names, trans_gre_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
trans_gre_prop_diff_links$LandUse <- "Transportation" 
trans_gre_prop_diff_links$LandUse <- as.factor(trans_gre_prop_diff_links$LandUse)
trans_gre_prop_diff_links$Solution <- "green" 
trans_gre_prop_diff_links$Solution <- as.factor(trans_gre_prop_diff_links$Solution)

#combine the results (differences) for all three solution types
diffresults_trans_2 <- rbind(trans_sou_prop_diff_links, trans_gra_prop_diff_links, trans_gre_prop_diff_links)
diffresults_trans_2$Link <- as.factor(diffresults_trans_2$Link)
diffresults_trans_2$ModelName <- as.factor(diffresults_trans_2$ModelName)

write.csv(diffresults_trans_2, "IterateResults_Difference_Trans_Opt2_27Oct2022.csv", row.names = FALSE)



#Calculate the proportion of positive simulations - INDUSTRIAL + SOURCE
indus_sou_prop <- list() 
for (i in 1:length(indus_sou)) {
  as.data.frame(prop_pos <- (indus_sou[[i]]$positive - indus_sou[[i]]$negative)/n_sims)
  nodes <- rownames(indus_sou[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  indus_sou_prop[[i]] <- prop_pos
}
names(indus_sou_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
indus_sou_prop_diff <- list() 
for (i in 1:length(indus_sou_prop)) {
  as.data.frame(prop_pos <- (indus_sou_prop[[i]] - indus_sou_prop[["Sims_Mod_full"]]))
  nodes <- rownames(indus_sou[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  indus_sou_prop_diff[[i]] <- prop_pos
}
names(indus_sou_prop_diff) <- names(myfiles)
#Convert from List to Tibble - SOURCE
indus_sou_prop_diff_tib <- as_tibble(do.call(cbind, indus_sou_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
indus_sou_prop_diff_tib$Node <- as.factor(indus_sou_prop_diff_tib$Node)
#Reformat Tibble
indus_sou_prop_tib_trans <- pivot_longer(indus_sou_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
indus_sou_prop_diff_links <- full_join(mod_edges_names, indus_sou_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
indus_sou_prop_diff_links$LandUse <- "Industrial" 
indus_sou_prop_diff_links$LandUse <- as.factor(indus_sou_prop_diff_links$LandUse)
indus_sou_prop_diff_links$Solution <- "source" 
indus_sou_prop_diff_links$Solution <- as.factor(indus_sou_prop_diff_links$Solution)

#Calculate the proportion of positive simulations - INDUSTRIAL + GRAY
indus_gra_prop <- list() 
for (i in 1:length(indus_gra)) {
  as.data.frame(prop_pos <- (indus_gra[[i]]$positive - indus_gra[[i]]$negative)/n_sims)
  nodes <- rownames(indus_gra[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  indus_gra_prop[[i]] <- prop_pos
}
names(indus_gra_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
indus_gra_prop_diff <- list() 
for (i in 1:length(indus_gra_prop)) {
  as.data.frame(prop_pos <- (indus_gra_prop[[i]] - indus_gra_prop[["Sims_Mod_full"]]))
  nodes <- rownames(indus_gra[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  indus_gra_prop_diff[[i]] <- prop_pos
}
names(indus_gra_prop_diff) <- names(myfiles)
#Convert from List to Tibble - GREEN
indus_gra_prop_diff_tib <- as_tibble(do.call(cbind, indus_gra_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
indus_gra_prop_diff_tib$Node <- as.factor(indus_gra_prop_diff_tib$Node)
#Reformat Tibble
indus_gra_prop_tib_trans <- pivot_longer(indus_gra_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
indus_gra_prop_diff_links <- full_join(mod_edges_names, indus_gra_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
indus_gra_prop_diff_links$LandUse <- "Industrial" 
indus_gra_prop_diff_links$LandUse <- as.factor(indus_gra_prop_diff_links$LandUse)
indus_gra_prop_diff_links$Solution <- "gray" 
indus_gra_prop_diff_links$Solution <- as.factor(indus_gra_prop_diff_links$Solution)

#Calculate the proportion of positive simulations - INDUSTRIAL + GREEN
indus_gre_prop <- list() 
for (i in 1:length(indus_gre)) {
  as.data.frame(prop_pos <- (indus_gre[[i]]$positive - indus_gre[[i]]$negative)/n_sims)
  nodes <- rownames(indus_gre[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  indus_gre_prop[[i]] <- prop_pos
}
names(indus_gre_prop) <- names(myfiles)
#Calculate the difference in the proportion of positive simulations between each n-1 model and the full model
indus_gre_prop_diff <- list() 
for (i in 1:length(indus_gre_prop)) {
  as.data.frame(prop_pos <- (indus_gre_prop[[i]] - indus_gre_prop[["Sims_Mod_full"]]))
  nodes <- rownames(indus_gre[[i]])
  prop_pos <- setNames(prop_pos, nodes)
  indus_gre_prop_diff[[i]] <- prop_pos
}
names(indus_gre_prop_diff) <- names(myfiles)
#Convert from List to Tibble - GREEN
indus_gre_prop_diff_tib <- as_tibble(do.call(cbind, indus_gre_prop_diff), rownames = NA) %>% rownames_to_column(var = "Node")
indus_gre_prop_diff_tib$Node <- as.factor(indus_gre_prop_diff_tib$Node)
#Reformat Tibble
indus_gre_prop_tib_trans <- pivot_longer(indus_gre_prop_diff_tib, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
indus_gre_prop_diff_links <- full_join(mod_edges_names, indus_gre_prop_tib_trans, by = "ModelName") %>% select(!c(From, To, Type))
indus_gre_prop_diff_links$LandUse <- "Industrial" 
indus_gre_prop_diff_links$LandUse <- as.factor(indus_gre_prop_diff_links$LandUse)
indus_gre_prop_diff_links$Solution <- "green" 
indus_gre_prop_diff_links$Solution <- as.factor(indus_gre_prop_diff_links$Solution)

#combine the results (differences) for all three solution types
diffresults_indus_2 <- rbind(indus_sou_prop_diff_links, indus_gra_prop_diff_links, indus_gre_prop_diff_links)
diffresults_indus_2$Link <- as.factor(diffresults_indus_2$Link)
diffresults_indus_2$ModelName <- as.factor(diffresults_indus_2$ModelName)

write.csv(diffresults_indus_2, "IterateResults_Difference_Indus_Opt2_27Oct2022.csv", row.names = FALSE)



###### OPTION 2: plot the differences for green infrastructure & residential #####
#If needed, load the results from CSV and get them in the correct format for plotting
diffresults_res_2 <- read.csv("./results/IterateResults_Difference_Resi_Opt2_27Oct2022.csv", header = TRUE, check.names=FALSE)
diffresults_resALL_2_plot <- gather(diffresults_res_2, key="Node", value="difference", 3:31)
diffresults_indus_2 <- read.csv("./results/IterateResults_Difference_Indus_Opt2_27Oct2022.csv", header = TRUE, check.names=FALSE)
diffresults_indusALL_2_plot <- gather(diffresults_indus_2, key="Node", value="difference", 3:31)
diffresults_trans_2 <- read.csv("./results/IterateResults_Difference_Trans_Opt2_27Oct2022.csv", header = TRUE, check.names=FALSE)
diffresults_transALL_2_plot <- gather(diffresults_trans_2, key="Node", value="difference", 3:31)

#select green solution
diffresults_resgre_2 <- filter(diffresults_res_2, Solution == "green")

diffresults_resgre_2_plot <- gather(diffresults_resgre_2, key="Node", value="difference", 3:31)


plot_physhealth_2 <- diffresults_resgre_2_plot %>%
  filter(Node == "Physical health") %>% mutate(Link = fct_reorder(Link, difference)) %>%
  ggplot(aes(x = difference, y = Link, fill = difference)) + geom_tile(color = "white", lwd = 0.6, linetype = 1) +
  theme_bw(base_size = 14) +
  scale_fill_gradient2(limits = c(-0.2, 0.2), breaks = c(-0.2, 0, 0.2), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Physical Health: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_resgre_2_plot, Node == "Physical health")$difference), y = "Residential->Bacteria",
           label = paste("Residential\n", "Green"))

plot_physhealth_top_2 <- diffresults_resgre_2_plot %>%
  filter(Node == "Physical health" & difference > 0) %>% mutate(Link = fct_reorder(Link, difference)) %>%
  ggplot(aes(x = difference, y = Link, fill = difference)) + geom_tile(color = "white", lwd = 2, linetype = 1) +
  theme_bw(base_size = 16) +
  scale_fill_gradient2(limits = c(0, 0.12), breaks = c(0, 0.06, 0.12), labels = c("No change", "6% increase", "12% increase"), midpoint = 0.06,
                       low = "#FFEFDD",
                       mid = "#FF998B",
                       high = "#A62C2B") +
  labs(x = "Physical Health: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_resgre_2_plot, Node == "Physical health" & difference >= 0)$difference), 
           y = "Residential->Toxics", label = paste("Residential\n", "Green"), size = 5)

plot_physhealth_bottom_2 <- diffresults_resgre_2_plot %>%
  filter(Node == "Physical health" & difference <= 0) %>% mutate(Link = fct_reorder(Link, difference)) %>%
  ggplot(aes(x = difference, y = Link, fill = difference)) + geom_tile(color = "white", lwd = 2, linetype = 1) +
  theme_bw(base_size = 16) +
  scale_fill_gradient2(limits = c(0, 0.12), breaks = c(0, 0.06, 0.12), labels = c("No change", "6% increase", "12% increase"), midpoint = 0.06,
                       low = "#FFEFDD",
                       mid = "#FF998B",
                       high = "#A62C2B") +
  labs(x = "Physical Health: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_resgre_2_plot, Node == "Physical health" & difference >= 0)$difference), 
           y = "Residential->Toxics", label = paste("Residential\n", "Green"), size = 5)

#Pull out outliers (top/bottom 6 values)
bottom_physhealth <- diffresults_resgre_2_plot %>% filter(Node == "Physical health") %>% dplyr::slice_min(difference, n = 6)
top_physhealth <- diffresults_resgre_2_plot %>% filter(Node == "Physical health") %>% dplyr::slice_max(difference, n = 6)
outliers_physhealth <- rbind(top_physhealth, bottom_physhealth)
outliers_physhealth$difference_perc <- outliers_physhealth$difference*100

plot_physhealth_outliers_2 <- outliers_physhealth %>%
  mutate(Link = fct_reorder(Link, difference_perc)) %>%
  ggplot(aes(x = difference_perc, y = Link, fill = difference_perc)) + geom_tile(color = "white", width = 1, height = 0.5) +
  theme_bw(base_size = 18) +
  scale_fill_gradient2(limits = c(-20, 20), breaks = c(-20, 0, 20), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "% Change in Physical Health Outcome", y = "Excluded Link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15)) + #size of the legend bar 
  annotate("text", x = max(filter(outliers_physhealth, Node == "Physical health")$difference_perc)-5, y = "Water temperature-*Water quality",
           label = paste("Land use: Residential\n", "Solution: Green Infra."), size = 6)

#Pull out outliers (top/bottom 6 values)
bottom_biodiv <- diffresults_resgre_2_plot %>% filter(Node == "Biodiversity") %>% dplyr::slice_min(difference, n = 6)
top_biodiv <- diffresults_resgre_2_plot %>% filter(Node == "Biodiversity") %>% dplyr::slice_max(difference, n = 6)
outliers_biodiv <- rbind(top_biodiv, bottom_biodiv)
outliers_biodiv$difference_perc <- outliers_biodiv$difference*100

plot_biodiv_outliers_2 <- outliers_biodiv %>%
  mutate(Link = fct_reorder(Link, difference_perc)) %>%
  ggplot(aes(x = difference_perc, y = Link, fill = difference_perc)) + geom_tile(color = "white", width = 1, height = 0.5) +
  theme_bw(base_size = 18) +
  scale_fill_gradient2(limits = c(-20, 20), breaks = c(-20, 0, 20), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "% Change in Biodiversity Outcome", y = "Excluded Link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 15)) + #size of the legend bar 
  annotate("text", x = max(filter(outliers_biodiv, Node == "Biodiversity")$difference_perc)-5, y = "Water temperature-*Water quality",
           label = paste("Land use: Residential\n", "Solution: Green Infra."), size = 6)

#combine Biodiv and Phys Health plots
diffresults_res_2_plot_biodiv_physhealth <- diffresults_resgre_2_plot %>% filter(Node == "Biodiversity" | Node == "Physical health")
diffresults_res_2_plot_biodiv_physhealth$difference_perc <- diffresults_res_2_plot_biodiv_physhealth$difference*100

  
plot_physhealth_biodiv_2 <- diffresults_res_2_plot_biodiv_physhealth %>% mutate(Link = fct_reorder(Link, difference_perc)) %>%
  ggplot(aes(x = difference_perc, y = Link, fill = difference_perc)) + geom_tile(color = "white", width = 1, height = 1) +
  theme_bw(base_size = 16) +
  facet_wrap("Node") +
  labs(x = "Percent change in outcome", y = "Excluded Link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  scale_fill_gradient2(limits = c(-20, 20), breaks = c(-20, 0, 20), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000")

bottom_biodiv <- diffresults_res_2_plot %>% filter(Node == "Biodiversity") %>% dplyr::slice_min(difference, n = 6)
top_biodiv <- diffresults_res_2_plot %>% filter(Node == "Biodiversity") %>% dplyr::slice_max(difference, n = 6)
outliers_biodiv <- rbind(top_biodiv, bottom_biodiv)
outliers_biodiv$difference_perc <- outliers_biodiv$difference*100

bottom_physhealth <- diffresults_res_2_plot %>% filter(Node == "Physical health") %>% dplyr::slice_min(difference, n = 6)
top_physhealth <- diffresults_res_2_plot %>% filter(Node == "Physical health") %>% dplyr::slice_max(difference, n = 6)
outliers_physhealth <- rbind(top_physhealth, bottom_physhealth)
outliers_physhealth$difference_perc <- outliers_physhealth$difference*100

outliers_physhealth_biodiv <- rbind(outliers_biodiv, outliers_physhealth)

plot_physhealth_biodiv_outliers <- outliers_physhealth_biodiv %>% mutate(Link = fct_reorder(Link, difference_perc)) %>%
  ggplot(aes(x = difference_perc, y = Link, fill = difference_perc)) + geom_tile(color = "white", width = 1, height = 1) +
  theme_bw(base_size = 16) +
  facet_wrap("Node") +
  labs(x = "Percent change in outcome", y = "Excluded Link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  scale_fill_gradient2(limits = c(-20, 20), breaks = c(-20, 0, 20), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000")

plot_physhealth_biodiv_outliers_categorical <- ggplot(outliers_physhealth_biodiv, aes(x = Node, y = Link, fill = difference_perc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + coord_fixed() + theme_bw() +
  theme(panel.grid.major = element_blank(), axis.text = element_text(size = 15), axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        strip.text.x = element_text(size = 15)) +
  scale_y_discrete(limits=rev) +
  scale_fill_gradient2(limits = c(-20, 20), breaks = c(-20, 0, 20), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000")

#combine Salmon and Economic wellbeing plots
diffresults_resgre_2_plot_salm_pval <- diffresults_resgre_2_plot %>% filter(Node == "Salmon" | Node == "Property value")
diffresults_resgre_2_plot_salm_pval$difference_perc <- diffresults_resgre_2_plot_salm_pval$difference*100


plot_salm_pval_2 <- diffresults_resgre_2_plot_salm_pval %>% mutate(Link = fct_reorder(Link, difference_perc)) %>%
  ggplot(aes(x = difference_perc, y = Link, fill = difference_perc)) + geom_tile(color = "white", width = 1, height = 1) +
  theme_bw(base_size = 16) +
  facet_wrap("Node") +
  labs(x = "Percent change in outcome", y = "Excluded Link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) +
  scale_fill_gradient2(limits = c(-20, 20), breaks = c(-20, 0, 20), labels = c("decrease", "no change", "increase"), midpoint = 0,
                       low = "#075AFF",
                       mid = "#f5f5f5",
                       high = "#FF0000")


##### OPTION 2: Plot the difference for ALL solutions and ALL benefit nodes #####
# RESIDENTIAL LAND USE
#calculate percent difference
diffresults_resALL_2_plot$difference_perc <- diffresults_resALL_2_plot$difference*100

#Add Node Type so we can filter by benefits, stressors, etc
key <- read.csv("./model/KEY_NodeType.csv", header = TRUE, check.names=FALSE) #load Node Type key
diffresults_resALL_2_plot_key <- full_join(diffresults_resALL_2_plot, key, by = "Node") # join the key with the results
diffresults_resALL_2_plot_key_nofull <- subset(diffresults_resALL_2_plot_key, ModelName != "Sims_Mod_full") # remove the full model from the dataframe



plot_allResALL_categ_byNode <- diffresults_resALL_2_plot_key_nofull %>% filter(Type == "human"| Type == "nature") %>%
  mutate(across(Node, factor, levels=c("Biodiversity", "Climate mitigation", "Salmon", "Water quality", "Access to nature", "Affordability", "Air quality",
                                       "Economic wellbeing", "Local foods", "Local jobs", "Mental health", "Physical health", "Property value",
                                       "Social cohesion", "Walkability"))) %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = Link, fill = difference_perc)) +
  geom_tile(color = "white", lwd = 0.6, linetype = 1) + coord_fixed() + theme_bw(base_size = 8) +
  theme(panel.grid.major = element_blank(), aspect.ratio=30/2) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(-105, 105), breaks = c(-105, -50, 0, 50, 105),
                                                      colours=c("#219ebc", "#8ecae6", "#FFFFFF", "#ef233c", "#d90429"),
                                                      labels = c("-100%", "-50%", "no change", "+50%", "+100%")) +
  facet_wrap(~Node, ncol = 15, labeller = label_wrap_gen(width = 12, multi_line = TRUE)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + #rotate the x axis labels
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Excluded Link", fill = element_blank(), size = 12)
ggsave("./figs/LOO_Resi_byNode.png", width = 12, height = 9, device='png', dpi=700)


# INDUSTRIAL LAND USE
#calculate percent difference
diffresults_indusALL_2_plot$difference_perc <- diffresults_indusALL_2_plot$difference*100

#Add Node Type so we can filter by benefits, stressors, etc
key <- read.csv("KEY_NodeType.csv", header = TRUE, check.names=FALSE) #load Node Type key
diffresults_indusALL_2_plot_key <- full_join(diffresults_indusALL_2_plot, key, by = "Node") # join the key with the results
diffresults_indusALL_2_plot_key_nofull <- subset(diffresults_indusALL_2_plot_key, ModelName != "Sims_Mod_full") # remove the full model from the dataframe


plot_allIndusALL_categ_byNode <- diffresults_indusALL_2_plot_key_nofull %>% filter(Type == "human"| Type == "nature") %>%
  mutate(across(Node, factor, levels=c("Biodiversity", "Climate mitigation", "Salmon", "Water quality", "Access to nature", "Affordability", "Air quality",
                                       "Economic wellbeing", "Local foods", "Local jobs", "Mental health", "Physical health", "Property value",
                                       "Social cohesion", "Walkability"))) %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = Link, fill = difference_perc)) +
  geom_tile(color = "white", lwd = 0.6, linetype = 1) + coord_fixed() + theme_bw(base_size = 8) +
  theme(panel.grid.major = element_blank(), aspect.ratio=30/2) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(-105, 105), breaks = c(-105, -50, 0, 50, 105),
                                                      colours=c("#219ebc", "#8ecae6", "#FFFFFF", "#ef233c", "#d90429"),
                                                      labels = c("-100%", "-50%", "no change", "+50%", "+100%")) +
  facet_wrap(~Node, ncol = 15, labeller = label_wrap_gen(width = 12, multi_line = TRUE)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + #rotate the x axis labels
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Excluded Link", fill = element_blank(), size = 12)
ggsave("./figs/LOO_Indus_byNode.png", width = 12, height = 9, device='png', dpi=700)


# TRANSPORTATION LAND USE
#calculate percent difference
diffresults_transALL_2_plot$difference_perc <- diffresults_transALL_2_plot$difference*100

#Add Node Type so we can filter by benefits, stressors, etc
#key <- read.csv("KEY_NodeType.csv", header = TRUE, check.names=FALSE) #load Node Type key
diffresults_transALL_2_plot_key <- full_join(diffresults_transALL_2_plot, key, by = "Node") # join the key with the results
diffresults_transALL_2_plot_key_nofull <- subset(diffresults_transALL_2_plot_key, ModelName != "Sims_Mod_full") # remove the full model from the dataframe



plot_allTransALL_categ_byNode <- diffresults_transALL_2_plot_key_nofull %>% filter(Type == "human"| Type == "nature") %>%
  mutate(across(Node, factor, levels=c("Biodiversity", "Climate mitigation", "Salmon", "Water quality", "Access to nature", "Affordability", "Air quality",
                                       "Economic wellbeing", "Local foods", "Local jobs", "Mental health", "Physical health", "Property value",
                                       "Social cohesion", "Walkability"))) %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = Link, fill = difference_perc)) +
  geom_tile(color = "white", lwd = 0.6, linetype = 1) + coord_fixed() + theme_bw(base_size = 8) +
  theme(panel.grid.major = element_blank(), aspect.ratio=30/2) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(-105, 105), breaks = c(-105, -50, 0, 50, 105),
                                                      colours=c("#219ebc", "#8ecae6", "#FFFFFF", "#ef233c", "#d90429"),
                                                      labels = c("-100%", "-50%", "no change", "+50%", "+100%")) +
  facet_wrap(~Node, ncol = 15, labeller = label_wrap_gen(width = 12, multi_line = TRUE)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + #rotate the x axis labels
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Excluded Link", fill = element_blank(), size = 12)
ggsave("./figs/LOO_Trans_byNode.png", width = 12, height = 9, device='png', dpi=700)



# Plot all land uses together
diffresults_ALL_2_plot_key_nofull <- rbind(diffresults_transALL_2_plot_key_nofull, diffresults_indusALL_2_plot_key_nofull, diffresults_resALL_2_plot_key_nofull)
diffresults_ALL_2_plot_key_nofull$LandUse <- as.factor(diffresults_ALL_2_plot_key_nofull$LandUse)
diffresults_ALL_2_plot_key_nofull$Node <- as.factor(diffresults_ALL_2_plot_key_nofull$Node)

write.csv(diffresults_ALL_2_plot_key_nofull, "IterateResults_Difference_AllUses_Opt2_27Oct2022.csv", row.names = F)

plot_ALL_byNode <- diffresults_ALL_2_plot_key_nofull %>% filter(Node == "Biodiversity"| Node == "Water quality"| Node == "Air quality"
                                                                              | Node == "Physical health") %>%
  mutate(across(Node, factor, levels=c("Biodiversity", "Water quality", "Air quality", "Physical health"))) %>%
  mutate(across(Solution, factor, levels=c("source", "gray", "green"))) %>%
  ggplot(aes(x = Solution, y = Link, fill = difference_perc)) +
  geom_tile(color = "white", lwd = 0.6, linetype = 1) + coord_fixed() + theme_bw(base_size = 8) +
  theme(axis.text.x = element_text(size = 10), axis.title=element_text(size=12, face="bold"), 
        strip.text.x = element_text(size = 8), legend.text=element_text(size=10)) +
  theme(panel.grid.major = element_blank(), aspect.ratio=19/2) +
  scale_y_discrete(limits=rev) + scale_fill_gradientn(limits = c(-105, 105), breaks = c(-105, -50, 0, 50, 105),
                                                      colours=c("#219ebc", "#8ecae6", "#FFFFFF", "#ef233c", "#d90429"),
                                                      labels = c("-100%", "-50%", "no change", "+50%", "+100%")) +
  facet_wrap(~LandUse + Node, ncol = 15, labeller = label_wrap_gen(width = 12, multi_line = TRUE)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + #rotate the x axis labels
  guides(fill = guide_colourbar(barwidth = 1.5,
                                barheight = 16)) + #size of the legend bar
  labs(x = "Stormwater Solution", y = "Excluded Link", fill = element_blank())
ggsave("./figs/LOO_AllUses_byNode.png", width = 13, height = 9, device='png', dpi=700)


##### OPTION 3: summarize perturbation results using solution+landuse - land use #####

#calculate the difference between the perturbation including SOURCE CONTROL and the perturbation of residential only
res_sou_diff <- list() 
for (i in 1:length(res_sou)) {
  as.data.frame(difference <- res_sou[[i]]$positive - res[[i]]$positive)
  nodes <- rownames(res_sou[[i]])
  difference <- setNames(difference, nodes)
  res_sou_diff[[i]] <- difference
}
names(res_sou_diff) <- names(myfiles)
#Convert from List to Tibble
res_sou_diff <- as_tibble(do.call(cbind, res_sou_diff), rownames = NA) %>% rownames_to_column(var = "Node")
res_sou_diff$Node <- as.factor(res_sou_diff$Node)
#Reformat Tibble
res_sou_diff_trans <- pivot_longer(res_sou_diff, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_sou_diff_links <- full_join(mod_edges_names, res_sou_diff_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_sou_diff_links$LandUse <- "Residential" 
res_sou_diff_links$LandUse <- as.factor(res_sou_diff_links$LandUse)
res_sou_diff_links$Solution <- "source" 
res_sou_diff_links$Solution <- as.factor(res_sou_diff_links$Solution)

#calculate the difference between the perturbation including GRAY INFRASTRUCTURE and the perturbation of residential only
res_gra_diff <- list() 
for (i in 1:length(res_gra)) {
  as.data.frame(difference <- res_gra[[i]]$positive - res[[i]]$positive)
  nodes <- rownames(res_gra[[i]])
  difference <- setNames(difference, nodes)
  res_gra_diff[[i]] <- difference
}
names(res_gra_diff) <- names(myfiles)
#Convert from List to Tibble
res_gra_diff <- as_tibble(do.call(cbind, res_gra_diff), rownames = NA) %>% rownames_to_column(var = "Node")
res_gra_diff$Node <- as.factor(res_gra_diff$Node)
#Reformat Tibble
res_gra_diff_trans <- pivot_longer(res_gra_diff, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_gra_diff_links <- full_join(mod_edges_names, res_gra_diff_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_gra_diff_links$LandUse <- "Residential" 
res_gra_diff_links$LandUse <- as.factor(res_gra_diff_links$LandUse)
res_gra_diff_links$Solution <- "gray" 
res_gra_diff_links$Solution <- as.factor(res_gra_diff_links$Solution)

#calculate the difference between the perturbation including GREEN INFRASTRUCTURE and the perturbation of residential only
res_gre_diff <- list() 
for (i in 1:length(res_gre)) {
  as.data.frame(difference <- res_gre[[i]]$positive - res[[i]]$positive)
  nodes <- rownames(res_gre[[i]])
  difference <- setNames(difference, nodes)
  res_gre_diff[[i]] <- difference
}
names(res_gre_diff) <- names(myfiles)
#Convert from List to Tibble
res_gre_diff <- as_tibble(do.call(cbind, res_gre_diff), rownames = NA) %>% rownames_to_column(var = "Node")
res_gre_diff$Node <- as.factor(res_gre_diff$Node)
#Reformat Tibble
res_gre_diff_trans <- pivot_longer(res_gre_diff, cols = -1) %>% pivot_wider(names_from = "Node", values_from = "value") %>% rename(ModelName = name)
#Join Results with the model names
res_gre_diff_links <- full_join(mod_edges_names, res_gre_diff_trans, by = "ModelName") %>% select(!c(From, To, Type))
res_gre_diff_links$LandUse <- "Residential" 
res_gre_diff_links$LandUse <- as.factor(res_gre_diff_links$LandUse)
res_gre_diff_links$Solution <- "green" 
res_gre_diff_links$Solution <- as.factor(res_gre_diff_links$Solution)

#combine the results (differences) for all through infra types
diffresults_res <- rbind(res_sou_diff_links, res_gra_diff_links, res_gre_diff_links)
diffresults_res$Link <- as.factor(diffresults_res$Link)
diffresults_res$ModelName <- as.factor(diffresults_res$ModelName)

write.csv(diffresults_res, "IterateResults_Difference_Resi_Opt2_5Oct2022.csv", row.names = FALSE)


###### OPTION 3: plot the differences for green infrastructure & residential #####

diffresults_res_gre <- filter(diffresults_res, Solution == "green")

diffresults_res_gre_plot <- gather(diffresults_res_gre, key="Node", value="difference", 3:31)
diffresults_res_gre_plot$difference_prop <- diffresults_res_gre_plot$difference/n_sims


plot_physhealth <- diffresults_res_gre_plot %>%
  filter(Node == "Physical health") %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw(base_size = 14) +
  scale_fill_gradient2(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0: no change", "0.5: moderate increase", "1: strong increase"), midpoint = 0.5,
                       low = "#EEEEEE",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Physical Health: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_plot, Node == "Physical health")$difference_prop), y = "Residential->Bacteria",
           label = paste("Residential\n", "Green"))

cutoff_physhealth <- filter(diffresults_res_gre_plot, Link == "Full" & Node == "Physical health")

plot_physhealth_top <- diffresults_res_gre_plot %>%
  filter(Node == "Physical health" & difference_prop >= cutoff_physhealth$difference_prop) %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 2, linetype = 1) +
  theme_bw(base_size = 16) +
  scale_fill_gradient2(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0: no change", "0.5: moderate increase", "1: strong increase"), midpoint = 0.5,
                       low = "#EEEEEE",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Physical Health: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_plot, Node == "Physical health" & difference_prop >= cutoff_physhealth$difference_prop)$difference_prop), 
           y = "Residential->Toxics", label = paste("Residential\n", "Green"), size = 5)

plot_biodiv <- diffresults_res_gre_plot %>%
  filter(Node == "Biodiversity") %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 0.8, linetype = 1) +
  theme_bw(base_size = 14) +
  scale_fill_gradient2(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0: no change", "0.5: moderate increase", "1: strong increase"), midpoint = 0.5,
                       low = "#EEEEEE",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Biodiversity: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_plot, Node == "Biodiversity")$difference_prop), y = "Residential->Bacteria",
           label = paste("Residential\n", "Green"))

  
cutoff_biodiv <- filter(diffresults_res_gre_plot, Link == "Full" & Node == "Biodiversity")

plot_biodiv_top <- diffresults_res_gre_plot %>%
  filter(Node == "Biodiversity" & difference_prop >= cutoff_biodiv$difference_prop) %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 2, linetype = 1) +
  theme_bw(base_size = 16) +
  scale_fill_gradient2(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0: no change", "0.5: moderate increase", "1: strong increase"), midpoint = 0.5,
                       low = "#EEEEEE",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Biodiversity: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15)) + #size of the legend bar 
  annotate("text", x = min(filter(diffresults_res_gre_plot, Node == "Biodiversity" & difference_prop >= cutoff_physhealth$difference_prop)$difference_prop), y = "Residential->Toxics",
           label = paste("Residential\n", "Green"), size = 5) #+
 # expand_limits(x=c(0.8, 0.9))



plot_salmo <- diffresults_res_gre_plot %>%
  filter(Node == "Salmon") %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  theme_bw() +
  scale_fill_gradient2(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0: no change", "0.5: moderate increase", "1: strong increase"), midpoint = 0.5,
                       low = "#EEEEEE",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Salmon: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 15)) #size of the legend bar

cutoff_salm <- filter(diffresults_res_gre_plot, Link == "Full" & Node == "Salmon")

plot_salm_top <- diffresults_res_gre_plot %>%
  filter(Node == "Salmon" & difference_prop >= cutoff_salm$difference_prop) %>% mutate(Link = fct_reorder(Link, difference_prop)) %>%
  ggplot(aes(x = difference_prop, y = Link, fill = difference_prop)) + geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  theme_bw() +
  scale_fill_gradient2(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0: no change", "0.5: moderate increase", "1: strong increase"), midpoint = 0.5,
                       low = "#EEEEEE",
                       mid = "#f5f5f5",
                       high = "#FF0000") +
  labs(x = "Salmon: Proportional difference in # positive simulations", y = "Excluded model link", fill = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 15)) #size of the legend bar


