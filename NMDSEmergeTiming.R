# NMDS for collection timing: ordination of emergence samples with 
# timing of sample collection, aka time

# reading in the data
sites<- read.csv("nmds_site.csv", row.names = 1)

# removing non-insects from the data set
sites2<-sites[-c(1,2,3,4,5,11,13,14)]
# checking the dimensions of sites2 to ensure non-insects were removed
dim(sites2)

# loading in vegan and ecodist
library(vegan)
library(ecodist)

# running the nmds, using a bray-curtis matrix and 2 dimensions
sites.nmds <- metaMDS(sites2 , distace= "bray" , k=2)

# checking the stress of the nmds to ensure good fit
stressplot(sites.nmds)
sites.nmds$stress
# R^2= 0.96, stress= 0.083, so fit is fairly good


# visualizing the nmds

# specifying data that will be included and colors for each polygon
type=c(rep("Early" , 3), rep("Early Mid" , 3) , 
       rep("Mid" , 3), rep ("Early Late", 3))
pch.type=type
col.type=c("black", "blue violet" , "chartreuse4", "blue")

# adding symbols and colors for the points in the polygons
pch.symbol= c(rep(15,3), rep(16,3) , rep(17,3) , rep(18,3))
col.symbol= c(rep("black" , 3), rep("chartreuse4" , 3), 
             rep ("blue", 3), rep("blue violet" , 3))
par(mar=c(5,5,2,1)+.1)

# making the plot
ordiplot(sites.nmds , type="n" , display= "sites", cex.lab=2, cex.axis=1.5)

# drawing polygons for each sampling period
ordihull(sites.nmds, groups=type , draw="polygon", col=col.type)

# adding the points
points(sites.nmds , display= "sites", pch=pch.symbol, col=col.symbol)

# adding text for the points (can be included, but not necessary if adding later)
text(sites.nmds, display="sites", cex=0.8, col="black")

# making the taxa into a vector to be overlaid over the ordination plot
bugs <- envfit(sites.nmds, sites2, permutations=999)
# below line plots the sites as just black dots, does not need to be included
  # plot(sites.nmds , display = "sites")

# plotting the taxa vectors over the ordination plot
plot(bugs, col="black")
