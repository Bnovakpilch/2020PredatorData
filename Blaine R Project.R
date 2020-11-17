#Let's start by loading our packages
library(tidyverse)
library(googleway) 
library(sf) 
library(rnaturalearth) 
library(rnaturalearthdata)
library(readxl)
library(plyr)
library(stats)
library(reshape2)

#Let's start by making a map of where the data was collected
#Import a map

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#Good it's an SF file
#Lets look at it.

ggplot(data = world) +
  geom_sf()

#It's pretty far out lets zoom in, plot our data, and make it non-gray.

ULE = ggplot(data = world) +
  geom_sf(fill = "peachpuff") +geom_point(mapping = aes(x = -92.012529, y = 30.307207))+ ggtitle("Data Collection Site")+ylab("Latitude")+
  xlab("Longitude")+ annotate(geom = "text", x = -92.012529, y = 30.55, label = "UL Ecology Center", fontface = "bold", color = "red4", size = 5) +
  coord_sf(xlim = c(-95, -90), ylim = c(28, 32), expand = FALSE)+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed"), panel.background = element_rect(fill = "powderblue"))

ULE

#Lets save it!

ggsave(filename = "ULE.pdf")

#Now lets read in our data.
Spider = read.csv("2020 Spider Data.csv", header = T)

#Look at all of those NA's! Let's clean them up!

Spider = Spider[,-c(3,5,7)]
Spider = na.omit(Spider)

#Let's order it by spider size

Spider = Spider[order(Spider$Predator.Size..Cm.),]

#Lets save it now that the NA's are all cleared up and the data is ordered. 
#Note that row names are false and column names are true, this is necessary for the data to save properly.

write.table(Spider, file = "2020 Spider Data Fixed.csv",row.names = F, col.names = T, sep = ",")

#Let's Visualize Some of It!
#How does spider size varry? 
SizeInPop = ggplot(Spider, aes(x = Spider$Predator.Size..Cm.))+ geom_histogram(binwidth = .25, fill = "red3")+xlab("Spider Size")+ ylab("Number of Individuals")+
  scale_x_continuous()+ ggtitle("Spider Size by Date")

SizeInPop

#How does it change over time though?
SizeXTime = ggplot(Spider, aes(x = Date, y =Spider$Predator.Size..Cm.))+ geom_boxplot(fill = "orange")+ylab("Spider Size")+ 
  scale_x_discrete()+theme(panel.background = element_rect(fill = "lightblue", color = "lightblue", size = 0.5, linetype = "solid"))+
  ggtitle("Spider Size by Date")

SizeXTime

#Is it significant though?

SpiderAOV = aov(Predator.Size..Cm.~Date, data = Spider)
summary.aov(SpiderAOV)

#P<0.0001 It's Significant! Spiders do grow!

#Now let's look at some other data from this summer:

Predation = read.csv("2020 Data - R Class.csv", header = T)

#Neat, lets see if jumping spiders had a prefered flower species.
#Subset our jumping spiders
PredationII = subset(Predation,Predation$Predator == "JMP")

#Rename them to their scientific name:
CODE2NAME = function(x){
  if_else(condition = x=="JMP", true = "Salticidae", false = "NA")}
PredationII$Predator = CODE2NAME(PredationII$Predator)

#Let's add a flower species column
PredationII$Flower.Species = PredationII$?..Flower.ID
PredationII$Flower.Species = str_sub(string = PredationII$Flower.Species, start = 1, end = 3)
PredationII$Flower.Species = toupper(PredationII$Flower.Species)

#Now that it's all uniform we can graph it!
ggplot(PredationII, aes(x = Flower.Species))+geom_bar()


#Finally let's see if some plants harbor more species of predators than others:
Predation$Flower.Species = Predation$?..Flower.ID
Predation$Flower.Species = str_sub(string = Predation$Flower.Species, start = 1, end = 3)
Predation$Flower.Species = toupper(Predation$Flower.Species)





