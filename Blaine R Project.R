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

#It's pretty far out lets zoom in, plot our data, and make it non-gray. "Map showing geographic location".

ULE = ggplot(data = world) +
  geom_sf(fill = "peachpuff") +geom_point(mapping = aes(x = -92.012529, y = 30.307207))+ ggtitle("Data Collection Site")+ylab("Latitude")+
  xlab("Longitude")+ annotate(geom = "text", x = -92.012529, y = 30.55, label = "UL Ecology Center", fontface = "bold", color = "red4", size = 5) +
  coord_sf(xlim = c(-95, -90), ylim = c(28, 32), expand = FALSE)+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed"), panel.background = element_rect(fill = "powderblue"))

ULE

#Lets save it! "Exporting and saving figures"

ggsave(filename = "ULE.pdf")

#Now lets read in our data. "Reading in data"
Spider = read.csv("2020 Spider Data.csv", header = T)

#Look at all of those NA's! Let's clean them up! "Example of indexing"

Spider = Spider[,-c(3,5,7)]
Spider = na.omit(Spider)

#Let's order it by spider size "Ordering"

Spider = Spider[order(Spider$Predator.Size..Cm.),]

#Lets save it now that the NA's are all cleared up and the data is ordered. "Exporting Dataset"
#Note that row names are false and column names are true, this is necessary for the data to save properly.

write.table(Spider, file = "2020 Spider Data Fixed.csv",row.names = F, col.names = T, sep = ",")

#Let's Visualize Some of It!
#How does spider size varry? "Histogram" 
SizeInPop = ggplot(Spider, aes(x = Spider$Predator.Size..Cm.))+ geom_histogram(binwidth = .25, fill = "khaki1")+xlab("Spider Size")+ ylab("Number of Individuals")+
  scale_x_continuous()+ ggtitle("Spider Size by Date")+theme(panel.background = element_rect(fill = "lightseagreen", color = "mintcream", size = 0.5, linetype = "solid"))

SizeInPop

#How does it change over time though? and how does position factor in with size?
SizeXTime = ggplot(Spider, aes(x = Date, y =Spider$Predator.Size..Cm., fill = Position))+ geom_boxplot()+ylab("Spider Size")+ 
  scale_x_discrete()+theme(panel.background = element_rect(fill = "lightblue", color = "lightblue", size = 0.5, linetype = "solid"))+
  ggtitle("Spider Size by Date")

SizeXTime

#Is it significant though? "Summarizing"

SpiderAOV = aov(Predator.Size..Cm.~Date, data = Spider)
summary.aov(SpiderAOV)

#P<0.0001 It's Significant! Spiders do grow!

#Lets look at the interaction between size and position "Subsetting", "Merge"
Size = Spider[,c(1,3)]
Position = Spider[,c(1,2)]
SizeXPosition = join(Size, Position, match = "first")

ggplot(data = SizeXPosition, aes(x = Position, y = Predator.Size..Cm.))+geom_boxplot()

#Now let's look at some other data from this summer:

Predation = read.csv("2020 Data.csv", header = T)

#Clean up "Reshaping"
Predation = Predation[,-c(4:6,8:10)]
Predation$Flower.ID = str_sub(string = Predation$Flower.ID, start = 1, end = 3)
Predation$Flower.ID = toupper(Predation$Flower.ID)
PredationI = dcast(Predation, Flower.ID~Predator)

#Crab spiders are listed as CRB but for some reason one is "crab spider" let's merge the columns
PredationI$CRB = PredationI$`crab spider`+ PredationI$CRB
PredationI = PredationI[, - 4]

#Get rid of the NA Column
PredationI = PredationI[,-9]

#Get rid of the "Flowers" with no data
PredationI = PredationI[-c(1:3, 5, 9),]

#Restructure it
PredationI = melt(PredationI)

PredationI$variable = CODE2NAME(PredationI$variable)

PredationII$Predator = CODE2NAME(PredationII$Predator)

#Let's add a flower species column
PredationII$Flower.Species = PredationII$ï..Flower.ID
PredationII$Flower.Species = str_sub(string = PredationII$Flower.Species, start = 1, end = 3)
PredationII$Flower.Species = toupper(PredationII$Flower.Species)

#Now that it's all uniform we can graph it!
ggplot(PredationI, aes(x =variable, y = value, fill = Flower.ID))+geom_bar(stat = "identity")+ylab("Predator Abundance")+theme(panel.background = element_rect(fill = "lightblue", color = "lightblue", size = 0.5, linetype = "solid"))+
  ggtitle("Predator Foraging Abundance & Preference")+xlab("Predator Species")

#Neat, lets see if jumping spiders had a prefered flower species.
#Subset our jumping spiders
PredationII = subset(Predation,Predation$Predator == "JMP")

#Rename them to their scientific name:
CODE2NAME = function(x){
  if_else(condition = x=="JMP", true = "Salticidae", false = "NA")}



