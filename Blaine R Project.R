#Let's start by loading our packages
library(tidyverse)
library(sf) 
library(rnaturalearth) 
library(rnaturalearthdata)
library(readxl)
library(plyr)
library(stats)
library(reshape2)


#Let's start by making a map of where the data was collected
#Let's make a dataframe with our coordinates "application of a dataframe"

ECL =data.frame(long = -92.012529, lat = 30.307207)

#Import a map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#Good it's an SF file
#Lets look at it.

ggplot(data = world) +
  geom_sf()

#It's pretty far out lets zoom in, plot our data, and make it non-gray. "Map showing geographic location".

ULE = ggplot(data = world) +
  geom_sf(fill = "peachpuff") +geom_point(ECL ,mapping = aes(x = long, y = lat))+ ggtitle("Data Collection Site")+ylab("Latitude")+
  xlab("Longitude")+ annotate(geom = "text", x = -92.012529, y = 30.55, label = "UL Ecology Center", fontface = "bold", color = "red4", size = 5) +
  coord_sf(xlim = c(-95, -90), ylim = c(28, 32), expand = FALSE)+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed"), panel.background = element_rect(fill = "powderblue"))

ULE

#Lets save it! "Exporting and saving figures"

ggsave(filename = "ULE.pdf")

#Now lets read in our data. "Reading in data"
Spider = read.csv("2020 Spider Data.csv", header = T)
view(Spider)

#Look at all of those NA's! Let's clean them up! "Example of indexing"

Spider = Spider[,-c(3,5,7)]
Spider = na.omit(Spider)
view(Spider)

#Let's order it by spider size "Ordering"

Spider = Spider[order(Spider$Predator.Size..Cm.),]
view(Spider)

#Lets save it now that the NA's are all cleared up and the data is ordered. "Exporting Dataset"
#Note that row names are false and column names are true, this is necessary for the data to save properly.

write.table(Spider, file = "2020 Spider Data Fixed.csv",row.names = F, col.names = T, sep = ",")

#Let's Visualize Some of It!
#How does spider size varry? "Histogram" 
SizeInPop = ggplot(Spider, aes(x = Spider$Predator.Size..Cm.))+ geom_histogram(binwidth = .25, fill = "salmon")+xlab("Spider Size")+ ylab("Number of Individuals")+
  scale_x_continuous()+ ggtitle("Spider Size by Date")+theme(panel.background = element_rect(fill = "lightblue", color = "mintcream", size = 0.5, linetype = "solid"))

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

#Lets look at the interaction between size and position "Subsetting", "Merge/Join"
Size = Spider[,c(1,3)]
Position = Spider[,c(1,2)]
SizeXPosition = join(Size, Position, match = "first")

#Plot it.

ggplot(data = SizeXPosition, aes(x = Position, y = Predator.Size..Cm., fill = Position))+geom_boxplot()+ ylab("Spider Size")+ 
  scale_x_discrete()+theme(panel.background = element_rect(fill = "lightblue", color = "lightblue", size = 0.5, linetype = "solid"))+
  ggtitle("Spider Size by Foraging Behavior")

#I wonder what the average size for each plant species is? "ddply"

AVGSZ = ddply(.data = SizeXPosition, .variable = "Plant.Species", summarise, Mean.Size = mean(Predator.Size..Cm.))

#More visualization:

ggplot(data = AVGSZ, aes (x = reorder(Plant.Species, - Mean.Size), y = Mean.Size, fill = Plant.Species))+ geom_bar(stat = "identity")+
  scale_x_discrete()+theme(axis.text.x=element_blank(), axis.ticks.x = element_blank() )+ggtitle("Mean Spider Size by Plant Species")+ 
  xlab("Plant Species")+ ylab("Average Spider Size")
  
#Now why does lynx spider size matter? Well their maximum prey size is actually linear
#So we can use this data to see what sort of prey they can catch.
#Lets make a custom function to convert this our size data into maximum prey size data. "Custom Function"

PreySize = function(x){
  y = 3.27+4.96*x
}

Spider$Prey.Size = PreySize(Spider$Predator.Size..Cm.)
view(Spider)

#Now let's graph it by date to see how prey size changes over time.

PreyXTime = ggplot(Spider, aes(x = Date, y =Prey.Size))+ geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .3)+ylab("Prey Size (mm)")+ 
  scale_x_discrete()+theme(panel.background = element_rect(fill = "lightblue", color = "lightblue", size = 0.5, linetype = "solid"))+
  ggtitle("Prey Potential by Date")

PreyXTime

#Now lets see when they can start preying on efficient pollinators, like the American Bumblebee. "ggplot w/ 2 geoms"

PreyXTime = PreyXTime + geom_hline(yintercept = c(16), linetype = "dotted", color = "red3", size = 1.5)

PreyXTime

#Cool!
#Now let's look at some other data from this summer:

Predation = read.csv("2020 Data.csv", header = T)
view(Predation)

#Clean up "Reshaping"
Predation = Predation[,-c(4:6,8:10)]
Predation$Flower.ID = str_sub(string = Predation$Flower.ID, start = 1, end = 3)
Predation$Flower.ID = toupper(Predation$Flower.ID)
view(Predation)

PredationI = dcast(Predation, Flower.ID~Predator)
view(PredationI)

#Crab spiders are listed as CRB but for some reason one is "crab spider" let's merge the columns
PredationI$CRB = PredationI$`crab spider`+ PredationI$CRB
PredationI = PredationI[, - 4]
View(PredationI)

#Get rid of the NA Column
PredationI = PredationI[,-9]
view(PredationI)

#Get rid of the "Flowers" with no data
PredationI = PredationI[-c(1:3, 5, 9),]
view(PredationI)

#Restructure it "melt"
PredationI = melt(PredationI)
View(PredationI)

#Now that it's all uniform we can graph it! "Bar Plot"
ggplot(PredationI, aes(x =reorder(variable, - value), y = value, fill = Flower.ID))+geom_bar(stat = "identity")+ylab("Predator Abundance")+theme(panel.background = element_rect(fill = "lightblue", color = "lightblue", size = 0.5, linetype = "solid"))+
  ggtitle("Predator Abundance & Foraging Preference")+xlab("Predator Species")
