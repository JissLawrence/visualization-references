














# Base plotting

# Using plot() to study to continous variables

ir<-iris
str(ir)












# Syntax
# plot(x=variable to be displayed on x axis, y = variable to be displayed on y axis)

plot(x=ir$Petal.Width,y=ir$Petal.Length)










# Adding xlabels, ylables and title

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"))











# Adding colors, plotting symbols







#Adding colors
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red")











#Adding different plotting symbol

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=2)








plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=3)

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=4)










#Adding  more options


plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=4,type="p",lwd=2)










# Making a conditional bivariate plot

# Seeing relationship across different species

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col=ir$Species)














plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),pch=as.numeric(ir$Species),col=ir$Species)










plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),cex=as.numeric(ir$Species))







plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),cex=as.numeric(ir$Species),col=ir$Species)







#Adding a legend

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),pch=as.numeric(ir$Species))
unique(ir$Species)
legend(0.2,7,c("Setosa","Versicolor","Verginica"),pch=1:3)







plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),xlab=c("Petal Width"),ylab=c("Petal Length"),col=ir$Species,pch=as.numeric(ir$Species))

legend(0.2,7,c("Setosa","Versicolor","Verginica"),pch=1:3,col=1:3)

#Studying a continous variable across groups
#Distribution of Sepal lengths across different species



# Univariate Analysis

#Box plots

boxplot(ir$Petal.Length)










summary(ir$Petal.Length) #Mean<Median, negatively skewed







boxplot(ir$Sepal.Width)
summary(ir$Sepal.Width) #Mean>Median, positively skewed

#Improving the asethetics of boxplot
boxplot(ir$Petal.Length,col="red",main="Distribution of Petal length")







plot(x=ir$Species,y=ir$Sepal.Width,xlab="Species",main="Sepal Length across sepcies",col="red")












#Using histograms
hist(ir$Sepal.Width,col="orange")





hist(ir$Sepal.Width,col="orange",labels=TRUE)

hist(ir$Sepal.Width,col="orange",freq=FALSE)
hist(ir$Sepal.Width,col="orange",labels=TRUE,freq=FALSE)
lines(density(ir$Sepal.Width))


#Adding multiple plots in single plotting window
par(mfrow=c(1,2))


plot(x=ir$Species,y=ir$Sepal.Width,xlab="Species",main="Sepal Width across sepcies",col="red")
plot(x=ir$Species,y=ir$Sepal.Length,xlab="Species",main="Sepal Length across sepcies",col="red")
dev.off()


















##Direct Marketing data
mk<-read.csv("DirectMarketing.csv")













library(ggplot2)

#Understand the relationship between Salary and AmountSpent
p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
p+geom_point()













#Understanding the conditional relationship based on Gender
q<-p+geom_point(aes(colour=Gender))

q
















q+xlab("Salary in $")+ylab("Expenditure in $")









#Making a trellis plot for both the genders and fitting a tred line
p+geom_point(aes(colour=Gender))+geom_smooth(aes(colour=Gender))

#Creating a trellis plot
p+geom_point(aes(colour=Gender))+geom_smooth(aes(colour=Gender))+facet_grid(Gender~.)











#Understanding Univaraites
p<-ggplot(mk,aes(x=AmountSpent))
p+geom_histogram()











#Understanding Gender wise distribution
p+geom_histogram(aes(fill=Gender))








#Modifying the position
p+geom_histogram(aes(fill=Gender,colour=Gender),position="stack",alpha=0.3)










#Alternative, draw a trellis plot
p+geom_histogram(aes(fill=Gender))+facet_grid(Gender~.)










#Polishing the graph
p+geom_histogram(aes(fill=Gender,colour=Gender),alpha=0.3)+facet_grid(Gender~.)











#Boxplots
p<-ggplot(mk,aes(y=AmountSpent,x=Gender,fill=Gender))
p+geom_boxplot()









#Density plots
options(scipen=999)
p<-ggplot(mk,aes(x=AmountSpent))
p+geom_density(aes(fill=Gender,colour=Gender),alpha=0.3)















#Conditional density plot
p+geom_density(aes(fill=Gender))










#Improving the plot
p+geom_density(aes(fill=Gender,colour=Gender),alpha=0.4)














#2d counts

p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
p+geom_point(aes)
p+geom_bin2d()












#Changing ticks on a continous scale
p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
p+geom_point()

p+geom_point()+scale_x_continuous(breaks=seq(0,150000,10000))+scale_y_continuous(breaks=seq(0,6000,500))


#Changing fill behaviour
p+geom_bin2d()+scale_fill_gradientn(colours=c("blue","white","red"))+scale_x_continuous(breaks=seq(0,150000,10000))+scale_y_continuous(breaks=seq(0,6000,500))

#Geospatial visualization
library(ggmap)
library(ggplot2)
library(dplyr)
library(rgdal)



map<-get_map("bangalore",maptype="hybrid")
ggmap(map)

#Overlaying data on the map from a text file
sh<-read.csv("schools.csv")

head(sh)

ggmap(map)+geom_point(data=sh,aes(x=long,y=lat),colour="red")


#https://data.cityofnewyork.us/Health/Health-Areas/5p78-k3zm
shape1<-readOGR(dsn="nyha_15a","nyha")

class(shape1)

#shape1 data,ploygons,plotOrder,bbox,proj4string

head(shape1@data)

#Look at the locational information
shape1@bbox
#Long-lat needs to be converted to proper format
shape1<-spTransform(shape1,CRS("+init=epsg:4326"))


#extract long-lat data
shape1.f<-fortify(shape1)

head(shape1.f)
head(shape1@data)

dim(shape1.f)
dim(shape1@data)

shape1@data$id<-unique(shape1.f$id)

#Merging the data
shapeM<-merge(shape1.f,shape1@data,by.x="id",by.y="id")

map<-get_map("New York City",maptype="terrain")
ggmap(map)+geom_polygon(data=shapeM,aes(x=long,y=lat,group=id,fill=Shape_Area),alpha=0.4,colour="black")+scale_fill_gradientn(colours = c("red","white","blue"))

options(scipen=999)

## SpatialPointsDataFrame Newyork Subway Entrance Data
shape2<-readOGR(dsn="Subway","DOITT_SUBWAY_ENTRANCE_01_13SEPT2010")

class(shape2)



#Components of shape2 :data, coordinate.nrs, coords,bbox,proj4string
shape2@

dim(shape2@data)

dim(shape2@coords)

#Contents of shape2@coords
head(shape2@coords)

#Data in Northings and Eastings: Convert it into Long-Lat
shape2<-spTransform(shape2,CRS("+init=epsg:4326"))

#Combine data and long latt info as a dataframe
dataC<-data.frame(shape2@data,shape2@coords)
head(dataC)
map<-get_map("New York City",maptype="satellite")
ggmap(map)+geom_point(data=dataC,aes(x=coords.x1,y=coords.x2),colour="red",alpha=0.3)
