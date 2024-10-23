library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library("ggmap")
library("rgdal")
library(mapproj)

#read in experimental infection data 
d= read_xlsx(path="HAAM ExpInf.xlsx")

#remove empty rows
d= subset(d, select = -c(15,16))

#replace NAs in control group with 0s 
d.control= d[16:20,]
d.control[is.na(d.control)] = 0

#subset infected groups
d.infect= d[1:15,]
d.infect[is.na(d.infect)] = 0



#calculate mean parasitemia in infected groups
dm= colMeans(d.infect[,2:12])

dm= as.data.frame(dm)
summarise(.data = dm, mean= mean(dm))
#8.52

#merge control and infected groups 
d2= merge(d[1:15,], d.control,  all=TRUE)

write.csv(d2, "/Users/gabbyatkinson/Desktop/Intro to R//HAAM_ExpInf_R.csv")

#replace NAs in columns with mean  
d2[is.na(d2)] = 8.52

#convert percentages to decimals 
p2d= function(x){
  y= x/100
  return(y)
}
 
d3= lapply(d2[,2:12], 
      function(x) {  
        if (typeof(x) == 'double'){  
          p2d(x) 
        } else { }
      }  
)  


d3= as.data.frame(d3)

#add Bird ID column to new dataset 
d3= cbind(d2$`Bird ID`, d3)

names(d3)[1]= "BirdID"

#melt dataframes to make plotting easier 
d3= melt(d3)

#check how many of the recorded parasitemia levels are above 20% (max birds can recover from)
for (i in d3$value){
  if (i >= 0.2 )
   print(i)
 
}

#30

#visualize parasitemia observations using histogram
ggplot(data = d3, aes(value))+ geom_histogram() +  xlab("Parasitemia") + ylab("Observations")

#visualize changes in parasitemia overtime 

ggplot(data = d3, aes(x= variable, y= value)) +
  geom_point(color= "coral2", size=2, shape= 23) +
  scale_x_discrete(name= "Days Post-Infection") +
  scale_y_continuous(name= "Parasitemia") +
  theme(axis.text.x = element_text(angle = 45)) 


#map where data was collected

m= readOGR(dsn = "cb_2014_us_county_5m/", layer = "cb_2014_us_county_5m")
m= spTransform(m, CRS("+proj=longlat +datum=WGS84"))
m= fortify(m)

ggplot()+ geom_polygon(data= m, aes(x= long, y= lat, group= group), 
                          colour= "thistle4", 
                          fill= "thistle2") +
  coord_map(xlim = c(-154.5,-160), ylim = c(18, 24)) +
  scale_y_continuous(name= 'Degrees N', breaks = seq(18,24,0.5)) +
  scale_x_continuous(name = 'Degrees W', breaks = seq(-154.5,-160,-0.5)) +
  theme(axis.text.x = element_text(angle = 45))
  
 


