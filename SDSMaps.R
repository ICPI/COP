#install and load packages
install.packages("tidyverse")
library("tidyverse")

install.packages("rgdal")
library("rgdal")

install.packages("cowplot")
library("cowplot")

##read in the spatial boundaries
shp<-readOGR(choose.files(default="",caption="Select .shp file"))
plot(shp)

##read in your data table
data<-read_csv(choose.files(default="",caption="Select .csv file"))

#I pulled a data table from panorama that needed some reformatting
data<-data%>%
      filter(Metrics...3=="Results")%>% #filtered out target and achievement values
      mutate(Value=gsub(",","",Metrics...4), #removed the comma separators from the metric
             orgunit_na=`level selector 1 geopp`, #renamed my snu column to match the shapefile column name
             Value=as.numeric(Value))%>% #changed the metric to numeric (it read in as a character)
      select(orgunit_na,Indicator,Value) #remove unnecessary columns
head(data)

##join
CoteDivore<-merge(shp,data,by="orgunit_na")

##mapping in Leaflet----
#install mapping libraries
install.packages("leaflet")
library(leaflet)
install.packages("BAMMtools")
library(BAMMtools)

#calculate the bin values
Breaks<-getJenksBreaks(CoteDivore@data$Value,6)

#create the color palette
col_pal <- colorBin(palette = 'RdPu',pretty=TRUE,bins=rev(Breaks),domain = CoteDivore@data$Value,reverse=FALSE,na.color="#aeaeae")

#plot the map
leaflet(CoteDivore)%>%
  addTiles()%>%
  addPolygons(data=CoteDivore, color='black', weight=1, opacity=.8, 
                            fillColor = ~col_pal(Value), fillOpacity = 1, label = paste0(CoteDivore$orgunit_na,":\n",round(CoteDivore$Value,1)))%>%
                addLegend('bottomleft', pal = col_pal,values = CoteDivore@data$Value, title = 'TX CURR (FY22)',opacity=1)

##Mapping in ggplot----
#convert shapefile to dataframe (necessary for ggplot2); 
CoteDivore_shp<-fortify(CoteDivore,region="uid")

#The fortify function removes the data that we want to map so we'll need to join that data back to the file
CoteDivore_data<-CoteDivore@data #extract data
CoteDivore_data$id<-CoteDivore_data$uid #create rename column to help with join
CoteDivore_df<-merge(CoteDivore_shp,CoteDivore_data,by="id")#join data to spatial dataframe

##create the color scale
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all() #this allows you to see your color options

#calculate breaks
Breaks<-getJenksBreaks(CoteDivore@data$Value,6)
Breaks<-Breaks[-1] #this removes the lowest value to fix an issue with how the legend was showing up

#plot the map
m<-ggmap(basemap)+
  geom_polygon(CoteDivore_df,mapping=aes(x=long,y=lat,group=group,fill=Value),color="#aeaeae",lwd=0.25)+
  scale_fill_fermenter("TX_CURR",type="seq",
                     palette="Oranges",
                     direction=1,
                     aesthetics="fill",
                     na.value="#eaeaea",
                     breaks=rev(Breaks),
                     guide=guide_legend(),
                     labels=c(">45,538",
                              "15,298-45,538",
                              "8,440-15,398",
                              "4,882-8,440",
                              "\u2264 4,882"))+
  coord_equal()+
  theme_void()+
  theme(legend.position = "bottom",
        legend.direction="vertical",
        legend.justification="left",
        legend.title = element_text("Currently on Treatment"),
        plot.margin = margin(0,1,0,1, "cm"))
m

plot_grid(m,m,m,nrow=1,ncol=3)

#save the map to file
ggsave(m,"<your file path here>/<file name>.png")

