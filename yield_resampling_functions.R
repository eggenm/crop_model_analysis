library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(soiltexture)

#################    FUNCTIONS   ##################################


#Use this function for a shortcut when using 2 sets of rainfall categories
get_resampled_data<-function(high_rainfall_data, low_rainfall_data, this_zone){
  high_rainfall_data<-high_rainfall_data%>%filter(zone==this_zone)
  low_rainfall_data<-low_rainfall_data%>%filter(zone==this_zone)
  
  temp1<-get_resample_means(high_rainfall_data, this_zone)
  temp2<-get_resample_means(low_rainfall_data, this_zone)
  combined_data<-rbind(temp1,  temp2)
  return(combined_data)
}


#Use this function to resample a single cut of one zone of data
get_resample_means=function(mydata, zone){
  means<-NULL
  j=1
  n<-600
  textures<-c('Lo', 'SaClLo', 'ClLo', 'Lo', 'SaClLo', 'ClLo')
  rows<-3600
  if(zone=='Senegal oriental'){
    textures<-c('SaLo', 'SaClLo', 'ClLo','SaLo', 'SaClLo', 'ClLo')
  }else if(zone=='Bassin arachidier'){
    textures<-c('SaLo', 'SaClLo', 'LoSa', 'SaLo', 'SaClLo', 'LoSa')
  }else if(zone == 'Zone sylvopastorale'){#ZS only has 2 significant textures in this data set
    n<-400
    rows<-2400
    textures<-c('SaLo', 'SaClLo', 'SaLo', 'SaClLo', 'SaLo', 'SaClLo')
  }
  means <- data.frame(matrix(ncol = 4, nrow = rows))
  #provide column names
  colnames(means) <- c('yield', 'texture','Rainfall',  'site' )
  
  for( x in list(
    mydata%>%filter(texture_class==textures[1] ),
    mydata%>%filter(texture_class==textures[2] ),
    mydata%>%filter(texture_class==textures[3] ),
    mydata%>%filter(texture_class==textures[4] ),
    mydata%>%filter(texture_class==textures[5] ),
    mydata%>%filter(texture_class==textures[6] )
  )
  ){
    for(i in 1:n){
      means$yield[j] = mean(sample(x$yield, 30, replace = FALSE, prob = NULL))#Take the mean of a set of 30 yield observations
      means$texture[j] = x$texture_class[1]
      means$Rainfall[j] = x$Rainfall[1]
      means$site  = x$zone[1]
      j=j+1
    }
  }
  return(means)
}


#########################################################
