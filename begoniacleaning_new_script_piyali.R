#Cleaning Begonia data based on Padme specimen report on 25.10.2019.

library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(countrycode)
library(CoordinateCleaner)

setwd("C:/Users/Piyali/Desktop/New folder")

begonia.occurence<-read.csv("Specimen_report.csv")

head(begonia.occurence)
dim(begonia.occurence)

begonia.occurence<-begonia.occurence %>% dplyr::select(Accepted.name, Collector.name,Collection.number,Collection.date,Decimal.latitude,Decimal.longitude,Altitude..m.,minor.locality, second.locality,botanical.district,countries)
begonia.occurence<-begonia.occurence %>% filter(!is.na(Decimal.longitude))%>% filter(!is.na(Decimal.latitude))

names(begonia.occurence)
class(begonia.occurence)
head(begonia.occurence)
begonia.occurence <- begonia.occurence%>%
  filter(!is.na(Decimal.longitude))%>%
  filter(!is.na(Decimal.latitude))
#begonia.occurence <- na.omit(begonia.occurence) #This one removes the row if any of the fields are NA... removes too much data
dim(begonia.occurence)

#data cleaning

clean <-begonia.occurence%>%
  cc_val(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_equ(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_cap(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_cen(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  #  cc_coun(lon="Decimal.longitude",lat="Decimal.latitude", iso3 = "countryCode")%>%
  cc_gbif(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_inst(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_sea(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_zero(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  #  cc_outl(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_dupl(lon="Decimal.longitude",lat="Decimal.latitude",species="Accepted.name",additions = "Collection.number")
#?cc_dupl

write.csv(begonia.occurence,file="bego.occ.cleaned2.csv")
?write.csv

#####visualize data on the map
wm<- borders("world", colour = "gray50", fill = "gray50")

ggplot()+coord_fixed()+wm +geom_point(data = begonia.occurence, aes(x= Decimal.longitude, y=Decimal.latitude),
                  colour="darkred", size=0.5)+ theme_bw()

#####clean data
begonia.occurence<-data.frame(begonia.occurence)
flags<-clean_coordinates(x=begonia.occurence, lon = "Decimal.longitude", lat = "Decimal.latitude", species = "Accepted.name", tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                                                                                                                        "zeros","seas"))     
summary(flags)
plot(flags,lon="Decimal.longitude",lat="Decimal.latitude")

#Exclude problematic records
dat_cleaned <- begonia.occurence[flags$.summary,]

#The flagged records
dat_flagged <- begonia.occurence[!flags$.summary,]


#cordinate cleaner function
#to avoid specifying it in each function
names(begonia.occurence)[4:3] <- c("decimallongitude", "decimallatitude")
names(begonia.occurence)

clean <-begonia.occurence%>%
  cc_val(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_equ(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_cap(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_cen(lon="Decimal.longitude",lat="Decimal.latitude")%>%
#  cc_coun(lon="Decimal.longitude",lat="Decimal.latitude", iso3 = "countryCode")%>%
  cc_gbif(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_inst(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_sea(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_zero(lon="Decimal.longitude",lat="Decimal.latitude")%>%
#  cc_outl(lon="Decimal.longitude",lat="Decimal.latitude")%>%
  cc_dupl(lon="Decimal.longitude",lat="Decimal.latitude",species="Accepted.name",additions = "Collection.number")
?cc_dupl

#cleaned <- begonia.occurence %>% cc_val(lon="Decimal.longitude",lat="Decimal.latitude")%>% cc_cap() %>% cc_cen() %>% cc_dupl() %>% cc_equ() %>% 
 # cc_gbif() %>% cc_inst() %>% cc_outl() %>% cc_sea() %>% cc_zero()

#################################################################################
#################################################################################

begonia.occurence$ID<-1:dim(begonia.occurence)[1]

####points that are not valid (remove duplicates)
clean_dupl <- begonia.occurence %>%
  cc_dupl(lon="Decimal.longitude",lat="Decimal.latitude",species="Accepted.name",additions = "Collection.number")


dupl1<-begonia.occurence[begonia.occurence$ID%in%clean_dupl$ID,]
dim(dupl1) #no exclamation point

dupl<-begonia.occurence[!begonia.occurence$ID%in%clean_dupl$ID,]
dim(dupl) #with exclamation point

#write the table dupl to see the results
write.table(dupl,"duplicates2.csv")
unique(begonia.occurence)
?unique
dim(begonia.occurence)

####Equivalent lon & lat
clean_equ <- begonia.occurence%>%
  cc_equ(lon="Decimal.longitude",lat="Decimal.latitude")

equ1<-begonia.occurence[begonia.occurence$ID%in%clean_equ$ID,]
dim(equ1) #no exclamation point

equ<-begonia.occurence[!begonia.occurence$ID%in%clean_equ$ID,]
dim(equ) #with exclamation point

#pdf(file="potato_equ_flag.pdf")
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-180,20), ylim=c(-60,60), axes=TRUE, col="light yellow")
#points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
#points(equ$LONGDEC, equ$LATDEC, col="red", cex=0.75)
#dev.off()

#write.csv(equ,file="potato_equ_flag.csv")

####Points that fall near the gbif institution
clean_gbif <- begonia.occurence%>%
  cc_gbif(lon="Decimal.longitude",lat="Decimal.latitude",species="Accepted.name")

gbif1<-begonia.occurence[begonia.occurence$ID%in%clean_gbif$ID,]
dim(gbif1)#no exclamation mark

gbif<-begonia.occurence[!begonia.occurence$ID%in%clean_gbif$ID,]
dim(gbif)#with exclamation mark


#pdf(file="potato_gbif_flag.pdf")
#plot(wrld_simpl,  axes=TRUE, col="light yellow")
#points(gbif$decimallongitude, gbif$decimallatitude, col="red", cex=0.75)
#dev.off()

#write.csv(gbif,file="potato_gbif_flag.csv")

####Points that fall near gardens and other institutions
clean_inst <- begonia.occurence%>%
  cc_inst(lon="Decimal.longitude",lat="Decimal.latitude",species="Accepted.name")

institutes1<-begonia.occurence[begonia.occurence$ID%in%clean_inst$ID,]
dim(institutes1)#no exclamation mark

institutes<-begonia.occurence[!begonia.occurence$ID%in%clean_inst$ID,]
dim(institutes)#with exclamation mark

#pdf(file="potato_inst_flag.pdf")
#plot(wrld_simpl,  axes=TRUE, col="light yellow")
#points(inst$decimallongitude, inst$decimallatitude, col="red", cex=0.75)
#dev.off()

#write.csv(inst,file="flag_inst_begonia_AMERICAS.csv")


####Points that are zero longitude and zero latitude and a radium around the zero lon and zero lat
clean_zero <- begonia.occurence%>%
  cc_zero(lon="Decimal.longitude",lat="Decimal.latitude")

?cc_zero#help

zero1<-begonia.occurence[begonia.occurence$ID%in%clean_gbif$ID,]
dim(zero1)#no exclamation mark

zero<-begonia.occurence[!begonia.occurence$ID%in%clean_gbif$ID,]
dim(zero)#with exclamation mark

names(zero)

#pdf(file="potato_zero_flag.pdf")

#plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
#points(zero$Specimen.long.decimal, zero$Specimen.lat.decimal, col="red", cex=0.75)
#dev.off()
#write.csv(zero,file="flag_zero_begonia_AMERICAS.csv")

####Points that fall into centroids
clean_cen <- begonia.occurence%>%
  cc_cen(lon="Decimal.longitude",lat="Decimal.latitude")

cen1<-begonia.occurence[begonia.occurence$ID%in%clean_gbif$ID,]
dim(cen1)#no exclamation mark

cen<-begonia.occurence[!begonia.occurence$ID%in%clean_gbif$ID,]
dim(cen)#with exclamation mark

cen

#plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
#points(cen$decimallongitude, cen$decimallatitude, col="red", cex=0.75)

#getwd()
#write.csv(cen,file="flag_cen_begonia_AM.csv")

#will keep all 4

####Points that fall into capitals, within a 10 km radium from centroid
clean_cap <- begonia.occurence%>%
  cc_cap(lon="Decimal.longitude",lat="Decimal.latitude")

cap1<-begonia.occurence[begonia.occurence$ID%in%clean_cap1$ID,]
dim(cap1)#no exclamation mark

cap<-begonia.occurence[!begonia.occurence$ID%in%clean_gbif$ID,]
dim(cap)#with exclamation mark

#getwd()
#write.csv(cap,file="flag_cap_begonia_AMERICAS.csv")

#plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
#points(cap$decimallongitude, cap$decimallatitude, col="red", cex=0.75)

#dim(data)#16271 occurence records
#data <- data[ ! data$BRAHMS %in% remove.cap, ]
#dim(data)#16268 occurence records

##points that fall into sea
clean_sea<- begonia.occurence%>%
  cc_sea(lon="Decimal.longitude",lat="Decimal.latitude")
  
sea1<-begonia.occurence[begonia.occurence$ID%in%clean_sea$ID,]
dim(sea1)#no exclamation mark

sea<-begonia.occurence[!begonia.occurence$ID%in%clean_sea$ID,]
dim(sea)#with exclamation mark


#Final dataset without the values flagged in previous tests
clean_dupl[clean_gbif$ID%in%clean_dupl$ID,]->clean_cc
clean_equ[clean_gbif$ID%in%clean_equ$ID,]->clean_cc
clean_gbif[clean_gbif$ID%in%clean_gbif$ID,]->clean_cc
clean_inst[clean_inst$ID%in%clean_inst$ID,]->clean_cc
clean_zero[clean_zero$ID%in%clean_zero$ID,]->clean_cc
clean_cen[clean_cen$ID%in%clean_cen$ID,]->clean_cc
clean_cap[clean_cap$ID%in%clean_cap$ID,]->clean_cc
clean_sea[clean_sea$ID%in%clean_sea$ID,]->clean_cc

dim(clean_cc)
begonia.occurence[begonia.occurence$ID%in%clean_cc$ID,]->clean_cc2
dim(clean_cc2)

begonia.occurence<-clean_cc2

#dim(data.pushback)
#dim(data)

names(begonia.occurence)
head(begonia.occurence)
dim(begonia.occurence)

#############


#######Removing coordinates with wrong altitude
getwd()
### ---------------------
### Read in Altitude Data

require(raster)

#alt.W<-raster("C:/Users/Piyali/Desktop/New folder/clipped elevation borneo/clipped_eleva.ovr")
#plot(alt.W)#shows a map of the Neotropics.
alt.SE<-raster("E:/MSc/Project-msc/elevation/srtm_1km.asc")
#plot(alt.SE)#map of world elevation.

#specify min and max lat and longs of borneo to crop
minlon<-108.26
maxlon<-119.50
minlat<--5.02
maxlat<-7.67

e<-extent(c(minlon, maxlon, minlat, maxlat))
borneo.alt<-crop(alt.SE,e)
plot(bornea.alt)

?writeRaster
writeRaster(borneo.alt,filename="borneo.alt,asc")
plot(borneo.alt)


###you can use SRTM to get altitude, bioclime data directly to R. just goolge SRTM (https://www.gis-blog.com/r-raster-data-acquisition/) and specify the tiles exactly which you want.
srtm <- getData('SRTM', lon)
alt<-alt.W

#################

#REPEAT THE PROCESS, but for all data together.
data<-data.dup
data<-data.sec2

### Creating a new table which contains the information we want...
distribution_data <- begonia.occurence %>%
  dplyr::select(Accepted.name,Collection.number,Decimal.longitude,Decimal.latitude,countries,Altitude..m.) 


#getting the number of rows or occurences in my table
dims <- dim(distribution_data)[1]
#print(species[[x]])
print(dims)

if (-1>0)
{
print("TRUE")  
}


if(length(distribution_data[,1])>0) #if there is data in the table, do the next steps...
  {
  
  
  # This bit of code removes any row for which there is no gps coodinate
  for(y in length(distribution_data[,1]):1){
    #      for(y in 1){
    #y=1
    if(is.na(distribution_data[y,2])){distribution_data <- distribution_data[-y,]}
  }
  
  distribution_data[,6] <- gsub("[^0-9]","", distribution_data[,6]) # column corresponding to the altitude data; sometimes there are characters (m, f), so this makes sure that there are only numbers extracted
  distribution_data[,6] <- as.numeric(distribution_data[,6]) # making sure that it is numeric 
  #  distribution_data[,9] <- gsub("[^0-9]","", distribution_data[,9])
  #distribution_data[,9] <- as.numeric(distribution_data[,9])
  distribution_data[,7] <- raster::extract(borneo.alt, SpatialPoints(distribution_data[,3:4]))#long first, then lat #here we are extracting the correspond altitude on the map, based on xy coordinates of interest
  distribution_data[,8] <- NA
  distribution_data[,9] <- NA
  
  #Number of samples for which altitude data is NA from the map extraction...
  length(which(is.na(distribution_data[,7])))
  index.alt.na<-which(is.na(distribution_data[,7]))
  flag.alt.na<-distribution_data[index.alt.na,]
  
  write.csv(flag.alt.na,file="flag.alt.na.csv")#write the sample for which the map extraction resulted in NA
  getwd()
  
  

  distribution_data[,8]<-as.numeric(distribution_data[,8])

  
  for(y in 1:10)
  {
    print(y+1)
  }
  
  for(y in 1:length(distribution_data[,1])) # For each of the rows in the distribution_Data table, verifying original altitude versus expected/extracted altitude
  {
    
    if(!is.na(distribution_data[y,7])) # only runs if extracted altitude is not NA.
    {
      if(!is.na(distribution_data[y,6])) # makes sure that the original altitude is not NA... DOes not remove it, ignores it...
      {
        if(distribution_data[y,6] != 0) #original altitude is different from zero
        {
          distribution_data[y,8] <- distribution_data[y,6] - distribution_data[y,7] #6: altitude, 7: map altitude, stores the difference between the too in 8
          if(distribution_data[y,8]<0)
          {distribution_data[y,8] <- 0-as.numeric(distribution_data[y,8]) #if answer is negative, it switches it to positive...
          }
        }
      }
      if(is.na(distribution_data[y,6])){distribution_data[y,8] <- NA} # just making sure that when altitude is NA in column 6, NA is put into column 8
      
      #      if(!is.na(distribution_data[y,9]))
      #      {
      #        if(distribution_data[y,9] != 0)
      #        {
      #          distribution_data[y,14] <- distribution_data[y,9] - distribution_data[y,12]
      #          if(distribution_data[y,14]<0)
      #          {distribution_data[y,14] <- 0-as.numeric(distribution_data[y,14])
      #          }
      #        }
      #}
      #      distribution_data[y,12] <- max(distribution_data[y,13], distribution_data[y,14], na.rm=T)
    }
  }
  
  #  distribution_data <- distribution_data[,-14]
  
  index <- which(distribution_data[,8]>500) # find all rows for which the values in column 8 that are bigger than 500
  
  if(length(index)>0) #if we did find that index is bigger than zero, or that there are values greater than 500
    {distribution_data2 <- distribution_data[-index,]} #minus removes the rows
  
  #This part prints out the problematic taxa
  flags <- distribution_data[index,]
  #  pdf(paste(directory, "solanum_dataset_altitude.pdf", sep=""))
  
  plot(wrld_simpl,  xlim=c(round(min(distribution_data$Decimal.longitude)),round(max(distribution_data$Decimal.latitude))), ylim=c(round(min(distribution_data$Decimal.longitude)),round(max(distribution_data$Decimal.latitude))),axes=TRUE, col="light yellow")
  #     plot(wrld_simpl, axes=TRUE, col="light yellow")
  
  points(distribution_data$Specimen.long.decimal, distribution_data$Specimen.lat.decimal, col="black", cex=0.75)
  points(flags$Specimen.long.decimal, flags$Specimen.lat.decimal, col="red", cex=0.75)
  #dev.off()
}  
#distribution_data <- distribution_data[,-13]
#distribution_data <- distribution_data[,-12]

if(dim(distribution_data2)[1]<dims){writeLines(paste("The entire dataset now has", dims-length(distribution_data2[,1]),"fewer points..."))}
#write.csv(distribution_data, file=paste(directory,"solanum_dataset_altitude.csv", sep=""))
#write.csv(table(distribution_data$genus.sp),file=paste(directory,"solanum_dataset_altitude_TABLE.csv", sep=""))



#write.csv(table(data$COUNTRY),file=paste(directory,"ORIGINAL_TABLE_COUNTRY.csv", sep=""))
#write.csv(table(data$genus.sp),file=paste(directory,"ORIGINAL_TABLE_SPECIES.csv", sep=""))

dim(distribution_data2)

table(distribution_data2$Accepted.name)

table(distribution_data2[,6])#Still are 2144 -Inf.....
hist(distribution_data2[,6])# shows map of altitude data retained in file...

#which(is.infinite(distribution_data2[,8]))->index2      
#distribution_data[index2,]->inf.probs
#head(inf.probs)
#dim(inf.probs)


#WRITE THE problematic taxa for altitude in the flag object
write.csv(flags,"flags_alt_BEG_BORNEO.csv")

#WRITE The reduced dataset, without the problematic taxa for altitude
write.csv(distribution_data2,"Borneo_altitude_cleaned.csv")



# To remove columns and just keep the ones you want
new_borneo_data <- flags_alt_BEG_BORNEO %>%
  dplyr::select(-Decimal.longitude, -Decimal.latitude)

colnames(new_borneo_data)
colnames(Borneo_altitude_cleaned)



colnames(new_borneo_data) <- c("X1", "Accepted.name",
                               "Collection.number", "countries",
                               "Altitude..m.", "V7",
                               "V8", "V9", "Decimal.longitude",
                               "Decimal.latitude", "X13")

new_borneo_data <- new_borneo_data %>% dplyr::select("X1", "Accepted.name",
                                                     "Collection.number","Decimal.longitude",
                                                     "Decimal.latitude", "countries",         
                                                     "Altitude..m.", "V7",
                                                     "V8", "V9")
# To combine two data frames (the one with the correct and the one with the updated coordinates)
new_borneo_data$Decimal.longitude <- as.character(new_borneo_data$Decimal.longitude)
Borneo_altitude_cleaned$Decimal.longitude <- as.character(Borneo_altitude_cleaned$Decimal.longitude)
Borneo_altitude_cleaned$Decimal.latitude <- as.character(Borneo_altitude_cleaned$Decimal.latitude)

all_data <- bind_rows(Borneo_altitude_cleaned, new_borneo_data)
view(all_data)  
  
  
