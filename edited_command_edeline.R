#Begonia dataset:
#Specimen lat long: GPS data that was on labels
#Geog lat long: GPS data that is pulled out of a Gazeteer.

#According to conversation with Mark Hughes, the gazeteer is not very precise for altitude...



#From the Padme extraction of Begonia occurence dataset, I want to 
#1) Import the dataset
library(CoordinateCleaner, countrycode, dismo, rgbif)

setwd("C:/Users/Piyali/Desktop/New folder")
getwd()
dir()
begonia.occurence<-read.csv("begonia_borneo_all.csv")

dim(begonia.occurence)
names(begonia.occurence)

#2) If there is a Specimen Lat decimal, and Specimen Long decimal, then keep the data, tag a Specimen Column data to keep track of this.
table(is.na(begonia.occurence$specimen.lat.decimal))
#table(is.na(begonia.occurence$Geog.lat.decimal))

#This creates the tag
#index.spec.beg<-is.na(begonia.occurence$specimen.lat.decimal)
#begonia.occurence$tag.spe<-as.numeric(!index.spec.beg)
#head(index.spec.beg)
#head(begonia.occurence$tag.spe)

#subset of the data where specimen data is present

#dim(begonia.occurence[!index.spec.beg,])
#toto1<-begonia.occurence[!index.spec.beg,]
#toto2<-begonia.occurence[index.spec.beg,]

#3) If there is no Specimen Lat Long decimal, then copy in the Geog data to keep a track of it.

#table(is.na(toto2$Geog.lat.decimal)) # so there is 3927 additional specimens that could be of use here in our analyses.
library(maptools)
data(wrld_simpl)

LONGDEC<-begonia.occurence$specimen.long.decimal
LATDEC<-begonia.occurence$specimen.lat.decimal
#Map it

plot(wrld_simpl,  xlim=c(round(min(LONGDEC)),round(max(LONGDEC))), ylim=c(round(min(LATDEC)),round(max(LATDEC))),axes=TRUE, col="light yellow")
points(LONGDEC, LATDEC, col="blue", cex=0.5)
#points(data.sec2$specimen.long.decimal, data.sec2$specimen.lat.decimal,col="red", cex=0.5)
#Save it
#write.csv(data,"Begonia_extraction_29101_07May2019.csv")

#Clean it.



######################################################
#Step 1: Check for wrong country
###################################################
library(dismo)

#POSSIBLY ADAPT FOR SABAH REGIONS??
# Set working directory

#dir.create("C://Users/egagnon/Documents/OneDrive/Projets_Recherche/2018_Solanum_phylogeny/Solanum_phylogeny/potato_maps/countries/")
#directory<-("C://Users/egagnon/Documents/OneDrive/Projets_Recherche/2018_Solanum_phylogeny/Solanum_phylogeny/potato_maps/countries/")
#setwd(directory)
getwd()


#In column COUNTRY, rename United States of America, to United States

sub("United States of America","United States",data$COUNTRY)->new
table(new)
data$Botanical.country->data$COUNTRY
new->data$COUNTRY
table(data$COUNTRY)

# Method one: use hijmans over function, as show in :
getData("countries")->world

data->data2
dim(data2)
as.numeric(data2$Specim)->data2$LONGDEC
as.numeric(data2$LATDEC)->data2$LATDEC
coordinates(data2)<-~LONGDEC+LATDEC
crs(data2)<-crs(world)
class(data2)

ovr <- over(data2, world)
head(ovr)
colnames(ovr)

cntr<-ovr$NAME_ENGLISH


j <-which((cntr) != as.vector(data2$COUNTRY))
j

length(j)#gives number of coordinaes which don't fall in the right country
#164

cbind(cntr, data2$COUNTRY)

cbind(cntr, data)[j,]->probs_j
table(probs_j$genus.sp)
table(probs_j$COUNTRY)

dim(probs_j)
#[1] 164 174
probs_j->flags.countries

#visualize
plot(wrld_simpl)
points(flags.countries$LONGDEC,flags.countries$LATDEC,col="red",pch=20,cex=0.75)

write.csv(flags.countries,"flags_ddmm_countries.csv")


#remove this data
data[-j,]->toto
dim(toto)
toto->data




##################
#Step 2: Sea Removal + Pushback script
##################

############################################################################################################################
# 1) Load packages 
############################################################################################################################

#install.packages("maptools")
library(maptools)
library(raster)
library(sp)

#data$genus.sp <- paste(data$GENUS,"_",data$SP1, sep="")

names(data)
data.coord <- data %>%
  dplyr::select(X, specimen.lat.decimal, specimen.long.decimal,accepted_name, specimen_name) 
names(data)


#examine the results
class(data.coord)
dim(data.coord)
head(data.coord)



#create an object of class "SpatialPoints" with the geographic coordinates of the specimens
data.coord.spatial <- SpatialPoints(begonia.occurence[,3:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
class(data.coord.spatial)

#####data.coord.spatial<-SpatialPoints(begonia.occurence[,3.2],proj4string = CRS("+proj=longlat+ datum=WGS84+ no_defs"))


############################################################################################################################
# 4) Read raster mask that indicates the grid cells that have climate data. The raster has a 30 arc secods resolution,
# or 0.008333334 X 0.008333334 degrees.
############################################################################################################################

setwd("C:/Users/egagnon/Documents/MODIS+CHIRPS bioclim data 3degree resolution/MOD11C3-CHIRPS_BIOCLIMS_03m")
wd <- getwd()
files <- list.files(path=wd, pattern='.asc', full.names=TRUE)
list(files)
#predictors <- stack(files)
#predictors
predictors.bio1<-raster(files[[1]])

############################################################################################################################
# 5) Determine which specimen records (that have geographic coordinates and have been determined to species) fall outside
# the mask for the climate data 
############################################################################################################################

#plot the mask
plot(predictors.bio1, useRaster=T, legend=F)
plot(data.coord.spatial, add=T, pch=19, cex=0.2, col="red")
axis(1)
mtext(side=1, "Longitude (degrees)", cex=1.5, line=3)
axis(2)
mtext(side=2, "Latitude (degrees)", cex=1.5, line=3)

#extract the values of the mask at the coordiantes of the specimen records
data.coord.land <- extract(predictors.bio1, data.coord[,3:2], method='simple')
class(data.coord.land)
summary(data.coord.land)
sum(is.na(data.coord.land)) #number of specimens falling outside the mask
sum(!is.na(data.coord.land)) #number of specimens falling inside the mask

#plot the mask and specimens that fall off the mask
plot(predictors.bio1, col="gray90", useRaster=T, legend=F)
plot(data.coord.spatial[which(is.na(data.coord.land))], add=T, pch=19, cex=0.2, col="red")
points(data.coord[which(is.na(data.coord.land)),3:2], pch=19, cex=0.5, col="red")
mtext(side=1, "Longitude (degrees)", cex=1.5, line=3)
mtext(side=2, "Latitude (degrees)", cex=1.5, line=3)


############################################################################################################################
# 6) Assign new coordinates to specimen records that fall < 2,000 m  from the mask for the climate data. The new coordinates
# are those of the center of the nearest grid cell with climate data.
############################################################################################################################

#obtain coordinates for the center of all grid cells in the mask with no NA values;
#first obtain cell number and values for all grid cells
data.mask.0.cell.values <- extract(predictors.bio1, coordinates(predictors.bio1), cellnumbers=T)
head(data.mask.0.cell.values)
#now obtain the coordinates for all grid cells with no NA values and plot them
data.mask.0.CoorNotNA <- xyFromCell(predictors.bio1, data.mask.0.cell.values[!is.na(data.mask.0.cell.values[,2]),1])
class(data.mask.0.CoorNotNA)
dim(data.mask.0.CoorNotNA)
head(data.mask.0.CoorNotNA)
#plot(predictors.bio1, col="gray70", useRaster=T, legend=F)
#plot(wrld_simpl,  axes=TRUE, col="light yellow")
#points(data.mask.0.CoorNotNA, pch=19, cex=0.01, col="blue")


#create a matrix that will hold the new (altered) coordiantes for the specimens that fall outside the mask,
#these new coordinate are those of the center of the nearest grid cell; the matrix also has a column for
#the distance (in meters) between the original coordinates and the new (altered) coordinates.
altered.coor <- matrix(NA, nrow=length(which(is.na(data.coord.land))), ncol=3)
colnames(altered.coor) <- c("AlteredLongitudeDecimal", "AlteredLatitudeDecimal", "DistFromOriginalCoorMeters")
dim(altered.coor)


#run a loop to assign the coordinates of the closest grid cell to each specimen record falling outside the mask,
#and to measure the distance between the original and the new (altered) coordinates
for(i in 1:length(which(is.na(data.coord.land))))
{
  all.dist <- pointDistance(data.mask.0.CoorNotNA, (data.coord[which(is.na(data.coord.land)),3:2])[i,], lonlat=T)
  altered.coor[i,3] <- min(all.dist)
  altered.coor[i,1:2] <- data.mask.0.CoorNotNA[which(all.dist<=min(all.dist)),]
}

#examine the results
summary(altered.coor)
dim(altered.coor)
head(altered.coor)
hist(altered.coor[,3], breaks=100)
sum(altered.coor[,3]<2000)
sum(altered.coor[,3]>=2000)

#plot the mask, the specimens that fall within 2,000 meters from the mask,
#and the altered coordinates for those specimens
#par(mar=c(5, 4, 4, 2)+0.1) #default
par(mar=c(5, 5, 4, 0.01))
#plot(predictors.bio1, col="gray90", useRaster=T, legend=F, cex.axis=1.5)
plot(wrld_simpl,  axes=TRUE, col="light yellow")
#plot(sr.Nicaragua.coord.spatial[which(is.na(sr.Nicaragua.coord.land))], add=T, pch=19, cex=0.2, col="red")
points((data.coord[which(is.na(data.coord.land)),3:2])[altered.coor[,3]<2000,], pch=19, cex=0.5, col="red")
points(altered.coor[altered.coor[,3]<2000,1:2], pch=21, cex=0.2, col="blue")
mtext(side=1, "Longitude (degrees)", cex=1.5, line=3.5)
mtext(side=2, "Latitude (degrees)", cex=1.5, line=3.5)
mtext(side=3, "Collection localities > 0 m  and < 2,000 m from mask (red),", cex=1.5, line=2)
mtext(side=3, "and respective altered coordinates (blue)", cex=1.5, line=0.7)

#plot the mask, and the specimens that fall at least 2,000 meters from the mask
#par(mar=c(5, 4, 4, 2)+0.1) #default
par(mar=c(5, 5, 4, 0.01))
#plot the mask and specimens that fall off the mask
#plot(predictors.bio1, col="gray90", useRaster=T, legend=F, cex.axis=1.5)
plot(wrld_simpl,  axes=TRUE, col="light yellow")

#plot(sr.Nicaragua.coord.spatial[which(is.na(sr.Nicaragua.coord.land))], add=T, pch=19, cex=0.2, col="red")
points((data.coord[which(is.na(data.coord.land)),3:2])[altered.coor[,3]>=2000,], pch=19, cex=0.7, col="red")
mtext(side=1, "Longitude (degrees)", cex=1.5, line=3.5)
mtext(side=2, "Latitude (degrees)", cex=1.5, line=3.5)
#mtext(side=3, "Collection localities >= 2,000 m from mask (red)", cex=1.5, line=2)
mtext(side=3, expression(paste("Collection localities", phantom(0)>=phantom(0) , "2,000 m from mask (red)", sep="")), cex=1.5, line=1.5)

############################################################################################################################
# 7) Create a data frame with the specimen records that fall outside the mask, their new coordinates and
# the distance in meters between the origninal and the new (altered) coordinates.
############################################################################################################################

#specimen.records.to.edit <- data.frame(data.coord[which(is.na(data.coord.land)),], altered.coor)
flags.pushback<- data.frame(data[which(is.na(data.coord.land)),], altered.coor)

dim(flags.pushback)
head(flags.pushback)
#table(flags.pushback$genus.sp)
#table(flags.pushback$COUNTRY)

#setwd("C://Users/egagnon/Documents/OneDrive/Projets_Recherche/2018_Solanum_phylogeny/Solanum_phylogeny")
write.csv(flags.pushback, file="flags_pushback_new.csv")
# examine that latter...

data.pushback<- data[!data$ID %in% flags.pushback$ID,]
data<-data.pushback
dim(data)
#colnames(specimen.records.to.edit)[4] <-  "CollectionNumberNumeric"
#colnames(specimen.records.to.edit)[5] <- "SeniorCollectorPersonID"




###################################################################################
#Step 3. Coordinate_Cleaner section
###################################################################################

# setup libraries and data
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(countrycode)
library(CoordinateCleaner)



data<-table.begonia
names(data)
#select columns of interest
data3 <- data %>%
  dplyr::select(specimen.name, Specimen.lat.decimal, Specimen.long.decimal, Botanical.country, BRU,
                ID, Date.1.Years, Specimen.alt, Current.accepted.det, collector, Collector.Number) 


data3 <- data %>%
  dplyr::select(genus.sp, lat, long, country, brahms) 

# remove records without coordinates

dim(data3)
data3 <- data3%>%
  filter(!is.na(Specimen.long.decimal))%>%
  filter(!is.na(Specimen.lat.decimal))


data3 <- data3%>%
  filter(!is.na(lat))%>%
  filter(!is.na(long))

dim(data3)


##plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = data3, aes(x = long, y = lat),
             colour = "darkred", size = 0.5)+
  theme_bw()

##plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = data3, aes(x = Specimen.long.decimal, y = Specimen.lat.decimal),
             colour = "darkred", size = 0.5)+
  theme_bw()


#convert country code from ISO2c to ISO3c

data3$ISO3C <-  countrycode(data3$COUNTRY, origin =  'country.name', destination = 'iso3c')

#to avoid specifying it in each function
names(data3)[2:3] <- c("decimallatitude", "decimallongitude")






####points that are not valid
clean_val <- data3 %>%
  cc_val()

data[!data3$BRAHMS%in%clean_val$BRAHMS,]->val
dim(val)


####Equivalent lon & lat
clean_equ <- data3%>%
  cc_equ()

data[!data3$ID%in%clean_equ$ID,]->equ
dim(equ)


#pdf(file="potato_equ_flag.pdf")

#plot(wrld_simpl, xlim=c(-180,-20), ylim=c(-60,60), axes=TRUE, col="light yellow")
#points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
#points(equ$LONGDEC, equ$LATDEC, col="red", cex=0.75)
#dev.off()

#write.csv(equ,file="potato_equ_flag.csv")

####Points that fall near the gbif institution
clean_gbif <- data3%>%
  cc_gbif()

data[!data3$ID%in%clean_gbif$ID,]->gbif
dim(gbif)


#pdf(file="potato_gbif_flag.pdf")
#plot(wrld_simpl,  axes=TRUE, col="light yellow")
#points(gbif$decimallongitude, gbif$decimallatitude, col="red", cex=0.75)
#dev.off()

#write.csv(gbif,file="potato_gbif_flag.csv")

####Points that fall near gardens and other institutions
clean_inst <- data3%>%
  cc_inst()

data[!data3$ID%in%clean_inst$ID,]->inst
dim(inst)

#pdf(file="potato_inst_flag.pdf")
#plot(wrld_simpl,  axes=TRUE, col="light yellow")
#points(inst$decimallongitude, inst$decimallatitude, col="red", cex=0.75)
#dev.off()

write.csv(inst,file="flag_inst_begonia_AMERICAS.csv")


####Points that are zero longitude and zero latitude and a radium around the zero lon and zero lat
clean_zero <- data3%>%
  cc_zero()

data[!data3$ID%in%clean_zero$ID,]->zero
dim(zero)

names(zero)

#pdf(file="potato_zero_flag.pdf")

plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(zero$Specimen.long.decimal, zero$Specimen.lat.decimal, col="red", cex=0.75)
#dev.off()
write.csv(zero,file="flag_zero_begonia_AMERICAS.csv")



#Final dataset without the values flagged in previous tests

clean_val[clean_val$ID%in%clean_equ$ID,]->clean_cc
clean_cc[clean_cc$ID%in%clean_gbif$ID,]->clean_cc
clean_cc[clean_cc$ID%in%clean_inst$ID,]->clean_cc
clean_cc[clean_cc$ID%in%clean_zero$ID,]->clean_cc

dim(clean_cc)
data[data3$ID%in%clean_cc$ID,]->clean_cc2
dim(clean_cc2)

data<-clean_cc2

dim(data.pushback)
dim(data)

names(data3)
head(data3)
dim(data3)

#############
####Points that fall into centroids
clean_cen <- data3%>%
  cc_cen()

data3[!data3$ID%in%clean_cen$ID,]->cen
dim(cen)
cen

plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(cen$decimallongitude, cen$decimallatitude, col="red", cex=0.75)

getwd()
write.csv(cen,file="flag_cen_begonia_AM.csv")

#will keep all 4

####Points that fall into capitals, within a 10 km radium from centroid
clean_cap <- data3%>%
  cc_cap()


data3[!data3$ID%in%clean_cap$ID,]->cap
dim(cap)
getwd()
write.csv(cap,file="flag_cap_begonia_AMERICAS.csv")

plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(cap$decimallongitude, cap$decimallatitude, col="red", cex=0.75)


#No longer needed: remove.cap<-c(82665,77260,85273,122387,122388,122393,122394,122395,122396,122397,122398,122399,122425,122426,122427,122459,122460,122462,122463,122464,122465,122466,122467,122468,122469,122470,122472,122896,122898,122899)
# I don't think that any of hese represent problems
#dim(data)#16271 occurence records
#data <- data[ ! data$BRAHMS %in% remove.cap, ]
#dim(data)#16268 occurence records

######### removing duplicates

clean_dupl <- data3%>%
  cc_dupl(.,species="Current.accepted.det",additions = c("collector","Collector.Number"))

data[!data$ID%in%clean_dupl$ID,]->dupl

head(dupl)
dim(dupl)
flags.dupl<-dupl
dim(data)

write.csv(flags.dupl,"flags_dupl.csv")

toto<-data
toto[toto$ID%in%clean_dupl$ID,]->toto
dim(toto)

dim(data)-dim(toto)
data->backup
toto->data.dup
toto->data
#Here I just remove the duplicates (a shit load of them!!) and then keep centroids and cap for now until I can doublecheck if they are bad data or not.

dim(data)



####################################################################
#Removing coordinates with wrong altitude

setwd("C://Users/egagnon/Documents/OneDrive/Projets_Recherche/2019_Tuber_project/Input/Mark_Begonia_Occurence_data/")
getwd()
### ---------------------
### Read in Altitude Data

require(raster)

alt.W<-raster("Q:/World/elevation_250m_world/SRTM_W_250m.tif")
plot(alt.W)#shows a map of the Neotropics.
alt.SE<-raster("Q:/World/elevation_250m_world/SRTM_SE_250m.tif")
plot(alt.SE)#map of southern Africa and Australia.

alt.W.SE <- mosaic(alt.W, alt.SE, fun=mean)

alt.W.SE<-
  
  # Eventually I want a global map, but for now, I'll just use alt.W
  srtm <- getData('SRTM', lon)
alt<-alt.W

#################

#REPEAT THE PROCESS, but for all data together.
data<-data.dup
data<-data.sec2

distribution_data <- data %>%
  dplyr::select(genus.sp, LATDEC, LONGDEC, COUNTRY, LATLONG,
                BRAHMS, YEAR, ALT, ALTMAX,COLLECTOR, NUMBER) 

distribution_data <- data %>%
  dplyr::select(genus.sp, Specimen.lat.decimal, Specimen.long.decimal, Botanical.country, COUNTRY, Geog.hierarchy.1,
                ID, Specimen.alt, collector, Collector.Number) 



#silence the next line if you just want a table with all the species that should be excluded.
#distribution_data[distribution_data$genus.sp%in%species[[x]],]->distribution_data


dims <- dim(distribution_data)[1]
#print(species[[x]])
print(dims)

if(length(distribution_data[,1])>0){
  
  # This bit of code removes any cide for which there is no gps coodinate
  for(y in length(distribution_data[,1]):1){
    #      for(y in 1){
    #y=1
    if(is.na(distribution_data[y,2])){distribution_data <- distribution_data[-y,]}
  }
  
  distribution_data[,8] <- gsub("[^0-9]","", distribution_data[,8])
  distribution_data[,8] <- as.numeric(distribution_data[,8])
  #  distribution_data[,9] <- gsub("[^0-9]","", distribution_data[,9])
  #distribution_data[,9] <- as.numeric(distribution_data[,9])
  distribution_data[,11] <- raster::extract(alt, SpatialPoints(distribution_data[,3:2]))#long first, then lat
  distribution_data[,12] <- NA
  distribution_data[,13] <- NA
  
  #Number of samples for which altitude data is NA...  
  length(which(is.na(distribution_data[,11])))
  index.alt.na<-which(is.na(distribution_data[,11]))
  flag.alt.na<-distribution_data[index.alt.na,]
  
  write.csv(flag.alt.na,file="flag.alt.na.csv")
  getwd()
  
  
  
  distribution_data[,12]<-as.numeric(distribution_data[,12])
  
  for(y in 1:length(distribution_data[,1]))
  {
    
    if(!is.na(distribution_data[y,11]))
    {
      if(!is.na(distribution_data[y,8]))
      {
        if(distribution_data[y,8] != 0)
        {
          distribution_data[y,12] <- distribution_data[y,8] - distribution_data[y,11]
          if(distribution_data[y,12]<0)
          {distribution_data[y,12] <- 0-as.numeric(distribution_data[y,12])
          }
        }
      }
      if(is.na(distribution_data[y,8])){distribution_data[y,12] <- NA}
      
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
  
  index <- which(distribution_data[,12]>500)
  
  if(length(index)>0){distribution_data <- distribution_data[-index,]}
  
  flags <- distribution_data[index,]
  #  pdf(paste(directory, "solanum_dataset_altitude.pdf", sep=""))
  
  plot(wrld_simpl,  xlim=c(round(min(distribution_data$Specimen.long.decimal)),round(max(distribution_data$Specimen.lat.decimal))), ylim=c(round(min(distribution_data$Specimen.long.decimal)),round(max(distribution_data$Specimen.lat.decimal))),axes=TRUE, col="light yellow")
  #     plot(wrld_simpl, axes=TRUE, col="light yellow")
  
  points(distribution_data$Specimen.long.decimal, distribution_data$Specimen.lat.decimal, col="black", cex=0.75)
  points(flags$Specimen.long.decimal, flags$Specimen.lat.decimal, col="red", cex=0.75)
  #dev.off()
}  
#distribution_data <- distribution_data[,-13]
#distribution_data <- distribution_data[,-12]

if(dim(distribution_data)[1]<dims){writeLines(paste("The entire dataset now has", dims-length(distribution_data[,1]),"fewer points..."))}
#write.csv(distribution_data, file=paste(directory,"solanum_dataset_altitude.csv", sep=""))
#write.csv(table(distribution_data$genus.sp),file=paste(directory,"solanum_dataset_altitude_TABLE.csv", sep=""))



#write.csv(table(data$COUNTRY),file=paste(directory,"ORIGINAL_TABLE_COUNTRY.csv", sep=""))
#write.csv(table(data$genus.sp),file=paste(directory,"ORIGINAL_TABLE_SPECIES.csv", sep=""))

dim(distribution_data)

table(distribution_data$genus.sp)

table(distribution_data[,12])#Still are 2144 -Inf.....

which(is.infinite(distribution_data[,12]))->index2      
distribution_data[index2,]->inf.probs
head(inf.probs)
dim(inf.probs)

data[data$ID%in%distribution_data$ID,]->data.alt
data[!data$ID%in%distribution_data$ID,]->flags.alt

write.csv(flags.alt,"flags_alt_BEG_Americas.csv")
data.alt->data

names(data)
dim(data)
head(data)
################################
#Saving final file
write.csv(data,"Begonia_occurence_Americas_9129.csv")
