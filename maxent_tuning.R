setwd("C:/Users/Piyali/Desktop/msc_paper/R_SDM")
library(raster)
library(dismo)
library(rgdal)
library(rJava)
library(MaxentVariableSelection)

#set bioclim variables
R1<-raster("1.tif")
R2<-raster("2.tif")
R3<-raster("3.tif")
R4<-raster("4.tif")
R5<-raster("5.tif")
R6<-raster("6.tif")
R7<-raster("7.tif")
R8<-raster("8.tif")
R9<-raster("9.tif")
R10<-raster("10.tif")
R11<-raster("11.tif")
R12<-raster("12.tif")
R13<-raster("13.tif")
R14<-raster("14.tif")
R15<-raster("15.tif")
R16<-raster("16.tif")
R17<-raster("17.tif")
R18<-raster("18.tif")
R19<-raster("19.tif")

#set no data values to zero
R1[R1==0]<-NA
R2[R2==0]<-NA
R3[R3==0]<-NA
R4[R4==0]<-NA
R5[R5==0]<-NA
R6[R6==0]<-NA
R7[R7==0]<-NA
R8[R8==0]<-NA
R9[R9==0]<-NA
R10[R10==0]<-NA
R11[R11==0]<-NA
R12[R12==0]<-NA
R13[R13==0]<-NA
R14[R14==0]<-NA
R15[R15==0]<-NA
R16[R16==0]<-NA
R17[R17==0]<-NA
R18[R18==0]<-NA
R19[R19==0]<-NA

#make bioclimate variables a stack
env<-stack(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17,R18,R19)

#read occurrence data
occdata<-read.csv("B_calcarea_data.csv")
occ_data<- occdata[,c("longitude","latitude")]

#extract bioclim variables in each occurrences
bioclim_values<- extract(env,occ_data)
data<-data.frame(bioclim_values)

#set background points
bg_locations<-randomPoints(env,1000)
bg_bio_clim<-extract(env,bg_locations)


#changing variables with MaxentVariableSelection

occ_full<-cbind(occ_data,bioclim_values)
write.csv(occ_full,"C:/Users/Piyali/Desktop/msc_paper/R_SDM/occ_full.csv",row.names = FALSE)


bg_full<-cbind(bg_locations,bg_bio_clim)
write.csv(bg_full,"C:/Users/Piyali/Desktop/msc_paper/R_SDM/bg_full.csv",row.names = FALSE)

MaxentVariableSelection::VariableSelection(maxent="c:/Users/Piyali/Downloads/Software/maxent/maxent.jar", 
                                           outdir="/Users/Piyali/Desktop/msc_paper/R_SDM/outputtest", 
                                           gridfolder="/Users/Piyali/Desktop/msc_paper/R_SDM/30sclippedascii",
                                           occurrencelocations="/Users/Piyali/Desktop/msc_paper/R_SDM/occ_full.csv",
                                           backgroundlocations="/Users/Piyali/Desktop/msc_paper/R_SDM/bg_full.csv",
                                           additionalargs="additionalargs",
                                           contributionthreshold=5, 
                                           correlationthreshold=0.9,
                                           betamultiplier=seq(2,6,0.5))







> `    > 



> 

