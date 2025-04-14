#
##
### This file cleans and reduces the DATRAS Exchange data products in prepartion for
### calculating stock biological parameters (e.g. maturity and weight at age).
### This file assumes the exchange data have already been downloaded using an external script.
#### Author: Elliot J. Brown (modified from Margit Eero)
##
#

# rm(list = ls())

#===
# Dependencies ----
#===
library(sf)

assYear <- as.integer(format(Sys.Date(), "%Y"))
readPath <- paste0("../", assYear, "/Surveys/")
savePath <- paste0("DataProcessed/Surveys/")

## Read in ICES area / subdivision layers
SDs <- st_read(dsn = "DataIn/ICES_areas/", layer = "ICES_Areas_20160601_cut_dense_3857")

#====

# #===
# # OPTIONAL: Update datras data by re-downloading - TAKES LONG TIME ----
# #===
# source(file = "WKBPLAICE 2024/Data/Surveys/01_DatrasDownloads.R")
# #====

if((file.exists("DataProcessed/Surveys/Ple.27.21-32_ca-hh.RDS") & file.exists("DataProcessed/Surveys/Ple.27.21-32_hl-hh.RDS"))){
  ca_hh_fin_bits_ibts <- readRDS(file = "DataProcessed/Surveys/Ple.27.21-32_ca-hh.RDS")
  hl_hh_fin_bits_ibts <- readRDS(file = "DataProcessed/Surveys/Ple.27.21-32_ca-hh.RDS")
}else{
  #===
  # Read in Data ----
  #===
  # Read in Datras files
  hh_ibts <- read.csv(list.files(path = readPath,
                                 pattern = "IBTS_HH_1965_AssYear_",
                                 full.names = TRUE)[which.max(as.Date(sub("IBTS_HH_1965_AssYear_(\\d{4}-\\d{2}-\\d{2}).csv", "\\1",
                                                                          basename(list.files(path = readPath,
                                                                                              pattern = "IBTS_HH_1965_AssYear_",
                                                                                              full.names = TRUE)))))],
                      stringsAsFactors=F)
  ca_ibts <- read.csv(list.files(path = readPath,
                                 pattern = "IBTS_CA_1965_AssYear_",
                                 full.names = TRUE)[which.max(as.Date(sub("IBTS_CA_1965_AssYear_(\\d{4}-\\d{2}-\\d{2}).csv", "\\1",
                                                                          basename(list.files(path = readPath,
                                                                                              pattern = "IBTS_CA_1965_AssYear_",
                                                                                              full.names = TRUE)))))],
                      stringsAsFactors=F)
  hl_ibts <- read.csv(list.files(path = readPath,
                                 pattern = "IBTS_HL_1965_AssYear_",
                                 full.names = TRUE)[which.max(as.Date(sub("IBTS_HL_1965_AssYear_(\\d{4}-\\d{2}-\\d{2}).csv", "\\1",
                                                                          basename(list.files(path = readPath,
                                                                                              pattern = "IBTS_HL_1965_AssYear_",
                                                                                              full.names = TRUE)))))],
                      stringsAsFactors=F)
  
  hh_bits <- read.csv(list.files(path = readPath,
                                 pattern = "BITS_HH_1991_AssYear_",
                                 full.names = TRUE)[which.max(as.Date(sub("BITS_HH_1991_AssYear_(\\d{4}-\\d{2}-\\d{2}).csv", "\\1",
                                                                          basename(list.files(path = readPath,
                                                                                              pattern = "BITS_HH_1991_AssYear_",
                                                                                              full.names = TRUE)))))],
                      stringsAsFactors=F)
  ca_bits <- read.csv(list.files(path = readPath,
                                 pattern = "BITS_CA_1991_AssYear_",
                                 full.names = TRUE)[which.max(as.Date(sub("BITS_CA_1991_AssYear_(\\d{4}-\\d{2}-\\d{2}).csv", "\\1",
                                                                          basename(list.files(path = readPath,
                                                                                              pattern = "BITS_CA_1991_AssYear_",
                                                                                              full.names = TRUE)))))],
                      stringsAsFactors=F)
  hl_bits <- read.csv(list.files(path = readPath,
                                 pattern = "BITS_HL_1991_AssYear_",
                                 full.names = TRUE)[which.max(as.Date(sub("BITS_HL_1991_AssYear_(\\d{4}-\\d{2}-\\d{2}).csv", "\\1",
                                                                          basename(list.files(path = readPath,
                                                                                              pattern = "BITS_HL_1991_AssYear_",
                                                                                              full.names = TRUE)))))],
                      stringsAsFactors=F)
  
  
  # hh_ibts<-read.csv("WKBPLAICE 2024/Data/Surveys/exchange/IBTS_HH_1965_AssYear.csv",sep=",",stringsAsFactors=F)
  # ca_ibts<-read.csv("WKBPLAICE 2024/Data/Surveys/exchange/IBTS_CA_1965_AssYear.csv",sep=",",stringsAsFactors=F)
  # hl_ibts<-read.csv("WKBPLAICE 2024/Data/Surveys/exchange/IBTS_HL_1965_AssYear.csv",sep=",",stringsAsFactors=F)
  # 
  # hh_bits<-read.csv("WKBPLAICE 2024/Data/Surveys/exchange/BITS_HH_1991_AssYear.csv",sep=",",stringsAsFactors=F)
  # ca_bits<-read.csv("WKBPLAICE 2024/Data/Surveys/exchange/BITS_CA_1991_AssYear.csv",sep=",",stringsAsFactors=F)
  # hl_bits<-read.csv("WKBPLAICE 2024/Data/Surveys/exchange/BITS_HL_1991_AssYear.csv",sep=",",stringsAsFactors=F)
  #====
  
  #===
  # Select Plaice in ca and hl files ----
  #===
  ca1_ibts<-subset(ca_ibts,SpecCodeType=="T"&SpecCode==172902)
  ca2_ibts<-subset(ca_ibts,SpecCodeType=="W"&SpecCode==127143)
  ca_ibts<-rbind(ca1_ibts,ca2_ibts)
  
  hl1_ibts<-subset(hl_ibts,SpecCodeType=="T"&SpecCode==172902)
  hl2_ibts<-subset(hl_ibts,SpecCodeType=="W"&SpecCode==127143)
  hl_ibts<-rbind(hl1_ibts,hl2_ibts)
  
  ca1<-subset(ca_bits,SpecCodeType=="T"&SpecCode==172902)
  ca2<-subset(ca_bits,SpecCodeType=="W"&SpecCode==127143)
  ca<-rbind(ca1,ca2)
  
  hl1<-subset(hl_bits,SpecCodeType=="T"&SpecCode==172902)
  hl2<-subset(hl_bits,SpecCodeType=="W"&SpecCode==127143)
  hl<-rbind(hl1,hl2)
  #====
  
  #===
  # Add SD and Station info to IBTS ----  
  #===
  # Make NS-IBTS haul data spatial object
  hh_ibts_sf <- st_as_sf(hh_ibts, coords = c("ShootLong", "ShootLat"), crs = 4326)
  
  # Match projections between ICES areas and Haul data 
  SDs <- st_make_valid(st_transform(SDs, crs = st_crs(hh_ibts_sf)))
  
  # Join / merge the ICES spatial information with the DATRAS hauls by geometries
  hh_ibts_sf <- st_join(x = hh_ibts_sf, y = SDs, join = st_nearest_feature, left = T)
  
  # reduce data to dataframe
  hh_ibts <- st_drop_geometry(hh_ibts_sf)

  # Select the rows within the relevant subdivisions
  hh_ibts1 <- hh_ibts[hh_ibts$SubDivisio %in% as.character(c(21:32)), ]
  
  # Add station information to ca file
  ca_sel_ibts<-ca_ibts[,names(ca_ibts)%in%c("Survey","Quarter","Country","Ship","Gear","StNo","HaulNo","Year","AreaCode","LngtCode","LngtClass","Sex","Maturity","PlusGr","Age","CANoAtLngt","IndWgt")]
  hh_sel_ibts<-hh_ibts1[,names(hh_ibts1)%in%c("Survey","Quarter","Country","Ship","Gear","StNo","HaulNo","Year","Month","Day","Stratum","HaulDur","ShootLat","ShootLong","HaulLat","HaulLong","StatRec","Depth","HaulVal","DataType","SubDivisio")]
  
  ca_sel_ibts<-ca_sel_ibts[order(ca_sel_ibts$Year,ca_sel_ibts$Quarter,ca_sel_ibts$Country,ca_sel_ibts$Ship,ca_sel_ibts$Gear,ca_sel_ibts$StNo,ca_sel_ibts$HaulNo),]
  hh_sel_ibts<-hh_sel_ibts[order(hh_sel_ibts$Year,hh_sel_ibts$Quarter,hh_sel_ibts$Country,hh_sel_ibts$Ship,hh_sel_ibts$Gear,hh_sel_ibts$StNo,hh_sel_ibts$HaulNo),]
  
  ca_hh_ibts<-merge(hh_sel_ibts,ca_sel_ibts,by=c("Survey","Quarter","Country","Ship","Gear","StNo","HaulNo","Year"),all=F)
  
  # write.csv(ca_hh_ibts,file="WKBPLAICE 2024/Data/Surveys/exchange/ca_hh_ibts.csv")
  
  # Add SD info to hl file
  hl_sel_ibts<-hl_ibts[,names(hl_ibts)%in%c("Year","Country","Quarter","Ship","Gear","StNo","HaulNo","Sex","TotalNo","SubFactor","LngtCode","LngtClass","HLNoAtLngt")]
  hl_sel_ibts<-hl_sel_ibts[order(hl_sel_ibts$Year,hl_sel_ibts$Quarter,hl_sel_ibts$Country,hl_sel_ibts$Ship,hl_sel_ibts$Gear,hl_sel_ibts$StNo,hl_sel_ibts$HaulNo),]
  
  hl_hh_ibts<-merge(hh_sel_ibts,hl_sel_ibts,by=c("Quarter","Country","Ship","Gear","StNo","HaulNo","Year"),all=F)
  
  # write.csv(hl_hh,file="WKBPLAICE 2024/Data/Surveys/exchange/hl_hh.csv")
  #====
  
  #===
  # Add SD and Station info to BITS ----  
  #===
  # Make BITS haul data spatial object
  hh_bits_sf <- st_as_sf(hh_bits, coords = c("ShootLong", "ShootLat"), crs = 4326)
  
  # Match projections between ICES areas and Haul data 
  # SDs <- ifelse(st_crs(SDs) == st_crs(hh_bits_sf), 
  #               SDs,
  #               st_make_valid(st_transform(SDs, crs = st_crs(hh_bits_sf))))
  SDs <- st_make_valid(st_transform(SDs, crs = st_crs(hh_bits_sf)))
  
  # Join / merge the ICES spatial information with the DATRAS hauls by geometries
  hh_bits_sf <- st_join(x = hh_bits_sf, y = SDs, join = st_nearest_feature, left = T)
  
  # reduce data to dataframe
  hh_bits <- st_drop_geometry(hh_bits_sf)
  
  ## Select the rows within the relevant subdivisions
  hh_bits1 <- hh_bits[hh_bits$SubDivisio %in% as.character(c(21:32)), ]
  
  # Add station information to ca file
  ca_sel<-ca[,names(ca)%in%c("Survey","Quarter","Country","Ship","Gear","StNo","HaulNo","Year","AreaCode","LngtCode","LngtClass","Sex","Maturity","PlusGr","Age","CANoAtLngt","IndWgt")]
  hh_sel<-hh_bits1[,names(hh_bits1)%in%c("Survey","Quarter","Country","Ship","Gear","StNo","HaulNo","Year","Month","Day","Stratum","HaulDur","ShootLat","ShootLong","HaulLat","HaulLong","StatRec","Depth","HaulVal","DataType","SubDivisio")]
  
  ca_sel<-ca_sel[order(ca_sel$Year,ca_sel$Quarter,ca_sel$Country,ca_sel$Ship,ca_sel$Gear,ca_sel$StNo,ca_sel$HaulNo),]
  hh_sel<-hh_sel[order(hh_sel$Year,hh_sel$Quarter,hh_sel$Country,hh_sel$Ship,hh_sel$Gear,hh_sel$StNo,hh_sel$HaulNo),]
  
  ca_hh<-merge(hh_sel,ca_sel,by=c("Survey","Quarter","Country","Ship","Gear","StNo","HaulNo","Year"),all=F)
  
  # write.csv(ca_hh,file="WKBPLAICE 2024/Data/Surveys/exchange/ca_hh.csv")
  
  # Add SD info to hl file
  hl_sel<-hl[,names(hl)%in%c("Year","Country","Quarter","Ship","Gear","StNo","HaulNo","Sex","TotalNo","SubFactor","LngtCode","LngtClass","HLNoAtLngt")]
  hl_sel<-hl_sel[order(hl_sel$Year,hl_sel$Quarter,hl_sel$Country,hl_sel$Ship,hl_sel$Gear,hl_sel$StNo,hl_sel$HaulNo),]
  
  hl_hh<-merge(hh_sel,hl_sel,by=c("Quarter","Country","Ship","Gear","StNo","HaulNo","Year"),all=F)
  
  # write.csv(hl_hh,file="WKBPLAICE 2024/Data/Surveys/exchange/hl_hh.csv")
  #====
  
  #===
  # ADJUST UNITS AND CLEAN DATA ----
  #===
  ## Start with NS-IBTS
  # get LngtClass in the same units. "." or "0" in LngtCode means it is in mm, "1" means it is in cm
  # Make all in cm
  mm_ca_ibts<-subset(ca_hh_ibts,LngtCode%in%c(".","0"))
  cm_ca_ibts<-subset(ca_hh_ibts,LngtCode%in%c("1"))
  
  mm_ca_ibts$LngtClass<-mm_ca_ibts$LngtClass/10
  ca_tmp1_ibts<-rbind(mm_ca_ibts,cm_ca_ibts)
  
  mm_hl_ibts<-subset(hl_hh_ibts,LngtCode%in%c(".","0"))
  cm_hl_ibts<-subset(hl_hh_ibts,LngtCode%in%c("1"))
  
  mm_hl_ibts$LngtClass<-mm_hl_ibts$LngtClass/10
  hl_tmp1_ibts<-rbind(mm_hl_ibts,cm_hl_ibts)                         # has less rows than hl_hh because some LngtCode -9
  
  # check whether HL needs cleaning
  # unique(hl_tmp1_ibts$LngtClass)
  hl_hh_fin_ibts<-hl_tmp1_ibts
  
  # write.csv(hl_hh_fin_ibts,file="WKBPLAICE 2024/Data/Surveys/exchange/hl_hh_fin_ibts.csv",row.names=F)
  
  ## BITS' Turn!
  # get LngtClass in the same units. "." or "0" in LngtCode means it is in mm, "1" means it is in cm
  # Make all in cm
  mm_ca<-subset(ca_hh,LngtCode%in%c(".","0"))
  cm_ca<-subset(ca_hh,LngtCode%in%c("1"))
  
  mm_ca$LngtClass<-mm_ca$LngtClass/10
  ca_tmp1<-rbind(mm_ca,cm_ca)
  
  mm_hl<-subset(hl_hh,LngtCode%in%c(".","0"))
  cm_hl<-subset(hl_hh,LngtCode%in%c("1"))
  
  mm_hl$LngtClass<-mm_hl$LngtClass/10
  hl_tmp1<-rbind(mm_hl,cm_hl)                         # has less rows than hl_hh because some LngtCode -9
  
  # check whether HL needs cleaning
  # unique(hl_tmp1$LngtClass)
  hl_hh_fin<-hl_tmp1
  
  # write.csv(hl_hh_fin,file="WKBPLAICE 2024/Data/Surveys/exchange/hl_hh_fin.csv",row.names=F)
  #====
  
  #===
  # Calculate CPUE for records not already reported as such ----
  ## Take into account DataType (multiply with subFactor) in HL: 
  #===
  ## Start with NS-IBTS.
  #CHECK WHICH TYPES YOU HAVE:
  # unique(hl_hh_fin_ibts$DataType)
  
  hl_hh_fin_ibts$HLNoAtLngt_1<-NA
  
  for (i in c(1:nrow(hl_hh_fin_ibts))){
    if(hl_hh_fin_ibts$DataType[i]==as.character("C")){
      hl_hh_fin_ibts$HLNoAtLngt_1[i]<-hl_hh_fin_ibts$HLNoAtLngt[i]
    }
    if(hl_hh_fin_ibts$DataType[i]==as.character("R")){
      hl_hh_fin_ibts$HLNoAtLngt_1[i]<-(hl_hh_fin_ibts$HLNoAtLngt[i]*hl_hh_fin_ibts$SubFactor[i])*(60/hl_hh_fin_ibts$HaulDur[i])
    }
    if(hl_hh_fin_ibts$DataType[i]==as.character("S")){
      hl_hh_fin_ibts$HLNoAtLngt_1[i]<-(hl_hh_fin_ibts$HLNoAtLngt[i]*hl_hh_fin_ibts$SubFactor[i])*(60/hl_hh_fin_ibts$HaulDur[i])
    }
    if(is.na(hl_hh_fin_ibts$DataType[i])){
      hl_hh_fin_ibts$HLNoAtLngt_1[i]<-hl_hh_fin_ibts$HLNoAtLngt[i]  
    }
  }
  
  ## BITS' Turn!
  # #CHECK WHICH TYPES YOU HAVE:
  # unique(hl_hh_fin$DataType)
  
  hl_hh_fin$HLNoAtLngt_1<-NA
  
  for (i in c(1:nrow(hl_hh_fin))){
    if(hl_hh_fin$DataType[i]==as.character("C")){   # C = Reported as CPUE
      hl_hh_fin$HLNoAtLngt_1[i]<-hl_hh_fin$HLNoAtLngt[i]
    }
    if(hl_hh_fin$DataType[i]==as.character("R")){   # R = Data by Haul (could be cpue or not depending on species/survey - if CPUE then SubFactor == 1)
      hl_hh_fin$HLNoAtLngt_1[i]<-(hl_hh_fin$HLNoAtLngt[i]*hl_hh_fin$SubFactor[i])*(60/hl_hh_fin$HaulDur[i])
    }
    if(hl_hh_fin$DataType[i]==as.character("S")){   # S = Subsampled haul, multiply by SubFactor which should be >1)
      hl_hh_fin$HLNoAtLngt_1[i]<-(hl_hh_fin$HLNoAtLngt[i]*hl_hh_fin$SubFactor[i])*(60/hl_hh_fin$HaulDur[i])
    }
    if(is.na(hl_hh_fin$DataType[i])){
      hl_hh_fin$HLNoAtLngt_1[i]<-hl_hh_fin$HLNoAtLngt[i]  
    }
  }
  #====
  
  #===
  # Clean and reshape CA data: 
  #===
  ## Start with NS-IBTS.
  # Clean CA data: 
  missing_ca_ibts <- ca_tmp1_ibts[which(ca_tmp1_ibts$LngtClass==-9|ca_tmp1_ibts$IndWgt==-9|ca_tmp1_ibts$IndWgt==0|ca_tmp1_ibts$Age==-95),]
  missing_rows_ca_ibts <- which(ca_tmp1_ibts$LngtClass==-9|ca_tmp1_ibts$IndWgt==-9|ca_tmp1_ibts$IndWgt==0|ca_tmp1_ibts$Age==-95)
  ca_tmp2_ibts<-ca_tmp1_ibts[-(missing_rows_ca_ibts),]  
  
  # CA data: add rows where CaNoLngt is larger than 1, so each row will represent one fish
  ca_tmp2_1_ibts<-subset(ca_tmp2_ibts,CANoAtLngt==1)
  ca_tmp2_2_ibts<-subset(ca_tmp2_ibts,CANoAtLngt>1)
  
  n_times_ibts<-c(ca_tmp2_2_ibts$CANoAtLngt)
  ca_tmp2_2_adj_ibts<-ca_tmp2_2_ibts[rep(seq_len(nrow(ca_tmp2_2_ibts)), n_times_ibts),]
  
  ca_hh_fin_ibts<-rbind(ca_tmp2_1_ibts,ca_tmp2_2_adj_ibts)
  
  # # Check the data
  # plot(ca_hh_fin_ibts$LngtClass,ca_hh_fin_ibts$IndWgt)
  
  ## BITS' Turn!
  missing_ca<-ca_tmp1[which(ca_tmp1$LngtClass==-9|ca_tmp1$IndWgt==-9|ca_tmp1$IndWgt==0|ca_tmp1$Age==-95),]
  missing_rows_ca<-which(ca_tmp1$LngtClass==-9|ca_tmp1$IndWgt==-9|ca_tmp1$IndWgt==0|ca_tmp1$Age==-95)
  ca_tmp2<-ca_tmp1[-(missing_rows_ca),]  
  
  # CA data: add rows where CaNoLngt is larger than 1, so each row will represent one fish
  ca_tmp2_1<-subset(ca_tmp2,CANoAtLngt==1)
  ca_tmp2_2<-subset(ca_tmp2,CANoAtLngt>1)
  
  n_times<-c(ca_tmp2_2$CANoAtLngt)
  ca_tmp2_2_adj<-ca_tmp2_2[rep(seq_len(nrow(ca_tmp2_2)), n_times),]
  
  ca_hh_fin<-rbind(ca_tmp2_1,ca_tmp2_2_adj)
  
  # # Number of fish in CA per year, qrt, SD
  # nr_fish_ca<-aggregate(ca_hh_fin$CANoAtLngt,by=list(ca_hh_fin$SD,ca_hh_fin$Country,ca_hh_fin$Quarter,ca_hh_fin$Year),FUN="length")
  
  # #check the data
   # plot(ca_hh_fin$LngtClass,ca_hh_fin$IndWgt)
  #====
  
  #===
  # Merge NS-IBTS & BITS
  #===
  ca_hh_fin_bits_ibts <- rbind(ca_hh_fin, ca_hh_fin_ibts)
  hl_hh_fin_bits_ibts <- rbind(hl_hh_fin, hl_hh_fin_ibts)
  
  # Optional save for later: 
  saveRDS(ca_hh_fin_bits_ibts, file = paste0(savePath, "Ple.27.21-32_ca-hh.RDS"))
  saveRDS(hl_hh_fin_bits_ibts, file = paste0(savePath, "Ple.27.21-32_hl-hh.RDS"))
  #====
}

#===
# Clean up data environment
#===
k <- c("ca_hh_fin_bits_ibts", "hl_hh_fin_bits_ibts", "catchNum", "catchcohorts")
r <- ls(pattern = "ca|hl|hh")[!ls(pattern = "ca|hl|hh") %in% k]

rm(list = r)
# rm(list = c("r"))
#====
