#
##
### This script calculates Stock weights at age for Ple.27.21+
### This script is dependent on earlier scripts
###   - "01_DatrasDownloads.R"
###   - "02_DatrasDataCleaning.R"
### Authored by Elliot Brown, based on scripts provided by Margit Eero
##
#

#===
# Dependencies ----
#===
library(lubridate)
library(ggplot2)
library(plotly)
library(data.table)
library(ggthemes)
DataYear <- year(Sys.time())-1
ebpal <- c("#2F3EEA", "#1FD082", "#030F4F", "#F6D04D", "#FC7634", "#F7BBB1", "#E83F48", "#008835", "#79238E", "#000000")
#====

#===
# Subset processed exchange data ----
#===
# Select the SD and quarter
ca_q1<-subset(ca_hh_fin_bits_ibts,Quarter==1&Age!=-9&Age!=0&Age<15)
hl_q1<-subset(hl_hh_fin_bits_ibts,Quarter==1)
#=====

#===
# Summarise and aggregate data ----
#===
# Add 1 to all rows just to be able to count the fish
ca_q1$Nr_fish<-1

# Round the length class
ca_q1$RLngtClass<-round(ca_q1$LngtClass)

# How many fish we have
nr_fish<-aggregate(ca_q1$CANoAtLngt,
                   by=list(ca_q1$Country,ca_q1$Quarter,ca_q1$Year),
                   FUN="length")

nr_fish<- aggregate(ca_q1$Nr_fish,
                    by=list(ca_q1$Year,ca_q1$Sex,ca_q1$RLngtClass,ca_q1$Age),
                    FUN="sum",
                    na.rm=T)
names(nr_fish)<-c("year","sex","LngtClass","age","nr_fish")

# Aggregate number of fish and average weight in samples
weight<-aggregate(ca_q1$IndWgt,
                  by=list(ca_q1$Year,ca_q1$Sex,round(ca_q1$RLngtClass),
                          ca_q1$Age),
                  FUN="mean",na.rm=T)
names(weight)<-c("year","sex","LngtClass","age","weight")
#=====

#===
# Length Frequencies "at sea" ----
#===
# Set parameters for data
lencl<-c(4:60)
age<-c(1:10)
year<-c(1999:(DataYear+1))

# Make tables for length freq data
length_freq_tmp<-by(hl_q1,list(hl_q1$Year,round(hl_q1$LngtClass)),function (x){
  sum(x$HLNoAtLngt_1)
})
length_freq_tab<-t(as.table(length_freq_tmp))
Lfreq_sea<-data.frame(year=rep(year, each=length(lencl)),LngtClass=rep(lencl,length(year)),HLNoLngt=NA)

# Fill the table
for (i in c(1:nrow(Lfreq_sea))){
  year_i<-Lfreq_sea[i,names(Lfreq_sea)==as.character("year")]
  lencl_i<-Lfreq_sea[i,names(Lfreq_sea)==as.character("LngtClass")]
  
  if(lencl_i%in%c(as.numeric(dimnames(length_freq_tab)[[1]]))){
    
    data_i<-length_freq_tab[as.numeric(dimnames(length_freq_tab)[[1]])==lencl_i,dimnames(length_freq_tab)[[2]]==year_i]
    Lfreq_sea[i,names(Lfreq_sea)==as.character("HLNoLngt")]<-data_i
    
  }else{
    Lfreq_sea[i,names(Lfreq_sea)==as.character("HLNoLngt")]<-0       
  }
}

# replace NA with 0
Lfreq_sea[which(is.na(Lfreq_sea$HLNoLngt)),names(Lfreq_sea)==as.character("HLNoLngt")]<-0

#=====

#===
# Summary of sampled fish: weight by length and by age ----
#===
# Number of samples
## Make tables  
nr_fish_samp_F<-data.frame(year=rep(year, each=length(lencl)),LngtClass=rep(lencl,length(year)), a1=NA,a2=NA,a3=NA,a4=NA,a5=NA,a6=NA,a7=NA,a8=NA,a9=NA,a10=NA)
nr_fish_samp_M<-nr_fish_samp_F

wgt_samp_F<-nr_fish_samp_F
wgt_samp_M<-nr_fish_samp_M

## Filling the tables: 
### Female  
for (i in 1:nrow( nr_fish_samp_F)){
  for (j in 3:ncol( nr_fish_samp_F)){
    year_i<-nr_fish_samp_F[i,names(nr_fish_samp_F)==as.character("year")]
    lencl_i<-nr_fish_samp_F[i,names(nr_fish_samp_F)==as.character("LngtClass")]
    age_i<-j-2
    
    if(lencl_i%in% weight[which(weight$year==year_i&weight$sex==as.character("F")& weight$age==age_i),names(weight)==as.character("LngtClass")]){
      
      show_wgt<-weight[which(weight$year==year_i&weight$LngtClass==lencl_i&weight$sex==as.character("F")&weight$age==age_i),]
      show_nr<-nr_fish[which(nr_fish$year==year_i&nr_fish$LngtClass==lencl_i&nr_fish$sex==as.character("F")&nr_fish$age==age_i),]
      
      nr_fish_samp_F[i,j]<-sum(show_nr$nr_fish)
      wgt_samp_F[i,j]<-mean(show_wgt$weight, na.rm=T)
      
    } else{              
      nr_fish_samp_F[i,j]<-0
      wgt_samp_F[i,j]<-NA
    }
  }
}

### Male
for (i in 1:nrow( nr_fish_samp_M)){
  for (j in 3:ncol( nr_fish_samp_M)){
    year_i<-nr_fish_samp_M[i,names(nr_fish_samp_M)==as.character("year")]
    lencl_i<-nr_fish_samp_M[i,names(nr_fish_samp_M)==as.character("LngtClass")]
    age_i<-j-2
    
    if(lencl_i%in% weight[which(weight$year==year_i&weight$sex==as.character("M")& weight$age==age_i),names(weight)==as.character("LngtClass")]){
      
      show_wgt<-weight[which(weight$year==year_i&weight$LngtClass==lencl_i&weight$sex==as.character("M")&weight$age==age_i),]
      show_nr<-nr_fish[which(nr_fish$year==year_i&nr_fish$LngtClass==lencl_i&nr_fish$sex==as.character("M")&nr_fish$age==age_i),]
      
      nr_fish_samp_M[i,j]<-sum(show_nr$nr_fish)
      wgt_samp_M[i,j]<-mean(show_wgt$weight, na.rm=T)
      
    } else{              
      nr_fish_samp_M[i,j]<-0
      wgt_samp_M[i,j]<-NA
    }
  }
}

# Number of fish sampled by length class:
## Female
nr_fish_samp_F$tot<-NA

for (i in 1:nrow(nr_fish_samp_F)){
  nr_fish_samp_F[i,names(nr_fish_samp_F)==as.character("tot")]<-sum(nr_fish_samp_F[i,names(nr_fish_samp_F)%in%c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10")])
}

##  Male        
nr_fish_samp_M$tot<-NA

for (i in 1:nrow(nr_fish_samp_M)){
  nr_fish_samp_M[i,names(nr_fish_samp_M)==as.character("tot")]<-sum(nr_fish_samp_M[i,names(nr_fish_samp_M)%in%c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10")])
}

## F and M combined
Lfreq_samp<-data.frame(year=rep(year, each=length(lencl)),LngtClass=rep(lencl,length(year)), tot_samp=NA)

for (i in 1:nrow(Lfreq_samp)){
  year_i<-Lfreq_samp[i,names(Lfreq_samp)==as.character("year")]
  lencl_i<-Lfreq_samp[i,names(Lfreq_samp)==as.character("LngtClass")]
  
  F_sum<-nr_fish_samp_F[which(nr_fish_samp_F$year==year_i&nr_fish_samp_F$LngtClass==lencl_i),names(nr_fish_samp_F)==as.character("tot")]
  M_sum<-nr_fish_samp_M[which(nr_fish_samp_M$year==year_i&nr_fish_samp_M$LngtClass==lencl_i),names(nr_fish_samp_M)==as.character("tot")]
  
  Lfreq_samp[i,names(Lfreq_samp)==as.character("tot_samp")] <-F_sum+ M_sum 
}  

# Number of fish sampled by age:
## Make tables
tot_samp_age_F<-data.frame(year=year,a1=NA,a2=NA,a3=NA,a4=NA,a5=NA,a6=NA,a7=NA,a8=NA,a9=NA,a10=NA)
tot_samp_age_M<-data.frame(year=year,a1=NA,a2=NA,a3=NA,a4=NA,a5=NA,a6=NA,a7=NA,a8=NA,a9=NA,a10=NA)  

## Female
for (i in 1:nrow(tot_samp_age_F)){
  for (j in 2:ncol(tot_samp_age_F)){ 
    
    year_i<-tot_samp_age_F[i,names(tot_samp_age_F)==as.character("year")]
    show_data<- nr_fish_samp_F[which(nr_fish_samp_F$year==year_i),]
    
    tot_samp_age_F[i,j]<-sum(show_data[,names(show_data)==names(tot_samp_age_F)[j]])
  } 
}

## Male
for (i in 1:nrow(tot_samp_age_M)){
  for (j in 2:ncol(tot_samp_age_M)){ 
    
    year_i<-tot_samp_age_M[i,names(tot_samp_age_M)==as.character("year")]
    show_data<- nr_fish_samp_M[which(nr_fish_samp_M$year==year_i),]
    
    tot_samp_age_M[i,j]<-sum(show_data[,names(show_data)==names(tot_samp_age_M)[j]])
  } 
}
#=====

#===
# Sex Ratios ----
#===
# Make tables
sex_ratio_F<- tot_samp_age_F
sex_ratio_F[,-1]<-NA

# Fill tables
for (i in 1:nrow(sex_ratio_F)){
  for (j in 2:ncol(sex_ratio_F)){
    sex_ratio_F[i,j]<-tot_samp_age_F[i,j]/(tot_samp_age_F[i,j]+tot_samp_age_M[i,j])
  }
}
#=====

#===
# Full survey numbers for raising ----
#===
# Make Tables
nr_fish_sea_M<-nr_fish_samp_M
nr_fish_sea_M[,-(1:2)]<-NA
nr_fish_sea_F<-nr_fish_sea_M

## Female
for (i in 1:nrow(nr_fish_sea_F)){
  for (j in 3:(ncol(nr_fish_sea_F)-1)){
    if(nr_fish_samp_F$tot[i]>0&!is.na(Lfreq_sea$HLNoLngt[i])){
      
      pct_age_tot<-nr_fish_samp_F[i,j]/nr_fish_samp_F[i,names(nr_fish_samp_F)==as.character("tot")]   
      F_M_ratio<-nr_fish_samp_F[i,names(nr_fish_samp_F)==as.character("tot")]/Lfreq_samp$tot_samp[i]
      Lfreq<-Lfreq_sea$HLNoLngt[i]
      
      nr_fish_sea_F[i,j]<-pct_age_tot*Lfreq* F_M_ratio
    }else{
      nr_fish_sea_F[i,j]<-0 
    }
  }
}           

## Male
for (i in 1:nrow(nr_fish_sea_M)){
  for (j in 3:(ncol(nr_fish_sea_M)-1)){
    if(nr_fish_samp_M$tot[i]>0&!is.na(Lfreq_sea$HLNoLngt[i])){
      
      pct_age_tot<-nr_fish_samp_M[i,j]/nr_fish_samp_M[i,names(nr_fish_samp_M)==as.character("tot")]   
      M_F_ratio<-nr_fish_samp_M[i,names(nr_fish_samp_M)==as.character("tot")]/Lfreq_samp$tot_samp[i]
      Lfreq<-Lfreq_sea$HLNoLngt[i]
      
      nr_fish_sea_M[i,j]<-pct_age_tot*Lfreq* M_F_ratio
    }else{
      nr_fish_sea_M[i,j]<-0 
    }
  }
}           

# Sum of numbers over age-groups, by length
## Females
for (i in 1:nrow(nr_fish_sea_F)){
  nr_fish_sea_F[i,names(nr_fish_sea_F)==as.character("tot")]<-sum(nr_fish_sea_F[i,names(nr_fish_sea_F)%in%c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10")])   
}    
## Males
for (i in 1:nrow(nr_fish_sea_M)){
  nr_fish_sea_M[i,names( nr_fish_sea_M)==as.character("tot")]<-sum(nr_fish_sea_M[i,names(nr_fish_sea_M)%in%c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10")])   
}  

# Sum of numbers over length-groups, by age
## Make tables
sum_nr_fish_sea_F<-tot_samp_age_F
sum_nr_fish_sea_F[,-1]<-NA
sum_nr_fish_sea_M<-sum_nr_fish_sea_F

## Females
for (i in 1:nrow(sum_nr_fish_sea_F)){ 
  for (j in 2:ncol(sum_nr_fish_sea_F)){ 
    year_i<-sum_nr_fish_sea_F[i,names(sum_nr_fish_sea_F)==as.character("year")]
    sum_nr_fish_sea_F[i,j]<-sum(nr_fish_sea_F[which(nr_fish_sea_F$year==year_i),names(nr_fish_sea_F)==names(sum_nr_fish_sea_F)[j]]) 
  } 
}

## Males
for (i in 1:nrow(sum_nr_fish_sea_M)){ 
  for (j in 2:ncol(sum_nr_fish_sea_M)){ 
    year_i<-sum_nr_fish_sea_M[i,names(sum_nr_fish_sea_M)==as.character("year")]
    sum_nr_fish_sea_M[i,j]<-sum(nr_fish_sea_M[which(nr_fish_sea_M$year==year_i),names(nr_fish_sea_M)==names(sum_nr_fish_sea_M)[j]]) 
  } 
}

#=====

#===
# Raise proportions to full survey numbers ----
#===
# Make tables
wgt_nr_F<-nr_fish_sea_F
wgt_nr_F[,3:ncol(wgt_nr_F)]<-NA
wgt_nr_M<-wgt_nr_F

## Female
for (i in 1:nrow(wgt_nr_F)){
  for (j in 3:(ncol(wgt_nr_F)-1)) {
    if(nr_fish_sea_F[i,j]>0&!is.na(wgt_samp_F[i,j])){
      wgt<-wgt_samp_F[i,j]
      nr<-nr_fish_sea_F[i,j]
      wgt_nr_F[i,j]<-wgt*nr
    }else{
      wgt_nr_F[i,j]<-0
    }
  }
}   

## Male
for (i in 1:nrow(wgt_nr_M)){
  for (j in 3:(ncol(wgt_nr_M)-1)) {
    if(nr_fish_sea_M[i,j]>0&!is.na(wgt_samp_M[i,j])){
      wgt<-wgt_samp_M[i,j]
      nr<-nr_fish_sea_M[i,j]
      wgt_nr_M[i,j]<-wgt*nr
    }else{
      wgt_nr_M[i,j]<-0
    }
  }
} 

# Sum weights by age over length classes
## Make Tables
sum_wgt_nr_F<-tot_samp_age_F
sum_wgt_nr_F[,-1]<-NA
sum_wgt_nr_M<-sum_wgt_nr_F

## Females
for (i in 1:nrow(sum_wgt_nr_F)){ 
  for (j in 2:ncol(sum_wgt_nr_F)){ 
    year_i<-sum_wgt_nr_F[i,names(sum_wgt_nr_F)==as.character("year")]
    sum_wgt_nr_F[i,j]<-sum(wgt_nr_F[which(wgt_nr_F$year==year_i),names(wgt_nr_F)==names(sum_wgt_nr_F)[j]]) 
  } 
}

## Males
for (i in 1:nrow(sum_wgt_nr_M)){ 
  for (j in 2:ncol(sum_wgt_nr_M)){ 
    year_i<-sum_wgt_nr_M[i,names(sum_wgt_nr_M)==as.character("year")]
    sum_wgt_nr_M[i,j]<-sum(wgt_nr_M[which(wgt_nr_M$year==year_i),names(wgt_nr_M)==names(sum_wgt_nr_M)[j]]) 
  } 
}
#=====

#=== 
# Stock mean weight at age ----
#===
# Make Tables
WAA_F<-sum_wgt_nr_F
WAA_F[,-1]<-NA
WAA_M<-WAA_F
WAA_combsex<-WAA_M

## Females
for (i in 1:nrow(WAA_F)){
  for (j in 2:ncol(WAA_F)){
    WAA_F[i,j]<-sum_wgt_nr_F[i,j]/sum_nr_fish_sea_F[i,j]
  }
}

## Males
for (i in 1:nrow(WAA_M)){
  for (j in 2:ncol(WAA_M)){
    WAA_M[i,j]<-sum_wgt_nr_M[i,j]/sum_nr_fish_sea_M[i,j]
  }
}

## comb of F and M
for (i in 1:nrow(WAA_combsex)){ 
  for (j in 2:ncol(WAA_combsex)){ 
    if( !is.na(WAA_F[i,j])&!is.na(WAA_M[i,j])){
      WAA_combsex[i,j]<-(WAA_F[i,j]*sex_ratio_F[i,j])+(WAA_M[i,j]*(1-sex_ratio_F[i,j]))
    }
    if(is.na(WAA_F[i,j])&!is.na(WAA_M[i,j])){
      WAA_combsex[i,j]<-WAA_M[i,j]
    }
    if(is.na(WAA_M[i,j])&!is.na(WAA_F[i,j])){
      WAA_combsex[i,j]<-WAA_F[i,j]    
    }
  }
}

## Convert weights to kg, not grams
WAA_combsex <- WAA_combsex/1000
WAA_combsex$year <- WAA_combsex$year*1000
WAA_combsex$year <- as.integer(as.character(WAA_combsex$year))

#=====

#===
# Reshape WAA data and calculate means +/- 95%CI  for plotting  ----
#===
# Wide to long format
swaal <- reshape(WAA_combsex,
                 varying = colnames(WAA_combsex)[-1],
                 v.names = "MeanWeight",
                 timevar = "Age",
                 times = colnames(WAA_combsex)[-1],
                 idvar = "year",
                 direction = "long")



## Calculate mean swaa for 1999:2019 ----
m19WAA <- aggregate(MeanWeight ~ Age,
                    data = swaal[swaal$year %in% c(1999:2019),],
                    FUN = mean)

# Calculate std err around the above mean 1999:2019
mw19stdr <- aggregate(MeanWeight ~ Age,
                      data = swaal[swaal$year %in% c(1999:2019), ],
                      FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
mw19stdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = mw19stdr$Age))
colnames(mw19stdr)[colnames(mw19stdr) %in% c("MeanWeight")] <- "StdErr"
mw19stdr$MeanWeight <- m19WAA$MeanWeight
mw19stdr$ymax <- mw19stdr$MeanWeight + 1.96*mw19stdr$StdErr
mw19stdr$ymin <- mw19stdr$MeanWeight - 1.96*mw19stdr$StdErr
m19WAA$year <- as.factor(rep("Mean 1999-2019", n = nrow(m19WAA)))
mw19stdr <- mw19stdr[order(mw19stdr$AgeN), ]

## Calculate previous 3 year mean ----
m3WAA <- aggregate(MeanWeight ~ Age,
                   data = swaal[swaal$year %in% c(((DataYear+1)-2):(DataYear+1)),],
                   FUN = mean)

# Calculate std err around the above 3 year mean
m3wstdr <- aggregate(MeanWeight ~ Age,
                     data = swaal[swaal$year %in% c(((DataYear+1)-2):(DataYear+1)), ],
                     FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
m3wstdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = m3wstdr$Age))
colnames(m3wstdr)[colnames(m3wstdr) %in% c("MeanWeight")] <- "StdErr"
m3wstdr$MeanWeight <- m3WAA$MeanWeight
m3wstdr$ymax <- m3wstdr$MeanWeight + 1.96*m3wstdr$StdErr
m3wstdr$ymin <- m3wstdr$MeanWeight - 1.96*m3wstdr$StdErr
m3WAA$year <- as.factor(rep("3y Mean", n = nrow(m3WAA)))
m3wstdr <- m3wstdr[order(m3wstdr$AgeN), ]


## Calculate mean of 1999 to current year ----
rmWAA <- aggregate(MeanWeight ~ Age,
                   data = swaal[swaal$year %in% c(1999:(DataYear+1)),],
                   FUN = mean)

# Standard error of 1999 -> mean
rmwstdr <- aggregate(MeanWeight ~ Age,
                     data = swaal[swaal$year %in% c(1999:(DataYear+1)), ],
                     FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
rmwstdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = rmwstdr$Age))
colnames(rmwstdr)[colnames(rmwstdr) %in% c("MeanWeight")] <- "StdErr"
rmwstdr$MeanWeight <- rmWAA$MeanWeight
rmwstdr$ymax <- rmwstdr$MeanWeight + 1.96*rmwstdr$StdErr
rmwstdr$ymin <- rmwstdr$MeanWeight - 1.96*rmwstdr$StdErr
rmWAA$year <- as.factor(rep("Running Mean", n = nrow(rmWAA)))
rmwstdr <- rmwstdr[order(rmwstdr$AgeN), ]

# Combine all years, 2002 -> mean and 15 year mean into one object
swaal <- rbind(swaal, m19WAA, rmWAA, m3WAA)
swaal$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = swaal$Age))
swaal <- swaal[order(swaal$AgeN), ]
swaal$yearF <- as.factor(as.character(swaal$year))

## Calculate 3y sliding window average for whole timeseries ----
### Calculate the mean ----
sw3sw <- frollmean(WAA_combsex[, -1], 3, na.rm = T)
sw3sw <- as.data.frame(do.call(cbind, sw3sw))
sw3sw$year <- WAA_combsex$year

#### Wide to long format
sw3swL <- reshape(sw3sw,
                 varying = colnames(sw3sw)[-11],
                 v.names = "MeanWeight",
                 timevar = "Age",
                 times = colnames(sw3sw)[-11],
                 idvar = "year",
                 direction = "long")
sw3swL$Age <- as.numeric(gsub(pattern = "V", replacement = "", x = sw3swL$Age))

### Calcualte the standard error ----
sw3stdr <- frollapply(WAA_combsex[, -1],
                      3,
                      FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
sw3stdr <- as.data.frame(do.call(cbind, sw3stdr))
sw3stdr$year <- WAA_combsex$year

#### Wide to long format
sw3stdrL <- reshape(sw3stdr,
                    varying = colnames(sw3stdr)[-11],
                    v.names = "StdErrWeight",
                    timevar = "Age",
                    times = colnames(sw3stdr)[-11],
                    idvar = "year",
                    direction = "long")
sw3stdrL$Age <- as.numeric(gsub(pattern = "V", replacement = "", x = sw3stdrL$Age))

### Calculate 95% confidence intervals ----
sw3swL <- merge(x = sw3swL, y = sw3stdrL, by = c("year", "Age"), all.x = TRUE)
sw3swL$ymax <- sw3swL$MeanWeight + (1.96*sw3swL$StdErrWeight)
sw3swL$ymin <- sw3swL$MeanWeight - (1.96*sw3swL$StdErrWeight)
sw3swL$AgeN <- sw3swL$Age
sw3swL$Age <- as.character(sw3swL$Age)
sw3swL$year <- as.numeric(sw3swL$year)
sw3swL$yearF <- as.factor(as.character(sw3swL$year))


## Calculate 5y sliding window average for whole timeseries ----
### Calculate the mean ----
sw5sw <- frollmean(WAA_combsex[, -1], 5, na.rm = T)
sw5sw <- as.data.frame(do.call(cbind, sw5sw))
sw5sw[year == 2002, -ncol(sw5sw)] <- sw5sw[year == 2003, -ncol(sw5sw)]
sw5sw$year <- WAA_combsex$year

#### Wide to long format
sw5swL <- reshape(sw5sw,
                  varying = colnames(sw5sw)[-11],
                  v.names = "MeanWeight",
                  timevar = "Age",
                  times = colnames(sw5sw)[-11],
                  idvar = "year",
                  direction = "long")
sw5swL$Age <- as.numeric(gsub(pattern = "V", replacement = "", x = sw5swL$Age))

### Calcualte the standard error ----
sw5stdr <- frollapply(WAA_combsex[, -1],
                      5,
                      FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
sw5stdr <- as.data.frame(do.call(cbind, sw5stdr))
sw5stdr[year == 2002, -ncol(sw5stdr)] <- sw5stdr[year == 2003, -ncol(sw5stdr)]
sw5stdr$year <- WAA_combsex$year

#### Wide to long format
sw5stdrL <- reshape(sw5stdr,
                    varying = colnames(sw5stdr)[-11],
                    v.names = "StdErrWeight",
                    timevar = "Age",
                    times = colnames(sw5stdr)[-11],
                    idvar = "year",
                    direction = "long")
sw5stdrL$Age <- as.numeric(gsub(pattern = "V", replacement = "", x = sw5stdrL$Age))

### Calculate 95% confidence intervals ----
sw5swL <- merge(x = sw5swL, y = sw5stdrL, by = c("year", "Age"), all.x = TRUE)
sw5swL$ymax <- sw5swL$MeanWeight + (1.96*sw5swL$StdErrWeight)
sw5swL$ymin <- sw5swL$MeanWeight - (1.96*sw5swL$StdErrWeight)
sw5swL$AgeN <- sw5swL$Age
sw5swL$Age <- as.character(sw5swL$Age)
sw5swL$year <- as.numeric(sw5swL$year)
sw5swL$yearF <- as.factor(as.character(sw5swL$year))

#=====

#===
# save results ----
#===
# write.csv(WAA_combsex,file="WKBPLAICE 2024/Data/BiologicalData/ple.27.21-32_StockWeightAtAge_1999-DataYear.csv")
# saveRDS(swaal, file = "WKBPLAICE 2024/Data/BiologicalData/ple.27.21-32_swaal.RDS")
# saveRDS(mw19stdr, file = "WKBPLAICE 2024/Data/BiologicalData/ple.27.21-32_SWAA_AveStdErr_1999-2019.RDS")
# saveRDS(rmwstdr, file = "WKBPLAICE 2024/Data/BiologicalData/ple.27.21-32_SWAA_AveStdErr_1999-DataYear.RDS")
# saveRDS(m3wstdr, file = "WKBPLAICE 2024/Data/BiologicalData/ple.27.21-32_SWAA_AveStdErr_3yr.RDS")
# saveRDS(sw3swL, file = "WKBPLAICE 2024/Data/BiologicalData/ple.27.21-32_SWAA_sw3yr")
#=====

#===
# Temporary plots of Stock Weights at age ----
#===
sw_annual_raw <- swaal[!swaal$year %in% c("Running Mean", "3ymean", "3y Mean", "Mean 1999-2019"),]
sw_annual_raw$AgeN <- as.numeric(gsub(pattern = "a", replacement = "", x = sw_annual_raw$Age))
sw_annual_raw$Age <- as.character(sw_annual_raw$AgeN)
sw_annual_raw$year <- as.numeric(as.character(sw_annual_raw$year))

## Annual SWAA Averaged From Survey Sampling ----
plt_sw_swaal <- ggplot() +
  geom_line(data = sw_annual_raw,
            mapping = aes(x = year,
                          y = MeanWeight,
                          group = AgeN,
                          colour = Age))+
  facet_grid(rows = "AgeN", scales = "free_y") +
  theme_clean() +
  theme(legend.position = "none")
ggplotly(plt_sw_swaal)

## 3 year sliding window average ----
plt_sw_sw3 <- ggplot() +
  geom_line(data = sw3swL,
            mapping = aes(x = year,
                          y = MeanWeight,
                          group = Age)) +
  geom_ribbon(data = sw3swL,
              mapping = aes(x = year,
                            ymin = ymin,
                            ymax = ymax,
                            group = Age),
              alpha = 0.2) +
  geom_point(data = sw_annual_raw,
             mapping = aes(x = year,
                           y = MeanWeight,
                           group = Age),
             shape = 1) +
  geom_vline(xintercept = 2001.75,
             linetype = 3) +
  facet_grid(rows = "AgeN", scales = "free_y") +
  theme_clean()
ggplotly(plt_sw_sw3)

## 5 year sliding window average ----
plt_sw_sw5 <- ggplot() +
  geom_line(data = sw5swL,
            mapping = aes(x = year,
                          y = MeanWeight,
                          group = Age,
                          colour = Age)) +
  geom_ribbon(data = sw5swL,
              mapping = aes(x = year,
                            ymin = ymin,
                            ymax = ymax,
                            group = Age,
                            fill = Age),
              alpha = 0.2) +
  geom_point(data = sw_annual_raw,
             mapping = aes(x = year,
                           y = MeanWeight,
                           group = Age),
             shape = 1) +
  geom_vline(xintercept = 2001.75,
             linetype = 3) +
  facet_grid(rows = "AgeN", scales = "free_y") +
  theme_clean() +
  theme(legend.position = "none")
ggplotly(plt_sw_sw5)
#===

#===
# Compare old and 3y sliding window stock weights at age ----
#===
### Old ----
plt_sw_old <- ggplot() +
  geom_line(data = mw19stdr,
            mapping = aes(x = AgeN,
                          y = MeanWeight),
            size = 0.7,
            colour = ebpal[1])+
  geom_ribbon(data = mw19stdr,
              mapping = aes(x=AgeN,
                            ymin = ymin,
                            ymax = ymax),
              fill = ebpal[1],
              alpha = 0.2)+
  geom_line(data = swaal[swaal$year %in% c(max(2020, ((DataYear+1)-4)):(DataYear+1)), ],
            mapping = aes(x = AgeN,
                          y = MeanWeight,
                          colour = year))+
  theme_clean() +
  scale_colour_manual(values = ebpal[3:length(ebpal)])+
  scale_x_continuous(breaks = function(x) pretty(x, n = 10))
ggplotly(plt_sw_old)

### 3y - All years on one panel  ----
plt_sw_old3y <- ggplot() +
  geom_line(data = mw19stdr,
            mapping = aes(x = AgeN,
                          y = MeanWeight),
            size = 0.7,
            colour = ebpal[10])+
  geom_errorbar(data = mw19stdr,
              mapping = aes(x=AgeN,
                            ymin = ymin,
                            ymax = ymax),
              colour = ebpal[10],
              alpha = 0.75)+
  geom_line(data = sw3swL[sw3swL$year %in% c(2002:(DataYear+1)), ],
            mapping = aes(x = AgeN,
                          y = MeanWeight,
                          group = yearF,
                          colour = yearF)) +
  # geom_ribbon(data = sw3swL,
  #             mapping = aes(x = AgeN,
  #                           ymin = ymin,
  #                           ymax = ymax,
  #                           group = yearF,
  #                           fill = yearF),
  #             alpha = 0.05) +
  theme_clean() +
  # scale_colour_manual(values = ebpal[3:length(ebpal)])+
  scale_x_continuous(breaks = function(x) pretty(x, n = 10))
ggplotly(plt_sw_old3y)

### 3y sliding window - Faceted by year ----
plt_sw_old3y_year <- ggplot() +
  geom_line(data = mw19stdr,
            mapping = aes(x = AgeN,
                          y = MeanWeight),
            size = 0.7,
            colour = ebpal[10])+
  geom_errorbar(data = mw19stdr,
              mapping = aes(x=AgeN,
                            ymin = ymin,
                            ymax = ymax),
              colour = ebpal[10],
              alpha = 0.75)+
  geom_line(data = sw3swL[sw3swL$year %in% c(2002:(DataYear+1)), ],
            mapping = aes(x = AgeN,
                          y = MeanWeight,
                          group = yearF,
                          colour = yearF)) +
  geom_ribbon(data = sw3swL[sw3swL$year %in% c(2002:(DataYear+1)), ],
              mapping = aes(x = AgeN,
                            ymin = ymin,
                            ymax = ymax,
                            group = yearF,
                            fill = yearF),
              alpha = 0.2) +
  theme_clean() +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) +
  facet_wrap(.~year)
ggplotly(plt_sw_old3y_year)
#====

#===
# Compare old and 5y sliding window stock weights at age ----
#===
### Old ----
plt_sw_old <- ggplot() +
  geom_line(data = mw19stdr,
            mapping = aes(x = AgeN,
                          y = MeanWeight),
            size = 0.7,
            colour = ebpal[1])+
  geom_ribbon(data = mw19stdr,
              mapping = aes(x=AgeN,
                            ymin = ymin,
                            ymax = ymax),
              fill = ebpal[1],
              alpha = 0.2)+
  geom_line(data = swaal[swaal$year %in% c(max(2020, ((DataYear+1)-4)):(DataYear+1)), ],
            mapping = aes(x = AgeN,
                          y = MeanWeight,
                          colour = year))+
  theme_clean() +
  scale_colour_manual(values = ebpal[3:length(ebpal)])+
  scale_x_continuous(breaks = function(x) pretty(x, n = 10))
ggplotly(plt_sw_old)

### 5y - All years on one panel  ----
plt_sw_old5y <- ggplot() +
  geom_line(data = mw19stdr,
            mapping = aes(x = AgeN,
                          y = MeanWeight),
            size = 0.7,
            colour = ebpal[10])+
  geom_errorbar(data = mw19stdr,
                mapping = aes(x=AgeN,
                              ymin = ymin,
                              ymax = ymax),
                colour = ebpal[10],
                alpha = 0.75)+
  geom_line(data = sw5swL[sw5swL$year %in% c(2002:(DataYear+1)), ],
            mapping = aes(x = AgeN,
                          y = MeanWeight,
                          group = yearF,
                          colour = yearF)) +
  # geom_ribbon(data = sw5swL,
  #             mapping = aes(x = AgeN,
  #                           ymin = ymin,
  #                           ymax = ymax),
  #             alpha = 0.2) +
  theme_clean() +
  # scale_colour_manual(values = ebpal[3:length(ebpal)])+
  scale_x_continuous(breaks = function(x) pretty(x, n = 10))
ggplotly(plt_sw_old5y)

### 5y sliding window - Faceted by year ----
plt_sw_old5y_year <- ggplot() +
  geom_line(data = mw19stdr,
            mapping = aes(x = AgeN,
                          y = MeanWeight),
            size = 0.7,
            alpha = 0.3,
            colour = ebpal[10])+
  geom_errorbar(data = mw19stdr,
                mapping = aes(x=AgeN,
                              ymin = ymin,
                              ymax = ymax),
                colour = ebpal[10],
                alpha = 0.3)+
  geom_line(data = sw5swL[sw5swL$year %in% c(2002:(DataYear+1)), ],
            mapping = aes(x = AgeN,
                          y = MeanWeight,
                          group = yearF,
                          colour = yearF)) +
  geom_ribbon(data = sw5swL[sw5swL$year %in% c(2002:(DataYear+1)), ],
              mapping = aes(x = AgeN,
                            ymin = ymin,
                            ymax = ymax,
                            group = yearF,
                            fill = yearF),
              alpha = 0.2) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) +
  facet_wrap(.~year)
ggplotly(plt_sw_old5y_year)
#====

#===
# Clean up environment ----
#===
k <- append(k, c("WAA_combsex", "swaal", "mw19stdr", "rmwstdr", "m3wstdr", "sw3swL", "sw5swL", "sw_annual_raw"))
rm(list = ls()[!ls() %in% k])
#====