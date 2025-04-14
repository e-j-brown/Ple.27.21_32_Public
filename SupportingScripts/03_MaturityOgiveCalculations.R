#
##
### This script calculates various maturity ogives for Ple.27.21+
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
# library(splines2)
# library(colf)
library(data.table)
library(ggthemes)
DataYear <- year(Sys.time())-1
ebpal <- c("#2F3EEA", "#1FD082", "#030F4F", "#F6D04D", "#FC7634", "#F7BBB1", "#E83F48", "#008835", "#79238E", "#000000")
#====

#===
# Import pre-prepared data ----
#===
ca_hh_fin_bits_ibts <- readRDS(file = "WKBPLAICE 2024/Data/Surveys/exchange/Ple.27.21-32_ca-hh.RDS")
hl_hh_fin_bits_ibts <- readRDS(file = "WKBPLAICE 2024/Data/Surveys/exchange/Ple.27.21-32_hl-hh.RDS")
#===



#===
# First prepare maturity data for calculating MOs ----
#===
# Select data for use in this script by removing rows without relevant observations
ca<-subset(ca_hh_fin_bits_ibts, Quarter==1 & Age!=-9 & Age!=0)
hl<-subset(hl_hh_fin_bits_ibts, Quarter==1)

# Re-code the spawning status so that they are all uniform and simplified from maturity stage to binary
ca$spawner<-NA
ca$nonspawner<-NA

ca$spawner <- ifelse(ca$Maturity %in% c("61", "I", "1", "A", "B", "Bb"), 0,
                     ifelse(ca$Maturity %in% c("II", "III", "IV", "IX", "M", "62", "63", "64", "65", "2", "3", "4", "5", "Ba", "C", "Ca", "Cb", "D", "Da", "Db", "E"), 1,
                            "NA"))

ca$nonspawner <- ifelse(ca$Maturity %in% c("61", "I", "1", "A", "B", "Bb"), 1,
                        ifelse(ca$Maturity %in% c("II", "III", "IV", "IX", "M", "62", "63", "64", "65", "2", "3", "4", "5", "Ba", "C", "Ca", "Cb", "D", "Da", "Db", "E"), 0,
                               "NA"))
ca$spawner <- as.numeric(as.character(ca$spawner))
ca$nonspawner <- as.numeric(as.character(ca$nonspawner))

# Sort the data
ca<-ca[order(ca$Year,ca$SubDivisio,ca$Sex,ca$Age,ca$LngtClass, ca$spawner,ca$nonspawner), ]

cax <- ca[is.na(ca$spawner) & is.na(ca$nonspawner), ]

# Calculate number of fish (mature or immature) by year+sex+age+LngtClass
maturity_lngt <- aggregate(cbind(spawner, nonspawner) ~ Year+Sex+Age+LngtClass,
                           data = ca,
                           FUN=sum)
#====

#===
# Calculate length frequencies from at sea observations ----
#===
# Establish parameters for calculations
lencl<-c(4:60)
age<-c(1:10)
year<-c(1999:(DataYear+1))

# Make table for length freq data
length_freq_tmp<-by(hl,list(hl$Year,round(hl$LngtClass)),function (x){
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

# Replace NA with 0
Lfreq_sea[which(is.na(Lfreq_sea$HLNoLngt)),names(Lfreq_sea)==as.character("HLNoLngt")]<-0
#====

#===
# Calculate proportions from sampled data (observations) ----
#===
# Make a table for number of fish sampled, for spawner and non-spawner
nr_fish_samp_sp_F<-data.frame(year=rep(year,each=length(lencl)),
                              lencl=rep(lencl,times=length(year)),
                              a1=NA,a2=NA,a3=NA,a4=NA,a5=NA,a6=NA,a7=NA,a8=NA,a9=NA,a10=NA)
nr_fish_samp_nonsp_F<-nr_fish_samp_sp_F
nr_fish_samp_sp_M<-nr_fish_samp_sp_F
nr_fish_samp_nonsp_M<-nr_fish_samp_sp_F

# Fill the table with how many fish have been sampled (mature, immature)
## Female
for (i in 1:nrow(nr_fish_samp_sp_F)){
  for (j in 3:ncol(nr_fish_samp_sp_F)){
    lencl_i<-nr_fish_samp_sp_F[i,names(nr_fish_samp_sp_F)==as.character("lencl")]
    age_i<- j-2
    year_i<-nr_fish_samp_sp_F[i,names(nr_fish_samp_sp_F)==as.character("year")]
    
    if(lencl_i %in% maturity_lngt[which(maturity_lngt$Year == year_i
                                        & maturity_lngt$Sex == as.character("F")
                                        & maturity_lngt$Age == age_i),
                                  names(maturity_lngt)==as.character("LngtClass")]){
      
      show_data<-maturity_lngt[which(maturity_lngt$Year==year_i
                                     &maturity_lngt$LngtClass==lencl_i
                                     &maturity_lngt$Sex==as.character("F")
                                     &maturity_lngt$Age==age_i),]
      
      nr_fish_samp_sp_F[i,j]<-sum(show_data$spawner)
      nr_fish_samp_nonsp_F[i,j]<-sum(show_data$nonspawner)
      
    } else{
      
      nr_fish_samp_sp_F[i,j]<-0
      nr_fish_samp_nonsp_F[i,j]<-0
    }
  }
}

## Male
for (i in 1:nrow(nr_fish_samp_sp_M)){
  for (j in 3:ncol(nr_fish_samp_sp_M)){
    
    lencl_i<-nr_fish_samp_sp_M[i,names(nr_fish_samp_sp_M)==as.character("lencl")]
    age_i<- j-2
    year_i<-nr_fish_samp_sp_M[i,names(nr_fish_samp_sp_M)==as.character("year")]
    
    if(lencl_i%in%maturity_lngt[which(maturity_lngt$Year==year_i&maturity_lngt$Sex==as.character("M")&maturity_lngt$Age==age_i),names(maturity_lngt)==as.character("LngtClass")]){
      
      show_data<-maturity_lngt[which(maturity_lngt$Year==year_i&maturity_lngt$LngtClass==lencl_i&maturity_lngt$Sex==as.character("M")&maturity_lngt$Age==age_i),]
      
      nr_fish_samp_sp_M[i,j]<-sum(show_data$spawner)
      nr_fish_samp_nonsp_M[i,j]<-sum(show_data$nonspawner)
      
    } else{
      
      nr_fish_samp_sp_M[i,j]<-0
      nr_fish_samp_nonsp_M[i,j]<-0
    }
  }
}

# Total number of fish sampled (immature+mature)  by length
nr_fish_sampled_tot<-nr_fish_samp_sp_F[,1:5]
nr_fish_sampled_tot[,3:ncol(nr_fish_sampled_tot)]<-NA
names(nr_fish_sampled_tot)[3:ncol(nr_fish_sampled_tot)]<-c("F_sp_nonsp","M_sp_nonsp","F_M_tot")

for (i in 1:nrow(nr_fish_sampled_tot)){
  f_sp<-sum(nr_fish_samp_sp_F[i,3: ncol(nr_fish_samp_sp_F)])
  f_nonsp<-sum(nr_fish_samp_nonsp_F[i,3: ncol(nr_fish_samp_nonsp_F)])
  
  m_sp<-sum(nr_fish_samp_sp_M[i,3: ncol(nr_fish_samp_sp_M)])
  m_nonsp<-sum(nr_fish_samp_nonsp_M[i,3: ncol(nr_fish_samp_nonsp_M)])
  
  nr_fish_sampled_tot[i,names( nr_fish_sampled_tot)==as.character("F_sp_nonsp")] <-f_sp+ f_nonsp
  nr_fish_sampled_tot[i,names( nr_fish_sampled_tot)==as.character("M_sp_nonsp")] <- m_sp+ m_nonsp
  nr_fish_sampled_tot[i,names( nr_fish_sampled_tot)==as.character("F_M_tot")] <- f_sp+ f_nonsp+ m_sp+ m_nonsp
}

# Total number of fish sampled, by age
sum_nr_sampl_age_sp_F<- data.frame(year=year,a1=NA,a2=NA,a3=NA,a4=NA,a5=NA,a6=NA,a7=NA,a8=NA,a9=NA,a10=NA)
sum_nr_sampl_age_nonsp_F<-sum_nr_sampl_age_sp_F
sum_nr_sampl_age_sp_M<-sum_nr_sampl_age_sp_F
sum_nr_sampl_age_nonsp_M<-sum_nr_sampl_age_sp_F

## Female
for (i in 1:nrow(sum_nr_sampl_age_sp_F)){
  for (j in 2:ncol(sum_nr_sampl_age_sp_F)){
    
    i_year<-sum_nr_sampl_age_sp_F[i,names(sum_nr_sampl_age_sp_F)==as.character("year")]
    sum_nr_sampl_age_sp_F[i,j]<-sum(nr_fish_samp_sp_F[which(nr_fish_samp_sp_F$year==i_year),names(nr_fish_samp_sp_F)==names(sum_nr_sampl_age_sp_F)[j]])
  }
}
#
for (i in 1:nrow(sum_nr_sampl_age_nonsp_F)){
  for (j in 2:ncol(sum_nr_sampl_age_nonsp_F)){
    
    i_year<-sum_nr_sampl_age_nonsp_F[i,names(sum_nr_sampl_age_nonsp_F)==as.character("year")]
    sum_nr_sampl_age_nonsp_F[i,j]<-sum(nr_fish_samp_nonsp_F[which(nr_fish_samp_nonsp_F$year==i_year),names(nr_fish_samp_nonsp_F)==names(sum_nr_sampl_age_nonsp_F)[j]])
  }
}

## Male
for (i in 1:nrow(sum_nr_sampl_age_sp_M)){
  for (j in 2:ncol(sum_nr_sampl_age_sp_M)){
    
    i_year<-sum_nr_sampl_age_sp_M[i,names(sum_nr_sampl_age_sp_M)==as.character("year")]
    sum_nr_sampl_age_sp_M[i,j]<-sum(nr_fish_samp_sp_M[which(nr_fish_samp_sp_M$year==i_year),names(nr_fish_samp_sp_M)==names(sum_nr_sampl_age_sp_M)[j]])
  }
}
#
for (i in 1:nrow(sum_nr_sampl_age_nonsp_M)){
  for (j in 2:ncol(sum_nr_sampl_age_nonsp_M)){
    
    i_year<-sum_nr_sampl_age_nonsp_M[i,names(sum_nr_sampl_age_nonsp_M)==as.character("year")]
    sum_nr_sampl_age_nonsp_M[i,j]<-sum(nr_fish_samp_nonsp_M[which(nr_fish_samp_nonsp_M$year==i_year),names(nr_fish_samp_nonsp_M)==names(sum_nr_sampl_age_nonsp_M)[j]])
  }
}
#====

#===
# Raise proportions to unsampled strata ----
#===
# Make tables 
nr_fish_sea_sp_F<-nr_fish_samp_sp_F
nr_fish_sea_sp_F[,3:ncol(nr_fish_sea_sp_F)]<-NA

nr_fish_sea_nonsp_F<-nr_fish_sea_sp_F
nr_fish_sea_sp_M<-nr_fish_sea_sp_F
nr_fish_sea_nonsp_M<-nr_fish_sea_sp_F

# Numbers at sea by maturity class by length
## Female spawner 
for (i in 1:nrow(nr_fish_sea_sp_F)){
  for (j in 3:ncol(nr_fish_sea_sp_F)){
    
    if(nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_sp_nonsp")]>0){ 
      mat_tot_ratio<-nr_fish_samp_sp_F[i,j]/nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_sp_nonsp")]
      female_tot_ratio<-nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_sp_nonsp")]/nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_M_tot")]
      
      tot_len<-Lfreq_sea[i,names(Lfreq_sea)==as.character("HLNoLngt")]
      nr_fish_sea_sp_F[i,j]<-mat_tot_ratio* female_tot_ratio*tot_len
    }else{
      nr_fish_sea_sp_F[i,j]<-0
    }
  }
} 

## Female nonspawner
for (i in 1:nrow(nr_fish_sea_nonsp_F)){
  for (j in 3:ncol(nr_fish_sea_nonsp_F)){
    
    if(nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_sp_nonsp")]>0){ 
      mat_tot_ratio<-nr_fish_samp_nonsp_F[i,j]/nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_sp_nonsp")]
      female_tot_ratio<-nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_sp_nonsp")]/nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_M_tot")]
      
      tot_len<-Lfreq_sea[i,names(Lfreq_sea)==as.character("HLNoLngt")]
      nr_fish_sea_nonsp_F[i,j]<-mat_tot_ratio* female_tot_ratio*tot_len
    }else{
      nr_fish_sea_nonsp_F[i,j]<-0
    }
  }
} 

## Male spawner
for (i in 1:nrow(nr_fish_sea_sp_M)){
  for (j in 3:ncol(nr_fish_sea_sp_M)){
    
    if(nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("M_sp_nonsp")]>0){ 
      mat_tot_ratio<-nr_fish_samp_sp_M[i,j]/nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("M_sp_nonsp")]
      male_tot_ratio<-nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("M_sp_nonsp")]/nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_M_tot")]
      
      tot_len<-Lfreq_sea[i,names(Lfreq_sea)==as.character("HLNoLngt")]
      nr_fish_sea_sp_M[i,j]<-mat_tot_ratio* male_tot_ratio*tot_len
    }else{
      nr_fish_sea_sp_M[i,j]<-0
    }
  }
} 

## Male non-spawner
for (i in 1:nrow(nr_fish_sea_nonsp_M)){
  for (j in 3:ncol(nr_fish_sea_nonsp_M)){
    
    if(nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("M_sp_nonsp")]>0){ 
      mat_tot_ratio<-nr_fish_samp_nonsp_M[i,j]/nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("M_sp_nonsp")]
      male_tot_ratio<-nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("M_sp_nonsp")]/nr_fish_sampled_tot[i,names(nr_fish_sampled_tot)==as.character("F_M_tot")]
      
      tot_len<-Lfreq_sea[i,names(Lfreq_sea)==as.character("HLNoLngt")]
      nr_fish_sea_nonsp_M[i,j]<-mat_tot_ratio* male_tot_ratio*tot_len
    }else{
      nr_fish_sea_nonsp_M[i,j]<-0
    }
  }
} 

# Sum of numbers at sea, by maturity class, by ages (sum over length-groups) - generate tables
sum_nr_sea_age_sp_F<-sum_nr_sampl_age_sp_F
sum_nr_sea_age_sp_F[,-1]<-NA 

sum_nr_sea_age_nonsp_F<- sum_nr_sea_age_sp_F
sum_nr_sea_age_sp_M<-sum_nr_sea_age_sp_F
sum_nr_sea_age_nonsp_M<-sum_nr_sea_age_sp_F

## Female spawner
for (i in 1:nrow(sum_nr_sea_age_sp_F)){
  for (j in 2:ncol(sum_nr_sea_age_sp_F)){
    
    i_year<-sum_nr_sea_age_sp_F[i,names(sum_nr_sea_age_sp_F)==as.character("year")]
    sum_nr_sea_age_sp_F[i,j]<-sum(nr_fish_sea_sp_F[which(nr_fish_sea_sp_F$year==i_year),names(nr_fish_sea_sp_F)==names(sum_nr_sea_age_sp_F)[j]])
  }
}

## Female non-spawner
for (i in 1:nrow(sum_nr_sea_age_nonsp_F)){
  for (j in 2:ncol(sum_nr_sea_age_nonsp_F)){
    
    i_year<-sum_nr_sea_age_nonsp_F[i,names(sum_nr_sea_age_nonsp_F)==as.character("year")]
    sum_nr_sea_age_nonsp_F[i,j]<-sum(nr_fish_sea_nonsp_F[which(nr_fish_sea_nonsp_F$year==i_year),names(nr_fish_sea_nonsp_F)==names(sum_nr_sea_age_nonsp_F)[j]])
  }
}

## Male spawner
for (i in 1:nrow(sum_nr_sea_age_sp_M)){
  for (j in 2:ncol(sum_nr_sea_age_sp_M)){
    
    i_year<-sum_nr_sea_age_sp_M[i,names(sum_nr_sea_age_sp_M)==as.character("year")]
    sum_nr_sea_age_sp_M[i,j]<-sum(nr_fish_sea_sp_M[which(nr_fish_sea_sp_M$year==i_year),names(nr_fish_sea_sp_M)==names(sum_nr_sea_age_sp_M)[j]])
  }
}

## Male non-spawner
for (i in 1:nrow(sum_nr_sea_age_nonsp_M)){
  for (j in 2:ncol(sum_nr_sea_age_nonsp_M)){
    
    i_year<-sum_nr_sea_age_nonsp_M[i,names(sum_nr_sea_age_nonsp_M)==as.character("year")]
    sum_nr_sea_age_nonsp_M[i,j]<-sum(nr_fish_sea_nonsp_M[which(nr_fish_sea_nonsp_M$year==i_year),names(nr_fish_sea_nonsp_M)==names(sum_nr_sea_age_nonsp_M)[j]])
  }
}
#====

#===
# Calculate the sex ratio ----
#===
sex_ratio_female<-sum_nr_sea_age_sp_F
sex_ratio_female[,-1]<-NA

for (j in 2:ncol(sex_ratio_female)){
  sex_ratio_female[,j]<-(sum_nr_sea_age_nonsp_F[,j]+sum_nr_sea_age_sp_F[,j])/(sum_nr_sea_age_nonsp_F[,j]+sum_nr_sea_age_sp_F[,j]+sum_nr_sea_age_nonsp_M[,j]+sum_nr_sea_age_sp_M[,j])
}
#====

#===
# Calculate Maturity Ogives ----
#===
# Generate tables for female, male and combined ogives
mat_ogive_F<-sum_nr_sampl_age_sp_F
mat_ogive_F[,-1]<-NA
mat_ogive_M<- mat_ogive_F
mat_ogive_combsex<- mat_ogive_F

# Female mature
for (i in 1:nrow (mat_ogive_F)){
  for (j in 2:ncol(mat_ogive_F)){
    mat_ogive_F[i,j]<- sum_nr_sea_age_sp_F[i,j]/(sum_nr_sea_age_sp_F[i,j]+sum_nr_sea_age_nonsp_F[i,j]) 
  }
}  
# Male mature
for (i in 1:nrow (mat_ogive_M)){
  for (j in 2:ncol(mat_ogive_M)){
    mat_ogive_M[i,j]<- sum_nr_sea_age_sp_M[i,j]/(sum_nr_sea_age_sp_M[i,j]+sum_nr_sea_age_nonsp_M[i,j]) 
  }
}  

# Combsex mature                  
for (i in 1:nrow (mat_ogive_combsex)){
  for (j in 2:ncol(mat_ogive_combsex)){
    mat_ogive_combsex[i,j]<-(mat_ogive_F[i,j]*sex_ratio_female[i,j])+(mat_ogive_M[i,j]*(1-sex_ratio_female[i,j])) 
  }  
}
#====

#===
# Combined Sexes reshape and calculate means +/- 95%CI for plotting ----
#===
## Complete timeseries of annual maturity ogives ----
mo <- reshape(mat_ogive_combsex,
              varying = colnames(mat_ogive_combsex)[-1],
              v.names = "PropMat",
              timevar = "Age",
              times = colnames(mat_ogive_combsex)[-1],
              idvar = "year",
              direction = "long")

## 15 year sliding window mean maturity ogives ----
mprop <- aggregate(PropMat ~ Age,
                   data = mo[mo$year %in% c(((DataYear+1)-14):(DataYear+1)),],
                   FUN = mean)
mstdr <- aggregate(PropMat ~ Age,
                   data = mo[mo$year %in% c(((DataYear+1)-14):(DataYear+1)), ],
                   FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
mstdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = mstdr$Age))
colnames(mstdr)[colnames(mstdr) %in% c("PropMat")] <- "StdErr"
mstdr$PropMat <- mprop$PropMat
mstdr$ymax <- mstdr$PropMat + 1.96*mstdr$StdErr
mstdr$ymin <- mstdr$PropMat - 1.96*mstdr$StdErr
mprop$year <- as.factor(rep("15y Mean", n = nrow(mprop)))

## Full assessment period mean maturity ogives ----
rmprop <- aggregate(PropMat ~ Age,
                    data = mo[mo$year %in% c(2002:(DataYear+1)),],
                    FUN = mean)
rmstdr <- aggregate(PropMat ~ Age,
                    data = mo[mo$year %in% c(2002:(DataYear+1)), ],
                    FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
rmstdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = rmstdr$Age))
colnames(rmstdr)[colnames(rmstdr) %in% c("PropMat")] <- "StdErr"
rmstdr$PropMat <- rmprop$PropMat
rmstdr$ymax <- rmstdr$PropMat + 1.96*rmstdr$StdErr
rmstdr$ymin <- rmstdr$PropMat - 1.96*rmstdr$StdErr
rmprop$year <- as.factor(rep("Running Mean", n = nrow(rmprop)))
rownames(rmstdr) <- NULL
mo <- rbind(mo, mprop, rmprop)
mo$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = mo$Age))
mo$yearF <- as.factor(as.character(mo$year))


# saveRDS(mo, file = "WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesLongAve_1999-AssYear.RDS")
# saveRDS(mstdr, file("WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesAveStdErr_15yr.RDS"))
# saveRDS(rmstdr, file("WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesAveStdErr_2002-AssYear.RDS"))
#=====

#===
# Female Only reshape and calculate means +/- 95%CI for plotting ----
#===
## Complete timeseries of annual maturity ogives ----
fmo <- reshape(mat_ogive_F,
               varying = colnames(mat_ogive_F)[-1],
               v.names = "PropMat",
               timevar = "Age",
               times = colnames(mat_ogive_F)[-1],
               idvar = "year",
               direction = "long")

## 15 year sliding window mean maturity ogives ----
fmprop <- aggregate(PropMat ~ Age,
                    data = fmo[fmo$year %in% c(((DataYear+1)-14):(DataYear+1)),],
                    FUN = mean)
fmstdr <- aggregate(PropMat ~ Age,
                    data = fmo[fmo$year %in% c(((DataYear+1)-14):(DataYear+1)), ],
                    FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
fmstdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = fmstdr$Age))
colnames(fmstdr)[colnames(fmstdr) %in% c("PropMat")] <- "StdErr"
fmstdr$PropMat <- fmprop$PropMat
fmstdr$ymax <- fmstdr$PropMat + 1.96*fmstdr$StdErr
fmstdr$ymin <- fmstdr$PropMat - 1.96*fmstdr$StdErr
fmprop$year <- as.factor(rep("15y Mean", n = nrow(fmprop)))

## Full assessment period mean maturity ogives ----
rfmprop <- aggregate(PropMat ~ Age,
                     data = fmo[fmo$year %in% c(2002:(DataYear+1)),],
                     FUN = mean)
rfmstdr <- aggregate(PropMat ~ Age,
                     data = fmo[fmo$year %in% c(2002:(DataYear+1)), ],
                     FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
rfmstdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = rfmstdr$Age))
colnames(rfmstdr)[colnames(rfmstdr) %in% c("PropMat")] <- "StdErr"
rfmstdr$PropMat <- rfmprop$PropMat
rfmstdr$ymax <- rfmstdr$PropMat + 1.96*rfmstdr$StdErr
rfmstdr$ymin <- rfmstdr$PropMat - 1.96*rfmstdr$StdErr
rfmprop$year <- as.factor(rep("Running Mean", n = nrow(rfmprop)))
rownames(rfmstdr) <- NULL
fmo <- rbind(fmo, fmprop, rfmprop)
fmo$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = fmo$Age))
fmo$yearF <- as.factor(as.character(fmo$year))


# saveRDS(fmo, file = "WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesFemaleLong_1999-AssYear.RDS")
# saveRDS(fmstdr, file("WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesFemaleAveStdErr_15yr.RDS"))
# saveRDS(rfmstdr, file("WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesFemaleAveStdErr_2002-AssYear.RDS"))
#=====

#===
# Male Only reshape and calculate means +/- 95%CI for plotting ----
#===
## Complete timeseries of annual maturity ogives ----
mmo <- reshape(mat_ogive_M,
               varying = colnames(mat_ogive_M)[-1],
               v.names = "PropMat",
               timevar = "Age",
               times = colnames(mat_ogive_M)[-1],
               idvar = "year",
               direction = "long")

## 15 year sliding window mean maturity ogives ----
mmprop <- aggregate(PropMat ~ Age,
                    data = mmo[mmo$year %in% c(((DataYear+1)-14):(DataYear+1)),],
                    FUN = mean)
mmstdr <- aggregate(PropMat ~ Age,
                    data = mmo[mmo$year %in% c(((DataYear+1)-14):(DataYear+1)), ],
                    FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
mmstdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = mmstdr$Age))
colnames(mmstdr)[colnames(mmstdr) %in% c("PropMat")] <- "StdErr"
mmstdr$PropMat <- mmprop$PropMat
mmstdr$ymax <- mmstdr$PropMat + 1.96*mmstdr$StdErr
mmstdr$ymin <- mmstdr$PropMat - 1.96*mmstdr$StdErr
mmprop$year <- as.factor(rep("15y Mean", n = nrow(mmprop)))

## Full assessment period mean maturity ogives ----
rmmprop <- aggregate(PropMat ~ Age,
                     data = mmo[mmo$year %in% c(2002:(DataYear+1)),],
                     FUN = mean)
rmmstdr <- aggregate(PropMat ~ Age,
                     data = mmo[mmo$year %in% c(2002:(DataYear+1)), ],
                     FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
rmmstdr$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = rmmstdr$Age))
colnames(rmmstdr)[colnames(rmmstdr) %in% c("PropMat")] <- "StdErr"
rmmstdr$PropMat <- rmmprop$PropMat
rmmstdr$ymax <- rmmstdr$PropMat + 1.96*rmmstdr$StdErr
rmmstdr$ymin <- rmmstdr$PropMat - 1.96*rmmstdr$StdErr
rmmprop$year <- as.factor(rep("Running Mean", n = nrow(rmmprop)))
rownames(rmmstdr) <- NULL
mmo <- rbind(mmo, mmprop, rmmprop)
mmo$AgeN <- as.numeric(sub(pattern = "a", replacement = "", x = mmo$Age))
mmo$yearF <- as.factor(as.character(mmo$year))


# saveRDS(mmo, file = "WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesMaleLong_1999-AssYear.RDS")
# saveRDS(mmstdr, file("WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesMaleAveStdErr_15yr.RDS"))
# saveRDS(rmmstdr, file("WKBPLAICE 2024/Data/BiologicalData/Ple.27.21-32_MaturitiesMaleAveStdErr_2002-AssYear.RDS"))
#=====

#===
# Calculate Sliding window mean FEMALE maturity ----
#===
## Calculate 3y sliding window average for whole timeseries ----
### Calculate the mean ----
mo3sw <- frollmean(mat_ogive_F[, -1], 3, na.rm = T)
mo3sw <- as.data.frame(do.call(cbind, mo3sw))
mo3sw$year <- mat_ogive_F$year

#### Wide to long format
mo3swL <- reshape(mo3sw,
                  varying = colnames(mo3sw)[-11],
                  v.names = "PropMat",
                  timevar = "Age",
                  times = colnames(mo3sw)[-11],
                  idvar = "year",
                  direction = "long")
mo3swL$Age <- as.numeric(gsub(pattern = "V", replacement = "", x = mo3swL$Age))

### Calcualte the standard error ----
sw3stdr <- frollapply(mat_ogive_F[, -1],
                      3,
                      FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
sw3stdr <- as.data.frame(do.call(cbind, sw3stdr))
sw3stdr$year <- mat_ogive_F$year

#### Wide to long format
sw3stdrL <- reshape(sw3stdr,
                    varying = colnames(sw3stdr)[-11],
                    v.names = "StdErrPropMat",
                    timevar = "Age",
                    times = colnames(sw3stdr)[-11],
                    idvar = "year",
                    direction = "long")
sw3stdrL$Age <- as.numeric(gsub(pattern = "V", replacement = "", x = sw3stdrL$Age))

### Calculate 95% confidence intervals ----
mo3swL <- merge(x = mo3swL, y = sw3stdrL, by = c("year", "Age"), all.x = TRUE)
mo3swL$ymax <- mo3swL$PropMat + (1.96*mo3swL$StdErrPropMat)
mo3swL$ymin <- mo3swL$PropMat - (1.96*mo3swL$StdErrPropMat)
mo3swL$AgeN <- mo3swL$Age
mo3swL$Age <- as.character(mo3swL$Age)

## Calculate 5y sliding window average for whole timeseries ----
### Calculate the mean ----
mo5sw <- frollmean(mat_ogive_F[, -1], 5, na.rm = T)
mo5sw <- as.data.frame(do.call(cbind, mo5sw))
#### Make the first year equal to the first calculable 5y average
mo5sw[year == 2002, -ncol(mo5sw)] <- mo5sw[year == 2003, -ncol(mo5sw)]
mo5sw$year <- mat_ogive_F$year

#### Wide to long format
mo5swL <- reshape(mo5sw,
                  varying = colnames(mo5sw)[-11],
                  v.names = "PropMat",
                  timevar = "Age",
                  times = colnames(mo5sw)[-11],
                  idvar = "year",
                  direction = "long")
mo5swL$Age <- as.numeric(gsub(pattern = "V", replacement = "", x = mo5swL$Age))

### Calcualte the standard error ----
sw5stdr <- frollapply(mat_ogive_F[, -1],
                      5,
                      FUN = function(x){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))})
sw5stdr <- as.data.frame(do.call(cbind, sw5stdr))
#### Make the first year equal to the first calculable 5y average
sw5stdr[year == 2002, -ncol(sw5stdr)] <- sw5stdr[year == 2003, -ncol(sw5stdr)]
sw5stdr$year <- mat_ogive_F$year

#### Wide to long format
sw5stdrL <- reshape(sw5stdr,
                    varying = colnames(sw5stdr)[-11],
                    v.names = "StdErrPropMat",
                    timevar = "Age",
                    times = colnames(sw5stdr)[-11],
                    idvar = "year",
                    direction = "long")
sw5stdrL$Age <- as.numeric(gsub(pattern = "V", replacement = "", x = sw5stdrL$Age))

### Calculate 95% confidence intervals ----
mo5swL <- merge(x = mo5swL, y = sw5stdrL, by = c("year", "Age"), all.x = TRUE)
mo5swL$ymax <- mo5swL$PropMat + (1.96*mo5swL$StdErrPropMat)
mo5swL$ymin <- mo5swL$PropMat - (1.96*mo5swL$StdErrPropMat)
mo5swL$AgeN <- mo5swL$Age
mo5swL$Age <- as.character(mo5swL$Age)
#====

#===
# Extract just annual maturity values by age ----
#===
mo_annual_raw <- fmo[!fmo$year %in% c("Running Mean", "15y Mean"),]
mo_annual_raw$AgeN <- as.numeric(gsub(pattern = "a", replacement = "", x = mo_annual_raw$Age))
mo_annual_raw$Age <- as.character(mo_annual_raw$AgeN)
mo_annual_raw$year <- as.numeric(mo_annual_raw$year)
#====

#===
# Comparisons of old Maturity Options ----
#===
## Plot Combined, Male and Female 15yr recent mean ----
plt_mo_SW <- ggplot() +
  geom_line(data = mstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[5],
            linetype = 3,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = mstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[5],
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = fmstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[9],
            linetype = 3,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = fmstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[9],
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = mmstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[8],
            linetype = 3,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = mmstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[8],
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  theme_clean()+
  scale_x_continuous(breaks = function(x) pretty(x, n = 5))
ggplotly(plt_mo_SW)

## Plot running mean vs 15y sliding window vs annual (Female Only) ----
plt_mo_FO <- ggplot() +
  geom_line(data = fmo[!fmo$year %in% c("Running Mean", "15y Mean") & as.numeric(as.character(fmo$year)) >= 2002, ],
            mapping = aes(x = AgeN,
                          y = PropMat,
                          group = yearF,
                          colour = yearF)) +
  geom_line(data = fmstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[3],
            linetype = 3,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = fmstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[3],
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = rfmstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[6],
            linetype = 2,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = rfmstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[6],
                linetype = 2,
                size = 0.7,
                alpha = 0.75) +
  theme_clean()+
  scale_x_continuous(breaks = function(x) pretty(x, n = 5))
ggplotly(plt_mo_FO)

## Plot old combinded ogive (mstdr), running mean vs 15yr recent window vs annual (Female Only) ----
plt_mo_rm15FO <- ggplot() +
  geom_line(data = fmo[!fmo$year %in% c("Running Mean", "15y Mean") & as.numeric(as.character(fmo$year)) >= 2002, ],
            mapping = aes(x = AgeN,
                          y = PropMat,
                          group = yearF,
                          colour = yearF)) +
  geom_line(data = fmstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[3],
            linetype = 3,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = fmstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[3],
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = rfmstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[6],
            linetype = 2,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = rfmstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[6],
                linetype = 2,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = mstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[10],
            linetype = 3,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = mstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[10],
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  theme_clean()+
  scale_x_continuous(breaks = function(x) pretty(x, n = 5))
ggplotly(plt_mo_rm15FO)

#====

#===
# Plot maturity by ages on separate facets ----
#===
## Annual Maturity Ogives Averaged From Survey Sampling ----
plt_sw_fmo <- ggplot() +
  geom_line(data = mo_annual_raw,
            mapping = aes(x = year,
                          y = PropMat,
                          group = Age,
                          colour = Age))+
  facet_grid(rows = "AgeN", scales = "free_y")+
  theme_clean() +
  theme(legend.position = "none")
ggplotly(plt_sw_fmo)

## 3 year sliding window average ----
plt_mo_sw3 <- ggplot() +
  geom_line(data = mo3swL,
            mapping = aes(x = year,
                          y = PropMat,
                          group = Age)) +
  geom_ribbon(data = mo3swL,
              mapping = aes(x = year,
                            ymin = ymin,
                            ymax = ymax,
                            group = Age),
              alpha = 0.2) +
  geom_point(data = mo_annual_raw,
             mapping = aes(x = year,
                           y = PropMat,
                           group = Age),
             shape = 1) +
  geom_vline(xintercept = 2001.75,
             linetype = 3) +
  facet_grid(rows = "AgeN", scales = "free_y") +
  theme_clean() +
  theme(legend.position = "none")
ggplotly(plt_mo_sw3)

## 5 year sliding window average ----
plt_mo_sw5 <- ggplot() +
  geom_line(data = mo5swL,
            mapping = aes(x = year,
                          y = PropMat,
                          group = Age)) +
  geom_ribbon(data = mo5swL,
              mapping = aes(x = year,
                            ymin = ymin,
                            ymax = ymax,
                            group = Age),
              alpha = 0.2) +
  geom_point(data = mo_annual_raw,
             mapping = aes(x = year,
                           y = PropMat,
                           group = Age),
             shape = 1) +
  geom_vline(xintercept = 2001.75,
             linetype = 3) +
  facet_grid(rows = "AgeN", scales = "free_y") +
  theme_clean() +
  theme(legend.position = "none")
ggplotly(plt_mo_sw5)

#====

#===
# Compare Maturity Ogives old and new ----
#===
## Compare 3 year Female only, sliding window with combined full timeseries average ----
mo3swL$yearF <- as.factor(as.character(mo3swL$year))

### All years on one panel
plt_mo_oldNew <- ggplot() +
  geom_line(data = mstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[10],
            linetype = 3,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = mstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[10],
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = mo3swL[mo3swL$year >= 2002,],
            mapping = aes(x = AgeN,
                          y = PropMat,
                          group = yearF,
                          colour = yearF)) +
  geom_errorbar(data = mo3swL[mo3swL$year >= 2002,],
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin,
                              group = yearF,
                              colour = yearF),
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  theme_clean() +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) 
ggplotly(plt_mo_oldNew)

### Faceted by year
plt_mo_oldNew_year <- ggplot() +
  geom_line(data = mstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[10],
            linetype = 3,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = mstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[10],
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = mo3swL[mo3swL$year >= 2002,],
            mapping = aes(x = AgeN,
                          y = PropMat,
                          group = yearF,
                          colour = yearF)) +
  geom_errorbar(data = mo3swL[mo3swL$year >= 2002,],
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin,
                              group = yearF,
                              colour = yearF),
                linetype = 3,
                size = 0.7,
                alpha = 0.75) +
  theme_clean() +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) +
  facet_wrap(.~year)
ggplotly(plt_mo_oldNew_year)

## Compare 5 year Female only, sliding window with combined full timeseries average ----
mo5swL$yearF <- as.factor(as.character(mo5swL$year))

### All years on one panel
plt_mo_oldNew <- ggplot() +
  geom_line(data = mstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[10],
            linetype = 5,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = mstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[10],
                linetype = 5,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = mo5swL[mo5swL$year >= 2002,],
            mapping = aes(x = AgeN,
                          y = PropMat,
                          group = yearF,
                          colour = yearF)) +
  geom_errorbar(data = mo5swL[mo5swL$year >= 2002,],
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin,
                              group = yearF,
                              colour = yearF),
                linetype = 5,
                size = 0.7,
                alpha = 0.75) +
  theme_clean() +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) 
ggplotly(plt_mo_oldNew)

### Faceted by year
plt_mo_oldNew_year <- ggplot() +
  geom_line(data = mstdr,
            mapping = aes(x = AgeN,
                          y = PropMat),
            colour = ebpal[10],
            linetype = 5,
            size = 0.7,
            alpha = 0.75) +
  geom_errorbar(data = mstdr,
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin),
                colour = ebpal[10],
                linetype = 5,
                size = 0.7,
                alpha = 0.75) +
  geom_line(data = mo5swL[mo5swL$year >= 2002,],
            mapping = aes(x = AgeN,
                          y = PropMat,
                          group = yearF,
                          colour = yearF)) +
  geom_errorbar(data = mo5swL[mo5swL$year >= 2002,],
                mapping = aes(x = AgeN,
                              ymax = ymax,
                              ymin = ymin,
                              group = yearF,
                              colour = yearF),
                linetype = 5,
                size = 0.7,
                alpha = 0.75) +
  theme_clean() +
  scale_x_continuous(breaks = function(x) pretty(x, n = 5)) +
  facet_wrap(.~year)
ggplotly(plt_mo_oldNew_year)
#====
#====

# #===
# # Attempt at fitting a spline to maturity ogives ----
# #===
# ## Fit spline to female only data from last 15 years ----
# fmo_red <- fmo[fmo$year %in% c(((DataYear+1)-14):(DataYear+1)),]
# fmomo <- colf_nls(PropMat ~ 1 + iSpline(AgeN, df=8),
#                   data=fmo_red,
#                   lower=c(-Inf, rep(0, 8)),
#                   control=nls.control(maxiter=1000,
#                                       tol=1e-09,
#                                       minFactor=1/2048))
# 
# plt_mo_FS <- ggplot(data = fmo_red,
#                     mapping = aes(x = AgeN,
#                                   y = PropMat)) +
#   geom_point(mapping = aes(group = year,
#                            colour = year)) +
#   geom_smooth(method = colf::colf_nls,
#               data = fmo_red,
#               formula = PropMat ~ 1 + iSpline(AgeN, df=8),
#               method.args = list(lower=c(-Inf, rep(0, 8)),
#                                  control=nls.control(maxiter=1000,
#                                                      tol=1e-09,
#                                                      minFactor=1/2048))) +
#   theme_clean()+
#   scale_x_continuous(breaks = function(x) pretty(x, n = 5))
# ggplotly(plt_mo_FS)
# #====

write.csv(mat_ogive_combsex[!mat_ogive_combsex$year %in% c(1999:2001), ],
          file = "WKBPLAICE 2024/Data/BiologicalData/ple_27_21-23_MO_combsex_annual.csv")

#===
# Clean Data Environment ----
#===
k <- append(k, c("mo", "mstdr", "rmstdr", "fmo", "fmstdr", "rfmstdr", "mmo", "mmstdr", "rmmstdr", "mat_ogive_F", "mat_ogive_M", "mat_ogive_combsex", "mo3swL", "mo5swL", "mo_annual_raw"))
rm(list = ls()[!ls() %in% k])
#=====