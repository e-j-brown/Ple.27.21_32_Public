set.seed(1)
mc.cores = 2
library(surveyIndex);
library(maps); library(mapdata);

sti="~/Documents/DATRAS";

## Species specific parameters:
cmSize=1;
spectrumMax=110;
ages=1:7
years=1992:2025   ### OBS OBS - remember to update this to include newest year!
outFolder=".";
genus="Pleuronectes"
bfamily="platessa";
quarter=1

fnam <- "PlaiceData2.RData"

if(!file.exists(fnam)){
    IBTS <- readExchangeDir(paste(sti,"/exchange/IBTS/",sep=""),strict=FALSE)
    
    IBTS<-addSpatialData(IBTS,"~/Documents/shapefiles/ICES_areas.shp")
    
    IBTS <-subset(IBTS,Species==paste(genus,bfamily),ICES_SUB %in% as.character(c("20","21","22","23","24")),HaulVal %in% c("V","C","A"),StdSpecRecCode %in% c(1,3),Year %in% years)
    
    
    BITS <- readExchangeDir(paste(sti,"/exchange/BITS/",sep=""),strict=FALSE)
    
    BITS<-addSpatialData(BITS,"~/Documents/shapefiles/ICES_areas.shp")
    
    
    BITS <-subset(BITS,Species==paste(genus,bfamily),ICES_SUB %in% as.character(c("20","21","22","23","24","25")),HaulVal %in% c("V","C","A","N"),StdSpecRecCode %in% c(1,3),Year %in% years)
    
    d <- c(IBTS,BITS)
    save(d,file=fnam,version=2)
} else {
    load(fnam)
}

if(FALSE){    
    load("cods_2008_2023.RData")
    
    class(cods_2008_2023)<-"DATRASraw"
    
    cods_2008_2023 <- DATRAS:::addExtraVariables(cods_2008_2023)
    ## OBS OBS, check!!
    cods_2008_2023[[1]]$LngtClas = cods_2008_2023[[1]]$LngtClass
    
    cods_2008_2023[[1]]$Age = cods_2008_2023[[1]]$AgeRings
    cods_2008_2023[[1]]$NoAtALK = cods_2008_2023[[1]]$CANoAtLngt
    
    factors = c("Gear","Country","Year","Ship")
    for(i in 1:3){
        for(j in factors){
            cods_2008_2023[[i]][,j] = factor(cods_2008_2023[[i]][,j])
        }
    }
    
    numerics = c("Age","NoAtALK","IndWgt")
    for(j in numerics){
        cods_2008_2023[[1]][,j] = as.numeric(cods_2008_2023[[1]][,j])
    }
    
    
    cods_2008_2023 <- subset(cods_2008_2023,Species==paste(genus,bfamily))
    cods_2008_2023<-addSpatialData(cods_2008_2023,"~/Documents/shapefiles/ICES_areas.shp")
    
    names(d[[1]])
    names(cods_2008_2023[[1]])
    
    cods_2008_2023
    
    test = c(d,cods_2008_2023)
    
    d = test
}

d$Gear[d$Gear=="OTB"] = "DTU70"
d$Gear[d$Gear=="TV "] = "DTU70"

xtabs(~Year+Gear+Quarter,d[[2]])

xtabs(~Gear,d[[2]])

xtabs(~Gear+Quarter,d[[2]])

xtabs(~Ship+Gear,d[[2]])

d <- addSpectrum(d,by=1)
d$Ntot <- rowSums(d$N)
aggregate(Ntot ~ Gear+Quarter,data=d[[2]],FUN=median)
gtab2 <- aggregate(Ntot ~ Gear+Quarter,data=d[[2]],FUN=sum)
gtab2 <- aggregate(Ntot ~ Gear,data=d[[2]],FUN=sum)
aggregate(log(Ntot+1) ~ Gear + Quarter,data=d[[2]],FUN=mean)
xtabs(Ntot~Gear+Quarter,d[[2]])
## GOV,TVS,TVL + evt. H20, FOT

d <- subset(d, Gear %in% c("GOV","TVS","TVL","DTU70") & Quarter %in% c("1","3","4"))

##d$Gear <- factor(d$Gear,levels=c("TVS","TVL","GOV"))

## Take care of "no oxygen" hauls
d$HaulDur[ d$HaulDur<1 | is.na(d$HaulDur) ] = 10

d=addSpectrum(d,cm.breaks=seq(0,spectrumMax,by=cmSize))

cms=attr(d,"cm.breaks");

## impute missing depths
summary(d$Depth)

dmodel=gam(Depth ~ s(lon,lat,k=200),data=d[[2]])
sel=subset(d,is.na(Depth))
sel$Depth=0; ## Guard against NA-error
d$Depth[is.na(d$Depth)]=predict(dmodel,newdata=sel[[2]])

## Check for enough age data in all years
xtabs(NoAtALK~Year+Age+Quarter,data=d[[1]])


dQ1 = subset(d,Quarter=="1")
dQ34 = subset(d,Quarter %in% c("3","4"))

dQ34[[1]] = subset(dQ34[[1]],Age>=1)
dQ34[[3]] = subset(dQ34[[3]],LngtCm>=10) ## Drop lengths < 10 cm (age 0) 
## Drop -95 age

xtabs(NoAtALK~Year+Age,data=dQ1[[1]]) ## max age around 7
xtabs(NoAtALK~Year+Age,data=dQ34[[1]]) ## max age around 7

##dQ34=fixAgeGroup(dQ34,0,n=3,fun=min)
dQ1=fixAgeGroup(dQ1,1,fun=min) ## 1992 and 1996 no age 1


dQ1=addSpectrum(dQ1,cm.breaks=seq(0,spectrumMax,by=cmSize))
dQ34=addSpectrum(dQ34,cm.breaks=seq(0,spectrumMax,by=cmSize))

## age max is ok (for now)
dQ1=fixAgeGroup(dQ1,age=max(ages),n=1,fun=max)
dQ34=fixAgeGroup(dQ34,age=max(ages),n=1,fun=max)


dQ1.ysplit = split(dQ1,dQ1$Year)

dQ34 = subset(dQ34,!Year %in% as.character(1991:1996))
dQ34.ysplit = split(dQ34,dQ34$Year) ## no ages until 1997!!!!



#######################
## Age-length keys
#######################
## Declare settings for ALK model
mf = "" 
ack=TRUE;
useBICs=TRUE;
varCofs=FALSE;
maxKs=50;


dQ1.ALK= mclapply(dQ1.ysplit,fitALK,minAge=min(ages),maxAge=max(ages),autoChooseK=ack,useBIC=useBICs,varCof=varCofs,maxK=maxKs,mc.cores=1)
dQ34.ALK= mclapply(dQ34.ysplit,fitALK,minAge=min(ages),maxAge=max(ages),autoChooseK=ack,useBIC=useBICs,varCof=varCofs,maxK=maxKs,mc.cores=1)




dQ1.Nage=mclapply(dQ1.ALK,predict,mc.cores=2)
dQ34.Nage=mclapply(dQ34.ALK,predict,mc.cores=2)

for(i in 1:length(dQ1.ALK)) dQ1.ysplit[[i]]$Nage=dQ1.Nage[[i]];
for(i in 1:length(dQ34.ALK)) dQ34.ysplit[[i]]$Nage=dQ34.Nage[[i]];

ddQ1 <- do.call("c",dQ1.ysplit)
ddQ34 <- do.call("c",dQ34.ysplit)

dQ1.ysplit=NULL;
dQ34.ysplit=NULL;

dd = c(ddQ1,ddQ34)

gc()

dd = removeZeroClusters(dd)


########################
## Prediction grid
########################

dd$ctime = as.numeric(as.character(dd$Year))

grid = getBathyGrid(dd,minDepth=5,maxDepth=250,resolution=8,maxDist=0.2,shapefile="~/Documents/shapefiles/ICES_areas.shp",select="ICES_SUB")

grid = subset(grid,ICES_SUB %in% as.character(21:25))

dd$Semester = factor(ifelse(dd$Quarter=="1","1","34"))

dd$YearQ = factor(paste(dd$Year,dd$Semester))
    
dd$Gear2 = as.character(dd$Gear)
dd$Gear3 = dd$Gear2

dd$Gear3[dd$Gear3=="TVL"] = "TVLS"
dd$Gear3[dd$Gear3=="TVS"] = "TVLS"


dd$Gear2[dd$Gear2=="TVL"] = "TVLS"
dd$Gear2[dd$Gear2=="TVS"] = "TVLS"
dd$Gear2[dd$Gear2=="GOV"] = "TVLS"

dd$Gear2 = factor(dd$Gear2)
dd$Gear3 = factor(dd$Gear3)

dd$HaulDur2 = dd$HaulDur
dd$HaulDur2[dd$Gear=="TVL"] = dd$HaulDur2[dd$Gear=="TVL"] * (79/59) ##median doorspread ratio

ddsub = subset(dd,Year %in% as.character(1999:max(years))) ## Both quarters complete from 1999 and also what is used in assessment now


grid$Gear2 = "TVLS"
grid$HaulDur2 = 30

yearsQ1 = sort(as.numeric(as.character(unique(subset(ddsub,Quarter=="1")$Year))))
yearsQ34 = sort(as.numeric(as.character(unique(subset(ddsub,Semester=="34")$Year))))

## We need a grid for each year for this model, because of the YearQ factor
gridlist = list()
gridlistQ4 = list()
for(yy in levels(dd$Year)){
    gridlist[[yy]] = grid
    gridlist[[yy]]$Year=yy
    gridlist[[yy]]$YearQ=paste(yy,"1")
    gridlist[[yy]]$Semester="1"
    if(as.character(yy) %in% yearsQ34){
        gridlistQ4[[yy]] = gridlist[[yy]]
        gridlistQ4[[yy]]$YearQ=paste(yy,"34")
        gridlistQ4[[yy]]$Semester="34"
    }
}

plot(grid$lon,grid$lat)
points(dd$lon,dd$lat,pch=".",col=2,cex=3)

modelsfms <- list()
gearused <- list()
modelsfms[["base"]] = rep("Year*Semester + s(sqrt(Depth),k=5,bs='ds',m=c(1,0),by=Semester) + Gear2 + s(Ship,bs='re') + s(lon,lat,bs='ds',m=c(1,0.5),k=80,by=Semester) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=6,id=1) + offset(log(HaulDur2))",length(ages))


modelsfms[["gearEstAll"]] = rep("Year*Semester + s(sqrt(Depth),k=5,bs='ds',m=c(1,0),by=Semester) + Gear + s(Ship,bs='re') + s(lon,lat,bs='ds',m=c(1,0.5),k=80,by=Semester) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=6,id=1) + offset(log(HaulDur))",length(ages))


modelsfms[["gearEstGOV"]] = rep("Year*Semester + s(sqrt(Depth),k=5,bs='ds',m=c(1,0),by=Semester) + Gear3 + s(Ship,bs='re') + s(lon,lat,bs='ds',m=c(1,0.5),k=80,by=Semester) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=6,id=1) + offset(log(HaulDur2))",length(ages))

modelsfms[["base.noship"]] = rep("Year*Semester + s(sqrt(Depth),k=5,bs='ds',m=c(1,0),by=Semester) + Gear2  + s(lon,lat,bs='ds',m=c(1,0.5),k=80,by=Semester) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=6,id=1) + offset(log(HaulDur2))",length(ages))


##suspicious low number of zeroes for DTU70? - probably ok, same with DK dataset


################################################3

models<-list()

##system.time( models[[names(modelsfms)[1] ]] <- getSurveyIdx(ddsub,ages=ages,predD=gridlist,fam="LogNormal",modelP=modelsfms[[1]],modelZ=modelsfms[[1]],gamma=1,cutOff=0.1,predfix=list(Gear2="TVLS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))

##system.time( models[[names(modelsfms)[2] ]] <- getSurveyIdx(ddsub,ages=ages,predD=gridlist,fam="LogNormal",modelP=modelsfms[[2]],modelZ=modelsfms[[2]],gamma=1,cutOff=0.1,predfix=list(Gear="TVS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))

##system.time( models[[names(modelsfms)[3] ]] <- getSurveyIdx(ddsub,ages=ages,predD=gridlist,fam="LogNormal",modelP=modelsfms[[3]],modelZ=modelsfms[[3]],gamma=1,cutOff=0.1,predfix=list(Gear3="TVLS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))

##system.time( models[[names(modelsfms)[4] ]] <- getSurveyIdx(ddsub,ages=ages,predD=gridlist,fam="LogNormal",modelP=modelsfms[[4]],modelZ=modelsfms[[4]],gamma=1,cutOff=0.1,predfix=list(Gear2="TVLS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))

##system.time( models[["base.gamma"]] <- getSurveyIdx(ddsub,ages=ages,predD=gridlist,fam="Gamma",modelP=modelsfms[[1]],modelZ=modelsfms[[1]],gamma=1,cutOff=0.1,predfix=list(Gear2="TVLS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))

## data subsetting
ddsub.noDTU70 = subset(ddsub,Gear %in% c("TVS","TVL","GOV"))
fm = rep("Year*Semester + s(sqrt(Depth),k=5,bs='ds',m=c(1,0),by=Semester) + s(Ship,bs='re') + s(lon,lat,bs='ds',m=c(1,0.5),k=80,by=Semester) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=6,id=1) + offset(log(HaulDur2))",length(ages))
##system.time( models[["base.noDTU70"]] <- getSurveyIdx(ddsub.noDTU70,ages=ages,predD=gridlist,fam="LogNormal",modelP=fm,modelZ=fm,gamma=1,cutOff=0.1,predfix=list(Gear2="TVLS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))

##ddsub.noQ3 = subset(ddsub,Quarter!="3")
##system.time( models[["base.noQ3" ]] <- getSurveyIdx(ddsub.noQ3,ages=ages,predD=gridlist,fam="LogNormal",modelP=modelsfms[[1]],modelZ=modelsfms[[1]],gamma=1,cutOff=0.1,predfix=list(Gear2="TVLS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))

##ddsub.noGOV = subset(ddsub,Gear!="GOV")
##system.time( models[["base.noGOV"]] <- getSurveyIdx(ddsub.noGOV,ages=ages,predD=gridlist,fam="LogNormal",modelP=modelsfms[[1]],modelZ=modelsfms[[1]],gamma=1,cutOff=0.1,predfix=list(Gear2="TVLS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))


fm2 = rep("Year*Semester + s(sqrt(Depth),k=5,bs='ds',m=c(1,0),by=Semester) + s(lon,lat,bs='ds',m=c(1,0.5),k=80,by=Semester) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=6,id=1) + offset(log(HaulDur2))",length(ages))

system.time( models[["base.noDTU70noShip"]] <- getSurveyIdx(ddsub.noDTU70,ages=ages,predD=gridlist,fam="LogNormal",modelP=fm2,modelZ=fm2,gamma=1,cutOff=0.1,predfix=list(Gear2="TVLS"),control=list(trace=TRUE,maxit=20,nthreads=1),mc.cores=mc.cores))


##################

modelsQ4 = list()

ddsubQ4 = subset(ddsub,Semester=="34")

##modelsQ4[[paste(names(modelsfms)[1],"Q4") ]] <- redoSurveyIndex(ddsubQ4,models[[1]],predD=gridlistQ4,predfix=list(Gear2="TVLS"))

##modelsQ4[[paste(names(modelsfms)[2],"Q4") ]] <- redoSurveyIndex(ddsubQ4,models[[2]],predD=gridlistQ4,predfix=list(Gear2="TVLS"))

##modelsQ4[[paste(names(modelsfms)[3],"Q4") ]] <- redoSurveyIndex(ddsubQ4,models[[3]],predD=gridlistQ4,predfix=list(Gear3="TVLS"))

##modelsQ4[[paste(names(modelsfms)[4],"Q4") ]] <- redoSurveyIndex(ddsubQ4,models[[4]],predD=gridlistQ4,predfix=list(Gear2="TVLS"))

##modelsQ4[["base.gamma Q4" ]] <- redoSurveyIndex(ddsubQ4,models[[5]],predD=gridlistQ4,predfix=list(Gear2="TVLS"))

##modelsQ4[["base.noDTU70 Q4"]] <-redoSurveyIndex(ddsubQ4,models[[6]],predD=gridlistQ4,predfix=list(Gear2="TVLS"))

##modelsQ4[["base.noQ3 Q4"]] <-redoSurveyIndex(ddsubQ4,models[[7]],predD=gridlistQ4,predfix=list(Gear2="TVLS"))

##modelsQ4[["base.noGOV Q4"]] <-redoSurveyIndex(ddsubQ4,models[[8]],predD=gridlistQ4,predfix=list(Gear2="TVLS"))

modelsQ4[["base.noDTU70noShip Q4"]] <-redoSurveyIndex(ddsubQ4,models[[1]],predD=gridlistQ4,predfix=list(Gear2="TVLS"))

pdf("Q4indices.pdf",width=10,height=12)
surveyIndex:::plot.SIlist(modelsQ4,rescale=TRUE)
dev.off()

pdf("Q1indices.pdf",width=10,height=12)
surveyIndex:::plot.SIlist(models,rescale=TRUE)
dev.off()

pdf("allindices.pdf",width=10,height=12)
surveyIndex:::plot.SIlist(c(models,modelsQ4),rescale=TRUE)
dev.off()


## sapply(lapply(modelsQ4,function(x) internalCons(na.omit(x$idx))[-6]),mean)
## ICQ4=sapply(lapply(modelsQ4,function(x) internalCons(na.omit(x$idx))[-6]),mean)
## IC=sapply(lapply(models,function(x) internalCons(x$idx)[-6]),mean) 
## EC = sapply(1,function(i) mean(externalCons(models[[i]]$idx[1:25,],modelsQ4[[i]]$idx[1:25,])))

## ictab = rbind(IC,ICQ4,EC,(IC+ICQ4+EC)/3)

## rownames(ictab)[4]<-"avg"
## library(xtable)
## sink("ICtab.tex")
## print(xtable(t(ictab),digits=3,caption="Average internal (IC) and external consistency (EC) for ages 1-6 and grand average (avg) for all models."))
## sink()

##AICs = sapply(1:5,function(i) AIC.surveyIdx(models[[i]]))
##BICs = sapply(1:5,function(i) AIC.surveyIdx(models[[i]],TRUE))

##dAIC = AICs -min(AICs)
##dBIC = BICs -min(BICs)


library(FLCore)
tofli<-function(x){
    fli <- FLQuant(t(x),dimnames=list(age=colnames(x),year=rownames(x)))
    fli <- FLIndex(index=fli)
}
pdf("ICQ1.pdf")
plot(tofli(models[[1]]$idx),type="internal") ##,use.rsq=FALSE)
dev.off()
pdf("ICQ4.pdf")
plot(tofli(modelsQ4[[1]]$idx),type="internal") ##,use.rsq=FALSE)
dev.off()



###########################

exportSI(models[[1]]$idx,ages,as.numeric(rownames(models[[1]]$idx)),toy=mean(subset(ddsub,Quarter=="1")$timeOfYear),file="Plaice2125-Q1.dat",nam="Q1 IBTS+BITS 1-7+")
write.table(models[[1]]$idx.CV,"Plaice2125-Q1-CV.csv")

tmp = na.omit(modelsQ4[[1]]$idx)
exportSI(tmp,ages,as.numeric(rownames(tmp)),toy=mean(subset(ddsub,Semester=="34")$timeOfYear),file="Plaice2125-Q4.dat",nam="Q4 IBTS+BITS 1-7+")
write.table(na.omit(modelsQ4[[1]]$idx.CV),"Plaice2125-Q4-CV.csv")

resids = list()
for(aa in 1:7) { cat("Residuals age ",aa,"\n"); resids[[aa]] <- residuals(models[[1]],a=aa) }

png("qqplots.png",width=1200,height=900,pointsize=20)
par(mfrow=c(3,3))
for(aa in 1:7){
    qqnorm(resids[[aa]],main=paste0("Age ",aa))
    abline(0,1,col=2)
}
dev.off()

png("residbygear.png",width=1200,height=900,pointsize=20)
par(mfrow=c(3,3),mar=c(4,4,4,4))
for(aa in 1:7){
    plot(ddsub.noDTU70$Gear,resids[[aa]],main=paste0("Age ",aa))
    abline(h=0,col=2)
}
dev.off()

allresids = do.call(c,resids)
png("residbyship.png",width=1200,height=900,pointsize=20)
par(mfrow=c(1,1))
plot(rep(ddsub.noDTU70$Ship,7),allresids,main="Residuals by ship")
abline(h=0,col=2)
dev.off()

########
## Maps
########

mycols = c(rev(heat.colors(9)),"darkred")
for(aa in 1:7){
    png(paste0("mapQ1age",aa,".png"),width=1200,height=1200,pointsize=20)
    surveyIdxPlots(models[[1]],subset(ddsub.noDTU70,Quarter=="1"),myids=NULL,predD=gridlist,select="absolutemap",year=yearsQ1,colors=mycols,par=list(mfrow=n2mfrow(length(yearsQ1)+1),mar=c(0,0,2,0),oma=c(6,2,2,2),cex=0.6),legend=FALSE,map.cex=1.5,mapBubbles=TRUE,cols=aa)
    title(paste("Q1 age",aa),outer=TRUE)
    mapLegend(models[[1]],yearsQ1,mycols)
    dev.off()
}

for(aa in 1:7){
    png(paste0("mapQ1age",aa,"noBub.png"),width=1200,height=1200,pointsize=20)
    surveyIdxPlots(models[[1]],subset(ddsub.noDTU70,Quarter=="1"),myids=NULL,predD=gridlist,select="absolutemap",year=yearsQ1,colors=mycols,par=list(mfrow=n2mfrow(length(yearsQ1)+1),mar=c(0,0,2,0),oma=c(6,2,2,2),cex=0.6),legend=FALSE,map.cex=1.5,mapBubbles=FALSE,cols=aa)
    title(paste("Q1 age",aa),outer=TRUE)
    mapLegend(models[[1]],yearsQ34,mycols)
    dev.off()
}

### repeat Q34

for(aa in 1:7){
    png(paste0("mapQ4age",aa,".png"),width=1200,height=1200,pointsize=20)
    surveyIdxPlots(modelsQ4[[1]],subset(ddsub.noDTU70,Semester=="34"),myids=NULL,predD=gridlist,select="absolutemap",year=yearsQ34,colors=mycols,par=list(mfrow=n2mfrow(length(yearsQ34)+1),mar=c(0,0,2,0),oma=c(6,2,2,2),cex=0.6),legend=FALSE,map.cex=1.5,mapBubbles=TRUE,cols=aa)
    title(paste("Q34 age",aa),outer=TRUE)
    mapLegend(modelsQ4[[1]],yearsQ34,mycols)
    dev.off()
}

for(aa in 1:7){
    png(paste0("mapQ4age",aa,"noBub.png"),width=1200,height=1200,pointsize=20)
    surveyIdxPlots(modelsQ4[[1]],subset(ddsub.noDTU70,Semester=="34"),myids=NULL,predD=gridlist,select="absolutemap",year=yearsQ34,colors=mycols,par=list(mfrow=n2mfrow(length(yearsQ34)+1),mar=c(0,0,2,0),oma=c(6,2,2,2),cex=0.6),legend=FALSE,map.cex=1.5,mapBubbles=FALSE,cols=aa)
    title(paste("Q34 age",aa),outer=TRUE)
    mapLegend(modelsQ4[[1]],yearsQ34,mycols)
    dev.off()
}


if(FALSE){
    retro <- retro.surveyIdx(models[[1]],ddsub.noDTU70,grid=NULL,predD=gridlist,npeels=3)
    
    
    plot(retro,rescale=FALSE)
    
    pdf("retroplot.pdf",width=10,height=7)
    plot(retro,rescale=TRUE)
    dev.off()
    plot(1,1,type=n)

    mohn.surveyIdx(retro,base=models[[1]],rescale=TRUE)
}





