#################################################
### Creating bar graphs for conference
#################################################

############# You must run this one section at a time #################

# read data
data=read.table("data_BFM_Stress.csv", sep=",",header=T)

# replacing variable names
data[data$BMI_M==1,"BMI_M"]<-"Low BMI"
data[data$BMI_M==0,"BMI_M"]<-"High BMI"
data[data$FM==1,"FM"]<-"Low FM"
data[data$FM==0,"FM"]<-"High FM"
data[data$Stress==1,"Stress"]<-"Low Stress"
data[data$Stress==0,"Stress"]<-"High Stress"
data$BMI_M=as.factor(data$BMI_M)
data$FM=as.factor(data$FM)
data$Stress=as.factor(data$Stress)
dimnames(data)[[2]][18]<-"BMI"
attach(data)

# SEM function
SEM=function(x){
	sd(x)/sqrt(length(x))
}

# creating charts
# function to get mean and SEM
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
	SEM = SEM(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
 return(data_sum)
}

ROIs=c("L_ACC","R_ACC","L_Amyg","R_Amyg","L_dlPFC","R_dlPFC","L_Insula","R_Insula","L_NA","R_NA","L_OFC","R_OFC","L_Hippocampus","R_Hippocampus")
pROIs=c("Left Antetior Cingulate Cortex","Right Antetior Cingulate Cortex","Left Amygdala","Right Amygdala","Left Dorsolateral Prefrontal Cortex","Right Dorsolateral Prefrontal Cortex","Left Insula","Right Insula","Left Nucleus Accumbens","Right Nucleus Accumbens","Left Orbitofrontal Cortex","Right Orbitofrontal Cortex","Left Hippocampus","Right Hippocampus")
XVar=c("BMI","FM","Stress")

setwd("M:/MBinks_Lab_Data/wquarles/EB 2017 abstracts/Pictures")

for(x in 1:length(XVar)){
for(i in 1:length(ROIs)){
df=data_summary(data,varname=ROIs[i],groupnames=XVar[x])
means=c(df[[2]][1],df[[2]][2])
errors=c(df[[3]][1],df[[3]][2])
y_max=max(means)+max(errors)
y_min=min(means)-max(errors)
if (y_min > 0){
y_min=0
}
for(e in 1:2){
if (means[e]<0){
errors[e]=errors[e]*-1
}
}
jpeg(paste(ROIs[i],XVar[x],".jpg",sep=""),width=960,height=960)
par(family="serif",cex=3)
p=barplot(df[[2]], main=pROIs[i], ylab="COPEs",names.arg=df[[1]], beside=T,col=c("black","brown4"),cex.lab=1.1,cex.main=1.25,cex.axis=1,cex.names=1.1,ylim=c(y_min,y_max))
segments(p,means,p,(means + errors),lwd=2)
segments(p-0.25,(means+errors),p+0.25,(means+errors),lwd=2)
dev.off()
}
}
