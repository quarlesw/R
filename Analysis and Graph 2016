setwd("GradSchoolAbstract")
WgtFciData=read.table("WgtFciData.csv",sep=",",head=T,fill=F)
attach(WgtFciData)
WgtFciData

plot(dFCI.O,dWgt,pch=20,col=ifelse(Group==1, "red", "blue"))
plot(dFCI.S,dWgt,pch=20,col=ifelse(Group==1, "red", "blue"))
plot(dFCI.H,dWgt,pch=20,col=ifelse(Group==1, "red", "blue"))
plot(dFCI.St,dWgt,pch=20,col=ifelse(Group==1, "red", "blue"))
plot(dFCI.FF,dWgt,pch=20,col=ifelse(Group==1, "red", "blue")) #this is the only one that doesn't seem like there was an effect after controlling for group

cor.test(dFCI.O,dWgt)
cor.test(dFCI.S,dWgt)
cor.test(dFCI.H,dWgt)
cor.test(dFCI.St,dWgt)
cor.test(dFCI.FF,dWgt)

fitO=lm(dWgt~dFCI.O*as.factor(Group)+Wgt1+Age)
fitO2=lm(dWgt~dFCI.O+as.factor(Group)+Wgt1)
fitO3=lm(dWgt~dFCI.O*as.factor(Group)+Age)
fitO4=lm(dWgt~dFCI.O+Wgt1+Age)
fitO5=lm(dWgt~dFCI.O+Wgt1)
fitO6=lm(dWgt~dFCI.O+Age)
summary(fitO2)

fitS=lm(dWgt~dFCI.S*as.factor(Group)+Wgt1+Age)
fitS2=lm(dWgt~dFCI.S*as.factor(Group)+Wgt1)
fitS3=lm(dWgt~dFCI.S*as.factor(Group)+Age)
fitS4=lm(dWgt~dFCI.S+Wgt1+Age)
fitS5=lm(dWgt~dFCI.S+Wgt1)
fitS6=lm(dWgt~dFCI.S+Age)
fitS7=lm(dWgt~dFCI.S+as.factor(Group))
fitS8=lm(dWgt~dFCI.S+as.factor(Group)+Wgt1)
summary(fitS8)

fitH=lm(dWgt~dFCI.H*as.factor(Group)+Wgt1+Age)
fitH2=lm(dWgt~dFCI.H*as.factor(Group)+Wgt1)
fitH3=lm(dWgt~dFCI.H*as.factor(Group)+Age)
fitH4=lm(dWgt~dFCI.H+Wgt1+Age)
fitH5=lm(dWgt~dFCI.H+Wgt1+Age+as.factor(Group))
fitH6=lm(dWgt~dFCI.H+Wgt1)
fitH7=lm(dWgt~dFCI.H+Wgt1+as.factor(Group))
summary(fitH7)

fitSt=lm(dWgt~dFCI.St*as.factor(Group)+Wgt1+Age)
fitSt2=lm(dWgt~dFCI.St*as.factor(Group)+Wgt1)
fitSt3=lm(dWgt~dFCI.St*as.factor(Group)+Age)
fitSt4=lm(dWgt~dFCI.St+as.factor(Group)+Age+Wgt1)
fitSt5=lm(dWgt~dFCI.St+as.factor(Group)+Wgt1)
fitSt6=lm(dWgt~dFCI.St+Wgt1)
summary(fitSt6)

fitFF=lm(dWgt~dFCI.FF*as.factor(Group)+Wgt1+Age)
fitFF2=lm(dWgt~dFCI.FF*as.factor(Group)+Wgt1)
fitFF3=lm(dWgt~dFCI.FF*as.factor(Group)+Age)
fitFF4=lm(dWgt~dFCI.FF+as.factor(Group)+Age)
fitFF5=lm(dWgt~dFCI.FF+as.factor(Group)+Wgt1)
fitFF6=lm(dWgt~dFCI.FF+Age+Wgt1)
fitFF7=lm(dWgt~dFCI.FF+Wgt1)
fitFF8=lm(dWgt~dFCI.FF+Age)
fitFF9=lm(dWgt~dFCI.FF+as.factor(Group))
summary(fitFF9)

####################################################################
############# Making the graphs for the abstract ###################
####################################################################

colarr=c("moccasin","darkseagreen1","springgreen4","khaki","lightcoral","ivory","lemonchiffon","blue2","brown4","chocolate1","cornsilk2","darkgoldenrod3","darkred","cyan2","gold","gray14","honeydew2","lawngreen","lightblue2","magenta","mediumpurple3","orange","olivedrab","palevioletred","royalblue2","salmon1","tan","yellowgreen")
FCI.O2=FCI.O1-dFCI.O

par(mfrow=c(1,2))

plot(y=c(FCI.O1,FCI.O2),x=c(rep(0,28),rep(1,28)),pch=20,col=colarr,axes=T)
for(i in 1:length(FCI.O1)){
	lines(c(0,1),c(FCI.O1[i],FCI.O2[i]),lwd=2,col=colarr[i])
}
axis(2)

plot(y=c(Wgt2,Wgt5),x=c(rep(0,28),rep(1,28)),pch=20,col=colarr,axes=T)
for(i in 1:length(Wgt2)){
	lines(c(0,1),c(Wgt2[i],Wgt5[i]),lwd=2,col=colarr[i])
}
axis(2)
