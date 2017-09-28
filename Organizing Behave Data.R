###### Organizing Behave Data #####

# Study name
sname="Study_name"

# Original subject size
n=36
n=1:n
n=formatC(n, width=3, flag="0")

# Subject numbers that dropped out of study
boo=c(14,15,18,21)
n=n[-boo]

# Subject numbers that dropped out halfway in
onlyhalf=c("012","020","027","035")
inc=n %in% onlyhalf
n_first=n[inc]
n_both=setdiff(n,n_first)

# Number of scans per subject
s=2

# Number of runs per scan
r=4

# Stimulus duration (in seconds)
stimdur=5

# Analysis Folder Location
analoc="C:/Users/quarlesw/Documents/Analysis/Study_name"
setwd(analoc)

responsetype=c("food","object","noresp")

for(w in 1:s){
	for(t in n_both){
		d=read.table(file.path("C:","Users","wquarles","Documents","behave",paste(sname,"-",t,"_",w,"_data.txt",sep="")),sep="\t",header=F)
		d=d[,c(3,5,7,8)]
		d[5]=rep(stimdur,240)
		d[6]=rep(1,240)
		colnames(d)=c("col1","col2","col3","col4","col5","col6")
		d[d=="NA"]<-NA
		for(y in 1:r){
			noresp=d[is.na(d$col4),]
			resp=subset(d, d$col4 != "NA")
			food=subset(resp,subset=(resp$col2<=15)|(resp$col2>=31&resp$col2<=45))
			object=subset(resp,subset=(resp$col2>=15&resp$col2<=30)|(resp$col2>=46&resp$col2<=60))
			noresp=noresp[noresp$col1==y,]
			food=food[food$col1==y,]
			object=object[object$col1==y,]
			noresp=noresp[,c(3,5,6)]
			food=food[,c(3,5,6)]
			object=object[,c(3,5,6)]
			for(x in responsetype){
				file.create(file.path(paste("Subject",t,"_",w,sep=""),"behave",paste("run",y,sep=""),paste(x,".txt",sep="")))
				write.table(eval(parse(text=x)),file.path(paste("Subject",t,"_",w,sep=""),"behave",paste("run",y,sep=""),paste(x,".txt",sep="")),sep="\t",quote=FALSE,col.names=FALSE,row.names=FALSE)
			}
		}
	}
}
for(t in n_first){
		d=read.table(file.path("C:","Users","wquarles","Documents","behave",paste(sname,"-",t,"_1_data.txt",sep="")),sep="\t",header=F)
		d=d[,c(3,5,7,8)]
		d[5]=rep(stimdur,240)
		d[6]=rep(1,240)
		colnames(d)=c("col1","col2","col3","col4","col5","col6")
		d[d=="NA"]<-NA
		for(y in 1:r){
			noresp=d[is.na(d$col4),]
			resp=subset(d, d$col4 != "NA")
			food=subset(resp,subset=(resp$col2<=15)|(resp$col2>=31&resp$col2<=45))
			object=subset(resp,subset=(resp$col2>=15&resp$col2<=30)|(resp$col2>=46&resp$col2<=60))
			noresp=noresp[noresp$col1==y,]
			food=food[food$col1==y,]
			object=object[object$col1==y,]
			noresp=noresp[,c(3,5,6)]
			food=food[,c(3,5,6)]
			object=object[,c(3,5,6)]
			for(x in responsetype){
				file.create(file.path(paste("Subject",t,"_1",sep=""),"behave",paste("run",y,sep=""),paste(x,".txt",sep="")))
				write.table(eval(parse(text=x)),file.path(paste("Subject",t,"_1",sep=""),"behave",paste("run",y,sep=""),paste(x,".txt",sep="")),sep="\t",quote=FALSE,col.names=FALSE,row.names=FALSE)
			}
		}
	}
	
