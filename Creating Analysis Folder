####### Creating Analysis Folder #######

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

# Number of levels in analysis
l=3

# Stimulus duration (in seconds)
stimdur=5

# Analysis Folder Location
analoc="C:/Users/quarlesw/Documents/Analysis"
setwd(analoc)

# Folders in each scan
foldernames=c("anatomy","behave","bold","model")

dir.create(sname)
setwd(sname)
dir.create("Template")
for(i in 1:l){
	dir.create(file.path("Template",paste("Level",i)))
}
if(l==3)
	dir.create("Level3")
for(w in 1:s){
	for(t in n_both){
		dir.create(paste("Subject",t,"_",w,sep=""))
		for(z in foldernames){
			dir.create(file.path(paste("Subject",t,"_",w,sep=""),z))
			for(y in 1:r){
				dir.create(file.path(paste("Subject",t,"_",w,sep=""),"behave",(paste("run",y,sep=""))))
				dir.create(file.path(paste("Subject",t,"_",w,sep=""),"bold",(paste("run",y,sep=""))))
			}
		}
	}
}
for(t in n_first){
	dir.create(paste("Subject",t,"_1",sep=""))
	for(z in foldernames){
		dir.create(file.path(paste("Subject",t,"_1",sep=""),z))
		for(y in 1:r){
			dir.create(file.path(paste("Subject",t,"_1",sep=""),"behave",(paste("run",y,sep=""))))
			dir.create(file.path(paste("Subject",t,"_1",sep=""),"bold",(paste("run",y,sep=""))))
		}
	}
}
