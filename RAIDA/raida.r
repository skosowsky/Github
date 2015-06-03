source("http://www.bioconductor.org/biocLite.R")
biocLite("limma")
	a
biocLite("qvalue")
	a
install.packages("http://cals.arizona.edu/~anling/software/RAIDA_1.0.tar.gz",repos=NULL,type="source")


setwd("C:\\Users\\Sean\\Documents\\R\\Data\\Metagenomic-Analysis-master\\Original data")
filelist<-as.list(list.files("C:\\Users\\Sean\\Documents\\R\\Data\\Metagenomic-Analysis-master\\Original data",pattern=".csv"))
filenames<-list.files("C:\\Users\\Sean\\Documents\\R\\Data\\Metagenomic-Analysis-master\\Original data",pattern=".csv")
for (i in 1:length(filenames)) assign(filenames[[i]], read.csv(filenames[[i]]))
for (i in 1:length(filelist)){ 
filelist[[i]]<-as.data.frame(read.csv(filelist[[i]]))}



n.list=list()
for(i in 1:length(filelist)){
n.list[[i]]<-dim(filelist[[i]])[2]}
raida.list<-list()
for(j in 1:length(filelist)){
raida.list[[j]]<-raida(filelist[[j]],n.list[[j]])}

for(i in 1:7){
for(j in 1:dim(filelist[[i]])[2])
if(colnames(filelist[[i]])[-1]==rownames(filelist[[5]])[j]&filelist[[5]][j,3]=="watered")
data.water=matrix(nrows=dim(filelist[[i]])[1],ncols=count(filelist[[5]][,3])[2,2],byrow=T)}

for(j in 1:dim(filelist[[1]])[2]){
data.water=matrix(nrow=dim(filelist[[1]])[1],ncol=count(filelist[[5]][,3])[2,2],byrow=T)
if(colnames(filelist[[1]])[-1][j]==as.vector(filelist[[5]]$Sample_ID)[j] & as.vector(filelist[[5]]$treatment)[j]=="watered"){
data.water=filelist[[1]][,j]}
}

data.water=matrix(nrow=dim(filelist[[1]])[1],ncol=count(filelist[[5]][,3])[2,2])
data.water<-as.data.frame(cbind(filelist[[1]][,which(colnames(filelist[[1]])[-1]==as.vector(filelist[[5]]$Sample_ID) & as.vector(filelist[[5]]$treatment)=="watered")]))

cat("$",as.vector(filelist[[5]][which(colnames(filelist[[1]])[-1]==as.vector(filelist[[5]]$Sample_ID) & as.vector(filelist[[5]]$treatment)=="watered"),1])[1],sep="")