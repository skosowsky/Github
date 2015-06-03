SEMSizesfiltered <- read.csv("~/Data/SEMSizesfiltered.csv")
attach(SEMSizesfiltered)
Species<-SEMSizesfiltered[,6]
data.SEM<-as.data.frame(cbind(as.character(paste0(Age,".",Plant)),as.numeric(Species)))
colnames(data.SEM)<-c("Age.Plant","Species")
attach(data.SEM)
counter=matrix(nrow=length(count(Species)[,1]),ncol=length(unique(Age.Plant)))
colnames(counter)<-as.character(unique(Age.Plant))
rownames(counter)<-as.character(sort(as.numeric(count(Species)[,1]),decreasing=F))
for(i in 1:length(count(data.SEM[,1])[,1])){
for(j in 1:length(count(data.SEM[,2])[,1])){
counter[j,i]=length(which(as.character(Species)==rownames(counter)[j]&as.character(Age.Plant)==colnames(counter)[i]))}}

cols<-c(0,count(as.numeric(substr(colnames(counter),1,2)))[,2])
SEM.list<-list()
SEM.cooc<-list()
sumcols<-vector(length=8)
for(i in 2:8){
	sumcols[1]=1
	sumcols[i]=sum(cols[1:i])
	SEM.list[[i-1]]<-as.data.frame(cbind(counter[,(sumcols[i-1]):sumcols[i]]))
	SEM.cooc[[i-1]]<-cooccur(SEM.list[[i-1]],type="spp_site",thresh=T,spp_names=F)}

c.SEM<-list()
for(i in 1:7){
c.SEM[[i]]<-as.data.frame(cbind(print(SEM.cooc[[i]])[,1],print(SEM.cooc[[i]])[,2],print(SEM.cooc[[i]])[,8],print(SEM.cooc[[i]])[,9]))
colnames(c.SEM[[i]])<-c("sp1","sp2","p lt","p gt")}
col<-c(1,3,4,5,6,7)
K.SEM<-list()
for(j in 1:6){
det.SEM<-vector(length=dim(c.SEM[[col[j]]])[1])
    for(i in 1:dim(c.SEM[[col[j]]])[1]){
        if(c.SEM[[col[j]]][i,4]<.025){
            det.SEM[i]="Positive"}
        else if(c.SEM[[col[j]]][i,4]>.975){
            det.SEM[i]="Negative"}
        else if(c.SEM[[col[j]]][i,4]>.025&c.SEM[[col[j]]][i,4]<.975){
            det.SEM[i]="Random"}}
K.SEM[[j]]<-as.data.frame(cbind(c.SEM[[col[j]]],det.SEM))}


for(j in 1:6){
K.SEM[[j]]<-as.data.frame(cbind(K.SEM[[j]],rep(j,dim(K.SEM[[j]])[1])))
colnames(K.SEM[[j]])<-c("sp1","sp2","p lt","p gt","Cooccurrence","Week")}


####Kdat is a dataframe binding all entries in K.SEM####
Kdat<-as.data.frame(rbind(K.SEM[[1]],K.SEM[[2]],K.SEM[[3]],K.SEM[[4]],K.SEM[[5]],K.SEM[[6]]))
Wk<-c(rep(1,2),rep(3,2),rep(4,18),rep(5,15),rep(6,91),rep(7,17))
Kdat<-as.data.frame(cbind(Kdat[,-6],Wk))

####Partitions data into 3 dataframes for positive, negative, and random cooccurances####
t.rand<-as.data.frame(rbind(Kdat[which(Kdat[,5]=="Random"),]))
t.pos<-as.data.frame(rbind(Kdat[which(Kdat[,5]=="Positive"),]))
t.neg<-as.data.frame(rbind(Kdat[which(Kdat[,5]=="Negative"),]))


###PLOTLY###
ply<-plotly()

t1<-list(x=t.rand$sp1,y=t.rand$sp2,z=as.factor(t.rand$Wk),
 mode = "markers",
 name="Random",
  marker = list(
    size = 12, 
    line = list(
      color = "rgba(217, 217, 217, 0.14)", 
      width = 0.5
    ), 
    opacity = 0.8
  ), 
  type = "scatter3d")
  
  t2<-list(x=t.pos$sp1,y=t.pos$sp2,z=as.factor(t.pos$Wk),
 mode = "markers", 
 name="Positive",
  marker = list(
    size = 12, 
    line = list(
      color = "rgba(217, 217, 217, 0.14)", 
      width = 0.5
    ), 
    opacity = 0.8
  ), 
  type = "scatter3d")
  
  t3<-list(x=t.neg$sp1,y=t.neg$sp2,z=as.factor(t.neg$Wk),
 mode = "markers", 
 name="Negative",
  marker = list(
    size = 12, 
    line = list(
      color = "rgba(217, 217, 217, 0.14)", 
      width = 0.5
    ), 
    opacity = 0.8
  ), 
  type = "scatter3d")
  
  dat=list(t1,t2,t3)
  layout <- list(title="SEM Species Co-occurrence Matrix by Week", xaxis=list(title="Species 1"), yaxis=list(title="Species 2"), zaxis=list(title="Week"))
resp <- ply$plotly(dat, kwargs=list(layout=layout, filename="SEM Species Co-occurrence Matrix"))
resp$url