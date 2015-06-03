abundance.by.weeks <- read.csv("~/Data/abundance by weeks.csv")
library("cooccur", lib.loc="~/R/R-3.2.0/library")
######"plyr" is needed for count function#######
library("plyr", lib.loc="~/R/R-3.2.0/library")
KARA<-abundance.by.weeks
rownames(KARA)<-KARA[,1]
KARA<-KARA[,-1]

####colmns includes 0 and the number of observations per week for the purpose of####
####creating partitioned dataframes for each week which are used to create      ####
####7 cooccur objects; one for each week.                                       ####
####KARA.cooc is the list of 7 cooccur objects                                  ####

colmns<-c(0,count(as.numeric(substr(colnames(KARA),6,6)))[,2])
KARA.list<-list()
KARA.cooc<-list()
sumcols<-vector(length=8)
for(i in 2:8){
	sumcols[1]=1
	sumcols[i]=sum(colmns[1:i])
	KARA.list[[i-1]]<-as.data.frame(cbind(KARA[,(sumcols[i-1]):sumcols[i]]))
	KARA.cooc[[i-1]]<-cooccur(KARA.list[[i-1]],type="spp_site",thresh=T,spp_names=F)}

####c.KARA is a list of dataframe including species and p-values####
c.KARA<-list()
for(i in 1:7){
c.KARA[[i]]<-as.data.frame(cbind(print(KARA.cooc[[i]])[,1],print(KARA.cooc[[i]])[,2],print(KARA.cooc[[i]])[,8],print(KARA.cooc[[i]])[,9]))
colnames(c.KARA[[i]])<-c("sp1","sp2","p lt","p gt")}

####K.KARA is a list of dataframes including species, p-values, and a factor####
####for whether cooccurance was positive, negative, or random               ####
col<-c(1,2,3,5,6,7)
K.KARA<-list()
for(j in 1:6){
det.KARA<-vector(length=dim(c.KARA[[col[j]]])[1])
    for(i in 1:dim(c.KARA[[col[j]]])[1]){
        if(c.KARA[[col[j]]][i,4]<.025){
            det.KARA[i]="Positive"}
        else if(c.KARA[[col[j]]][i,4]>.975){
            det.KARA[i]="Negative"}
        else if(c.KARA[[col[j]]][i,4]>.025&c.KARA[[col[j]]][i,4]<.975){
            det.KARA[i]="Random"}}
K.KARA[[j]]<-as.data.frame(cbind(c.KARA[[col[j]]],det.KARA))}

for(j in 1:6){
K.KARA[[j]]<-as.data.frame(cbind(K.KARA[[j]],rep(j,dim(K.KARA[[j]])[1])))
colnames(K.KARA[[j]])<-c("sp1","sp2","p lt","p gt","Cooccurrence","Week")}


####Kdat is a dataframe binding all entries in K.KARA####
Kdat<-as.data.frame(rbind(K.KARA[[1]],K.KARA[[2]],K.KARA[[3]],K.KARA[[4]],K.KARA[[5]],K.KARA[[6]]))
Wk<-c(rep(1,3),rep(2,4),3,rep(5,10),6,7)
Kdat<-as.data.frame(cbind(Kdat[,-6],Wk))

####Partitions data into 3 dataframes for positive, negative, and random cooccurances####
t.rand<-as.data.frame(rbind(Kdat[which(Kdat[,5]=="Random"),]))
t.pos<-as.data.frame(rbind(Kdat[which(Kdat[,5]=="Positive"),]))
t.neg<-as.data.frame(rbind(Kdat[which(Kdat[,5]=="Negative"),]))


####ployly object####
ploy<-plotly()

trace1<-list(x=t.rand$sp1,y=t.rand$sp2,z=as.factor(t.rand$Wk),
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
  
  trace2<-list(x=t.pos$sp1,y=t.pos$sp2,z=as.factor(t.pos$Wk),
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
  
  trace3<-list(x=t.neg$sp1,y=t.neg$sp2,z=as.factor(t.neg$Wk),
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
  
  dat=list(trace1,trace2,trace3)
  layout <- list(title="Species Co-occurrence Matrix by Week", xaxis=list(title="Species 1"), yaxis=list(title="Species 2"), zaxis=list(title="Week"))
resp <- ploy$plotly(dat, kwargs=list(layout=layout, filename="Species Co-occurrence Matrix", fileopt="overwrite"))
resp$url