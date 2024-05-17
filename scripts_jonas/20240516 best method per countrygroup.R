library(ForestForesight)
setwd("C:/data/storage/accuracy_analysis/resultaten_202405/")
files=list.files(pattern="*.csv")
data(countries)
countries=as.data.frame(vect(countries))
countries=countries[,c("name","group")]
alldata=data.frame()
for (i in 1:length(files)){
data=read.csv(files[i])
alldata=rbind(alldata,data)
}
alldata=merge(alldata,countries,by="name")
sumstats=aggregate(cbind(TP,FP,FN)~method+group,alldata,sum)
#calculate precision, recall and f05 for each method and iso3
sumstats$precision=sumstats$TP/(sumstats$TP+sumstats$FP)
sumstats$recall=sumstats$TP/(sumstats$TP+sumstats$FN)
sumstats$F05=1.25*sumstats$precision*sumstats$recall/(0.25*sumstats$precision+sumstats$recall)
#save only the best method per iso3 based on F05
#plot sumstats but first reshape the dataframe so that F05 becomes the value and every column is a different method from the column method
library(reshape2)
sumstatsmelt=melt(sumstats,id.vars=c("method","iso3"))
library(ggplot2)
#plot with points per method
ggplot(sumstatsmelt,aes(x=iso3,y=value,colour=method))+geom_point()+facet_wrap(~variable,scales="free_y")
ggplot(sumstatsmelt,aes(x=iso3,y=value,fill=method))+geom_bar(stat="identity")+facet_wrap(~variable,scales="free_y")

#plot with points per method
ggplot(sumstatsmelt,aes(x=iso3,y=value,colour=method))+geom_point()+facet_wrap(~variable,scales="free_y")
newdat=data.frame()

for(i in unique(sumstats$group)){
subdata=sumstats[sumstats$group==i,]
bestmethod=subdata$method[which.max(subdata$F05)]
#cat the country, lowest F05 and highest F05
cat(paste(i,bestmethod,max(subdata$F05),max(subdata$F05),"\n"))
newdat=rbind(newdat,alldata[alldata$group==i & alldata$method==bestmethod,])
}
dir.create("result")

write.csv(newdat,"result/bestmethods.csv")
