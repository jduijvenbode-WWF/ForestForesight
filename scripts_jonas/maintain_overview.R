library(terra)
library(aws.s3)
setwd("D:/ff-dev/results/")
intal_bucket="wwf-ff-global"
Sys.setenv("AWS_DEFAULT_REGION"="eu-west-1")
areas=vect("../integratedalerts.geojson")
world=vect("../borders.geojson")



# Set the bucket and object details




while(T){
  completefile=list.files(recursive=T,pattern="tif$")
  overview=t(t(table(dirname(completefile))))
  overview=data.frame("tile_id"=row.names(overview),count=overview[,1])
  areas2=merge(areas,overview,by="tile_id")
  ###########overview visualsation#########
  png("overview.png",width=2000,height=1100)
  plot(world)
  plot(areas2,"count",add=T,alpha=0.5,
       breaks=unique(floor(quantile(overview$count,seq(0,1,0.1)))),
       breakby="cases",legend="bottomleft",
       plg=list( # parameters for drawing legend
         title = paste(substr(Sys.time(),1,16),"\nprogress:",sum(overview$count),paste0("(",round(sum(overview$count)/(1.03*max(overview$count))),"%)")),
         title.cex = 3, # Legend title size
         cex = 3 # Legend text size
       ),
       pax=list( # parameters for drawing axes
         cex.axis = 2 # Axis text size 
       ), 
       cex.main = 3 # Title text size
  )
  text(areas2,paste0(areas$tile_id,"\n",areas2$count),halo=T,cex=0.7)
  
  dev.off()
  a=s3sync(bucket=intal_bucket,verbose=T)
  Sys.sleep(30*60)
  print(Sys.time())
}
