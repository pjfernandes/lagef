extract.mean<-function(file,stat="mean",x,y){

if (!require(RCurl)) install.packages('raster')
library(raster)

arquivo<-brick(file)

if (stat=="mean"){
imagem_resultado<-calc(arquivo,mean)
} 

if (stat=="median"){
imagem_resultado<-calc(arquivo,median)
} 

writeRaster(media,paste0(file,"_",stat,".tif"),overwrite=T)

if (length(x)>=1 && length(y)>=1){
df<-data.frame(x=x,y=y)
coordinates(df)<-c("x","y")

v<-as.vector(extract(arquivo,df))
res<-data.frame(VALUES=v)
write.csv(res,"values_extracted.csv",row.names = F)

v_m<-as.vector(extract(imagem_resultado,df))

print(paste(stat,"is",v_m))
}

}


