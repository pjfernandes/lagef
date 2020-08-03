#' extract values from ww3 data function
#'
#' This function allows you to extract values from ww3 data.
#' @param var.
#' @keywords ww3
#' @export
#' @examples
#' extract.values()

extract.values<-function(dir,x,y,ncluster=1){
  
  if (!require(raster)) install.packages('raster')
  if (!require(parallel)) install.packages('parallel')
  if (!require(doParallel)) install.packages('doParallel')
  if (!require(foreach)) install.packages('foreach')
  if (!require(sp)) install.packages('sp')
  if (!require(rgdal)) install.packages('rgdal')
  if (!require(lubridate)) install.packages('lubridate')
  if (!require(ggplot2)) install.packages('ggplot2')
  
  require(raster)
  require(parallel)
  require(doParallel)
  require(foreach)
  require(sp)
  require(rgdal)
  require(lubridate)
  require(ggplot2)
  
  setwd(dir)
  
  arquivos<-dir(dir,pattern=".grb2$")
  var<-substr(arquivos[1],17,18)
  #projecao<-crs(brick(arquivos[[1]]))
  
  df<-data.frame(x=321,y=-29)
  coordinates(df)<-c("x","y")
  
  cl<-makeCluster(ncluster)
  registerDoParallel(cl)
  
  c<-foreach(i=1:length(arquivos), .packages = "raster",.combine = "c") %dopar% {
    ls<-list()
    ano<-as.numeric(substr(arquivos[i],20,23))
    mes<-as.numeric(substr(arquivos[i],24,25))
    
    if (((ano>=2017 && mes>=10) || (ano>=2018))==FALSE) {
    b<-brick(arquivos[i])
    } else {
      b<-brick(arquivos[i])
      b<-b[[-nlayers(b)]]
    }
    ls<-append(ls,b)
  }
  
  sequencia_final<-.POSIXct(character(0), tz="UTC")
  
  for (arquivo in arquivos){
    ano<-substr(arquivo,20,23)
    mes<-substr(arquivo,24,25)
    b<-brick(arquivo)
    data_inicial <-as.POSIXct(paste0(ano,"-",mes,"-",01," 00:00:00"),"%Y-%m-%d %H", tz="UTC")
    dias<-as.numeric(days_in_month(data_inicial))
    data_final <-as.POSIXct(paste0(ano,"-",mes,"-",dias," 24:00:00"),"%Y-%m-%d %H", tz="UTC")
    sequencia<-seq(data_inicial,data_final,by="3 hour")
    sequencia_final<-c(sequencia_final,sequencia)
    }
    
  
  system.time(
    valores<-foreach(i=1:length(c), .packages = "raster",.combine = "c") %dopar% {
      #for (i in 1:length(c)){
      x<-numeric()
      x2<-as.vector(extract(c[[i]],df))
      x<-c(x,x2)
    }
  )
  
  
  resultado<-unique(data.frame(sequencia_final,valores))
  names(resultado)<-c("data",var)
  
  dir.create("RESULTS")
  bmp(file = "RESULTS/fig.bmp",width = 1500,height = 611)
  plot(resultado[,1],resultado[,2],ylab=paste(var),xlab="Date",t="l",main=paste(var,"for","x=",df$x,"y=",df$y))
  abline(h=median(resultado[,2],na.rm=T),col="red")
  dev.off()
  
  write.csv(resultado,"RESULTS/table.csv",row.names = F)
}