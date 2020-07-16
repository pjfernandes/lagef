#' download ww3 function
#'
#' This function allows you to download ww3 data.
#' @param var.
#' @keywords ww3
#' @export
#' @examples
#' download.ww3()

download.ww3<-function(var="hs",anos,meses,dir=getwd()) {

if (!require(raster)) install.packages('raster')
if (!require(sp)) install.packages('sp')
if (!require(rgal)) install.packages('rgdal')
require(raster)
require(sp)
require(rgdal)

setwd(dir)  
  
var<-var
anos<-anos

if (is.numeric(meses)) {
  meses<-paste0("0",meses)
} else {
  meses<-meses
}

meses2<-character()
for(mes in meses) {
  if(nchar(mes)>2) {
    meses2<-c(meses2,substr(mes,2,3))
  } else {
    meses2<-c(meses2,mes)
  }
}
meses<-meses2 #VETOR FINAL COM OS MESES  

for (ano in anos) {

  for (mes in meses)  {
    url<-paste0("ftp://ftp.nodc.noaa.gov/pub/data.nodc/ncep/nww3/",ano,"/",mes,"/","glo_30m/")
    arquivo<-paste0("multi_1.glo_30m.",var,".",ano,mes,".grb2")
    print(paste0("-----Baixando-----  ",url,arquivo))
    tryCatch({
      download.file(paste0(url,arquivo),arquivo,mode="wb")
    }, error=function(e){})
  }
}

}
