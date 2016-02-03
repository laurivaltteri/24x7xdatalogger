## 24x7xData logger analysis tools
## CC Lauri Ahonen, 2015 Brain Work Research Centre

read_data_folder <- function(basepath,dateconst="") {
  file_list <- list.files(basepath, pattern = paste0(dateconst,".*.txt"))
  df <-data.frame(tsm=character(0),lat=numeric(0),lon=numeric(0))
  for (tfile in file_list) {
    tmp<-tryCatch(read.table(paste0(basepath,tfile),sep="\t",fill=TRUE,blank.lines.skip=TRUE,
    fileEncoding="UTF-16LE",colClasses = "character",strip.white=TRUE),error=function(e) e)
    if(inherits(tmp, "error")) next
    if (dim(tmp)[2]>4) {
      tmp<-tmp[tmp$V3=="Track",c(1,4,5)]
      tmp$lat<-as.numeric(tmp$V4);tmp$V4<-NULL
      tmp$lon<-as.numeric(tmp$V5);tmp$V5<-NULL
      tmp$tsm<-as.POSIXct(tmp$V1);tmp$V1<-NULL
      tmp<-tmp[complete.cases(tmp),]
      df<-rbind(df,tmp)
    }
  }
  df
}

library(ggmap)
draw_tracks_on_map <- function(trackdata,datestart=NULL,dateend=NULL,zoom=0) {

  # filter dates
  trackdata<-trackdata[order(trackdata$tsm),]
  if (! is.null(datestart)) {
    trackdata<-trackdata[trackdata$tsm>=as.POSIXct(datestart),]
  }
  if (! is.null(dateend)) {
    trackdata<-trackdata[trackdata$tsm<=as.POSIXct(dateend),]
  }

  # zooming options
  locbox <- c(min(trackdata$lon)-1, min(trackdata$lat)-1, max(trackdata$lon)+1,max(trackdata$lat)+1)
  if (is.numeric(zoom) && zoom!=0) {
    mapimg <- get_map(location = c(lon=median(trackdata$lon),lat=median(trackdata$lat)), zoom = zoom, maptype = "terrain")
  }
  else if (is.character(zoom)) {
    mapimg <- get_map(location = zoom, maptype = "terrain")
  }
  else {
    mapimg <- get_map(location = locbox, maptype = "terrain")
  }

  # legend labels (only 5 labels is accepted!?)
  fivelabels<-quantile(trackdata$tsm,seq(0,1,length.out=5))
  fivelabels<-format(fivelabels,"%d.%m.%Y %H:%M")
  fivebreaks<-quantile(as.integer(trackdata$tsm),seq(0,1,length.out=5))
  names(fivebreaks)<-NULL#;fivebreaks<-as.character(fivebreaks)
  names(fivelabels)<-NULL
  fivelabels<-as.character(fivelabels)

  # ggplot map and path
  # p <- ggmap(mapimg) + geom_path(aes(lon,lat,color=as.integer(tsm)),trackdata,size = 1)
  p <- ggmap(mapimg) + geom_point(aes(lon,lat,color=as.integer(tsm)),trackdata,size = 2.5)
  p <- p + scale_colour_gradientn(colours = rainbow(7),guide = guide_colourbar(title="Dates"),breaks=fivebreaks,labels=fivelabels)#
  p <- p + ggtitle(paste("GPS data between dates\n",format(min(trackdata$tsm),"%d.%m.%Y %H:%M"),
  "and",format(max(trackdata$tsm),"%d.%m.%Y %H:%M"))) + theme_bw()
  p
}

# source('~/Documents/R/24logger/routines.R', chdir = TRUE)
# trackerdata <- read_data_folder('~/OneDrive/24loggerdata/')
# png('~/Documents/R/24logger/maptest.png',1156,1028)
# draw_tracks_on_map(trackerdata,"2015-10-03","2015-10-11",zoom=13)
# dev.off()

    # tfile<-file(paste0(basepath,ifile), open="r", encoding="UTF-16LE")
    # ttable<-read.table(tfile,sep="\t",fill=TRUE,blank.lines.skip=TRUE)
    # #strip.white=TRUE,colClasses=c("POSIXct","NULL","factor","numeric","numeric"))
    # ttable<-ttable[ttable$V3=="Track",c(1,4,5)]
    # ttable<-ttable[complete.cases(ttable),]
    # tlen<-dim(ttable)[1]
    # tmp <-data.frame(tsm=character(tlen),lat=numeric(tlen),lon=numeric(tlen))
    # tmp$lat<-as.numeric(ttable$V4)
    # tmp$lon<-as.numeric(levels(ttable$V5)[ttable$V5])
    # tmp$tsm<-as.POSIXct(ttable$V1)
    # df<-rbind(df,tmp)
