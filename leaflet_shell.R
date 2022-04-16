#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LOAD PACKAGES, DATA & HELPER FUNCTIONS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
packages <- c('dplyr','ggplot2','reshape2','fields','earth','leaflet','randomcoloR','data.table','rgdal','rgeos','htmlwidgets','stringr','tidyr','plyr')
lapply(packages,library,character.only=TRUE)
zipcleaner <- function(a,b) unlist(mapply(function(x,y) ifelse(grepl('[A-Za-z]{1}',x),toupper(substr(x,1,y)),str_sub(paste0('00',strsplit(gsub(' ','',as.character(x)),'-')[[1]][1]),start=-5)),a,b))

setwd('~/Documents/-\ projects/-\ maps')

zipdata <- fread(paste0(getwd(),'/zipcode_master.csv'))
zipdata$ZIP <- zipcleaner(zipdata$ZIP,5)

dims <- c('ZIP')
metrics <- c('ZPOP','ZHU','MEDAGE')

agg_table <- function(t,dims,metrics,fillna=T){
  o = ''
  for(d in dims){
    t <- eval(parse(text=paste0("t[!is.na(t$",d,"),]")))
    o <- paste0(o,d,',')
  }
  if(fillna==T){
    t[is.na(t)] <- 0
  }
  print(o)
  t <- eval(parse(text=paste0("t[,c(dims,metrics),with=F][,lapply(.SD,sum),by = d][order(",substr(o,1,nchar(o)-1),")]")))
  t$count <- 1
  t
}

mapdata <- agg_table(zipdata,dims,metrics)
mapdata

if('ZIP' %in% dims){
  mapfile <- readOGR(paste0(getwd(),'/shapefile_zip_us_ca'),'usca')
  colnames(mapfile@data)[1] <- 'ZIP'
} else {
  mapfile <- readOGR(paste0(getwd(),'/shapefile_cty_us'),'gz_2010_us_050_00_500k')
  colnames(mapfile@data)[3] <- 'FIPS'
}

mapfile@data <- cbind(mapfile@data,data.frame(seq(nrow(mapfile@data))))
colnames(mapfile@data)[length(colnames(mapfile@data))] <- 'idx'
mapfile_copy <- mapfile@data[,c('ZIP','idx')]
mapfile_copy <- setDT(merge(mapfile_copy,mapdata,by=dims[1],all.x=T))[order(idx)]
mapfile_copy[is.na(mapfile_copy$count)]$count <- 0
cl <- length(colnames(mapfile_copy))
mapfile@data <- cbind(mapfile@data,mapfile_copy[,colnames(mapfile_copy)[3:cl],with=F])

state_capitals <- fread(paste0(getwd(),'/state_capitals.csv'))

color_func <- colorRampPalette(c('#FFFFFF','#C5F4FF','#77A6FF','#2F59C6'))
#palette <- c(0,1,3,5,10,20,30,50,100,200,300,500,1000,2000,5000,10000,20000,50000,100000,200000)
palette <- quantile(mapdata$MEDAGE,seq(0, 100, length.out=26)/100)
palette_final <- colorBin(color_func(length(unique(palette))), domain = mapfile@data$ZPOP, bins = unique(palette), pretty=F)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GENERATE MAP
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
leaflet() %>% addProviderTiles(providers$OpenStreetMap,options = providerTileOptions(minZoom=4,maxZoom=18)) %>%
  setView(lat = 40.1006,lng = -97.3512,zoom = 5) %>%
  addLegend('bottomleft',
            pal = palette_final,
            values = mapfile@data$MEDAGE,
            title = 'Population',
            opacity = 1,
            group = 'General') %>%
  addPolygons(data = mapfile,
            stroke = T,
            weight = 0.05,
            color = '#222',
            fill = T,
            fillColor = ~palette_final(MEDAGE),
            fillOpacity = 1,
            popup = ~MEDAGE,
            label = ~MEDAGE,
            group = 'General') %>%
  addLayersControl(position = 'bottomright',
  overlayGroups = c('General'),
  options = layersControlOptions(collapsed=F,autoZIndex=F))


