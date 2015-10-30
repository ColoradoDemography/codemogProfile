#' Creates a simple map that highlights a Colorado County
#'
#' This function creates a map that highlights one county to be used in the profile process.
#'
#' @param fips is the fips code for the county to highlight
#'
cp_countymap=function(fips){

suppressPackageStartupMessages(require(rgdal))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(ggthemes))
suppressPackageStartupMessages(require(gridExtra))
suppressPackageStartupMessages(require(maptools))
suppressPackageStartupMessages(require(RCurl))

#Pulls the COunty Outlines
j=getURL("http://54.69.15.55/CensusAPI/geojson.php?table=p1&sumlev=50&db=c2010&state=8&zoom=9")
gj=readOGR(j, "OGRGeoJSON", verbose=FALSE)
gj=fortify(gj)

#Pulls the County to Highlight
j1=getURL(paste0("http://54.69.15.55/CensusAPI/geojson.php?table=p1&sumlev=50&db=c2010&state=8&zoom=9&county=", as.numeric(fips)))
gj1=readOGR(j1, "OGRGeoJSON", verbose=FALSE)
gj1=fortify(gj1)

m=ggplot()+
  geom_map(data=gj, map=gj,
           aes(x=long, y=lat, map_id=id),
           fill=rgb(239,239,239, max=255), color=rgb(92,102,112, max=255), size=.25)+
  geom_map(data=gj1, map=gj1,
           aes(x=long, y=lat, map_id=id),
           fill=rgb(0,149,58, max=255), color=rgb(92,102,112, max=255), size=.25)+
  coord_map(project="albers", lat0=40, lat1=39)+
  theme_map()+
  theme(panel.background=element_rect(fill=rgb(239,239,239, max=255), color=rgb(239,239,239, max=255)),
        plot.background=element_rect(fill=rgb(239,239,239, max=255), color=rgb(239,239,239, max=255)))

return(m)
}
