#'  CO Poverty Rates
#'
#'  This function pulls data from the most recent ACS on poverty
#'  @param fips The FIPS of the Place or County to use for the graph
#'  @param base The base font size (in points) to use. Defaults to 12.
#'  @param state  The State FIPS to use.  Defaults to CO.
#'



cp_poverty=function(fips, state="08"){
  require(dplyr, quietly=TRUE)
  require(codemog, quietly=TRUE)

  poverty=codemog_api(data="b06012",db="acs1115" ,geonum=paste("1", state, fips, sep=""),meta="no")
  poverty[,7:ncol(poverty)]=as.numeric(as.character(poverty[,7:ncol(poverty)]))

  poverty=poverty%>%
    mutate(poverty_rate=b06012002/b06012001)%>%
    select(geoname:geonum, poverty_rate)

  return(poverty)
}
