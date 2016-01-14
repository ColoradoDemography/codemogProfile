#'  Median Household Income Function
#'
#'  This function pulls data and generates a graph of the income distribution
#'  for the areas selected based on the ACS 09-13 5-year File.
#'
#'  @param fips The FIPS of the Place or County to use
#'  @param state  The State FIPS to use.  Defaults to CO.
#'  @param fips2 The FIPS of the Place or County to use for comparison
#'  @param state2 The State FIPS to use as comparison.  Defaults to CO.
#'

ms_mhi=function(fips, fips2="", state="08", state2="08"){
  require(scales, quietly=TRUE)
  require(stringr, quietly=TRUE)
  require(dplyr, quietly=TRUE)


mhi1=codemog_api(data="b19013",db="acs1014", geonum=paste("1", state, fips, sep=""), meta="no")%>%
  mutate(geoname=str_trim(geoname, side="both"),
         MHI=dollar(as.numeric(b19013001)))%>%
  separate(geoname, into=c("geoname", "state_name"), sep=",")%>%
  select(-state_name, -b19013001)

mhi2=codemog_api(data="b19013",db="acs1014", geonum=paste("1", state2, fips2, sep=""), meta="no")%>%
  mutate(geoname=str_trim(geoname, side="both"),
         MHI_CO=dollar(as.numeric(b19013001)))

mhi3=mhi2%>%
  select(MHI_CO)

mhi=bind_cols(mhi1, mhi3)

return(mhi)
}
