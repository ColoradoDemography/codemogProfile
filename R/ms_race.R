#'  Race Function
#'
#'  This function pulls data and generates a graph of the Racial Distribution
#'  for the areas selected based on the 2000 and 2010 Census data files.
#'
#'  @param fips The FIPS of the Place or County to use
#'  @param state  The State FIPS to use.  Defaults to CO.
#'  @param fips2 The FIPS of the Place or County to use for comparison
#'  @param state2 The State FIPS to use as comparison.  Defaults to CO.
#'

ms_race=function(fips, state="08"){
  require(scales, quietly=TRUE)
  require(dplyr, quietly=TRUE)

  p11_10=codemog_api(data="p11", geonum=paste("1", state, fips, sep=""),meta="no")%>%
    select(geoname:p11011)%>%
    rename(TotalPop=p11001, Hispanic=p11002, NonHispanic=p11003, NHWhite=p11005, NHBlack=p11006,
           NHAIAN=p11007, NHAsian=p11008, NHNHOPI=p11009, NHOther=p11010, NHTwo=p11011 )%>%
    gather(race, Census.2010, TotalPop:NHTwo, -geoname:-geonum)

  p4_00=codemog_api(data="p4", db="c2000",geonum=paste("1", state, fips, sep=""),meta="no")%>%
    select(geoname:p4011)%>%
    rename(TotalPop=p4001, Hispanic=p4002, NonHispanic=p4003, NHWhite=p4005, NHBlack=p4006,
           NHAIAN=p4007, NHAsian=p4008, NHNHOPI=p4009, NHOther=p4010, NHTwo=p4011 )%>%
    gather(race, Census.2000, TotalPop:NHTwo, -geoname:-geonum)
p4=inner_join(p11_10, p4_00)%>%
  mutate(Change=percent((as.numeric(Census.2010)-as.numeric(Census.2000))/as.numeric(Census.2000)),
         Census.2000=comma(as.numeric(Census.2000)),
         Census.2010=comma(as.numeric(Census.2010)))
return(p4)
}
