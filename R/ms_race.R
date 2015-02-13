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

  p11_10=codemog_api(data="p7", geonum=paste("1", state, fips, sep=""),meta="no")
  p11_10[,7:ncol(p11_10)]=as.numeric(as.character(p11_10[,7:ncol(p11_10)]))

  p11_10=p11_10%>%
    select(geoname:p11011)%>%
    mutate(TotalPop=p11001, Hispanic=p11002, NonHispanic=p11003, NHWhite=p11005, NHBlack=p11006,
           NHAIAN=p11007, NHAsian=p11008, NHNHOPI=p11009, NHOther=p11010, NHTwo=p11011,
           HispanicP=percent(Hispanic/TotalPop),
           NonHispanicP=percent(NonHispanic/TotalPop),
           NHWhiteP=percent(NHWhite/TotalPop),
           NHBlackP=percent(NHBlack/TotalPop),
           NHAIANP=percent(NHAIAN/TotalPop),
           NHAsianP=percent(NHAsian/TotalPop),
           NHNHOPIP=percent(NHNHOPI/TotalPop),
           NHOtherP=percent(NHOther/TotalPop),
           NHTwoP=percent(NHTwo/TotalPop),
           TotalPop=comma(p11001), Hispanic=comma(p11002), NonHispanic=comma(p11003), NHWhite=comma(p11005), NHBlack=comma(p11006),
           NHAIAN=comma(p11007), NHAsian=comma(p11008), NHNHOPI=comma(p11009), NHOther=comma(p11010), NHTwo=comma(p11011))%>%
    select(-p11001:-p11011)%>%
    gather(race, Census.2010, TotalPop:NHTwoP, -geoname:-geonum)

  p4_00=codemog_api(data="p4", db="c2000",geonum=paste("1", state, fips, sep=""),meta="no")
  p4_00[,7:ncol(p4_00)]=as.numeric(as.character(p4_00[,7:ncol(p4_00)]))
  p4_00=p4_00%>%
    select(geoname:p4011)%>%
    mutate(TotalPop=p4001, Hispanic=p4002, NonHispanic=p4003, NHWhite=p4005, NHBlack=p4006,
           NHAIAN=p4007, NHAsian=p4008, NHNHOPI=p4009, NHOther=p4010, NHTwo=p4011,
           HispanicP=percent(Hispanic/TotalPop),
           NonHispanicP=percent(NonHispanic/TotalPop),
           NHWhiteP=percent(NHWhite/NonHispanic),
           NHBlackP=percent(NHBlack/NonHispanic),
           NHAIANP=percent(NHAIAN/NonHispanic),
           NHAsianP=percent(NHAsian/NonHispanic),
           NHNHOPIP=percent(NHNHOPI/NonHispanic),
           NHOtherP=percent(NHOther/NonHispanic),
           NHTwoP=percent(NHTwo/NonHispanic),
           TotalPop=comma(p4001), Hispanic=comma(p4002), NonHispanic=comma(p4003), NHWhite=comma(p4005), NHBlack=comma(p4006),
           NHAIAN=comma(p4007), NHAsian=comma(p4008), NHNHOPI=comma(p4009), NHOther=comma(p4010), NHTwo=comma(p4011))%>%
    select(-p4001:-p4011)%>%
    gather(race, Census.2000, TotalPop:NHTwoP, -geoname:-geonum)
p4=inner_join(p11_10, p4_00)
return(p4)
}
