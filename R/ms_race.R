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

  p9_10=codemog_api(data="p9", geonum=paste("1", state, fips, sep=""),meta="no")
  p9_10[,7:ncol(p9_10)]=as.numeric(as.character(p9_10[,7:ncol(p9_10)]))

  p9_10=p9_10%>%
    select(geoname:p9011)%>%
    mutate(TotalPop=p9001, Hispanic=p9002, NonHispanic=p9003, NHWhite=p9005, NHBlack=p9006,
           NHAIAN=p9007, NHAsian=p9008, NHNHOPI=p9009, NHOther=p9010, NHTwo=p9011,
           HispanicP=percent(round(Hispanic/TotalPop, 3)),
           NonHispanicP=percent(round(NonHispanic/TotalPop,3)),
           NHWhiteP=percent(round(NHWhite/TotalPop, 3)),
           NHBlackP=percent(round(NHBlack/TotalPop, 3)),
           NHAIANP=percent(round(NHAIAN/TotalPop,3)),
           NHAsianP=percent(round(NHAsian/TotalPop,3)),
           NHNHOPIP=percent(round(NHNHOPI/TotalPop, 3)),
           NHOtherP=percent(round(NHOther/TotalPop,3)),
           NHTwoP=percent(round(NHTwo/TotalPop,3)),
           TotalPop=comma(p9001), Hispanic=comma(p9002), NonHispanic=comma(p9003), NHWhite=comma(p9005), NHBlack=comma(p9006),
           NHAIAN=comma(p9007), NHAsian=comma(p9008), NHNHOPI=comma(p9009), NHOther=comma(p9010), NHTwo=comma(p9011))%>%
    select(-p9001:-p9011)%>%
    gather(race, Census.2010, TotalPop:NHTwoP, -geoname:-geonum)

  p4_00=codemog_api(data="p4", db="c2000",geonum=paste("1", state, fips, sep=""),meta="no")
  p4_00[,7:ncol(p4_00)]=as.numeric(as.character(p4_00[,7:ncol(p4_00)]))
  p4_00=p4_00%>%
    select(geoname:p4011)%>%
    mutate(TotalPop=p4001, Hispanic=p4002, NonHispanic=p4003, NHWhite=p4005, NHBlack=p4006,
           NHAIAN=p4007, NHAsian=p4008, NHNHOPI=p4009, NHOther=p4010, NHTwo=p4011,
           HispanicP=percent(round(Hispanic/TotalPop,3)),
           NonHispanicP=percent(round(NonHispanic/TotalPop,3)),
           NHWhiteP=percent(round(NHWhite/TotalPop,3)),
           NHBlackP=percent(round(NHBlack/TotalPop,3)),
           NHAIANP=percent(round(NHAIAN/TotalPop,3)),
           NHAsianP=percent(round(NHAsian/TotalPop,3)),
           NHNHOPIP=percent(round(NHNHOPI/TotalPop,3)),
           NHOtherP=percent(round(NHOther/TotalPop,3)),
           NHTwoP=percent(round(NHTwo/TotalPop,3)),
           TotalPop=comma(p4001), Hispanic=comma(p4002), NonHispanic=comma(p4003), NHWhite=comma(p4005), NHBlack=comma(p4006),
           NHAIAN=comma(p4007), NHAsian=comma(p4008), NHNHOPI=comma(p4009), NHOther=comma(p4010), NHTwo=comma(p4011))%>%
    select(-p4001:-p4011)%>%
    gather(race, Census.2000, TotalPop:NHTwoP, -geoname:-geonum)
p4=inner_join(p9_10, p4_00)
return(p4)
}
