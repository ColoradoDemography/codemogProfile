#'  CO Housing Unit Table
#'
#'  This function pulls data on housing types by vacancy, vacancy type, and
#'  tenure from the 2000 and 2010 Census data.
#'
#'  @param fips The FIPS of the Place or County to use for the graph
#'  @param state  The State FIPS to use.  Defaults to CO.
#'


ms_housing=function(fips, state="08"){
require(scales, quietly=TRUE)
require(dplyr, quietly=TRUE)

h3_10=codemog_api(data="h3", geonum=paste("1", state, fips, sep=""),meta="no")
h3_10[,7:ncol(h3_10)]=as.numeric(as.character(h3_10[,7:ncol(h3_10)]))
h3_10=h3_10%>%rename(Total=h3001, Occupied=h3002, Vacant=h3003)%>%
  mutate(OccPercent=percent(Occupied/Total),
         VacPercent=percent(Vacant/Total),
         Occupied=comma(Occupied),
         Vacant=comma(Vacant))

h4_10=codemog_api(data="h4", geonum=paste("1", state, fips, sep=""),meta="no")
h4_10[,7:ncol(h4_10)]=as.numeric(as.character(h4_10[,7:ncol(h4_10)]))
h4_10=h4_10%>%  mutate(Owner=h4002+h4003,
                       Renter=h4004)%>%
  select(-h4001:-h4004)%>%
  mutate(ownPercent=percent(Owner/(Owner+Renter)),
         rentPercent=percent(Renter/(Owner+Renter)),
         Owner=comma(Owner),
         Renter=comma(Renter))
h5_10=codemog_api(data="h5", geonum=paste("1", state, fips, sep=""),meta="no")
h5_10[,7:ncol(h5_10)]=as.numeric(as.character(h5_10[,7:ncol(h5_10)]))
h5_10=h5_10%>%
  mutate(Seasonal=h5006,
         Other=h5002+h5003+h5004+h5005+h5007+h5008)%>%
  select(-h5001:-h5008)%>%
  mutate(seasPercent=percent(Seasonal/(Other+Seasonal)),
         otherPercent=percent(Other/(Other+Seasonal)),
         Other=comma(Other),
         Seasonal=comma(Seasonal))
housing_10=inner_join(h3_10,h5_10)%>%inner_join(h4_10)%>%
  gather(var, Census.2010, Total:rentPercent, -geoname, -state, -county, -place,-tract,-bg,-geonum)
h3_00=codemog_api(data="h3",db="c2000", geonum=paste("1", state, fips, sep=""),meta="no")
h3_00[,7:ncol(h3_00)]=as.numeric(as.character(h3_00[,7:ncol(h3_00)]))
h3_00=h3_00%>%rename(Total=h3001, Occupied=h3002, Vacant=h3003)%>%
  mutate(OccPercent=percent(Occupied/Total),
         VacPercent=percent(Vacant/Total),
         Total=comma(Total),
         Occupied=comma(Occupied),
         Vacant=comma(Vacant))

h4_00=codemog_api(data="h4",db="c2000", geonum=paste("1", state, fips, sep=""),meta="no")
h4_00[,7:ncol(h4_00)]=as.numeric(as.character(h4_00[,7:ncol(h4_00)]))
h4_00=h4_00%>%  rename(Owner=h4002,
                       Renter=h4003)%>%
  select(-h4001)%>%
  mutate(ownPercent=percent(Owner/(Owner+Renter)),
         rentPercent=percent(Renter/(Owner+Renter)),
         Owner=comma(Owner),
         Renter=comma(Renter))
h5_00=codemog_api(data="h5",db="c2000", geonum=paste("1", state, fips, sep=""),meta="no")
h5_00[,7:ncol(h5_00)]=as.numeric(as.character(h5_00[,7:ncol(h5_00)]))
h5_00=h5_00%>%
  mutate(Seasonal=h5005,
         Other=h5002+h5003+h5004+h5006+h5007)%>%
  select(-h5001:-h5007)%>%
  mutate(seasPercent=percent(Seasonal/(Other+Seasonal)),
         otherPercent=percent(Other/(Other+Seasonal)),
         Other=comma(Other),
         Seasonal=comma(Seasonal))

housing_00=inner_join(h3_00,h5_00)%>%inner_join(h4_00)%>%
  gather(var, Census.2000, Total:rentPercent, -geoname, -state, -county, -place,-tract,-bg,-geonum)

housing=inner_join(housing_00,housing_10)
return(housing)
}
