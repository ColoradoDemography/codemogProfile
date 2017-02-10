#'  Income Distribution Graph
#'
#'  This function pulls data and generates a graph of the income distribution
#'  for the areas selected based on the ACS 09-13 5-year File.
#'
#'  @param fips The FIPS of the Place or County to use
#'  @param state  The State FIPS to use.  Defaults to CO.
#'  @param fips2 The FIPS of the Place or County to use for comparison
#'  @param state2 The State FIPS to use as comparison.  Defaults to CO.

ms_income=function(fips, fips2="", state="08", state2="08", base=12){
require(ggplot2, quietly=TRUE)
require(car, quietly=TRUE)
require(stringr, quietly=TRUE)
require(grid, quietly=TRUE)
require(scales, quietly=TRUE)
require(tidyr, quietly=TRUE)
require(dplyr, quietly=TRUE)
require(codemog, quietly=TRUE)


hhinc1=codemog_api(data="b19001",db="acs1115", geonum=paste("1", state, fips, sep=""), meta="no")%>%
  select(-b19001001)%>%
  gather(var, value, b19001002:b19001017, -geoname, -state, -county, -place,-tract,-bg,-geonum)%>%
  mutate(geoname=str_trim(geoname, side="both"),
         var2=str_sub(var, -2,-1),
         var3=as.numeric(as.character(var2)),
         group=car::recode(var3, "2=1; 3:4=2; 5:6=3;7:8=4;9:10=5; 11=6;12=7;13=8;14=9;
                      15=10;16=11;17=12"))%>%
  group_by(geoname,group)%>%
  summarise(value=sum(as.numeric(value)))%>%

  mutate(cat=ordered(group, levels=1:12, labels=c("Less than $10,000","$10,000 to $19,999","$20,000 to $29,999",
                                                  "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                  "$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999",
                                                  "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")))%>%
  separate(geoname, into=c("geoname", "state_name"), sep=",")%>%
  select(-state_name, -group)%>%
  mutate(p=as.numeric(value)/sum(as.numeric(value)))

hhinc2=codemog_api(data="b19001",db="acs1115", geonum=paste("1", state2, fips2, sep=""), meta="no")%>%
  select(-b19001001)%>%
  gather(var, value, b19001002:b19001017, -geoname, -state, -county, -place,-tract,-bg,-geonum)%>%
  mutate(geoname=str_trim(geoname, side="both"),
         var2=str_sub(var, -2,-1),
         var3=as.numeric(as.character(var2)),
         group=car::recode(var3, "2=1; 3:4=2; 5:6=3;7:8=4;9:10=5; 11=6;12=7;13=8;14=9;
                      15=10;16=11;17=12"))%>%
  group_by(geoname,group)%>%
  summarise(value=sum(as.numeric(value)))%>%

  mutate(cat=ordered(group, levels=1:12, labels=c("Less than $10,000","$10,000 to $19,999","$20,000 to $29,999",
                                                  "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                  "$60,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999",
                                                  "$125,000 to $149,999","$150,000 to $199,999","$200,000 or more")))%>%
  mutate(p=as.numeric(value)/sum(as.numeric(value)))

hhinc=bind_rows(hhinc1, hhinc2)

p=hhinc%>%ggplot(aes(x=cat, y=as.numeric(p), fill=geoname))+
  geom_bar(stat="identity", position="dodge")+
  scale_y_continuous(label=percent)+
  scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                    name="Area")+
  theme_codemog(base_size=base)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x="Income (in 2014 Dollars)", y="Percentage", title="Household Income Distribution\n Source: 2015 ACS 5-Year File")

return(p)
}
