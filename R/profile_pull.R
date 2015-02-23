#'  MainStreets Pull Function
#'
#'  This function creates the tables, graphs, and file paths for merging
#'  into the Mainstreets Profile InDesign template. The output of the function
#'  is a dataframe with the proper formatting to do the data merege.
#'
#'  @param fips The FIPS of the Place or County to use
#'  @param state  The State FIPS to use.  Defaults to CO.
#'  @param fips2 The FIPS of the Place or County to use for comparison
#'  @param state2 The State FIPS to use as comparison.  Defaults to CO.
#'  @param od The output directory for the graphs being created.
#'
ms_muni=function(fips, fips2="", state="08", state2="08", od=""){
require(codemog, quietly=TRUE)
require(robR, quietly=TRUE)
require(tidyr, quietly=TRUE)
require(stringi, quietly=TRUE)
require(dplyr, quietly=TRUE)


yrs=c(1990,1995,2000,2005,2010, 2013,2015,2020,2025,2030,2035,2040)

## Graphs
# This set makes all of the graphs and saves them to the output directory

ed=ms_ed(fips=fips,fips2=fips2, state=state, state2=state2, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
ggsave(filename=paste0("ed_",fips,".png"), ed, path=od, width=96, height=48, units="mm")
age=ms_census_age(fips=fips,state=state, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
ggsave(filename=paste0("age_",fips,".png"), age, path=od, width=95, height=55, units="mm")
hh=ms_hh(fips=fips, state=state, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
ggsave(filename=paste0("hh_",fips,".png"), hh, path=od, width=118, height=69, units="mm")
incdist=ms_income(fips=fips, fips2=fips2, state=state, state2=state2, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
ggsave(filename=paste0("incdist_",fips,".png"), incdist, path=od, width=155, height=75, units="mm")
popchart=muni_ts_chart(fips, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
ggsave(filename=paste0("popchart_",fips,".png"), popchart, path=od,width=93, height=53, units="mm")

## This Section Generates the requisite Population TimeSeries
popMuni=muni_est%>%
  mutate(year=as.numeric(as.character(year)),
         placefips=as.numeric(as.character(placefips)),
         geonum=as.numeric(as.character(geonum)))%>%
  select(geonum, placefips, municipality, year, totalPopulation)%>%
  bind_rows(muni_hist%>%select(-countyfips))%>%
  filter(year %in% yrs)%>%
  group_by(placefips,municipality,year)%>%
  summarise(totalPop=sum(totalPopulation))%>%
  filter(placefips==as.numeric(fips))%>%
  rename(name=municipality)%>%ungroup()%>%
  mutate(growthRate=round(ann.gr(lag(totalPop), totalPop, year-lag(year)), digits=2),
         totalPop=comma(totalPop))

popCO=county_est%>%
  mutate(year=as.numeric(as.character(year)),
         countyfips=as.numeric(as.character(countyfips)),
         geonum=as.numeric(as.character(geonum)))%>%
  select(geonum, countyfips, year, totalPopulation)%>%
  bind_rows(county_hist%>%select(-c(datatype, county)))%>%
  filter(year %in% yrs, countyfips>0)%>%
  group_by(year)%>%
  summarise(totalPop=sum(totalPopulation, na.rm=T))%>%
  mutate(name="Colorado",
         growthRate=paste0(round(ann.gr(lag(totalPop), totalPop, year-lag(year)), digits=1),"%"),
         totalPop=comma(totalPop))
pop=popMuni%>%
  select(-c(placefips, growthRate))%>%
  mutate(name="muni")%>%
    bind_rows(popCO%>%select(-growthRate))%>%
  mutate(geoname=name,
         name=paste(name,year,"pop",sep="_"),
         geonum=as.numeric(paste("108", fips, sep="")))%>%
  select(-year, -geoname)%>%
  spread(name,totalPop)
popr=popMuni%>%
  select(-c(placefips, totalPop))%>%
  mutate(name="muni")%>%
  bind_rows(popCO%>%select(-totalPop))%>%
  mutate(name=paste(name,year,"gr",sep="_"),
         geonum=as.numeric(paste("108", fips, sep="")))%>%
  select(-year)%>%
  spread(name,growthRate)

### Census Pulls Using the API
housing=ms_housing(fips, state)%>%
  gather(type, value,Census.2000:Census.2010,  -geoname:-geonum)%>%
  mutate(name=paste0("hh",var,type))%>%
  select(geonum, geoname, value,name)%>%
  spread(name, value)
race=ms_race(fips, state)%>%
  gather(type, value,Census.2010:Census.2000,  -geoname:-geonum)%>%
  mutate(name=paste0("race",race,type))%>%
  select(geonum, geoname, value,name)%>%
  mutate(geonum=as.numeric(geonum))%>%
  spread(name, value)
mhi=ms_mhi(fips=fips, fips2=fips2, state=state, state2=state2)%>%mutate(geonum=as.numeric(geonum))

df=inner_join(pop, popr, by="geonum")%>%
  inner_join(housing, by="geonum")%>%
  inner_join(race, by="geonum")%>%
  inner_join(mhi, by="geonum")%>%
  mutate(ed=paste0(od,"/ed_",fips,".png"),
         agegraph=paste0(od,"/age_",fips,".png"),
         hhgraph=paste0(od,"/hh_",fips,".png"),
         incdistchart=paste0(od,"/incdist_",fips,".png"),
         popchart=paste0(od,"/popchart_",fips,".png"))
save.xlsx(paste(od, "/rawdata_",fips,".xlsx", sep=""), pop, popr, housing, hh$data, race, mhi, ed$data, age$data, incdist$data)

return(df)
}

#'  MainStreets Pull Function - Counties
#'
#'  This function creates the tables, graphs, and file paths for merging
#'  into the Mainstreets Profile InDesign template. The output of the function
#'  is a dataframe with the proper formatting to do the data merege.
#'
#'  @param fips The FIPS of the Place or County to use
#'  @param state  The State FIPS to use.  Defaults to CO.
#'  @param fips2 The FIPS of the Place or County to use for comparison
#'  @param state2 The State FIPS to use as comparison.  Defaults to CO.
#'  @param od The output directory for the graphs being created.

ms_county=function(fips, fips2="", state="08", state2="08", od=""){
  require(codemog, quietly=TRUE)
  require(robR, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(scales, quietly=TRUE)
  require(stringi, quietly=TRUE)
  require(dplyr, quietly=TRUE)


  yrs=c(1990,1995,2000,2005,2010, 2013,2015,2020,2025,2030,2035,2040)

  ## Graphs
  # This set makes all of the graphs and saves them to the output directory

  ed=ms_ed(fips=fips,fips2=fips2, state=state, state2=state2, base=5.5)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("ed_",fips,".png"), ed, path=od, width=172, height=75, units="mm")
  edpath=paste0(od,"/ed_",fips,".png")
  age=ms_census_age(fips=fips,state=state, base=5.5)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("age_",fips,".png"), age, path=od, , width=95, height=55, units="mm")
  agepath=paste0(od,"/age_",fips,".png")
  hh=ms_hh(fips=fips, state=state)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("hh_",fips,".png"), hh, path=od, width=118, height=69, units="mm")
  hhpath=paste0(od,"/hh_",fips,".png")
  incdist=ms_income(fips=fips, fips2=fips2, state=state, state2=state2, base=5.5)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("incdist_",fips,".png"), incdist, path=od, width=109, height=60, units="mm")
  incdistpath=paste0(od,"/incdist_",fips,".png")
  popchart=county_ts_chart(fips, base=5.5)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("popchart_",fips,".png"), popchart, path=od,width=93, height=53, units="mm")
  popchartpath=paste0(od,"/popchart_",fips,".png")

  ## This Section Generates the requisite Population TimeSeries
  popCount=county_est%>%
    mutate(year=as.numeric(as.character(year)),
           countyfips=as.numeric(as.character(countyfips)),
           geonum=as.numeric(as.character(geonum)),
           datatype="Estimate",
           county=str_replace(county, " COUNTY", ""))%>%
    select(geonum, countyfips, county, year, totalPopulation)%>%
    bind_rows(county_hist)%>%
    filter(year %in% yrs)%>%
    group_by(countyfips,county,year)%>%
    summarise(totalPop=sum(totalPopulation))%>%
    filter(countyfips==as.numeric(fips))%>%
    rename(name=county)%>%ungroup()%>%
    mutate(growthRate=round(ann.gr(lag(totalPop), totalPop, year-lag(year)), digits=2),
           totalPop=comma(totalPop))
  popCast=county_forecast%>%
    select(countyfips, county, year, totalPopulation, datatype)%>%
    filter(year %in% yrs, datatype=="Forecast", countyfips==as.numeric(fips))%>%
    group_by(county,year)%>%
    summarise(totalPop=sum(totalPopulation))%>%
    rename(name=county)%>%ungroup()%>%
    mutate(growthRate=round(ann.gr(lag(totalPop), totalPop, year-lag(year)), digits=2),
           totalPop=comma(totalPop))
  popCastCO=county_forecast%>%
    select(countyfips, county, year, totalPopulation, datatype)%>%
    filter(year %in% yrs, datatype=="Forecast")%>%
    group_by(year)%>%
    summarise(totalPop=sum(totalPopulation))%>%
    ungroup()%>%
    mutate(name="Colorado",
           growthRate=round(ann.gr(lag(totalPop), totalPop, year-lag(year)), digits=2),
           totalPop=comma(totalPop))
  popCO=county_est%>%
    mutate(year=as.numeric(as.character(year)),
           countyfips=as.numeric(as.character(countyfips)),
           geonum=as.numeric(as.character(geonum)))%>%
    select(geonum, countyfips, year, totalPopulation)%>%
    bind_rows(county_hist%>%select(-c(datatype, county)))%>%
    filter(year %in% yrs, countyfips>0)%>%
    group_by(year)%>%
    summarise(totalPop=sum(totalPopulation, na.rm=T))%>%
    mutate(name="Colorado",
           growthRate=round(ann.gr(lag(totalPop), totalPop, year-lag(year)), digits=2),
           totalPop=comma(totalPop))
  pop=popMuni%>%
    select(-c(placefips, growthRate))%>%
    bind_rows(popCO%>%select(-growthRate))%>%
    mutate(geoname=name,
           name=paste(name,year,"pop",sep="_"),
           geonum=as.numeric(paste("108", fips, sep="")))%>%
    select(-year)%>%
    spread(name,totalPop)
  popr=popMuni%>%
    select(-c(placefips, totalPop))%>%
    bind_rows(popCO%>%select(-totalPop))%>%
    mutate(name=paste(name,year,"gr",sep="_"),
           geonum=as.numeric(paste("108", fips, sep="")))%>%
    select(-year)%>%
    spread(name,growthRate)

  ### Census Pulls Using the API
  housing=ms_housing(fips, state)%>%
    gather(type, value,Census.2000:Change,  -geoname:-geonum)%>%
    mutate(name=paste0("hh",var,type))%>%
    select(geonum, geoname, value,name)%>%
    spread(name, value)
  race=ms_race(fips, state)%>%
    gather(type, value,Census.2010:Change,  -geoname:-geonum)%>%
    mutate(name=paste0("race",race,type))%>%
    select(geonum, geoname, value,name)%>%
    mutate(geonum=as.numeric(geonum))%>%
    spread(name, value)
  mhi=ms_mhi(fips=fips, fips2=fips2, state=state, state2=state2)%>%mutate(geonum=as.numeric(geonum))

  df=inner_join(pop, popr, by="geonum")%>%
    inner_join(housing, by="geonum")%>%
    inner_join(race, by="geonum")%>%
    inner_join(mhi, by="geonum")%>%
    mutate(edgraph=edpath,
           agegraph=agepath,
           hhgraph=hhpath,
           incdistchart=incdistpath,
           popchart=popchartpath)

  return(df)
}

