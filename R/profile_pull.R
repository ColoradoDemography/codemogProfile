#'  MainStreets Pull Function
#'
#'  This function creates the tables, graphs, and file paths for merging
#'  into the Community Profile InDesign template. The output of the function
#'  is a dataframe with the proper formatting to do the data merege.
#'
#'  @param fips The FIPS of the Place or County to use
#'  @param state  The State FIPS to use.  Defaults to CO.
#'  @param fips2 The FIPS of the Place or County to use for comparison
#'  @param state2 The State FIPS to use as comparison.  Defaults to CO.
#'  @param od The output directory for the graphs being created.
#'
ms_muni=function(fips, fips2="", countyfips, countyname, state="08", state2="08", od=""){
  require(codemog, quietly=TRUE)
  require(codemogAPI, quietly=TRUE)
  require(scales, quietly=TRUE)
  require(rmarkdown, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(stringi, quietly=TRUE)
  require(dplyr, quietly=TRUE)


  # yrs=c("1990","1995","2000","2010","2014","2015","2020","2025","2030","2035","2040")
  yrs=c(1990,1995,2000,2010,2014,2015,2020,2025,2030,2035,2040)
  cntynum=as.numeric(countyfips)
  # countyname=county_est%>%filter(countyfips==cntynum, year==2014)%>%select(county)
  ## Graphs
  # This set makes all of the graphs and saves them to the output directory
  fips_t=fips
  fips=as.numeric(fips)
  ed=ms_ed(fips=fips_t,fips2=fips2, state=state, state2=state2, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("ed_",fips_t,".png"), ed, path=od, width=96, height=48, units="mm")
  age=ms_census_age(fips=fips_t,state=state, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("age_",fips_t,".png"), age, path=od, width=95, height=55, units="mm")
  hh=ms_hh(fips=fips_t, state=state, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("hh_",fips_t,".png"), hh, path=od, width=118, height=69, units="mm")
  incdist=ms_income(fips=fips_t, fips2=fips2, state=state, state2=state2, base=7.5)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("incdist_",fips_t,".png"), incdist, path=od, width=155, height=75, units="mm")
  popchart=muni_ts_chart(fips, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("popchart_",fips_t,".png"), popchart, path=od,width=93, height=53, units="mm")
  jobchart=ms_jobs(fips=countyfips, countyname=countyname, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"), plot.title = element_text(hjust = 0, size = rel(1.25), face = "bold"))
  ggsave(filename=paste0("jobchart_",fips_t,".png"), jobchart, path=od,width=93, height=53, units="mm")
  forecastchart=ms_forecast(fips=countyfips, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"), plot.title = element_text(hjust = 0, size = rel(1.2), face = "bold"))
  ggsave(filename=paste0("forecastchart_",fips_t,".png"), forecastchart, path=od,width=93, height=53, units="mm")
  popagechart=ms_popage(fips=countyfips, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"), plot.title = element_text(hjust = 0, size = rel(1.2), face = "bold"))
  ggsave(filename=paste0("popagechart_",fips_t,".png"), popagechart, path=od,width=93, height=53, units="mm")
  map=cp_countymap(cntynum)
  ggsave(paste0("map_", as.character(countyname), ".png"), map, h=51, w=80, units="mm")


  ## This Section Generates the requisite Population TimeSeries
  popMuni=muni_hist(fips,1990:2015)%>%
    filter(year %in% yrs)%>%
    mutate(year=as.numeric(year))%>%
    arrange(year)%>%
    mutate(popChange=comma(totalpopulation-lag(totalpopulation)),
           growthRate=paste0(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)), digits=1),"%"),
           totalPop=comma(totalpopulation))
  muni_pop_chng1015=popMuni%>%filter(year==2015)%>%select(popChange)

  popCO=county_profile(0, 1990:2015, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    mutate(name="Colorado",
           totalpopulation=as.numeric(totalpopulation),
           year=as.numeric(year),
           growthRate=paste0(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)), digits=1),"%"),
           totalPop=comma(totalpopulation))
  popCounty=county_profile(cntynum, 1990:2015, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    arrange(year)%>%
    mutate(name=county,
           year=as.numeric(year),
           totalpopulation=as.numeric(totalpopulation),
           growthRate=paste0(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)), digits=1),"%"),
           totalPop=comma(totalpopulation))
  pop=popMuni%>%
    select(-c(placefips, growthRate, popChange))%>%
    mutate(name="muni")%>%
    bind_rows(popCO%>%select(-growthRate))%>%
    bind_rows(popCounty%>%select(-c(countyfips, growthRate))%>%
                mutate(name="county"))%>%
    mutate(geoname=name,
           name=paste(name,year,"pop",sep="_"),
           geonum=as.numeric(paste("108", fips_t, sep="")),
           municipality=ifelse(is.na(municipality), county, municipality))%>%
    select(-municipality:-totalpopulation, -countyfips:-geoname)%>%
    spread(name,totalPop)
  popr=popMuni%>%
    select(-c(placefips, totalPop, popChange))%>%
    mutate(name="muni")%>%
    bind_rows(popCO%>%select(-totalPop))%>%
    bind_rows(popCounty%>%select(-c(countyfips, totalPop))%>%
                mutate(name="county"))%>%
    mutate(name=paste(name,year,"gr",sep="_"),
           geonum=as.numeric(paste("108", fips_t, sep="")))%>%
    select(-municipality:-totalpopulation, -countyfips:-county)%>%
    spread(name,growthRate)

  countyjobs=jobchart$data%>%
    filter(year==2014)%>%
    mutate(county_jobs_2015=comma(jobs,0),
           geonum=as.numeric(paste("108", fips_t, sep="")))%>%
    select(county_jobs_2015, geonum)
  coli=county_coli%>%
    filter(countyfips==cntynum)%>%
    mutate(coli_level=paste(coli, level, sep=", "))%>%
    select(coli_level)
  yrs_f=c(2005,2010,2015,2020,2025,2030)
  jobsfips=ifelse(cntynum==1|cntynum==5|cntynum==13|cntynum==14| cntynum==31|cntynum==35|
                    cntynum==59, 500, cntynum)
  j=county_job_forecast(jobsfips, yrs_f)%>%
    arrange(countyfips,year)%>%
    mutate(cntyfips=countyfips,
           jobChange=as.numeric(totalJobs-lag(totalJobs)),
           jobChangep=jobChange/lag(totalJobs),
           countyfips=as.numeric(countyfips))
  p=county_sya(cntynum, yrs_f)%>%
    # filter(year %in% yrs)%>%
    mutate(countyfips=ifelse(countyfips==1|countyfips==5|countyfips==13|countyfips==14| countyfips==31|countyfips==35|
                               countyfips==59, 500, as.numeric(countyfips)),
           county=ifelse(countyfips==500, "Denver Metropolitan Area", county))%>%
    group_by(countyfips, county, year)%>%
    summarise(totalPopulation=sum(as.numeric(totalpopulation)))%>%
    arrange(countyfips, year)%>%
    mutate(popChange=totalPopulation-lag(totalPopulation),
           popChangep=popChange/lag(totalPopulation))
  popjobsforecast=inner_join(j,p)%>%
    select(countyfips, county, year,totalPopulation, totalJobs)%>%
    gather(variable, value, -countyfips, -year, -county)%>%
    filter(countyfips==jobsfips, year>2005)%>%
    mutate(forecast_name="county",
           name=paste(forecast_name, variable, year, sep="_"),
           value=comma(value,0))%>%
    select(name, value, forecast_name)%>%
    spread(name, value)


  forecastnumbers=forecastchart$data%>%
    mutate(name=paste(variable,year,sep="_"),
           econ_name=county,
           value=comma(value,0))%>%
    select(econ_name,name, value)%>%
    spread(name,value)


  ### Census Pulls Using the API
  housing=ms_housing(fips_t, state)%>%
    gather(type, value,Census.2000:Census.2010,  -geoname:-geonum)%>%
    mutate(name=paste0("hh",var,type))%>%
    select(geonum, geoname, value,name)%>%
    spread(name, value)
  race=ms_race(fips_t, state)%>%
    gather(type, value,Census.2010:Census.2000,  -geoname:-geonum)%>%
    mutate(name=paste0("race",race,type))%>%
    select(geonum, geoname, value,name)%>%
    mutate(geonum=as.numeric(geonum))%>%
    spread(name, value)
  mhi=ms_mhi(fips=fips_t, fips2=fips2, state=state, state2=state2)%>%mutate(geonum=as.numeric(geonum))

  df=inner_join(pop, popr, by="geonum")%>%
    inner_join(housing, by="geonum")%>%
    inner_join(race, by="geonum")%>%
    inner_join(mhi, by="geonum")%>%
    inner_join(countyjobs, by="geonum")%>%
    bind_cols(forecastnumbers)%>%
    bind_cols(popjobsforecast)%>%
    mutate(coli_level=coli$coli_level,
           munichng_1015=muni_pop_chng1015$popChange,
           countyName=as.character(countyname),
           ed=paste0(od,"/ed_",fips,".png"),
           agegraph=paste0(od,"/age_",fips,".png"),
           hhgraph=paste0(od,"/hh_",fips,".png"),
           incdistchart=paste0(od,"/incdist_",fips,".png"),
           popchart=paste0(od,"/popchart_",fips,".png"),
           jobchart=paste0(od,"/jobchart_",fips,".png"),
           forecastchart=paste0(od,"/forecastchart_",fips,".png"),
           popagechart=paste0(od,"/popagechart_",fips,".png"),
           map=paste0(od,"/map_",as.character(countyname),".png"))
  names(df)[132:140]=c("@ed", "@agegraph", "@hhgraph", "@incdistchart", "@popchart", "@jobchart", "@forecastchart", "@popagechart", "@map")
  # save.xlsx(paste(od, "/rawdata_",fips,".xlsx", sep=""), pop, popr, housing, hh$data, race, mhi, ed$data, age$data, incdist$data, jobchart$data)
  # rmarkdown::render(system.file("misc", "muni_profile_charts.Rmd", package = "codemogProfile"), output_file=paste0(od,"/muniprofileCharts",fips,".html"))
  return(df)
}

#'  MainStreets Pull Function - Counties
#'
#'  This function creates the tables, graphs, and file paths for merging
#'  into the Community Profile InDesign template. The output of the function
#'  is a dataframe with the proper formatting to do the data merege.
#

#'  @param fips The FIPS of the Place or County to use
#'  @param state  The State FIPS to use.  Defaults to CO.
#'  @param fips2 The FIPS of the Place or County to use for comparison
#'  @param state2 The State FIPS to use as comparison.  Defaults to CO.
#'  @param od The output directory for the graphs being created.
#'
cp_county=function(fips, countyname, fips2="", state="08", state2="08", od=""){
  require(codemog, quietly=TRUE)
  require(codemogProfile, quietly=TRUE)
  require(codemogAPI, quietly=TRUE)
  require(scales, quietly=TRUE)
  require(rmarkdown, quietly=TRUE)
  require(robR, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(stringi, quietly=TRUE)
  require(dplyr, quietly=TRUE)


  yrs=c("1990","1995","2000","2010", "2014","2015","2020","2025","2030","2035","2040")
  cntynum=as.numeric(fips)
  # countyname=county_est%>%filter(countyfips==cntynum, year==2014)%>%select(county)
  ## Graphs
  # This set makes all of the graphs and saves them to the output directory

  ed=ms_ed(fips=fips,fips2=fips2, state=state, state2=state2, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("ed_",fips,".png"), ed, path=od, width=96, height=48, units="mm")
  age=ms_census_age(fips=fips,state=state, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("age_",fips,".png"), age, path=od, width=95, height=55, units="mm")
  hh=ms_hh(fips=fips, state=state, base=6)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("hh_",fips,".png"), hh, path=od, width=118, height=69, units="mm")
  incdist=ms_income(fips=fips, fips2=fips2, state=state, state2=state2, base=7.5)+theme(legend.text=element_text(size=7.2), legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("incdist_",fips,".png"), incdist, path=od, width=155, height=75, units="mm")
  popchart=county_ts_chart(fips, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"))
  ggsave(filename=paste0("popchart_",fips,".png"), popchart, path=od,width=93, height=53, units="mm")
  jobchart=ms_jobs(fips=fips, countyname=countyname, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"), plot.title = element_text(hjust = 0, size = rel(1.25), face = "bold"))
  ggsave(filename=paste0("jobchart_",fips,".png"), jobchart, path=od,width=93, height=53, units="mm")
  forecastchart=ms_forecast(fips=fips, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"), plot.title = element_text(hjust = 0, size = rel(1.2), face = "bold"))
  ggsave(filename=paste0("forecastchart_",fips,".png"), forecastchart, path=od,width=93, height=53, units="mm")
  popagechart=ms_popage(fips=fips, base=6)+theme(legend.key.size=unit(1, "mm"), legend.margin=unit(0, "mm"), panel.margin=unit(0, "mm"), plot.title = element_text(hjust = 0, size = rel(1.2), face = "bold"))
  ggsave(filename=paste0("popagechart_",fips,".png"), popagechart, path=od,width=93, height=53, units="mm")
  map=cp_countymap(cntynum)
  ggsave(paste0("map_", as.character(countyname), ".png"), map, h=51, w=80, units="mm")
  ## This Section Generates the requisite Population TimeSeries


  popCO=county_profile(0, 1990:2015, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    mutate(name="Colorado",
           totalpopulation=as.numeric(totalpopulation),
           year=as.numeric(year),
           growthRate=paste0(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)), digits=1),"%"),
           totalPop=comma(totalpopulation))
  popCounty=county_profile(cntynum, 1990:2015, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    arrange(year)%>%
    mutate(name=county,
           year=as.numeric(year),
           totalpopulation=as.numeric(totalpopulation),
           growthRate=paste0(round(ann_gr(lag(totalpopulation), totalpopulation, year-lag(year)), digits=1),"%"),
           totalPop=comma(totalpopulation),
           popChange=totalpopulation-lag(totalpopulation))
  county_pop_chng1015=popCounty%>%filter(year==2015)%>%select(popChange)
  pop=popCounty%>%
    select(-c(countyfips, growthRate, popChange))%>%
    mutate(name="county")%>%
    bind_rows(popCO%>%select(-growthRate))%>%
    mutate(geoname=name,
           name=paste(name,year,"pop",sep="_"),
           geonum=as.numeric(paste("108", fips, sep="")))%>%
    select(-county:-totalpopulation, -countyfips, -geoname)%>%
    spread(name,totalPop)
  popr=popCounty%>%select(-c(countyfips, totalPop, totalPop, popChange))%>%
    mutate(name="county")%>%
    bind_rows(popCO%>%select(-totalPop))%>%
    mutate(name=paste(name,year,"gr",sep="_"),
           geonum=as.numeric(paste("108", fips, sep="")))%>%
    select(-county:-totalpopulation, -countyfips)%>%
    spread(name,growthRate)

  countyjobs=jobchart$data%>%
    filter(year==2014)%>%
    mutate(county_jobs_2014=comma(jobs,0),
           geonum=as.numeric(paste("108", fips, sep="")))%>%
    select(county_jobs_2014, geonum)
  coli=county_coli%>%
    filter(countyfips==cntynum)%>%
    mutate(coli_level=paste(coli, level, sep=", "))%>%
    select(coli_level)

  forecastnumbers=forecastchart$data%>%
    mutate(name=paste(variable,year,sep="_"),
           econ_name=county,
           value=comma(value,0))%>%
    select(econ_name,name, value)%>%
    spread(name,value)

  yrs_f=c("2005", "2010", "2015", "2020", "2025", "2030")
  jobsfips=ifelse(cntynum==1|cntynum==5|cntynum==13|cntynum==14| cntynum==31|cntynum==35|
                    cntynum==59, 500, cntynum)
  yrs_f=c(2005,2010,2015,2020,2025,2030)
  jobsfips=ifelse(cntynum==1|cntynum==5|cntynum==13|cntynum==14| cntynum==31|cntynum==35|
                    cntynum==59, 500, cntynum)
  j=county_job_forecast(jobsfips, yrs_f)%>%
    arrange(countyfips,year)%>%
    mutate(cntyfips=countyfips,
           jobChange=as.numeric(totalJobs-lag(totalJobs)),
           jobChangep=jobChange/lag(totalJobs),
           countyfips=as.numeric(countyfips))
  p=county_sya(cntynum, yrs_f)%>%
    # filter(year %in% yrs)%>%
    mutate(countyfips=ifelse(countyfips==1|countyfips==5|countyfips==13|countyfips==14| countyfips==31|countyfips==35|
                               countyfips==59, 500, as.numeric(countyfips)),
           county=ifelse(countyfips==500, "Denver Metropolitan Area", county))%>%
    group_by(countyfips, county, year)%>%
    summarise(totalPopulation=sum(as.numeric(totalpopulation)))%>%
    arrange(countyfips, year)%>%
    mutate(popChange=totalPopulation-lag(totalPopulation),
           popChangep=popChange/lag(totalPopulation))
  popjobsforecast=inner_join(j,p)%>%
    select(countyfips, county, year,totalPopulation, totalJobs)%>%
    gather(variable, value, -countyfips, -year, -county)%>%
    filter(countyfips==jobsfips, year>2005)%>%
    mutate(forecast_name="county",
           name=paste(forecast_name, variable, year, sep="_"),
           value=comma(value,0))%>%
    select(name, value, forecast_name)%>%
    spread(name, value)

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
    inner_join(countyjobs, by="geonum")%>%
    bind_cols(forecastnumbers)%>%
    bind_cols(popjobsforecast)%>%
    mutate(coli_level=coli$coli_level,
           countychng_1015=county_pop_chng1015$popChange,
           countyName=as.character(countyname),
           ed=paste0(od,"/ed_",fips,".png"),
           agegraph=paste0(od,"/age_",fips,".png"),
           hhgraph=paste0(od,"/hh_",fips,".png"),
           incdistchart=paste0(od,"/incdist_",fips,".png"),
           popchart=paste0(od,"/popchart_",fips,".png"),
           jobchart=paste0(od,"/jobchart_",fips,".png"),
           forecastchart=paste0(od,"/forecastchart_",fips,".png"),
           popagechart=paste0(od,"/popagechart_",fips,".png"),
           map=paste0(od,"/map_",as.character(countyname),".png"))
  names(df)[122:130]=c("@ed", "@agegraph", "@hhgraph", "@incdistchart", "@popchart", "@jobchart", "@forecastchart", "@popagechart", "@map")
  #   save.xlsx(paste(od, "/rawdata_",fips,".xlsx", sep=""), pop, popr, housing, hh$data, race, mhi, ed$data, age$data, incdist$data, jobchart$data)
  # rmarkdown::render(system.file("misc", "muni_profile_charts.Rmd", package = "codemogProfile"), output_file=paste0(od,"/muniprofileCharts",fips,".html"))
  return(df)
}
