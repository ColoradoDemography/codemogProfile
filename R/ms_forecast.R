#' Creates a Chart comparing Forecast Job and Population Growth in CO
#'
#' Uses the data from the State Demography Office package codemog to
#' create a growth showing population and job changes by Colorado County
#' from 2010 to 2030.
#' @param fips is the fips code for the county being examined
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#'
ms_forecast=function(fips, base=12){
require(car, quietly=TRUE)
require(codemog, quietly=TRUE)
require(ggplot2, quietly=TRUE)
require(scales, quietly=TRUE)
require(grid, quietly=TRUE)
require(tidyr, quietly=TRUE)
require(dplyr, quietly=TRUE)

fips=as.numeric(fips)
jobsfips=ifelse(fips==1|fips==5|fips==13|fips==14| fips==31|fips==35|
                             fips==59, 500, fips)
if (fips==1|fips==5|fips==13|fips==14|fips==31|fips==35| fips==59){
  popfips=c(1,5,13,14,31,35,59)
  }else{
    popfips=fips
  }

yrs=c(2005,2010,2015,2020,2025,2030)

j=county_job_forecast(jobsfips,yrs )%>%
  # filter(year %in% yrs)%>%
  arrange(countyfips,year)%>%
  mutate(countyfips=as.numeric(countyfips),
         jobChange=as.numeric(totalJobs-lag(totalJobs)),
         jobChangep=jobChange/lag(totalJobs))
p=county_sya(popfips, yrs)%>%
  # filter(year %in% yrs)%>%
  mutate(countyfips=ifelse(countyfips==1|countyfips==5|countyfips==13|countyfips==14| countyfips==31|countyfips==35|
                           countyfips==59, 500, as.numeric(countyfips)),
         county=ifelse(countyfips==500, "Denver Metropolitan Area", county))%>%
  group_by(countyfips, county, year)%>%
  summarise(totalPopulation=sum(as.numeric(totalpopulation)))%>%
  arrange(countyfips, year)%>%
  mutate(popChange=totalPopulation-lag(totalPopulation),
         popChangep=popChange/lag(totalPopulation))
d=inner_join(j,p)%>%
  select(countyfips, county, year, jobChange, popChange)%>%
  gather(variable, value, -countyfips, -year, -county)%>%
  filter(countyfips==jobsfips, year>2005)

gg=d%>%
  ggplot(aes(x=year, y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")+
  scale_y_continuous(label=comma)+
  scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(0,149,58,max=255)),
                    name="",
                    breaks=c("jobChange", "popChange"),
                    labels=c("Job Change","Population Change"))+
  theme_codemog(base_size=base)+
  theme(plot.title = element_text(hjust = 0, size = rel(1.25), face = "bold"))+
  labs(x="Year", y="Change", title=paste0(d$county," County\nForecast Change in Population and Jobs 2010 to 2030\nSource:State Demography Office"))
return(gg)
}
