#' Creates a chart with two plots, one for population trend, one for components of change
#'
#' This function creates two seperate time-series plots, a line chart for popultion trends
#' and a dodged bar chart for components of change over time.  The two plots are stacked together and
#' placed into one chart.
#'
#' @param fips is the fips code for the main area to be compared
#' @param state is the state that the original fips
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#'
#'
cp_migjobs=function(fips, countyname, base=12){
require(codemog, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(grid, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)

j=county_jobs%>%
  filter(sector_id==0, year<=2013)%>%
  select(countyfips, year, jobs)%>%
  group_by(countyfips)%>%
  arrange(year)%>%
  mutate(jobs=as.numeric(jobs),
         jobChange=jobs-lag(jobs))
nm=county_profile%>%
  filter(year>2000)%>%
  select(year, countyfips, netMigration)

p=inner_join(j,nm, by=c("countyfips", "year"))%>%
filter(countyfips==as.numeric(fips), year>2001)%>%
  ggplot()+
  geom_bar(stat="identity",aes(x=year, y=jobChange, fill="jobChange"))+
  geom_line(aes(x=year, y=netMigration,color="netMigration"), size=1.15)+
  scale_x_continuous(breaks=2002:2013)+
  scale_color_manual(" ",values=c("jobChange"=rgb(31,74,126, max=255), "netMigration"=rgb(191,32,38, max=255)),
                     labels=c("Net Migration"))+
  scale_fill_manual("", values=rgb(31,74,126, max=255), labels="Job Change")+
  theme_codemog(base_size=base)+
  theme(legend.box="horizontal")+
  labs(x="Year", y="Count", title=paste(countyname, "County Job Change and Net Migration, 2002 to 2013\nSource: State Demography Office"))

return(p)
}
