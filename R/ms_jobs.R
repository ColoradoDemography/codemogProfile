#' Creates a Chart showing the Total Estimates Jobs series for each County in Colorado
#'
#' Uses State Demography Office data to create a chart showing the timeseries of Total Estimated Jobs
#' (which means it includes Proprietors and Agricultural Workers) for each Colorado county in 2013.
#'
#' @param fips is the fips code for the county being examined
#' @param countyname  This parameter puts the name of the county in the chart
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
ms_jobs=function(fips, countyname, base=12){
  require(car, quietly=TRUE)
  require(codemog, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)
  require(grid, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(dplyr, quietly=TRUE)

  total_jobs=county_jobs%>%
    filter(countyfips==as.numeric(fips),sector_id==0)%>%
    mutate(jobs=car::recode(jobs, "'S'=NA"),
           jobs=round(as.numeric(jobs),0),
           year=as.numeric(as.character(year)))%>%
    ggplot(aes(x=year, y=as.numeric(jobs), group=countyfips))+
    geom_rect(aes(xmin=2008, xmax=2010, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_rect(aes(xmin=2001, xmax=2003, ymin=-Inf, ymax=+Inf), fill=rgb(208, 210, 211, max = 255), alpha=.03)+
    geom_line(color=rgb(0, 168, 58, max = 255), size=1.5)+
    scale_x_continuous(breaks=c(2001:max(unique(county_jobs$year)) ))+
    scale_y_continuous(labels=comma)+
    theme_codemog(base_size=base)+
    labs(x="Year", y="Jobs", title=paste0(countyname," County Total Estimated Jobs, 2001 to",max(unique(county_jobs$year)) ,"\nSource: State Demography Office"))

  return(total_jobs)
}
