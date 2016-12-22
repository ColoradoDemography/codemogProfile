#' Creates a \code{ggplot2} chart of the population for a CO county
#'
#' Takes some basic input on the timeperiod and county then creates a
#' plot of the data in \code{ggplot2}.  Similar to the county_ts_data()
#' function.  Can create timeseries from 1990 to 2040 (beyond 2013 are
#' forecasts).
#' Note: Requires dplyr, ggplot2, ggthemes, scales, and grid R packages.
#'
#' @param fips The County FIPS number (without leading Zeros)
#' @param beginyear The first year in the timeseries Defaults to 1990.
#' @param endyear The first year in the timeseries Defaults to 2013.
#' @param base Base font size.



county_ts_chart=function(fips, beginyear=1990, base=12){
  require(dplyr, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)
  require(grid, quietly=TRUE)
  fips=as.numeric(fips)

  d=county_profile(fips, beginyear:2015, "totalpopulation")%>%
    select(countyfips, county, year, totalPopulation=totalpopulation)
  # %>%
    #     bind_rows(select(county_hist, countyfips, county, year, totalPopulation))
    # bind_rows(county_hist)%>%
    # filter(countyfips==fips, year>=beginyear)%>%
    # group_by(county,countyfips, year)%>%
    # summarise(totalPopulation=sum(totalPopulation))

  p=d%>%
    ggplot(aes(x=as.factor(year), y=as.integer(totalPopulation), group=countyfips))+
    geom_line(color=codemog_pal['dkblu'], size=1.75)+
    labs(x="Year", y="Population", title=paste(d$county,"County Population,", beginyear, "to", max(d$year), sep=" "))+
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=90))
  return(p)
}
