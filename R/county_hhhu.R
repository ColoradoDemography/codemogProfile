#' Creates a \code{ggplot2} chart of the housing units and households for a CO county
#'
#' Takes some basic input on the timeperiod and county then creates a
#' plot of the data in \code{ggplot2}.  Similar to the county_ts_data()
#' function.  Can create timeseries from 1990 to 2040 (beyond 2015 are
#' forecasts).
#' Note: Requires dplyr, ggplot2, ggthemes, scales, and grid R packages.
#'
#' @param fips The County FIPS number (without leading Zeros)
#' @param beginyear The first year in the timeseries Defaults to 1990.
#' @param endyear The first year in the timeseries Defaults to 2013.
#' @param base Base font size.



county_hhhu=function(fips, beginyear=1990, base=14){
  require(dplyr, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)
  require(grid, quietly=TRUE)
  fips=as.numeric(fips)

  d=county_profile(fips, beginyear:2015, c("totalhousingunits", "households"))%>%
    select(countyfips, county, year, totalhousingunits, households)%>%
    gather(variable, value, -countyfips:-year)

  p=d%>%
    ggplot(aes(x=as.factor(year), y=as.integer(value), group=variable, color=variable))+
    geom_line( size=1.75)+
    labs(x="Year", y="Estimate",
         caption="Source: Colorado State Demography Office")+
    scale_color_manual(values=c(rgb(31,74,126,max=255),rgb(191,32,38,max=255)),
                       labels=c("Households", "Housing Units"))+
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=90),
          legend.title = element_blank(),
          plot.caption = element_text(size=rel(.75)))
  return(p)
}
