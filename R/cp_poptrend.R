#' Creates a chart with two plots, one for population trend, one for components of change
#'
#' This function creates two seperate time-series plots, a line chart for popultion trends
#' and a dodged bar chart for components of change over time.  The two plots are stacked together and
#' placed into one chart.
#'
#' @param fips is the fips code for the main area to be compared
#' @param countyname is the name for the county to be used in the title.
#' @param base is the base text size for the ggplot2 object and codemog_theme()


cp_poptrend=function(fips,countyname, base=12){
require(codemog, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(gridExtra, quietly = TRUE)
require(scales, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)

fips=as.numeric(fips)

  pd=county_profile%>%
  gather(variable, value, -year, -countyfips)%>%
  filter(variable=="netMigration" | variable=="naturalIncrease", countyfips==fips)%>%
  ggplot(aes(x=year, y=value, fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_continuous(breaks=1985:2013)+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,32,38,max=255)),
                    name="",
                    breaks=c("netMigration", "naturalIncrease"),
                    labels=c("Net Migration","Natural Increase"))+
  theme_codemog(base_size=base)+
  labs(y="Component Value", x= "Year")

pd2=county_profile%>%
  filter(countyfips==fips)%>%
  ggplot()+
  geom_line(aes(x=year, y=(householdPopulation+groupQuartersPopulation)), color=rgb(0,149,58, max=255), size=1.15)+
  scale_x_continuous(breaks=1985:2013)+
  scale_y_continuous(labels=comma)+
  theme_codemog(base_size=base)+
  labs(y="Population", x="", title=paste(countyname, "County Population Trend and Components of Change, 1985 to 2013\nSource: State Demography Office"))

# pd=ggplot_gtable(ggplot_build(pd))
# pd2=ggplot_gtable(ggplot_build(pd2))
#
# maxWidth = unit.pmax(pd$widths[2:3], pd2$widths[2:3])
#
# pd$widths[2:3] <- maxWidth
# pd2$widths[2:3] <- maxWidth
#
pd3=grid.arrange(pd2,pd)

return(pd3)
}
