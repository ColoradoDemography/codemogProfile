#'Creates an Area chart showing the Group Quarters and Total Population
#'
#'This function creates a ggpplot2 chart that shows the total population
#'and the group quarter population over time from 1985 to 2013.  The chart
#'gives an idea of how important GQ is for an area.
#'
#' @param fips is the fips code for the main area to be compared
#' @param countyname is the name for the county to be used in the title.
#' @param base is the base text size for the ggplot2 object and codemog_theme()

cp_popGQ=function(fips, countyname, base=12){
require(codemog, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(gridExtra, quietly = TRUE)
require(scales, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)

fips=as.numeric(fips)

a=county_profile%>%
  filter(countyfips==fips)%>%
  ggplot()+
  geom_area(aes(x=year, y=(householdPopulation+groupQuartersPoulation)), fill=rgb(31,74,126, max=255))+
  geom_area(aes(x=year, y=householdPopulation), fill=rgb(216,199,34, max=255))+
  geom_area(aes(x=year, y=groupQuartersPoulation), fill=rgb(191,32,38, max=255))+
  scale_x_continuous(breaks=1985:2013)+
  scale_y_continuous(labels=comma)+
  theme_codemog(base_size=base)+
  labs(y="Population", x="", title=paste(countyname, "County Population Trend by Type, 1985 to 2013\nSource: State Demography Office"))

return(a)
}
