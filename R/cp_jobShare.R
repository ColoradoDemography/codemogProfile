#' Creates a chart that shows the Share of  Employment by industry for CO counties
#'
#'
#'This function produces a chart showwing the percentage of the total employment
#'in each industry.
#'
#' @param fips is the fips code for the main area to be compared
#' @param countyname is the name of the county the chart is being made for.
#' @param base is the base text size for the ggplot2 object and codemog_theme() Deafults to 12
#'
#'

cp_jobShare=function(fips, countyname, base=12){

require(codemog, quietly=TRUE)
require(ggplot2, quietly=TRUE)
require(stringr, quietly=TRUE)
require(dplyr, quietly=TRUE)
require(tidyr, quietly=TRUE)
require(scales, quietly=TRUE)
require(grid, quietly=TRUE)

fips=as.numeric(fips)

p=county_jobShare%>%
  filter(countyfips==fips, year==2014)%>%
  ggplot(aes(x=reorder(sector_name,jobShare), y=jobShare, group=countyfips))+
  geom_bar(stat="identity", fill=rgb(0,149,58, max=255))+
  scale_y_continuous(labels=percent)+
  theme_codemog(base_size=base)+
  theme(title=element_text(size=rel(1)), legend.position="none", panel.grid.minor=element_line(colour = rgb(210, 210, 210, max = 255), size=base*.05))+
  guides(fill=FALSE)+
  coord_flip()+
  labs(x="",y="Share of Jobs", title=paste(countyname, "County Share of Employment by Industry, 2014\nSource: State Demography Office"))
return(p)
}
