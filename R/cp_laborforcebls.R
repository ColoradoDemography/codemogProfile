
#' Creates a timeseries chart of the Labor Force in CO
#'
#' This function uses labior force data from the Bureau of Labor Statistics to create a time series of the labor
#' force for a given CO county.
#'
#'
#' @param fips is the fips code for the main area to be compared
#' @param state is the state that the original fips
#' @param base is the base text size for the ggplot2 object and codemog_theme() Deafults to 12
#'
#'


cp_laborforcebls=function(fips, countyname, base=12){
require(codemog, quietly=TRUE)
require(ggplot2, quietly=TRUE)
require(stringr, quietly=TRUE)
require(dplyr, quietly=TRUE)
require(tidyr, quietly=TRUE)
require(scales, quietly=TRUE)
require(grid, quietly=TRUE)


p=county_lf_bls%>%
  filter(countyfips==as.numeric(fips))%>%
  ggplot(aes(x=year, y=laborForce, group=countyfips))+
  geom_bar(width=0.75,stat="identity", fill=rgb(21,74,126,max=255))+
  scale_y_continuous(label=comma)+
  theme_codemog()+
  theme(panel.grid.minor=element_line(colour = rgb(210, 210, 210, max = 255), size=base*.05))+
  labs(x="Year", y="Labor Force", title=paste(countyname,"County Labor Force, 1990-2015\nSource:Bureau of Labor Statistics"))
return(p)
}
