
#' Creates a chart showing the laborforce by age for a suppluies CO county compared to the State
#'
#' This function uses State Demography Office Labor Force by Age Estimates by County to
#' create a chart that compares the distribution of a given county to the State.
#'
#' @param fips is the fips code for the main area to be compared
#' @param state is the state that the original fips
#' @param base is the base text size for the ggplot2 object and codemog_theme() Deafults to 12
#'
#'
cp_laborforceage=function(fips, countyname, base){
require(codemog, quietly=TRUE)
require(ggplot2, quietly=TRUE)
require(stringr, quietly=TRUE)
require(dplyr, quietly=TRUE)
require(tidyr, quietly=TRUE)
require(scales, quietly=TRUE)
require(grid, quietly=TRUE)


d=county_lfage%>%
  filter(countyfips==as.numeric(fips) | countyfips==0)%>%
  group_by(countyfips)%>%
  mutate(share=laborForce/sum(laborForce))

p=d%>%
  ggplot(aes(x=age, y=share, fill=county))+
  geom_bar(width=.8,stat="identity", position="dodge")+
  scale_y_continuous(label=percent)+
  scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                    name="Geography")+
  theme_codemog(base_size=base)+
  theme(panel.grid.minor=element_line(colour = rgb(210, 210, 210, max = 255), size=base*.05))+
  labs(x="Age", y="Labor Force", title=paste(countyname,"County Labor Force by Age, 2015\nSource:State Demography Office"))
return(p)
}
