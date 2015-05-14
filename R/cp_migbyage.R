#' Creates a Chart of Nig Migration Age Profiles
#'
#' Uses State Demography Office estimates of the Net Migration by age for each county to
#' create a chart that shows net migration by single year of age for a list of counties
#'
#' @param fips_list is the fips code(s) for the countt(ies) of interest
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#'
cp_migbyage=function(fips_list, base=12, agecat="five"){
require(car, quietly=TRUE)
require(codemog, quietly=TRUE)
require(ggplot2, quietly=TRUE)
require(scales, quietly=TRUE)
require(grid, quietly=TRUE)
require(tidyr, quietly=TRUE)
require(dplyr, quietly=TRUE)

codemog_pal=c(rgb(31,73,125, max=255),
              rgb(192,80,77, max=255),
              rgb(101, 80, 60, max=255),
              rgb(239, 117, 33, max=255),
              rgb(119, 171, 67, max = 255),
              rgb(208, 210, 211, max = 255),
              rgb(210, 210, 210, max = 255))

names=county_est%>%
  filter(countyfips %in% as.numeric(fips_list), year==2013)%>%
  select(countyfips, county)

x=county_migbyage%>%
  filter(countyfips %in% fips_list, age<90)%>%
  mutate(countyfips=as.numeric(countyfips))%>%
  inner_join(names,by="countyfips")%>%
  ggplot(aes(x=age, y=netMigration, color=county))+
  geom_line(size=1.1)+
  scale_colour_manual(name="",values=codemog_pal)+
  scale_x_discrete(breaks=switch(agecat, five=c(5,10,15,20,25,35,30,35,40,45,50,55,60,65,70,75,80,85), ten=c(10,20,30,40,50,60,70,80)))+
  theme_codemog(base_size = base)+
  labs(x="Age", y="Net Migration", title="Net Migration by Age, 2000 to 2010\nSource:U.S. Census Bureau and State Demography Office")
return(x)
}
