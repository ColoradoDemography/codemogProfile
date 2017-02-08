#' Creates a Chart of Nig Migration Age Profiles
#'
#' Uses State Demography Office estimates of the Net Migration by age for each county to
#' create a chart that shows net migration by single year of age for a list of counties.
#' Data comes from a demographic analysis of each county using births and deaths from each year
#' to interpolate the implied migration between the two decennial census' in 2000 and 2010.
#'
#' @param fips_list is the fips code(s) for the countt(ies) of interest
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#'
cp_migbyage=function(fips_list, base=14, agecat="five"){
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
  geom_hline(yintercept=0, color="black")+
  scale_colour_manual(name="",values=codemog_pal)+
  theme_codemog(base_size = base)+
  labs(x="Age", y="Net Migration",
       caption="Source: Colorado State Demography Office")+
  theme(plot.caption = element_text(size=rel(.75)))
return(x)
}
