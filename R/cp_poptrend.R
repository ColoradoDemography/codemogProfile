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
  require(codemogAPI, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(gridExtra, quietly = TRUE)
require(scales, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)

fips=as.numeric(fips)
yrs=2005:2015

  pd=county_profile(fips, yrs, vars=c("netmigration", "naturalincrease"))%>%
  mutate(netmigration=as.numeric(netmigration),
         naturalincrease=as.numeric(naturalincrease))%>%
  gather(variable, value, -countyfips:-year)%>%
  ggplot(aes(x=year, y=value, fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_continuous(breaks=yrs)+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,32,38,max=255)),
                    name="",
                    breaks=c("netmigration", "naturalincrease"),
                    labels=c("Net Migration","Natural Increase"))+
  theme_codemog(base_size=base)+
  theme(axis.text.x=element_text(angle=90),
        legend.key.size=unit(rel(1), "mm"))+
  labs(y="Value", x= "Year")

pd2=county_profile(fips, yrs, vars="totalpopulation")%>%
  mutate(totalpopulation=as.numeric(totalpopulation))%>%
  ggplot()+
  geom_line(aes(x=year, y=totalpopulation), color=rgb(0,149,58, max=255), size=1)+
  scale_x_continuous(breaks=yrs)+
  scale_y_continuous(labels=comma)+
  theme_codemog(base_size=base)+
  theme(axis.text.x=element_blank(),
        plot.title=element_text(size=rel(1.25)))+
  labs(y="Population", x="")
       # , title=paste(countyname, "County Population Trend and Components of Change\nSource: State Demography Office"))

pd3=grid.arrange(pd2,pd)

return(pd3)
}
