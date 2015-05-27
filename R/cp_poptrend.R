fips="089"
countyname="Otero"


cp_poptrend=function(fips,countyname){
require(codemog, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(gridExtra, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)

fips=as.numeric(fips)

  pd=county_profile%>%
  gather(variable, value, -year, -countyfips)%>%
  filter(variable=="netMigration" | variable=="naturalIncrease", countyfips==fips)%>%
  ggplot(aes(x=year, y=value, fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_continuous(breaks=1985:2013)+
  scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,32,38,max=255)),
                    name="",
                    breaks=c("netMigration", "naturalIncrease"),
                    labels=c("Net Migration","Natural Increase"))+
  theme_codemog()+
  labs(y="Component Value", x= "Year")

pd2=county_profile%>%
  filter(countyfips==fips)%>%
  ggplot()+
  geom_line(aes(x=year, y=(householdPopulation+groupQuartersPoulation)), color=rgb(0,149,58, max=255), size=1.15)+
  scale_x_continuous(breaks=1985:2013)+
  theme_codemog()+
  labs(y="Population", x="", title=paste(countyname, "County Population Trend and Components of Change, 1985 to 2013\nSource: State Demography Office"))
pd3=grid.arrange(pd2,pd)

return(pd3)
}
