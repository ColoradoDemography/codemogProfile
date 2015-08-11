#'Creates a bar chart showing the Group Quarters population by age
#'
#'This function creates a ggpplot2 chart that shows the GQ population
#'by age at the 2010 Census.  The chart
#'gives an idea of how important GQ is for an age group in a place.
#'
#' @param fips is the fips code for the county to be charted
#' @param base is the base text size for the ggplot2 object and codemog_theme()


cp_GQage=function(fips, state="08", base=12){
require(codemog, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(grid, quietly = TRUE)
require(scales, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)



d10=codemog_api(data="pco1",db="c2010",geonum=paste("1",state , fips,sep=""),meta="no")
d10[,7:ncol(d10)]=as.numeric(as.character(d10[,7:ncol(d10)]))

p=d10%>%
  mutate(age1=pco1003+pco1022,
         age2=pco1004+pco1023,
         age3=pco1005+pco1024,
         age4=pco1006+pco1025,
         age5=pco1007+pco1026,
         age6=pco1008+pco1027,
         age7=pco1009+pco1028,
         age8=pco1010+pco1029,
         age9=pco1011+pco1030,
         age10=pco1012+pco1031,
         age11=pco1013+pco1032,
         age12=pco1014+pco1033,
         age13=pco1015+pco1034,
         age14=pco1016+pco1035+pco1017+pco1036+pco1018+pco1037+pco1019+pco1038+pco1020+pco1039)%>%
  select(geoname:geonum, age1:age14)%>%
  gather(variable, value, -geoname:-geonum)%>%
  mutate(agecat=ordered(as.factor(variable), levels=c("age1", "age2", "age3", "age4",
                                                      "age5", "age6", "age7", "age8", "age9",
                                                      "age10","age11","age12", "age13", "age14"),
                        labels=c("Under 5 years", "5 to 9", "10 to 14","15 to 19" ,"20 to 24", "25 to 29","30 to 34",
                                 "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
                                 "60 to 64", "65 and Over")),
         population="Group Quarters")%>%
  ggplot(aes(x=agecat, y=value, fill=population))+
  geom_bar(stat="identity", fill=rgb(31,74,126, max=255))+
  scale_y_continuous(label=comma)+
#   scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
#                     name="Census Year",
#                     breaks=c("2000", "2010"),
#                     labels=c("2000","2010"))+
  theme_codemog(base_size=base)+
  labs(x="Age", y="Population", title=paste(d10$geoname, "Group Quarters Population by Age, 2010 \nSource: U.S. Census Bureau"))
return(p)


}








