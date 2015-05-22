#' Creates a chart comparing an areas age distributions in 2000 and 2010
#'
#' Uses the codemog_api function to access Census data to create a ggplot2 chart for
#' use in profiles comparing the age distribution at the 2000 and 2010 Censuses.
#'
#' @param fips is the fips code for the place or county
#' @param state is the state that of the fips in the first argument

ms_census_age=function(fips, state="08", base=12){
  require(ggplot2, quietly=TRUE)
  require(stringi, quietly=TRUE)
  require(scales, quietly=TRUE)
  require(grid, quietly=TRUE)
  require(reshape2, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(dplyr, quietly=TRUE)
  d10=codemog_api(data="p12",db="c2010",geonum=paste("1",state , fips,sep=""),meta="no")
  d00=codemog_api(data="p12",db="c2000",geonum=paste("1",state , fips,sep=""), meta="no")
  d10[,7:56]=as.numeric(as.character(d10[,7:56]))
  d00[,7:56]=as.numeric(as.character(d00[,7:56]))
  d10c=d10%>%
    mutate(age1=p12003+p12027+p12004+p12028,
           age2=p12005+p12029+p12006+p12030,
           age3=p12007+p12031+p12008+p12009+p12010+p12032+p12033+p12034,
           age4=p12011+p12012+p12035+p12036,
           age5=p12013+p12014+p12037+p12038,
           age6=p12015+p12016+p12039+p12040,
           age7=p12017+p12018+p12019+p12041+p12042+p12043,
           age8=p12020+p12021+p12022+p12023+p12024+p12025+
             p12044+p12045+p12046+p12047+p12048+p12049)%>%
    select(geoname:geonum,age1:age8)%>%
    melt(id=c("geoname", "state", "county", "place", "tract", "bg", "geonum"))%>%
    mutate(
      agecat=ordered(as.factor(variable), levels=c("age1", "age2", "age3", "age4",
                                                   "age5", "age6", "age7", "age8"),
                     labels=c("Less than 9", "10 to 17", "18 to 24", "25 to 34","35 to 44",
                              "45 to 54", "55 to 64", "65 and Over")),
      year="2010",
      geoname=stri_trans_general(geoname, id="Title"))
  d00c=d00%>%
    mutate(age1=p12003+p12027+p12004+p12028,
           age2=p12005+p12029+p12006+p12030,
           age3=p12007+p12031+p12008+p12009+p12010+p12032+p12033+p12034,
           age4=p12011+p12012+p12035+p12036,
           age5=p12013+p12014+p12037+p12038,
           age6=p12015+p12016+p12039+p12040,
           age7=p12017+p12018+p12019+p12041+p12042+p12043,
           age8=p12020+p12021+p12022+p12023+p12024+p12025+
             p12044+p12045+p12046+p12047+p12048+p12049)%>%
    select(geoname:geonum,age1:age8)%>%
    melt(id=c("geoname", "state", "county", "place", "tract", "bg", "geonum"))%>%
    mutate(
      agecat=ordered(as.factor(variable), levels=c("age1", "age2", "age3", "age4",
                                                   "age5", "age6", "age7", "age8"),
                     labels=c("Less than 9", "10 to 17", "18 to 24", "25 to 34","35 to 44",
                              "45 to 54", "55 to 64", "65 and Over")),
      year="2000",
      geoname=stri_trans_general(geoname, id="Title"))
  d=rbind(d10c, d00c)#%>%spread(year,value)
  p=ggplot(d, aes(x=agecat, y=value, fill=year))+
    geom_bar(stat="identity", position="dodge")+#, fill=rgb(31,74,126, max=255))+
    scale_y_continuous(label=comma)+
    scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                      name="Census Year",
                      breaks=c("2000", "2010"),
                      labels=c("2000","2010"))+
    theme_codemog(base_size=base)+
    labs(x="Age", y="Population", title=paste(d10c$geoname, "Population by Age \nSource: U.S. Census Bureau"))
  return(p)
}
