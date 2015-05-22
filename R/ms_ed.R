#' Creates a Chart comparing educational attainment of two areas
#'
#' Uses the codemog_api function to access ACS data (defaults to 13-5yr) to create a ggplot2 chart for
#' use in profiles.
#'
#' @param fips is the fips code for the main area to be compared
#' @param state is the state that the original fips
#' @param fips2 is the second area to be compared Defaults to Blank
#' @param state2 is the state of the second place to be compared, is set to call up CO since fips2 is blank Defaults to 08
#' @param base is the abse text size for the ggplot2 object and codemog_theme()
ms_ed=function(fips, state="08", fips2="", state2="08", base=12){
  require(stringi, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)
  require(grid, quietly=TRUE)
  require(reshape2, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(dplyr, quietly=TRUE)


  d13p=codemog_api(data="b15003",db="acs0913",geonum=paste("1",state , fips,sep=""),meta="no")
  d13p[,7:32]=as.numeric(as.character(d13p[,7:32]))
  d13pm=d13p%>%
    mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
             b15003012+b15003013+b15003014+b15003015+b15003016,
           ed2=b15003017+b15003018,
           ed3=b15003019+b15003020+b15003021,
           ed4=b15003022,
           ed5=b15003023+b15003024+b15003025)%>%
    select(geoname:geonum,ed1:ed5)%>%
    melt(id=c("geoname", "state", "county", "place", "tract", "bg", "geonum"))%>%
    mutate(agecat=ordered(as.factor(variable), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                          labels=c("Less than High School",
                                   "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                   "Graduate or \nProfessional Degree")))%>%
    separate(geoname, into=c("geoname","statename"),sep=",")%>%
    select(-statename)%>%
    mutate(geoname=stri_trans_general(geoname,id="Title"))

  d13c=codemog_api(data="b15003",db="acs0913",geonum=paste("1",state2 , fips2,sep=""),meta="no")
  d13c[,7:32]=as.numeric(as.character(d13c[,7:32]))
  d13cm=d13c%>%
    mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
             b15003012+b15003013+b15003014+b15003015+b15003016,
           ed2=b15003017+b15003018,
           ed3=b15003019+b15003020+b15003021,
           ed4=b15003022,
           ed5=b15003023+b15003024+b15003025)%>%
    select(geoname:geonum,ed1:ed5)%>%
    melt(id=c("geoname", "state", "county", "place", "tract", "bg", "geonum"))%>%
    mutate(agecat=ordered(as.factor(variable), levels=c("ed1", "ed2", "ed3", "ed4",
                                                        "ed5"),
                          labels=c("Less than High School",
                                   "High School Graduate \n(or GED)","Some College or \nAssociate's Degree", "Bachelor's Degree",
                                   "Graduate or \nProfessional Degree")))%>%
    mutate(geoname=stri_replace_all_charclass(geoname, "\\p{WHITE_SPACE}", ""))
  d=rbind(d13cm,d13pm)%>%
    group_by(geoname)%>%
    mutate(p=value/sum(value))
  p=ggplot(d, aes(x=agecat, y=p, fill=geoname))+
    geom_bar(stat="identity", position="dodge")+#, fill=rgb(31,74,126, max=255))+
    scale_y_continuous(label=percent)+
    scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                      name="Geography")+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=0))+
    labs(x="Educational Attainment", y="Population", title="Educational Attainment for 25 and Older \nSource: ACS 2013 5-Year File")
  return(p)

}
