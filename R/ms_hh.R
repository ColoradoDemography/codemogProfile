#'  CO Household Types Bar Chart
#'
#'  This function creates a bar graphs using ggplot2
#'  that shows household change from 2000 to 2010 by comparing Census Counts.
#'  @param fips The FIPS of the Place or County to use for the graph
#'  @param base The base font size (in points) to use. Defaults to 12.
#'  @param state  The State FIPS to use.  Defaults to CO.
#'



ms_hh=function(fips, base=12, state="08"){
require(ggplot2, quietly=TRUE)
require(car, quietly=TRUE)
require(grid, quietly=TRUE)
require(scales, quietly=TRUE)
require(dplyr, quietly=TRUE)
require(codemog, quietly=TRUE)

p18_10=codemog_api(data="p18", geonum=paste("1", state, fips, sep=""),meta="no")
p18_10[,7:ncol(p18_10)]=as.numeric(as.character(p18_10[,7:ncol(p18_10)]))
p18_10=p18_10%>%
  select(geoname:geonum,p18001, p18002, p18007, p18008)%>%
  rename(Total=p18001, Family.Households=p18002, Nonfamily.Households=p18007, Nonfamily.Alone=p18008)

p21_10=codemog_api(data="p21", geonum=paste("1", state, fips, sep=""),meta="no")
p21_10[,7:ncol(p21_10)]=as.numeric(as.character(p21_10[,7:ncol(p21_10)]))
p21_10=p21_10%>%
  select(geoname:geonum, p21005, p21009, p21012, p21020, p21024, p21027, p21030)%>%
  mutate(Family.Kids=p21005+p21009+p21012+p21020+p21024+p21027,
         Nonfamily.Alone.65=p21030)%>%
  select(-p21005:-p21030)
hh10=inner_join(p18_10, p21_10)%>%
  mutate(year="2010")

p20_00=codemog_api(data="p20", db="c2000", geonum=paste("1", state, fips, sep=""),meta="no")
p20_00[,7:ncol(p20_00)]=as.numeric(as.character(p20_00[,7:ncol(p20_00)]))
p20_00=p20_00%>%
  select(geoname:geonum,p20001, p20003, p20005, p20009, p20012, p20014, p20015, p20016, p20018, p20020, p20024, p20027, p20029, p20030, p20030)%>%
  mutate(Total=p20001, Family.Households=p20003+p20018, Nonfamily.Households=p20014+p20029, Nonfamily.Alone=p20015+p20030,
         Family.Kids=p20005+p20009+p20012+p20020+p20024+p20027, Nonfamily.Alone.65=p20030)%>%
  select(-p20001:-p20030)


hh00=p20_00%>%
  mutate(year="2000")


hh=bind_rows(hh10,hh00)%>%
  gather(var, Value, Total:Nonfamily.Alone.65, -geoname, -state, -county, -place,-tract,-bg,-geonum, -year)%>%
  mutate(var2=recode(var,"'Total'=1;'Family.Households'=2; 'Family.Kids'=3;'Nonfamily.Households'=4; 'Nonfamily.Alone'=5; 'Nonfamily.Alone.65'=6"),
         var=ordered(var2, levels=1:6, labels=c("Households", "Family Households", "Family Households\nw/ Under 18", "Nonfamily Households",
                                                "Nonfamily Households\nLiving Alone", "Nonfamily Households\nLiving Alone over 65")))

p=hh%>%ggplot(aes(x=var, y=Value, fill=year))+
  geom_bar(position="dodge", stat="identity")+
  theme_codemog(base_size=base)+
  scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                    name="Census Year")+
  scale_y_continuous(labels=comma)+
  labs(x="Household Type", y="Number of Households", title=paste(hh$geoname, "Household Types\n Source: Census 2000 and 2010"))
return(p)
}



