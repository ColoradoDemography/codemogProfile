
require(codemog, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(grid, quietly = TRUE)
require(scales, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)



d10=codemog_api(data="pco1",db="c2010",geonum=paste("1",state , fips,sep=""),meta="yes")
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
         age14=pco1016+pco1035,
         age15=pco1017+pco1036,
         age16=pco1018+pco1037,
         age17=pco1019+pco1038,
         age18=pco1020+pco1039)%>%
  select(geoname:geonum, age1:age18)%>%
  gather(variable, value, -geoname:-geonum)%>%
  mutate()














