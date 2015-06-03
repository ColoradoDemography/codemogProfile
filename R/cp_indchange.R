
#' Creates a chart with two plots, one for population trend, one for components of change
#'
#' This function creates two seperate time-series plots, a line chart for popultion trends
#' and a dodged bar chart for components of change over time.  The two plots are stacked together and
#' placed into one chart.
#'
#' @param fips is the fips code for the main area to be compared
#' @param state is the state that the original fips
#' @param base is the base text size for the ggplot2 object and codemog_theme() Deafults to 12
#' @param peakyear is both a switch and an input, if set to 1 it uses the max employment between 2002 and 2008, otherwise it uses what is supplied.
#'
#'

cp_indchange=function(fips,  countyname, base=12, peakyear=1){

require(codemog, quietly=TRUE)
require(ggplot2, quietly=TRUE)
require(stringr, quietly=TRUE)
require(dplyr, quietly=TRUE)
require(tidyr, quietly=TRUE)
require(scales, quietly=TRUE)
require(grid, quietly=TRUE)

fips=as.numeric(fips)

## This part switches the function from automatically searching for a peak employment year, to using a supplied year based on the value of peak year
if(peakyear==1){
  #This line makes a dataframe that has one value, the number of jobs for the peak employment year
  max=filter(county_jobs,sector_id==0,year>2001, year<2009, countyfips==fips)%>%
    summarize(max=max(jobs))
  # This line turns the dataframe into a numeric vector
  max=as.numeric(as.vector(as.matrix(max)))
  # Uses the numeric jobs number from above 'max' to filter the jobs data and get a year to index on
maxyear=filter(county_jobs,sector_id==0, year>2001, year<2009, countyfips==fips, jobs==max)%>%
  select(year)
#creates a variable to make the chart title
my=as.vector(as.matrix(maxyear))
# Creates a value out of the maximum year that will be equal to the variable to use for the analysis
maxyear=paste0("j_",str_sub(as.vector(as.matrix(maxyear)),-2,-1),"13")
} else {
  my=peakyear
  # Creates a value out of the maximum year that will be equal to the variable to use for the analysis
  maxyear=paste0("j_",str_sub(as.vector(as.matrix(peakyear)),-2,-1),"13")
}

# Makes the plot
p=county_indchange%>%
  filter(countyfips==fips)%>%
  gather(variable, value, -countyfips:-sector_name)%>%
  filter(variable==maxyear)%>%
  mutate(name="jobShare")%>%
  select(-variable)%>%
  spread(name, value)%>%
  arrange(desc(jobShare))%>%
  ggplot(aes(x=reorder(sector_name,jobShare), y=jobShare, group=countyfips))+
  geom_bar(stat="identity", fill=rgb(0,149,58, max=255))+
  geom_line(aes(y=1),color=rgb(191,32,38, max=255),size=1.3)+
  scale_y_continuous(labels=percent)+
  theme_codemog(base_size=base)+
  theme(title=element_text(size=rel(1)), legend.position="none", panel.grid.minor=element_line(colour = rgb(210, 210, 210, max = 255), size=base*.05))+
  guides(fill=FALSE)+
  coord_flip()+
  labs(x="",y="Share of Jobs", title=paste(countyname, "County Employment in 2013 as a Percent of",my, "Employment\nSource:State Demography Office"))

return(p)

}
