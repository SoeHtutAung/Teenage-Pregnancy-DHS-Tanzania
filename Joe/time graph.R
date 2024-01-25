library(ggplot2)
library(tidyr)

### This is the code I used to generate the graph on our midway presentation

# data taken manually from STATcompiler tables
# https://www.statcompiler.com/en/
tzn_nmr <- data.frame(
  Year = c(1992, 1996, 1999, 2004, 2007, 2010, 2015, 2022),
  rural = c(32, 33, 36, 30, 26, 23, 20, 21),
  rural_l = c(27, 27, 27, 25, 20, 18, 17, 16),
  rural_h = c(38, 39, 46, 36, 31, 28, 25, 24),
  urban = c(59, 27, 59, 39, 43, 36, 39, 35),
  urban_l = c(34, 16, 35, 23, 27, 24, 29, 24),
  urban_h = c(83, 37, 84, 54, 60, 47, 49, 46)
)

# make objects for lower and upper bounds of the CIs
lowersh <- c(tzn_nmr$rural_l, tzn_nmr$urban_l)
uppersh <- c(tzn_nmr$rural_h, tzn_nmr$urban_h)

tzn_nmr %>%
  gather(Residence, value, rural, urban) %>% # not 100% what 'gather' does, but it works
  ggplot( aes( x=Year , y=value , color=Residence ) ) + 
  geom_point( pch=17 , size=2 ) + # triangles
  geom_line( linewidth=1 , linetype="dotted" ) +
  geom_ribbon( # ribbons create upper and lower lines and fill them in
    aes( ymin=lowersh , ymax=uppersh , fill=Residence ) ,
    linetype=0 ,
    alpha=0.25 ) +
  theme_bw() + # classic background
  scale_color_manual( values = c("darkorange","blue") ) + # this is important when you have two different lines on the same plot
  theme( legend.position = c(0.85,0.8) ,
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5) ) +
  xlim( 1992,2022 ) + ylim( 0,100 ) + # changing the limits
  ylab( "NMR (per 1,000 live births)" ) +
  ggtitle("Neonatal Mortality Rates in Tanzania","by Place of Residence") # arg1 Title, arg2 Subtitle
