# Final analysis
# Taken from John Godlee

D<-read.csv("3_Code_etiquette/LPI_data.csv")

library(tidyr)
DF<-gather(D,"year","abundance",9:53)
library(readr)
DF$year <- parse_number(DF$year)
names(DF)
names(DF) <- tolower(names(DF))
DF$abundance <- as.numeric(DF$abundance)

library(dplyr)
lpiBiomes<- DF %>%group_by(biome)%>%summarise(Pop. = n())
lpiBiomes[1:5,1:2]

library(ggplot2)
ThemeForLPI <- function()
{
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),axis.text.y=element_text(size=12),axis.title.x=element_text(size=14, face="plain"),axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),legend.title = element_blank(),legend.position=c(0.9, 0.9))}

levels(DF$biome)

# graph
type = "bar"
plot <- ggplot(DF, aes(biome, color = biome)) + {if(type=="bar")geom_bar() else geom_point(stat="count")} +
  ThemeForLPI() + ylab("Number of populations") + xlab("Biome") +
  theme(legend.position = "none")
plot# plot the ggplot plot