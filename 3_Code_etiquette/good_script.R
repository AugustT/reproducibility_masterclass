# Creating plots of LPI population frequency by biome
# http://www.livingplanetindex.org/home/index
# 2017_10_12
# The Coding Club Team (ourcodingclub@gmail.com)

# Libraries ----
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)

# Import data from .csv ----
LPI <- read.csv("3_Code_etiquette/LPI_data.csv")

# Define Functions ----
# Consistent plot aesthetics for ggplot2
theme.LPI <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=14, face="plain"),
          axis.title.y = element_text(size=14, face="plain"),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),
          legend.title = element_blank(),
          legend.position=c(0.9, 0.9))
}

# Clean data ----

# long format, so abundance records in one col.
LPI_long <- gather(LPI, "year", "abundance", 9:53)  

# Force years to numeric
LPI_long$year <- parse_number(LPI_long$year)

# Check names and make consistent case
names(LPI_long)
names(LPI_long) <- tolower(names(LPI_long))

# Force abundance to numeric
LPI_long$abundance <- as.numeric(LPI_long$abundance)

# Create summary data frame of population `n`
LPI_biome_summ <- LPI_long %>% 
  group_by(biome) %>%
  summarise(population = n())

# Check data frame has formed correctly
head(LPI_biome_summ)

# Create plot of population frequency per biome ----

# bar graph
type = "bar"
biome_barplot <- ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type == "bar") geom_bar() else geom_point(stat = "count")
} +
  theme.LPI() + 
  ylab("Number of populations") + 
  xlab("Biome") +
  theme(legend.position = "none")

plot(biome_barplot)