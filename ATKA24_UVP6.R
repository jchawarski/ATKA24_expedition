# UVP6 Analysis
require(tidyverse)
require(data.table)

setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA")


ecotax.df <-as.data.frame(fread("UVP6/export_14590_20241118_2255/ecotaxa_Calanoida.tsv"))   
ecotax.df2 <-as.data.frame(fread("UVP6/export_14590_20241118_2255/ecotaxa_Calanidae.tsv"))   

ecotax.df <- rbind(ecotax.df, ecotax.df2)

ecotax.df$object_annotation_category <- as.factor(ecotax.df$object_annotation_category)

levels(ecotax.df$object_annotation_category)[levels(ecotax.df$object_annotation_category) ==  "Calanoida"] <- "Copepoda"
levels(ecotax.df$object_annotation_category)[levels(ecotax.df$object_annotation_category) ==  "Calanidae"] <- "Copepoda"

ecotax.df2 <-as.data.frame(fread("UVP6/export_14590_20241118_2255/ecotaxa_Eumalacostraca.tsv"))                           # READ A SINGLE SITE
ecotax.df2$object_annotation_category <- as.factor(ecotax.df2$object_annotation_category)


levels(ecotax.df$object_annotation_category)[levels(ecotax.df$object_annotation_category) ==  "Eumalacostraca"] <- "Euphausiid"



ecotax.df <- rbind(ecotax.df, ecotax.df2)



hist(ecotax.df$object_depth_max)

ecotax.df %>% #filter(sample_id %in% "atka24_47") %>%
  ggplot(aes(x=object_depth_max, linetype=object_annotation_category)) + geom_density() + 
  coord_flip() + scale_x_reverse() +
  theme_bw()
  
  #+ facet_grid(~sample_id)
