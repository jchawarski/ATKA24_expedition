# UVP6 Analysis

# Last updated 07-02-2025


# Questions: IK fjord has a thin scattering layer located at 105-110 m. What is the scattering layer composed of? 
# Hypothesis: Thin scattering layer of copepods (Metridia, detrivore) feeding on an optimal marine snow particle type
# Station ATKA24_11_CTD has the most distinct "thin layer"... start there.

require(tidyverse)
require(data.table)

setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA")


stn.11    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_11.tsv"))   
stn.10    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_10.tsv"))   
stn.09    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_09.tsv"))   
stn.08    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_08.tsv"))   
stn.07    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_07.tsv"))   
stn.06    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_06.tsv"))   
stn.05    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_05.tsv"))   
stn.04    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_04.tsv"))   
stn.03    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_03.tsv"))   
stn.02    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_02.tsv"))   


IK.image <- rbind(stn.11, stn.10, stn.09, stn.08, stn.07, stn.06, stn.05, stn.04, stn.03, stn.02)
IK.image$fjord <- "NK"

stn.16    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_16.tsv"))   
stn.21    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_21.tsv"))   
stn.22    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_22.tsv"))   
stn.23    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_23.tsv"))   
stn.24    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_24.tsv"))   
stn.30    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_30.tsv"))   
stn.31    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_31.tsv"))   
stn.32    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_32.tsv"))   
stn.36    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_36.tsv"))   
stn.37    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_37.tsv"))   
stn.38    <-   as.data.frame(fread("UVP6/export__TSV_14590_20250702_1810/ecotaxa_atka24_38.tsv"))   

NK.image <- rbind(stn.16, stn.21, stn.22, stn.23, stn.24, stn.30, stn.31, stn.32, stn.36, stn.37, stn.38)
NK.image$fjord <- "NK"


det <- rbind(IK.image, NK.image)



det <- stn.11 %>% filter(object_annotation_category %in% c("detritus", "detritus"))


# Morphometrics of marine snow

colnames(det)[41] <- "object_percarea"

morph <- det %>% 
  
  dplyr::select(object_id,
                sample_id,
                fjord,
        #SIZE        
                object_area,
                object_perim.,
                object_major,
                object_minor,
                object_feret,
                object_fractal,
                object_skelarea,
                
        #STRUCTURE
                object_percarea,
                object_slope,
                object_skew,
                object_kurt,
                object_meanpos,
        
        #SHAPE
                object_circ.,
                object_elongation, 
                object_thickr, 
                object_symetriev,
                object_symetrieh,
        
        #GREYNESS
                object_intden,
                object_range,
                object_mean,
                object_median,
                object_stddev, 
                object_min,
                object_max, 
                object_mode
                
               )



trim <- function(x)
{
  quant <- quantile(x,c(0.005,0.995))
  x[x < quant[1]] <- NA
  x[x > quant[2]] <- NA
  return(x)
}

morph <- morph %>% mutate(across(4:28, trim)) %>% # applies the above function to all variables in morph space
  drop_na() # removes any rows (particles) that have NA values (<0.001 prob)



# Compute two other indices BEFORE tranformation - both are normal
morph$object_cv <- 100*(morph$object_stddev/morph$object_mean) # structure
morph$object_sr <- 100*(morph$object_stddev/(morph$object_max-morph$object_min)) # structure 




# SIZE
morph$object_area <- log10(morph$object_area) # ehh not really
morph$object_perim. <- log10(morph$object_perim.) # leads to a successful normalization
morph$object_major <- log10(morph$object_major) # leads to a successful normalization
morph$object_minor <- morph$object_minor  # already nicely distributed
morph$object_feret <- log10(morph$object_feret) # leads to a successful normalization
morph$object_fractal <- log10(morph$object_fractal) # unsuccessful
morph$object_skelarea <- log10(morph$object_skelarea) # leads to a successful normalization

#STRUCTURE

morph$object_percarea <- log10(morph$object_percarea) # not successful at all
morph$object_slope <- log10(morph$object_slope) # leads to a successful normalization
morph$object_stddev <- morph$object_stddev  # no need 
morph$object_skew <- morph$object_skew 
morph$object_kurt <- morph$object_kurt 
morph$object_meanpos <- morph$object_meanpos

# SHAPE
morph$object_symetrieh <- log10(morph$object_symetrieh) # leads to a successful normalization
morph$object_symetriev <- log10(morph$object_symetriev) # leads to a successful normalization
morph$object_thickr <- morph$object_thickr 
morph$object_elongation <- log10(morph$object_elongation) # leads to a successful normalization
morph$object_circ. <- morph$object_circ.

# GREY LEVEL
morph$object_intden <- log10(morph$object_intden) # semi-successful
morph$object_median <- morph$object_median
morph$object_mean <- morph$object_mean
morph$object_range <- log10(morph$object_range) # leads to a successful normalization
morph$object_mode <- morph$object_mode


morph <- morph %>% dplyr::select(-object_symetrieh, -object_feret, -object_cv, -object_median, -object_min)

#compute correlation matrix
require(ggcorrplot)
corr_matrix <- cor(morph[4:25], method = "spearman")
p <- ggcorrplot(corr_matrix, method = "square",
                type = "full", lab = TRUE, lab_size = 3,
                colors = c("blue", "white", "red"),
                title = "Spearman Rank Correlation Matrix",
                tl.srt = 90) 

# if results of corr show factors that are above 0.9, go back re-run and remove in morph_scaled



morph.pca <- prcomp(morph_scaled, scale. = F, center=T)
morph.pca_transform = as.data.frame(-morph.pca$x[,1:4]) # transforms the pca object and subsets only the first 4 PCs 

fviz_nbclust(morph.pca_transform, kmeans, method = 'wss') # uses the elbow method to determine the value of k



### NOT WORKING ### ----- warnings "Quick-TRANSfer stage steps exceeded maximum"

kmeans_morph.pca = kmeans(morph.pca_transform, centers = 4, nstart = 100)



col_pal <- c("#485696", "#B33951",  "#558564", "#119DA4") # assign colors to each class



fviz_cluster(kmeans_morph.pca, data = morph.pca_transform, geom = "point", axes = c(1,2)) + scale_fill_manual(values=col_pal) # visualize 1st and 2nd axes
fviz_cluster(kmeans_morph.pca, data = morph.pca_transform, geom = "point", axes = c(3,4)) # visualize 3rd and 4th axes

#find the nearest neighbor centroids! will be useful for plotting later
require(FNN)
centroids <- get.knnx(morph.pca_transform, kmeans_morph.pca$centers, 12)
# create a better way for customized plotting

# IMPORTANT #
morph.pca_transform$cluster <- kmeans_morph.pca$cluster

p1 <-   
  morph.pca_transform %>% ggplot(aes(x=PC1, y=PC2, color=factor(cluster))) + geom_point(alpha=0.8) + 
  scale_color_manual(values=col_pal) + 
  xlab("PC1 (45%)") + ylab("PC2 (18%)") + 
  theme_minimal() + theme(legend.position="none")

p2 <- 
  morph.pca_transform %>% ggplot(aes(x=PC3, y=PC4, color=factor(cluster))) + geom_point(alpha=0.8) + 
  scale_color_manual(values=col_pal) + 
  xlab("PC3 (15%)") + ylab("PC4 (6%)") + 
  theme_minimal() + theme(legend.position="none")

require(cowplot)
plot_grid(p1, p2, ncol=2)



det$cluster <- morph.pca_transform$cluster


plot(det$object_depth_min, det$cluster)

det %>% ggplot(aes(x=object_depth_min, color=cluster, fill=cluster)) + geom_density(alpha=0.5, adjust = 1/5)+ facet_grid(rows = vars(cluster))


det %>% ggplot(aes(x=object_depth_min, color=cluster, fill=cluster)) + geom_histogram(binwidth = 1)+ facet_grid(rows = vars(cluster))



det %>% ggplot(aes(x=object_depth_min, y=object_major)) + geom_point()

# plot the size descriptors for each cluster
# PERIMETER
p1 <- 
  det %>% ggplot(aes(x=cluster, y=object_perim., group=cluster, fill=factor(cluster))) + 
  scale_fill_manual(values = col_pal) + 
  #scale_x_discrete(labels=cl.lab) + 
  ylab("log(perimeter)") + xlab("") +  labs(title = "SIZE") + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# ESD
p1 <- 
  det %>% ggplot(aes(x=cluster, y=object_esd, group=cluster, fill=factor(cluster))) + 
  scale_fill_manual(values = col_pal) + 
  #scale_x_discrete(labels=cl.lab) + 
  ylab("ESD (mm)") + xlab("") +  labs(title = "SIZE") + ylim(5,25) + 
  geom_boxplot(outlier.shape = NA) + theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))



# plot the shape descriptors for each cluster
p2 <- 
  det %>% ggplot(aes(x=cluster, y=object_circ., group=cluster, fill=factor(cluster))) + 
  scale_fill_manual(values = col_pal) + 
  #scale_x_discrete(labels=cl.lab) + 
  ylab("circularity") + xlab("") +labs(title = "SHAPE") + 
  geom_boxplot(outlier.shape = NA) + theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
# plot the brightness descriptors for each cluster
p3 <- 
  det %>% ggplot(aes(x=cluster, y=object_mean, group=cluster, fill=factor(cluster))) + 
  scale_fill_manual(values = col_pal) + 
  #scale_x_discrete(labels=cl.lab) + 
  ylab("mean grey level") + xlab("") +  labs(title = "BRIGHTNESS") + ylim(195,235) + 
  geom_boxplot(outlier.shape = NA) + theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
# plot the structure descriptors for each cluster
p4 <- 
  det %>% ggplot(aes(x=cluster, y=object_stddev, group=cluster, fill=factor(cluster))) +
  scale_fill_manual(values = col_pal) + labs(title = "STRUCTURE") +
  #scale_x_discrete(labels=cl.lab) + 
  ylab("std dev grey level") + xlab("") +  ylim(0, 45) + 
  geom_boxplot(outlier.shape = NA) + theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

require(cowplot)
composite <- plot_grid(p1, p2, p3, p4, ncol=2)

ggsave("marinesnowboxplot_Figure4.png", plot = composite, width = 3.5, height = 4, dpi = 300)



# Copepod vertical distribution

cop <-NK.image %>% filter(object_annotation_category %in% c("Calanoida", "Copepoda<Maxillopoda", "Calanidae"))

cop %>% filter(object_major > 35) %>% 
  ggplot(aes(x=object_depth_min)) + geom_density(alpha=0.5, adjust = 1/100)



cop %>% filter(object_major  > 35) %>%
  ggplot(aes(x=-object_depth_min)) + geom_histogram(binwidth = 1) + coord_flip() + facet_grid(cols = vars(sample_id))

cop %>% ggplot(aes(x=object_major)) + geom_histogram(binwidth = 1)
hist(cop$object_major)


# Krill

krill <- NK.image %>% filter(object_annotation_category %in% "Eumalacostraca")

krill %>% #filter(object_major  < 35) %>%
  ggplot(aes(x=-object_depth_min)) + geom_histogram(binwidth = 1) + coord_flip() + facet_grid(cols = vars(sample_id))

# gelatinous

table(IK.image$object_annotation_category)

gel <- NK.image %>% filter(object_annotation_category %in% c("tentacle<Cnidaria", "Ctenophora<Metazoa", "Cnidaria<Metazoa"))

gel %>% 
  ggplot(aes(x=-object_depth_min)) + geom_histogram(binwidth = 1) + coord_flip() + facet_grid(cols = vars(sample_id))


################


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
