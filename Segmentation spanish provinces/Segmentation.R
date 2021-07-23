if (!require(checkpoint)) {
  if (!require(devtools)) {
    install.packages("devtools", repos = "http://cran.us.r-project.org")
    require(devtools)
  }
  devtools::install_github("RevolutionAnalytics/checkpoint",
                           ref = "v0.3.2", # could be adapted later,
                           # as of now (beginning of July 2017
                           # this is the current release on CRAN)
                           repos = "http://cran.us.r-project.org")
  require(checkpoint)
}
# nolint start
if (!dir.exists("~/.checkpoint")) {
  dir.create("~/.checkpoint")
}
# nolint end
# install packages for the specified CRAN snapshot date
checkpoint(snapshotDate = package_date,
           project = path_to_wd,
           verbose = T,
           scanForPackages = T,
           use.knitr = F,
           R.version = R_version)
rm(package_date)



#Libraries
library(ggplot2)
library(rgdal)
library(rgeos)
library(raster)
library(viridis)
library(dplyr)
library(gtable)
library(tidyr)
library(ggdendro)
library(factoextra)
library(PerformanceAnalytics)
library(data.table)
library(ggrepel)
library(gridExtra)


# File path
path_data <- '/Users/eugendanielwettstein/Documents/Uni/IEMadrid/One Drive/IE Students/MBD Term1 - Dokumente/General/Machine Learning 1/Things in R/Provinces_clustered_labels_no.csv'
path_data_2 <- '/Users/eugendanielwettstein/Documents/Uni/IEMadrid/One Drive/IE Students/MBD Term1 - Dokumente/General/Machine Learning 1/Segmentation/Things in R/Provinces_clustered_labels.csv'
path_data_3 <- '/Users/eugendanielwettstein/Documents/Uni/IEMadrid/One Drive/IE Students/MBD Term1 - Dokumente/General/Machine Learning 1/Segmentation/Provinces_clustered.csv'
# Reading and cleaning the data

spain <- read.csv(path_data_2) 
spain_full <- read.csv(path_data_2)
spain_cluster <- read.csv(path_data_3)
setDT(spain)
setDT(spain_full)
setDT(spain_cluster)
names(spain)[names(spain) == "cluster_labels"] <- 'Clusters'
names(spain_cluster)[names(spain_cluster) == "cluster_labels"] <- 'Clusters'
spain_cor <- select(spain_cluster,-province)
spain_cor <- select(spain_cor,-col_27,-col_28,-Clusters)
row.names(spain_cor) <- spain_cluster$province
# spain_cor <- as.numeric(spain_cor)
# setDT(spain_cor)
# spain_cor$cluster_labels <- NULL
original <- select(spain_full,-ind_wholesale,-ind_retail,-ind_actindex,-col_27,-col_28)
result <- original[-1]
row.names(result) <- spain_full$province
# spain <- spain[,population := as.integer(population)]

#reading data for map

spain_geo <- readRDS('/Users/eugendanielwettstein/Documents/Uni/IEMadrid/One Drive/IE Students/MBD Term1 - Dokumente/General/Machine Learning 1/Things in R/Spain data for plot country/gadm36_ESP_2_sf.rds')

#cleaning data
spain_geo$NAME_2[40] <- 'La Coruña'
spain$province[45] <- 'Santa Cruz de Tenerife'


#join Data

spain_geo_plot <- spain_geo %>% inner_join(spain, by = c('NAME_2' = 'province'))
spain_geo_full <- spain_geo %>% inner_join(spain_full, by = c('NAME_2' = 'province'))

#plotting map

ggplot(spain_geo_full, aes(group = Clusters)) + 
  geom_sf(mapping = aes(fill = Clusters),
          color = "white",
          size = 0.1) +  
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Clustering of spanish provinces") +
  scale_fill_manual(values=c("#c4c4c4", "#007044",'#dfae00','#aa151b','#0142ae'))

ggplot(spain_geo_plot, aes(colour = c('red','orange','yellow','green','black'))) + 
  geom_sf(mapping = aes(fill = Clusters),
          color = "white",
          size = 0.1) + 
  ggtitle("Clustering of spanish provinces") + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_manual(values=c("#c4c4c4", "#007044",'#dfae00','#aa151b','#0142ae'))

#plotting clusters

spplot(spain_geo)

chart.Correlation(spain_cor) #correlation map

dd <- dist(spain_cor, method = "euclidean")
hc <- hclust(dd, method = "ward.D2")


fviz_dend(hc, cex = 1.2,k = 5, #dendrogram for clustering
          k_colors = 'npg',
          color_labels_by_k = TRUE,
          rect = F,
          rect_fill = F,
          phylo_layout = 'layout.gem',
          ggtheme = theme_classic(),
          xlab = "Provinces", ylab = "", sub = "") + scale_y_continuous() + theme(
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 20, face = "bold")) + ggtitle("Clustering of spanish provinces") +
  scale_color_manual(values=c('#dfae00','#0142ae',"#c4c4c4","#007044",'#aa151b','black'))

plt1 <- ggplot(spain,aes(y = ind_turis, x = unemprate, group = Clusters,size = population,color = Clusters)) + 
  geom_point() + theme_classic() + geom_text_repel(aes(label = ifelse(Clusters %in% c('Metropolitans','Costal high potential hotspots') | ind_turis > 7500 | unemprate > 0.11,as.character(province),'')), 
                box.padding = unit(0.6, "lines")) + ggtitle('Unemployment rate vs. Tourism Index') +
  labs(x = 'Unemployment Rate', y = 'Tourism Index') +
  scale_color_discrete(labels = c('Semi developed \nresidential areas','Metropolitans','Rural provinces with stable income', 'Costal high potential hotspots','Rural provinces with\nless stable income')) +
  scale_color_manual(values=c("#c4c4c4", "#007044",'#dfae00','#aa151b','#0142ae'))

plt2 <- ggplot(spain,aes(y = ind_turis, x = ind_rest,labels = province,size = population, color = Clusters)) + 
  geom_point() + 
  theme_classic() +
  geom_text_repel(aes(label = ifelse(Clusters %in% c('Metropolitans','Costal high potential hotspots') | ind_turis > 7500 ,as.character(province),'')), box.padding = unit(0.6, "lines")) + 
  ggtitle('Restaurant and Bar index vs.Tourism Index') + labs(y = 'Tourism Index', x = 'Restaurants & Bars Index') + scale_color_discrete(labels = c('Semi developed \nresidential areas','Metropolitans','Rural provinces with stable income', 'Costal high potential hotspots','Rural provinces with\nless stable income')) +
  scale_color_manual(values=c("#c4c4c4", "#007044",'#dfae00','#aa151b','#0142ae'))


grid.arrange(plt2,plt1, nrow = 1)
