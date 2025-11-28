library(tidyverse)
library(Makurhini)
library(sf)
library(terra)
library(raster)

## Autre alternatives de packages
## ==========================================================================================

## Comprehepnsione des Unité de végétatiaon à faire
## 2:raphia
## 4: mangrove intacte
## 5: mangrove dégradée
## 6: foret intacte
## 7: foert degradée
## 
## 
##========================================================================================
#Load the shapefile
veg25<- sf::read_sf("repair_veg_Antrema_2025.shp")

veg17 <- sf::read_sf("veg_Antrema_2017.shp")
# Creation d'une liste
veg_list <- list(
  "2017"=veg17,
  "2025"=veg25
)

delineation <- sf::read_sf("delineation_Antrema.shp")
# On a 2599 patch forestiers dans AP (tout es comprise pour le modeles à part les mangroves, qui devrait etre mis a part)
# Calcul des indices metriques paysagères (indices de fragmentation)
#veg_bis <- veg |> filter(DN %in% c(2,6))


# Looping For random foerest classification

# distances à tester
distances <- c(100, 200)
nodes <- c(0.5)# FAO Madagascar

# liste pour stocker les résultats
res_list <- list()

# boucle
for (year in names(veg_list)) {
  veg_data <- veg_list[[year]]

for (d in distances) {
  for (n in nodes) {
    
  cle <- paste0("Year",year,"Dist",d,"_Node",n)
  res_list[[cle]] <- MK_Fragmentation(
    nodes = veg_data |> filter(DN %in% c(2,4,5,6,7)),
    edge_distance = d,
    landscape_area = 20100,
    area_unit = "ha",
    perimeter_unit = "km",
    min_node_area = n,
    plot = TRUE
  )
  message("Finished : ",cle)
  }
}
  }

# afficher les résultats
res_list

res_summary <- map_df(
  names(res_list),
  function(name) {
    # extraire la table de résumé (13 lignes)
    df <- as.data.frame(res_list[[name]][["Summary landscape metrics (Viewer Panel)"]])
    
    # extraire les infos du nom
    year <- as.numeric(sub(".*Year([0-9]+).*", "\\1", name))
    dist <- as.numeric(sub(".*Dist([0-9]+).*", "\\1", name))
    node <- as.numeric(sub(".*Node([0-9.]+).*", "\\1", name))
    
    df$year <- year
    df$edge_distance <- dist
    df$min_node_area <- node
    
    return(df)
  }
)

res_summary_statistics_patch <- map_df(
  names(res_list),
  function(name) {
    # extraire la table de résumé (13 lignes)
    df <- as.data.frame(res_list[[name]][["Patch statistics shapefile"]])
    
    # extraire les infos du nom
    year <- as.numeric(sub(".*Year([0-9]+).*", "\\1", name))
    dist <- as.numeric(sub(".*Dist([0-9]+).*", "\\1", name))
    node <- as.numeric(sub(".*Node([0-9.]+).*", "\\1", name))
    
    df$year <- year
    df$edge_distance <- dist
    df$min_node_area <- node
    
    return(df)
  }
)
res_summary_statistics_patch


#================================================================================

res.frag.shp = res.frag$`Patch statistics shapefile`
ggplot() + geom_sf(res.frag.shp,aes(fill = PARA))+
  geom_sf(data=delineation)


#♣examples

# Computate the area of each patch
veg17$area = veg17 %>% sf::st_area()/10000
veg17$area =veg17$area %>% as.numeric()

#Compute the dIICconnector / dIIPC

res.dpIIC<- Makurhini::MK_dPCIIC(nodes = veg17 %>% filter(area >0.5),
                                 attribute = "area",
                                 area_unit = "ha",
                                 distance = list(type="edge"),
                                 overall = TRUE,
                                 metric = c("IIC",'PC'),
                                 distance_thresholds = c(50,150,300))# seuil maximum pour 


res.dpIIC_25<- Makurhini::MK_dPCIIC(nodes = veg25 %>% filter(area >0.5),
                                 attribute = "area",
                                 area_unit = "ha",
                                 distance = list(type="edge"),
                                 overall = TRUE,
                                 metric = c("IIC",'PC'),
                                 distance_thresholds = c(50,150,300))# 






plot(res.dpIIC_25$d250$node_importances_d250)



Propitechus
library(classInt)
# Classification selon Quantile
interQ<- classInt::classIntervals(res.dpIIC$d200$dIICconnector,n = 4,style = "quantile",
                                  rtimes = "kmeans")
interQ$brks
# Classification selon Jenks
interJ<- classInt::classIntervals(res.dpIIC$d200$dIICconnector,n = 4,style = "jenks",
                                  rtimes = "kmeans")
interJ$brks
# Figures
ggplot() + geom_sf(data = bound)+
  geom_sf(data = res.dpIIC$d1500,aes(fill = cut(dIICconnector,breaks = interJ$brks)),color = NA)+
  scale_fill_brewer(type = "qual",name = "dIICconnector",
                    palette = "RdYlGn")+
  theme_minimal()+
  labs(title = "dIICconnector cas du Site Bioculturel d'Antrema",caption = "RAKOTONIAINA,2025")



ggplot() + geom_sf(data = bound)+
  geom_sf(data = res.dpIIC$d1500,aes(fill = dIICconnector),color = NA)+
  scale_fill_gradient()+
  theme_minimal()+
  labs(title = "dIICconnector cas du Site Bioculturel d'Antrema",caption = "RAKOTONIAINA,2025")


################################### MAKURHINI########################################
data(dis)

View(dist_original)
plot(habitat_nodes)


PC_example <- MK_dPCIIC(nodes = habitat_nodes, attribute = NULL, 
                        distance = list(type = "centroid"),
                        parallel = NULL, metric = "PC", probability = 0.5, 
                        distance_thresholds = c(250,150,3000, 10000))



class(PC_example)
PC_example











