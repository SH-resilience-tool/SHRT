accessibility_output <- function(nodes, shapefile, demand, dist_mat2){  

library(rgdal)
library(igraph)
library(rgeos)

# # shp2 <- readOGR("C:/Users/Cate/Desktop/toy model marzo/file per toy_model/munich.shp")
# shp_input <- sf::st_read("C:/Users/Cate/Desktop/toy model marzo/file per toy_model/munich.shp")  #NON FUNZIONA
# shp <- as(shp_input, "Spatial")
# 
# # print(length(shp))
# nodes <- read.delim("C:/Users/Cate/Desktop/toy model febbraio/file per toy_model/nodes_munich_metro.txt", header = TRUE, sep = ",")
# edges <- read.delim("C:/Users/Cate/Desktop/toy model febbraio/file per toy_model/edges_munich_metro.txt", header = TRUE, sep = ";")
# demand <- read.delim("C:/Users/Cate/Desktop/toy model febbraio/file per toy_model/munich_demand_prova.txt", header = TRUE, sep = ";")
# # g1 <- graph.data.frame(edges2, directed = T)
# ##############
# n_district<-length(shp) ##numero distretti
# stops_district<-nodes[,c(1,3,4)]
# stops_district<-nodes
# 
# ##stops district deve essere nome, x,y,zona
# zone<-0
# for (zone in 1:n_district){
#    print(zone)
#    print(nrow(nodes))
#   stops_for_demand<-sapply(1:nrow(nodes),function(i)
#     list(id=stops_district[i,1],
#          gContains(shp[zone,],SpatialPoints(stops_district[i,2:3],proj4string=CRS(proj4string(shp))))))
# 
#   
#   if (length(which(stops_for_demand[2,]=="TRUE"))==0){next}
#   
#   stops_district[which(stops_for_demand[2,]=="TRUE"),4]<-zone  #assegnare zona a ogni fermata
# }
# 
# colnames(stops_district)[colnames(stops_district) == "V4"] <- "nodes_within_map"
# stops_district
# dist_mat2_district0<-matrix()
# dist_mat2_district<-matrix()

stops_district <- nodes


district_names<-rownames(shapefile)
n_district<-nrow(shapefile)
dist_mat2_district0<-matrix(0, n_district, nrow(nodes))
dist_mat2_district<-matrix(0, n_district, n_district)

zone<-0
for (zone in 1:n_district){
  print(zone)
  if (length(which(stops_district$within_map==zone))==1){
    dist_mat2_district0[zone,]<-dist_mat2[which(stops_district$within_map==zone),]
  }
  if (length(which(stops_district$within_map==zone))>1){
    dist_mat2_district0[zone,]<-colMeans(dist_mat2[which(stops_district$within_map==zone),])
  }
}

# print(dist_mat2_district0)

stops_district[is.na(stops_district)==TRUE]<-sample(1:length(unique(stops_district$within_map)), size=length(stops_district[is.na(stops_district)==TRUE]), replace=T)
zone<-0
for (zone in 1:n_district){
  print(zone)
  if (length(which(stops_district$within_map==zone))==0){next}
  if (length(which(stops_district$within_map==zone))==1){
    dist_mat2_district[,zone]<- dist_mat2_district0[,which(stops_district$within_map==zone)]
  }
  if (length(which(stops_district$within_map==zone))>1){
    dist_mat2_district[,zone]<- rowMeans(dist_mat2_district0[,which(stops_district$within_map==zone)])
  }}


for (i in 1:nrow(demand)){
     demand$from_zone[i]<-stops_district[which(demand$from[i]==stops_district$label),4]
     demand$to_zone[i]<-stops_district[which(demand$to[i]==stops_district$label),4]
}

demand_zone<-matrix(0, n_district, n_district)
for (o in 1:nrow(demand_zone)){
  for (d in 1:ncol(demand_zone)){
demand_zone[o,d]<-sum(demand$demand[which((demand$from_zone==o)&(demand$to_zone==d))])
  }}

# rownames(dist_mat2_district)<-1:length(shp)
# colnames(dist_mat2_district)<-1:length(shp)

# rownames(dist_mat2_district)<-shp$name
# colnames(dist_mat2_district)<-shp$name

rownames(dist_mat2_district)<-district_names
colnames(dist_mat2_district)<-district_names

origin<-rep(rownames(dist_mat2_district),ncol(dist_mat2_district))
destination<- rep(rownames(dist_mat2_district), each = ncol(dist_mat2_district))
distances<-as.vector(dist_mat2_district)
flows<-as.vector(demand_zone)
accessibility<-cbind(flows,origin,destination,distances)
# colnames(accessibility)<-c('flows', 'origin', 'destination', 'distance')

return(accessibility)

# write.table(accessibility, file=paste0(getwd(), "/output_accessibility.txt"), sep="\t", row.names = F, quote=F)

}