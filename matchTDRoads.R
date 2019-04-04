require(osmdata)
require(sf)
require(leaflet)
require(igraph)
require(stplanr)
require(dodgr)
rr<-getTDRoads(specURL = "http://static.data.gov.hk/td/traffic-speed-map/en/tsm_dataspec.pdf")

rr$route<-as.vector(rr$route)

map<-leaflet() %>% 
      addTiles()
  
map %>% addMarkers(data=as(rr, "SpatialPointsDataFrame"))

rrp<-as(rr, "SpatialPointsDataFrame")

hkh<-opq("hong kong") %>%
  add_osm_feature(key="highway") %>%
  osmdata_sf()

ids<-unique(as.vector(rrp$Lines.ID))
pids<-unique(unlist(strsplit(ids,"-")))

A<-matrix(0,length(pids),length(pids))
rownames(A)<-pids
colnames(A)<-pids

# sapply(1:length(rownames(A)),function(i){
#   sapply(1:length(colnames(A)),function(j){
#     A[i,j]<<-sum(ids==paste0(rownames(A)[i],"-",colnames(A)[j]))
#   })
# })
# sum(A)