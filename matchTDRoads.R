require(osmdata)
require(sf)
require(leaflet)
require(igraph)
require(stplanr)
require(dodgr)
require(Matrix)

### Get road specification
rr<-getTDRoads(specURL = "http://static.data.gov.hk/td/traffic-speed-map/en/tsm_dataspec.pdf")
rr$route<-as.vector(rr$route)

### handy map
map<-leaflet() %>% 
      addTiles()

### draw those end points  
map %>% addMarkers(data=as(rr, "SpatialPointsDataFrame"))

### convert to sf
rrsf<-st_as_sf(rr)

### create directed line graph based on converted lat. lon.
G<-createDG(rrsf,
            colnames = rrsf$route, 
            attrname = 'route')
V(G)$route_type<-as.factor(rrsf$route_type) ### factor seems no use here
V(G)$district<-as.factor(rrsf$district)
V(G)$geometry<-rrsf$geometry

### Draw the directed line graph

### color scheme on different districts
pal<-colorNumeric(rainbow(4),domain = 1:4)

### plot
plotRoads<-function(G,...){
  plot(G,
       vertex.size = 0.5 + V(G)$route_type,
       vertex.color = pal(V(G)$district),
       edge.arrow.size=0.1,
       vertex.label=NA,
       layout=layout_nicely(G),...)
  
  legend('topright',
         legend=c(levels(rrsf$route_type),
                  levels(rrsf$district)),
         pt.cex=c(0.5+1:length(levels(rrsf$route_type)),
                  rep(1,length(levels(rrsf$district)))),
         col='black',
         pt.bg = c(rep('white',2),
                   pal(1:length(levels(rrsf$district)))),
         pch=21)
}
plotRoads(G)






### try GIS on induced graph from K
kg<-induced.subgraph(G,V(G)[V(G)$district == 2])
##components(kg) ## 2 only

map %>% addPolylines(data=do.call(rbind,lapply(V(kg)$geometry,function(l){
  l %>% as('Spatial')
})))

### get data from Open Street Map
hkh<-opq("hong kong") %>%
  add_osm_feature(key="highway") %>%
  osmdata_sf()

hkr<-opq("hong kong") %>%
  add_osm_feature(key="route") %>%
  osmdata_sf()


st_cast()

pp<-V(kg)$geometry[[1]] %>% 
  st_cast('MULTIPOINT') %>% 
  st_sfc() %>%
  st_set_crs(4326)

as.vector(hkh$osm_lines$name)
M<-st_intersects(pp,hkh$osm_lines)

st_cast(V(kg)$geometry[[1]][1,],'POINT')

p1<-st_point(V(kg)$geometry[[1]][1,]) %>%
  st_sfc() %>%
  st_set_crs(4326)
p2<-st_point(V(kg)$geometry[[1]][2,]) %>%
  st_sfc() %>%
  st_set_crs(4326)


M1<-st_is_within_distance(hkh$osm_lines,p1,dist=6)
hkh$osm_lines[which(apply(M1,1,any)),]$name

hkh$osm_lines[which(apply(M1,1,any)),]$osm_id

M2<-st_is_within_distance(hkh$osm_lines,p2,dist=5)
hkh$osm_lines[which(apply(M2,1,any)),]$name

#####
require(osrm)

trip<-osrmRoute(c("start",st_coordinates(p1)),
          c("end",st_coordinates(p2)), 
          overview = "full",
          sp=T)

map %>% addPolylines(data=trip) ## got a dubious u-turn

###
cluster<-components(kg)
ckg<-induced.subgraph(kg,
                 V(kg)[which(cluster$membership == 1)])

plotRoads(ckg)
get.adjacency(ckg)

degree(ckg,mode = "out")


se<-V(ckg)[degree(ckg,mode = "in") == 0];sk<-V(ckg)[degree(ckg,mode = "out") == 0]

m<-sapply(sk,function(v){
  sapply(se,function(u){
    max(sapply(all_simple_paths(ckg,from=u,to=v),length))
  })
})
m;mind<-which(m==max(m),arr.ind = T)



V(ckg)$bcolor<-"black"
# V(ckg)$bcolor[all_simple_paths(ckg,from=se[3],to=sk[3])[[1]]]<-"white"

rse<-se[-mind[1]];rsk<-sk[-mind[2]]
lgp<-all_simple_paths(ckg,from=se[mind[1]],to=sk[mind[2]])[[1]]
p<-list(lgp)
V(ckg)$bcolor[lgp]<-"yellow"

tmp<-sapply(rsk, function(v){
  sapply(rse, function(u){
    max(sapply(all_simple_paths(ckg,from=u,to=v),function(p){
      if (length(lgp) > 0){
        length(p) - length(intersection(lgp,p))  
      }else{
        length(p)
      }
    }))    
  })
})

tmpind<-which(tmp==max(tmp),arr.ind = T)

tmppath<-all_simple_paths(ckg,from=rse[tmpind[1]],to=rsk[tmpind[2]])[[1]]
p<-c(p,list(tmppath))
V(ckg)$bcolor[tmppath]<-"white"
tmppath<-all_simple_paths(ckg,from=rse[-tmpind[1]],to=rsk[-tmpind[2]])[[1]]
p<-c(p,list(tmppath))
V(ckg)$bcolor[tmppath]<-"pink"

plotRoads(ckg,vertex.frame.color=V(ckg)$bcolor)

r<-list(list(from=se[mind[1]],to=sk[mind[2]]),
     list(from=rse[tmpind[1]],to=rsk[tmpind[2]]),
     list(from=rse[-tmpind[1]],to=rsk[-tmpind[2]])
     )


ind<-3

st<-V(ckg)$geometry[r[[ind]]$from][[1]][c(1,3)]
ed<-V(ckg)$geometry[r[[ind]]$to][[1]][c(2,4)]

tmpR<-osrmRoute(src=c("start",st),dst=c("end",ed),
                overview="full",sp=T)

st<-V(ckg)$geometry[p[[ind]]][1][[1]][c(2,4)]
ed<-V(ckg)$geometry[p[[ind]]][16][[1]][c(1,3)]
tmpR<-osrmRoute(src=c("start",st),dst=c("end",ed),
                overview="full",sp=T)

map %>% 
  addPolylines(data=tmpR) %>%
  addPolylines(data=V(ckg)$geometry[p[[ind]]] %>% 
                 st_sfc() %>% st_set_crs(4326), color="red")





showOnMap(hkh$osm_lines[which(apply(M,1,any)),]) %>% 
  addPolylines(data=V(kg)$geometry[[1]],color='red') 





?st_intersects

V(kg)$geometry[[1]][c(1,3)]


