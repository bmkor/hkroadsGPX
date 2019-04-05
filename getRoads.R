require(osmdata)
require(sf)
require(leaflet)
require(igraph)
require(stplanr)
require(dodgr)

#### osm data request ####

hkh<-opq("hong kong") %>%
      add_osm_feature(key="highway") %>%
      osmdata_sf()
# 
# hkb<-opq("hong kong") %>% 
#   add_osm_feature(key="bridge") %>%
#   osmdata_sf()
# 
# hkm<-opq("hong kong") %>%
#   add_osm_feature(key="man_made") %>%
#   osmdata_sf()

# hkj<-opq("hong kong") %>% 
#   add_osm_feature(key="junction") %>%
#   osmdata_sf()
# 
# hkr<-opq("hong kong") %>% 
#   add_osm_feature(key="route") %>%
#   osmdata_sf()

#### helper for filtering osm data ####
grepOSMName<-function(data,expr){
  data[which(grepl(expr,data$name)),]
}

#### helper for converting sf to sp ####
sfToSpatial<-function(sfd){
  sfd<-as(sfd,"Spatial")
  sfd<-spTransform(sfd,CRS("+init=epsg:4326"))
  sfd
}

#### handy map leaflet ####
map <- leaflet() %>% 
  addTiles()

showOnMap<-function(data){
  if (class(data)[1] == "sf"){
    data <- data %>% sfToSpatial()
  }
  m<-leaflet() %>% 
    addTiles() %>%
    addPolylines(data=data, popup=~name) %>%
    addCircles(data=data %>% as("SpatialPoints"), radius = 5)
  m
}

#### output to gpx ####
exportGPX<-function(data,file){
  if (class(data)[1] == "sf"){
    data <- data %>% 
      st_cast("POINT") %>%
      sfToSpatial()
  }
  require(rgdal,quietly = T)
  writeOGR(data, dsn=file,
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)  
}


#### example1: Tuen Mun Road ####
tm<-grepOSMName(hkr$osm_lines,expr="Tuen Mun Road|Tsing Lung Tau Bridge") %>%
  sfToSpatial()
showOnMap(tm)

#### example2: Nathan Road ####
nr<-grepOSMName(hkr$osm_lines,expr="Nathan Road|nathan road") %>%
  sfToSpatial()
showOnMap(nr)

#### example3: Lady Clementi's Ride ####
cr<-grepOSMName(hkh$osm_lines, expr = "Clementi") %>%
  sfToSpatial()
showOnMap(cr)

#### example4: Sir Cecil's Ride ####
cer<-grepOSMName(hkh$osm_lines, expr = "Cecil") %>%
  sfToSpatial()
showOnMap(cer)


### graph ####

h<-grepOSMName(hkh$osm_lines,expr="Nathan Road|nathan road|Boundary|Lai Chi Kok|Kwai Chung")
showOnMap(h)

###### Using graph

createDG<-function(h,colnames=h$name,attrname="road_name"){
  r<-st_touches(h,h)
  m<-(as.matrix(r)*1)
  require(stplanr, quietly = T)
  lapply(1:dim(h)[1],function(i){
    pp<-st_coordinates(line_to_points(h[i,]))[2,]
    sapply(which(m[i,] == 1),function(j){
      tmp<-st_coordinates(line_to_points(h[j,]))[1,]
      if (tmp[1] != pp[1] || tmp[2] != pp[2]){
        m[i,j] <<- 0
      }
    })
  })
  colnames(m) <- colnames
  graph_from_adjacency_matrix(m,mode="directed", add.colnames = attrname)  
}


#### hypergraph but not of much use ####
createHG<-function(g){
  rN<-unique(V(g)$road_name)  
  rnm<-matrix(0,ncol=length(rN),nrow=length(rN),
              dimnames=list(rN,rN))
  
  sapply(1:dim(rnm)[1],function(i){
    sapply(which(V(g)$road_name == rownames(rnm)[i]),function(k){
      tmp<-V(g)[head_of(g,E(g)[from(V(g)[k])])]$road_name
      if (length(tmp) > 0){
        sapply(tmp,function(n){
          j = which(colnames(rnm) == n)
          rnm[i,j] <<- rnm[i,j] + 1
        })
      }
    })  
  })
  
  en<-sapply(strsplit(rN," "),function(w){
    w<-w[-1]
    paste0(w,collapse = " ")
  })
  colnames(rnm)<-en
  graph_from_adjacency_matrix(rnm,mode = "directed",add.colnames = "en_name")
}

########

hh<-grepOSMName(hkh$osm_lines,
                expr="Nathan Road|Austin Road|Canton Road|Parkes Street|Ning Po Street|Temple Street|Jordan Road|Woosung|Kansu Street|Shanghai Street")
showOnMap(hh)
g<-createDG(hh)

rN<-unique(V(g)$road_name)
gg<-induced.subgraph(g,
                 V(g)[which(V(g)$road_name == rN[2])])
components(gg,mode="weak") #### strong is trivial
plot(gg,vertex.size = 5, edge.arrow.size=0.2) ### one-way road...

hg<-createHG(g)

#### nice plot but misleading
plot(hg, vertex.size =5, vertex.label=V(hg)$en_name,vertex.label.dist=2.5,
     edge.arrow.size=0.22)

####### 
m<-sapply(V(hg),function(v){
  sapply(V(hg),function(u){
    w<-sapply(all_simple_paths(hg,from=u,to=v,
                               mode="out"),length)
    if(length(w)>0){
      max(w)  
    } else {
      0
    }
  })
})
rownames(m)<-V(hg)$en_name
colnames(m)<-V(hg)$en_name
which(m==max(m), arr.ind=T)

all_simple_paths(hg,from=V(hg)[2],to=V(hg)[5],mode="out")

V(hg)$en_name[all_simple_paths(hg,from=V(hg)[2],to=V(hg)[5])[[6]]]
V(hg)$en_name[5]
fv=V(g)[grepl(V(hg)$en_name[2], V(g)$road_name)]
tv=V(g)[grepl(V(hg)$en_name[5], V(g)$road_name)]

mm<-sapply(tv,function(v){
  sapply(fv,function(u){
    w<-sapply(all_simple_paths(g,from=u,to=v),function(p){
      length(unique(V(g)$road_name[p]))
    })
    if(length(w) > 0){
      max(w)
    } else {
      0
    }
  })
})

# which(mm == max(mm),arr.ind = T)
# ind<-which(sapply(all_simple_paths(g,from=fv[24],to=tv[2]),length) == max(mm))
paths<-all_simple_paths(g,from=fv[1],to=tv[2])[ind]
lapply(paths,function(p){
  unique(V(g)$road_name[p])
})
showOnMap(hh[as.vector(V(g)[paths[[1]]]),])

#### export interesting route 3
exportGPX(hh[as.vector(V(g)[paths[[1]]]),],file="route3.gpx")
geojsonio::geojson_write(hh[as.vector(V(g)[paths[[1]]]),] %>% as("Spatial"),
                         file="route3.geojson")





