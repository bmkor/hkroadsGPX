### Suppose we downloaded some TD speed map data by using downloadData.R ###
require(xts)

lf<-list.files("Data/")

ts<-lapply(strsplit(lf,".fst"),function(fn){
  as.POSIXct(strptime(fn,
                      format = "%Y%m%d-%M%S"))
})

ts<-xts(lf,order.by = as.POSIXct(unlist(ts),origin="1970-01-01"))
#periodicity(ts)



### use matchTDRoads to find a Directed graph for Kowloon route, ckg ####
i<-10
df<-read.fst(paste0("Data/",ts[[i]]))
df[which(df$id %in% V(ckg)$route),]

ts[[7]]


