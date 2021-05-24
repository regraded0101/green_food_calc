library(gdistance)
library(rgdal)
library(raster)
library(foreach)
library(doParallel)
# https://stackoverflow.com/questions/28575364/distance-calculator-between-the-ports
new  <- raster(ncol=360*5, nrow= 180*5)
projection(new) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


map <- rgdal::readOGR(dsn = "data/shape_files", layer = "world_map")
r <- rasterize(map, new)

#replace values to 1 and 99999
values(r)[is.na(values(r))] <- 1
values(r)[values(r)>1] <- 99999

ports <- as.matrix(location_data[!location_data$uk_port,c(2,3)])


p <- transition(r, function(x){1/mean(x)}, 8)
p <- geoCorrection(p)



# Create cluster with number of cores of the system.
cl <- makeCluster(detectCores())
registerDoParallel(cl)

i <- 1
nrow_data <- nrow(ports)
results <- foreach(i=icount(nrow_data), .combine='rbind', .packages="gdistance") %dopar% {
  A <- cbind(ports[i,1],ports[i,2])
  r <- matrix(NA, ncol=3,nrow=nrow_data)
  r[,1] <- i
  j <- i+1
  while(j<=nrow_data){
    r[j,2] <- j
    B <- cbind(ports[j,1],ports[j,2])
    tryCatch({
      path <- shortestPath(p, A,B, output = "SpatialLines")
      r[j,3] <- SpatialLinesLengths(path ,longlat=TRUE)
    }, error=function(e){})
    j <- j+1
  }
  r[1:nrow_data,]
}



