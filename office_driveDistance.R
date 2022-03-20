library(httr)

allTimes <- c(300,600,900,1200,1500,1800,2100,2400,2700,3000)
#timeInSeconds = 300
centerLoc <-c("34.0484454","-118.2635762")
create_url <- function(timeInSeconds,center){
  url <- paste0("https://api.tomtom.com/routing/1/calculateReachableRange/",center[1],"%2C",center[2],"/json?timeBudgetInSec=",timeInSeconds,"&traffic=true&avoid=unpavedRoads&vehicleEngineType=electric&key=AbEg6ZHAyrgcFKC5KLfss4vltswqcfGu")
  return(url)
}
url <- create_url(timeInSeconds,centerLoc)
sst <-GET(url)
get_coords <- content(sst, as="text", encoding="UTF-8") %>% 
  fromJSON(flatten=TRUE)
lats <- get_coords$reachableRange$boundary$latitude
lons <- get_coords$reachableRange$boundary$longitude
listCoords <- list()
for(i in 1:length(allTimes)){
  url <- create_url(allTimes[i], centerLoc)
  sst <-GET(url)
  get_coords <- content(sst, as="text", encoding="UTF-8") %>% 
    fromJSON(flatten=TRUE)
  lats <- get_coords$reachableRange$boundary$latitude
  lons <- get_coords$reachableRange$boundary$longitude
  listCoords[[i]] <- get_coords$reachableRange$boundary
}


m <- leaflet() %>% 
  setView(centerLoc[2],centerLoc[1], zoom = 10) %>%
  addTiles() 
for(i in 1:3){
  m=m %>% addPolygons(map=m,lat=listCoords[[i]]$latitude,lng=listCoords[[i]]$longitude, weight = 3, fillColor = 'purple', color = 'purple')
}
