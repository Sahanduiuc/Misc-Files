library(httr)
library(geojsonio)
library(highcharter)
library(jsonlite)
library(purrr)
library(geojson)
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)

api_key <- "ENTER YOUR API KEY HERE"

### Getting pizza places from downtown 
count <- 0 
big_list_full <- list()

for(i in 1:20){
  offset <- count
  
  url <- paste0("https://api.yelp.com/v3/businesses/search?categories=pizza&location=New%20York%2C%20NY&limit=50&offset=", offset,
                "&radius=6000")
  
  res <- GET(url, add_headers('Authorization' = paste("bearer", api_key)))
  
  ugh <- content(res)
  
  big_list_full[[i]] <- ugh[[1]] %>% map(., function(x){
    tibble(name = x$name, rating = x$rating, reviews = x$review_count, 
           long = x$coordinates$longitude, lat = x$coordinates$latitude)})
  
  count <- count + 50
  
  print(i)
}

nyc_pizza <- map(big_list_full, ~.x %>% do.call(rbind, .)) %>% do.call(rbind, .) 

### Getting pizza places from uptown
count <- 0 
big_list_full2 <- list()

for(i in 1:20){
  offset <- count
  
  url <- paste0("https://api.yelp.com/v3/businesses/search?categories=pizza&longitude=-73.949793&latitude=40.801742&limit=50&offset=", offset,
                "&radius=6000")
  
  res <- GET(url, add_headers('Authorization' = paste("bearer", api_key)))
  
  ugh <- content(res)
  
  big_list_full2[[i]] <- ugh[[1]] %>% map(., function(x){
    tibble(name = x$name, rating = x$rating, reviews = x$review_count, 
           long = x$coordinates$longitude, lat = x$coordinates$latitude)})
  
  count <- count + 50
  
  print(i)
}

nyc_pizza2 <- map(big_list_full2, ~.x %>% do.call(rbind, .)) %>% do.call(rbind, .) 

## Final df 
nyc_pizza_fin <- rbind(nyc_pizza, nyc_pizza2) %>% distinct()



### Bringing in NYC Map ###
nyc <- geojson_read(paste0("https://raw.githubusercontent.com/veltman/snd3/master/data/nyc-neighborhoods.geo.json"), 
                    what="sp")

nyc_fin <-  spTransform(nyc, CRS("+proj=utm +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) %>% 
  as.geojson(.) %>% .[1] %>% fromJSON(., simplifyVector = FALSE)

#### Converting coordinates from lat long to geometry 
pizza_ab <- nyc_pizza_fin %>% 
  rename(y = lat, x = long) %>% mutate(z = .1)

pizza_points <- SpatialPointsDataFrame(coords = pizza_ab %>% dplyr::select(x, y), data = pizza_ab,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 

pizza_points_conv <- spTransform(pizza_points, CRS("+proj=utm +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")) %>%
  as_tibble %>% 
  dplyr::select(name, rating, reviews, x.1, y.1) %>% rename(x = x.1, y = y.1)

#### Pizza place by neighb
pizza_hit_list <- list()

thing <- over(pizza_points, spTransform(nyc, CRS("+proj=longlat +datum=WGS84")))

names(thing) <- NULL
thing[is.na(thing)] <- 0

pizza_points_conv <- pizza_points_conv %>% mutate(neighb = thing[,1])
pizza_points_conv_ab <- pizza_points_conv %>% filter(x > 2606000, y < 10000000)
pizza_points_conv_ab <- pizza_points_conv_ab %>% filter(neighb %in% as.character(nyc[[1]][125:164]))

pizza_ugh <- pizza_points_conv_ab %>% mutate(y = y -9638721) %>%
  mutate(z = 1)

## NYC Neighborhoods
neighbs <- pizza_points_conv_ab %>% pull(neighb) %>% unique %>% as.character

### Drilldown data for JS ###
pizza_drill_data_loc <- list()

for(j in neighbs){
  
  datums <- list() 
  tbl <- pizza_points_conv_ab %>% filter(neighb == j)
  
  for(i in 1:nrow(tbl)){
    
    x_tooltip <- tbl %>% pull(rating) %>% .[i]
    x <- x_tooltip + runif(1, -.1, .1)
    
    y <- tbl %>% pull(reviews) %>% .[i]
    y_tooltip <- str_replace(as.character(y), " ", ",")
    
    name <- tbl %>% pull(name) %>% .[i]
    name <- str_replace_all(name, "'", "\\\\'")
    datums[[i]] <- paste0("{","name:", "'", name, "'", ",", "x:", x, ",", "x_tooltip:", x_tooltip, ",",
                          "y:", y, ",", "y_tooltip:", y_tooltip, "}")
  }
  pizza_drill_data_loc[[j]] <- paste(datums %>% unlist, collapse = ",")
}


## 
for(i in 125:164){
  nyc_fin$features[i][[1]]$properties$NAME <- str_replace_all(as.character(nyc[[1]][i]), "_", " ")
}

## Adjusting highcharter drill up button 
hcoptslang <- getOption("highcharter.lang")
hcoptslang$drillUpText <- "Return to map"
options(highcharter.lang = hcoptslang)


## Applying color gradient to map
places_per_region <- matrix(NA)

for(i in 1:length(neighbs)){
  places_per_region[i] <- pizza_points_conv_ab %>% filter(neighb == neighbs[i]) %>% nrow
}

colorify <- colorRamp(c("#faa357", "#ab4669"))

quantiles <- cut(places_per_region, 
                 breaks=quantile(places_per_region, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                 include.lowest=TRUE) %>% as.numeric 

colors <- colorify((quantiles)^3/max((quantiles)^3))

colors_mat <- matrix(NA, 33, 3)
for(i in 1:33){
  colors_mat[i,] <- colors[which(neighbs == as.character(nyc[[1]][124 + i])),]
}
colors <- colors_mat



### Drilldown JS #### 
pizza_js <- paste0("function(e) {
if (!e.seriesOptions) {
var chart = this,
drilldowns = {
'125': {
name: '", str_replace_all(as.character(nyc[[1]][125]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '125',
data: [", pizza_drill_data_loc$Battery_Park_City_Lower_Manhattan,
"]
},
'126': {
name: '", str_replace_all(as.character(nyc[[1]][126]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '126',
data: [", pizza_drill_data_loc$SoHo_TriBeCa_Civic_Center_Little_Italy,
"]
},
'127': {
name: '", str_replace_all(as.character(nyc[[1]][127]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '127',
data: [", pizza_drill_data_loc$Chinatown,
"]
},
'128': {
name: '", str_replace_all(as.character(nyc[[1]][128]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '128',
data: [", pizza_drill_data_loc$Lower_East_Side,
"]
},
'129': {
name: '", str_replace_all(as.character(nyc[[1]][129]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '129',
data: [", pizza_drill_data_loc$East_Village,
"]
},
'130': {
name: '", str_replace_all(as.character(nyc[[1]][130]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '130',
data: [", pizza_drill_data_loc$West_Village,
"]
},
'131': {
name: '", str_replace_all(as.character(nyc[[1]][131]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '131',
data: [", pizza_drill_data_loc$Hudson_Yards_Chelsea_Flatiron_Union_Square,
"]
},
'132': {
name: '", str_replace_all(as.character(nyc[[1]][132]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '132',
data: [", pizza_drill_data_loc$Clinton,
"]
},
'133': {
name: '", str_replace_all(as.character(nyc[[1]][133]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '133',
data: [", pizza_drill_data_loc$Midtown_Midtown_South,
"]
},
'134': {
name: '", str_replace_all(as.character(nyc[[1]][134]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(56,108,176,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '134',
data: [", pizza_drill_data_loc$Lincoln_Square,
"]
},
'135': {
name: '", str_replace_all(as.character(nyc[[1]][135]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '135',
data: [", pizza_drill_data_loc$Upper_West_Side,
"]
},
'136': {
name: '", str_replace_all(as.character(nyc[[1]][136]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '136',
data: [", pizza_drill_data_loc$Yorkville,
"]
},
'137': {
name: '", str_replace_all(as.character(nyc[[1]][137]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '137',
data: [", pizza_drill_data_loc$Upper_East_Side_Carnegie_Hill,
"]
},
'138': {
name: '", str_replace_all(as.character(nyc[[1]][138]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '138',
data: [", pizza_drill_data_loc$East_Harlem_South,
"]
},
'139': {
name: '", str_replace_all(as.character(nyc[[1]][139]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '139',
data: [", pizza_drill_data_loc$East_Harlem_North,
"]
},
'140': {
name: '", str_replace_all(as.character(nyc[[1]][140]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '140',
data: [", pizza_drill_data_loc$Central_Harlem_South,
"]
},
'141': {
name: '", str_replace_all(as.character(nyc[[1]][141]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '141',
data: [", pizza_drill_data_loc$Central_Harlem_North_Polo_Grounds,
"]
},
'142': {
name: '", str_replace_all(as.character(nyc[[1]][142]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '142',
data: [", pizza_drill_data_loc$Morningside_Heights,
"]
},
'143': {
name: '", str_replace_all(as.character(nyc[[1]][143]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '143',
data: [", pizza_drill_data_loc$Hamilton_Heights,
"]
},
'144': {
name: '", str_replace_all(as.character(nyc[[1]][144]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '144',
data: [", pizza_drill_data_loc$Manhattanville,
"]
},
'145': {
name: '", str_replace_all(as.character(nyc[[1]][145]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '145',
data: [", pizza_drill_data_loc$Washington_Heights_North,
"]
},
'146': {
name: '", str_replace_all(as.character(nyc[[1]][146]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '146',
data: [", pizza_drill_data_loc$Washington_Heights_South,
"]
},
'147': {
name: '", str_replace_all(as.character(nyc[[1]][147]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '147',
data: [", pizza_drill_data_loc$Marble_Hill_Inwood,
"]
},
'148': {
name: '", str_replace_all(as.character(nyc[[1]][148]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '148',
data: [", pizza_drill_data_loc$Murray_Hill_Kips_Bay,
"]
},
'149': {
name: '", str_replace_all(as.character(nyc[[1]][149]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '133',
data: [", pizza_drill_data_loc$Turtle_Bay_East_Midtown,
"]
},
'150': {
name: '", str_replace_all(as.character(nyc[[1]][150]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '150',
data: [", pizza_drill_data_loc$Lenox_Hill_Roosevelt_Island,
"]
},
'151': {
name: '", str_replace_all(as.character(nyc[[1]][151]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '151',
data: [", pizza_drill_data_loc$Stuyvesant_Town_Cooper_Village,
"]
},
'152': {
name: '", str_replace_all(as.character(nyc[[1]][152]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '152',
data: [", pizza_drill_data_loc$Gramercy,
"]
},
'153': {
name: '", str_replace_all(as.character(nyc[[1]][153]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '153',
data: [", pizza_drill_data_loc$Highbridge,
"]
},
'154': {
name: '", str_replace_all(as.character(nyc[[1]][154]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '154',
data: [", pizza_drill_data_loc$West_Concourse,
"]
},
'155': {
name: '", str_replace_all(as.character(nyc[[1]][155]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '155',
data: [", pizza_drill_data_loc$Mount_Hope,
"]
},
'156': {
name: '", str_replace_all(as.character(nyc[[1]][156]), "_", " "), "',
tooltip: {headerFormat: '', pointFormat: '<b>Name:</b> {point.name} <br> <b>Rating:</b> {point.x_tooltip} <br> <b># of Reviews:</b> {point.y_tooltip}'},
color: 'rgba(237,7,7,.5)',
type: 'scatter',
marker: {enabled: true, symbol: 'circle', radius: 3},
drilldownName: '156',
data: [", pizza_drill_data_loc$University_Heights_Morris_Heights,
"]
}
},

series = [drilldowns[e.point.drilldownName]];

var str = e.point.name

chart.colorAxis[0].update({
showInLegend: false, visible: false, minColor: '#EF965900', maxColor: '#EF965900',
min: undefined, max: undefined
})

chart.title.update({
text: 'Pizza Restaurants ' + str.replace(\"_\", \" \")
});

chart.subtitle.update({
text: 'By ratings and number of reviews'
});

chart.yAxis[0].update({
min: 0, max: null, visible: true
})

chart.xAxis[0].update({
visible: true
})

chart.legend.update({
title: {text: null}
})

chart.addSingleSeriesAsDrilldown(e.point, series[0]);

chart.applyDrilldown();
}

}")

pizza_js2 <- "function(e) {

var chart = this;

chart.yAxis[0].update({
min: -4845000, max: -4815000, visible: false 
})

chart.xAxis[0].update({
visible: false 
})

chart.title.update({
text: 'The Pizza of Manhattan'
})

chart.subtitle.update({
text: 'Top restaurants are points on the map'
})

chart.colorAxis[0].update({
showInLegend: true, visible: true, min: 0, max: 120, 
maxColor: '#AB465759', minColor: '#FAA36959'
})

chart.legend.update({
title: {text: 'Number of Restaurants'}
})


                   }"


## Final chart ##  
highchart() %>% 
  hc_chart(backgroundColor = "white",
           events = list(
             drilldown = JS(pizza_js),
             drillup = JS(pizza_js2))) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][125]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "125", 
                                 NAME = str_replace_all(as.character(nyc[[1]][125]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[1,1]/255, green = colors[1,2]/255, blue = colors[1,3]/255, alpha = .35), 
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[125]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][126]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "126",
                                 NAME = str_replace_all(as.character(nyc[[1]][126]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[2,1]/255, green = colors[2,2]/255, blue = colors[2,3]/255, alpha = .35), 
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[126]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][127]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "127", 
                                 NAME = str_replace_all(as.character(nyc[[1]][127]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[3,1]/255, green = colors[3,2]/255, blue = colors[3,3]/255, alpha = .35), 
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[127]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][128]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "128",
                                 NAME = str_replace_all(as.character(nyc[[1]][128]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[4,1]/255, green = colors[4,2]/255, blue = colors[4,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[128]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][129]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "129", 
                                 NAME = str_replace_all(as.character(nyc[[1]][129]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[5,1]/255, green = colors[5,2]/255, blue = colors[5,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[129]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][130]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "130",
                                 NAME = str_replace_all(as.character(nyc[[1]][130]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[6,1]/255, green = colors[6,2]/255, blue = colors[6,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[130]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][131]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "131", 
                                 NAME = str_replace_all(as.character(nyc[[1]][131]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[7,1]/255, green = colors[7,2]/255, blue = colors[7,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[131]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][132]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "132",
                                 NAME = str_replace_all(as.character(nyc[[1]][132]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[8,1]/255, green = colors[8,2]/255, blue = colors[8,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[132]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][133]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "133",
                                 NAME = str_replace_all(as.character(nyc[[1]][133]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[9,1]/255, green = colors[9,2]/255, blue = colors[9,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[133]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][134]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "134", 
                                 NAME = str_replace_all(as.character(nyc[[1]][134]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[10,1]/255, green = colors[10,2]/255, blue = colors[10,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[134]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][135]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "135",
                                 NAME = str_replace_all(as.character(nyc[[1]][135]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[11,1]/255, green = colors[11,2]/255, blue = colors[11,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[135]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][136]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "136", 
                                 NAME = str_replace_all(as.character(nyc[[1]][136]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[12,1]/255, green = colors[12,2]/255, blue = colors[12,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[136]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][137]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "137",
                                 NAME = str_replace_all(as.character(nyc[[1]][137]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[13,1]/255, green = colors[13,2]/255, blue = colors[13,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[137]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][138]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "138", 
                                 NAME = str_replace_all(as.character(nyc[[1]][138]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[14,1]/255, green = colors[14,2]/255, blue = colors[14,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[138]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][139]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "139",
                                 NAME = str_replace_all(as.character(nyc[[1]][139]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[15,1]/255, green = colors[15,2]/255, blue = colors[15,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[139]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][140]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "140", 
                                 NAME = str_replace_all(as.character(nyc[[1]][140]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[16,1]/255, green = colors[16,2]/255, blue = colors[16,3]/255, alpha = .35), 
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[140]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][141]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "141",
                                 NAME = str_replace_all(as.character(nyc[[1]][141]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[17,1]/255, green = colors[17,2]/255, blue = colors[17,3]/255, alpha = .35), 
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[141]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][142]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "142", 
                                 NAME = str_replace_all(as.character(nyc[[1]][142]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[18,1]/255, green = colors[18,2]/255, blue = colors[18,3]/255, alpha = .35), 
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[142]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][143]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "143",
                                 NAME = str_replace_all(as.character(nyc[[1]][143]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[19,1]/255, green = colors[19,2]/255, blue = colors[19,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[143]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][144]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "144", 
                                 NAME = str_replace_all(as.character(nyc[[1]][144]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[20,1]/255, green = colors[20,2]/255, blue = colors[20,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[144]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][145]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "145",
                                 NAME = str_replace_all(as.character(nyc[[1]][145]), "_", " "))),
                joinBy = "NAME",
                color = rgb(red = colors[21,1]/255, green = colors[21,2]/255, blue = colors[21,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[145]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][146]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "146", 
                                 NAME = str_replace_all(as.character(nyc[[1]][146]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[22,1]/255, green = colors[22,2]/255, blue = colors[22,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[146]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][147]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "147",
                                 NAME = str_replace_all(as.character(nyc[[1]][147]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[23,1]/255, green = colors[23,2]/255, blue = colors[23,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[147]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][148]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "148",
                                 NAME = str_replace_all(as.character(nyc[[1]][148]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[24,1]/255, green = colors[24,2]/255, blue = colors[24,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[148]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][149]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "149", 
                                 NAME = str_replace_all(as.character(nyc[[1]][149]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[25,1]/255, green = colors[25,2]/255, blue = colors[25,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[149]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][150]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "150",
                                 NAME = str_replace_all(as.character(nyc[[1]][150]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[26,1]/255, green = colors[26,2]/255, blue = colors[26,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[150]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][151]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "151", 
                                 NAME = str_replace_all(as.character(nyc[[1]][151]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[27,1]/255, green = colors[27,2]/255, blue = colors[27,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[151]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][152]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "152",
                                 NAME = str_replace_all(as.character(nyc[[1]][152]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[28,1]/255, green = colors[28,2]/255, blue = colors[28,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[152]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][153]), "_", " "), 
                data = list(list(drilldown = TRUE, drilldownName = "153", 
                                 NAME = str_replace_all(as.character(nyc[[1]][153]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[29,1]/255, green = colors[29,2]/255, blue = colors[29,3]/255, alpha = .35),
                mapData = list(x = 1, type = "FeatureCollection", features = nyc_fin$features[153]),
                type = "map", geojson = TRUE, showInLegend = FALSE,  tooltip = list(pointFormat = "")) %>%
  hc_add_series(name = str_replace_all(as.character(nyc[[1]][155]), "_", " "),
                data = list(list(drilldown = TRUE, drilldownName = "155",
                                 NAME = str_replace_all(as.character(nyc[[1]][155]), "_", " "))),
                joinBy = "NAME", borderColor = "#c7c3c3",
                color = rgb(red = colors[30,1]/255, green = colors[30,2]/255, blue = colors[30,3]/255, alpha = .35),
                mapData = list(type = "FeatureCollection", features = nyc_fin$features[155]),
                type = "map", geojson = TRUE, showInLegend = FALSE, tooltip = list(pointFormat = "")) %>% 
  hc_add_series(data = pizza_points_conv_ab %>% mutate(y = y -9659871) %>% filter(rating >= 4.5 & reviews > 100) %>% 
                  mutate(z = 1), type = "bubble", color = hex_to_rgba("#d94d23", alpha = .01),
                maxSize = 5, minSize = 2.5,
                tooltip = list(headerFormat = "", 
                               pointFormat = "<b><b/> {point.name} <br> {point.rating} Stars <br> {point.reviews} Reviews"),
                showInLegend = FALSE) %>%
  hc_xAxis(visible = FALSE) %>% 
  hc_yAxis(visible = FALSE, min = -4845000, max = -4815000) %>% 
  hc_drilldown(drillUpButton = list(position = list(x = 0, y = -35))) %>% 
  hc_title(text = list("The Pizza of Manhattan"), 
           style = list(fontFamily = "Montserrat", fontWeight = "bold")) %>% 
  hc_subtitle(text = list("Top restaurants are points on the map"), 
              style = list(fontFamily = "Montserrat")) %>% 
  hc_colorAxis(., maxColor = rgb(red = min(colors[,1])/255, green = min(colors[,2])/255, blue = min(colors[,3])/255, alpha = .35), 
               minColor = rgb(red = max(colors[,1])/255, green = max(colors[,2])/255, blue = max(colors[,3])/255, alpha = .35),
               min = 1, max = 120) %>%
  hc_legend(title = list(text = "Number of Restaurants", style = list(fontWeight = "light", fontFamily = "Montserrat", 
                                                                      textAlign = "center")))




