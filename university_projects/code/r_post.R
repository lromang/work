

## Código para conectar con base de datos en PostgreSQL.
## realizar estadística descriptiva.
##------------------------------------------------------
## Librerias utilizadas
##------------------------------------------------------
library(RPostgreSQL)
library(rCharts)
library(ggplot2)
library(ggmap)
library(rMaps)
library(googleVis)
library(tidyr)
library(sp)
library(deldir)
library(rgdal)
library(rgeos)
##------------------------------------------------------
## Declarar el administrador de bases de datos
drv <- dbDriver("PostgreSQL")
## Abrir la conexión a la base presidencia
con <- dbConnect(drv, dbname="presidencia")

##------------------------------------------------------
## Queries
##------------------------------------------------------
## Estaciones fecha
estaciones_fecha <- dbGetQuery(con, "SELECT estacion, fecha, avg(valor) as valor_promedio, parametro, lon, lat FROM cont, estaciones WHERE estacion = cve_estacion GROUP BY estacion, fecha,  parametro, lon, lat;")
estaciones_fecha$fecha <- as.Date(estaciones_fecha$fecha)
png("./graphs/t_series.png",width = 1000, height = 1000)
ggplot(data = estaciones_fecha, aes(x = fecha, y = valor_promedio, col = estacion)) + geom_line() + facet_wrap(~ parametro)
dev.off()
## Estaciones mes y año
## Pareamos la base de datos de estaciones con la de registros, de esta manera
## obtenemos las observaciones georeferenciadas.
estaciones <- dbGetQuery(con,"SELECT estacion, EXTRACT(month FROM fecha) as mes, EXTRACT(year FROM fecha) as año, avg(valor) as valor_promedio, parametro, lon, lat FROM cont, estaciones WHERE estacion = cve_estacion GROUP BY estacion, mes, año, parametro, lon, lat;")
hospitales <- dbGetQuery(con, "SELECT * FROM recursos;")
##-----------------
## Visualizaciones
##-----------------
## Comportamiento contaminantes a lo largo del año
png("./graphs/hist_month.png",width = 1000, height = 1000)
ggplot(data = estaciones, aes(x = as.factor(mes), y = valor_promedio, col = parametro)) +
    geom_point(position = "jitter") + theme(panel.background = element_blank()) +
    xlab("Mes") + ylab("Valor promedio")
dev.off()
##### Mapas
##
hospitales$loc=paste(hospitales$Latitude, hospitales$Longitude, sep=":")
G <- gvisGeoChart(hospitales, "loc",
                   options=list(displayMode="Markers", 
                   colorAxis="{colors:['purple', 'red', 'orange', 'grey']}",
                   backgroundColor="lightblue"), chartid="EQ")
plot(G)
##
estaciones$valor_promedio <- extract_numeric(estaciones$valor_promedio)
## Mapa de estaciones
map <- Leaflet$new()
map$setView(c(19.4224742,-99.1261728), zoom = 12)
map$tileLayer(provider = 'Stamen.Watercolor')
map$marker(estaciones[,c(7,6)],
  bindPopup = 'Hi. I am a popup'
)
map

## Voronoi

voronoipolygons = function(layer) {
    require(deldir)
    crds = layer[,c(6,7)]
    z = deldir(crds[,1], crds[,2])
    w = tile.list(z)
    polys = vector(mode='list', length=length(w))
    require(sp)
    for (i in seq(along=polys)) {
        pcrds = cbind(w[[i]]$x, w[[i]]$y)
        pcrds = rbind(pcrds, pcrds[1,])
        polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
    }
    
    SP = SpatialPolygons(polys)
    voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=unique(crds[,1]), 
        y=unique(crds[,2]), row.names=sapply(slot(SP, 'polygons'), 
        function(x) slot(x, 'ID'))))
}

vor <- voronoipolygons(estaciones)
td <- tempdir()
if(nchar(Sys.getenv("OSGEO4W_ROOT") > 0)) {
    OLDPWD <- getwd()
    setwd(td)
    td <- "."
}
writeOGR(vor, td, "vor", driver="ESRI Shapefile")
## gráfica
points <- c()
for (i in seq(1, nrow(estaciones),  4) ){
    points[i] <- paste0("L.circle([",
                       estaciones$lat[i],",",
                       estaciones$lon[i],"],",
                       estaciones$valor_promedio,", {")
    points[i + 1] <-    "color: 'red',"
    points[i + 2] <-    "fillColor: '#f03',"
    points[i + 3] <-    "fillOpacity: 0.5}).addTo(map)"        
}

# Escribimos archivo
fileConn<-file("output.txt")
writeLines(points, fileConn)
close(fileConn)

###########
## Segundo intento.
###########
crds = layer[,c(6,7)]
z = deldir(crds[,1], crds[,2])

ll <- apply(z$dirsgs, 1, FUN=function(X) {
    readWKT(sprintf("LINESTRING(%s %s, %s %s)", X[1], X[2], X[3], X[4]))
})

## Convert SpatialLines list to SpatialPolygons object
pp <- gPolygonize(ll)

## Plot to check that it works    
set.seed=11
plot(pp, col=1:length(unique(layer$estacion)))


