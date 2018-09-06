# Generate table of cell areas
library(dismo)
library(scales)
library(rgeos)
library(XML)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(gdalUtils)
library(raster)
library(sp)
library(maptools)
library(geosphere)
#library(mregions)
library("sf")


make_cell_areas <- function(cell_locs, m, EEZ = NA) {
  crs.geo <- CRS("+init=EPSG:4326")  # geographical, datum WGS84
  if (is.na(EEZ)) {
    m.sp <- map2SpatialPolygons(m, IDs=m$names,proj4string=crs.geo)
    m.sp <- gSimplify(m.sp, tol = 0.00001)
  } else m.sp <- EEZ


  for (i in 1:length(cell_locs$lt)) {
    lt = cell_locs$lt[i]; ln = cell_locs$ln[i]
    geo_strx <- paste0("+proj=laea +lon_0=",ln," +lat_0=",lt," +datum=WGS84")
    crs.geox <- CRS(geo_strx)
    grcl <- data.frame(lon = c(ln, ln+5, ln+5, ln), lat = c(lt+5, lt+5, lt, lt))
    coordinates(grcl) <- ~ lon + lat
    projection(grcl) <- "+init=epsg:4326"
    grclp <- Polygon(grcl)
    cells <- Polygons(list(grclp), "onecell")
    cells2 <- SpatialPolygons(list(cells))
    projection(cells2) <- crs.geo

    # m.diff <- gDifference(cells2, m.sp)
    cellb <- gIntersection(m.sp, cells2, byid = TRUE, drop_lower_td = TRUE)
    if (is.null(cellb)) {
      cell_locs$garea[i] <- 0
      cell_locs$areax[i] <- 0
    } else {
      m.diff <- gDifference(cells2, cellb)
      if (is.null(m.diff)) {
        cell_locs$garea[i] <- 0
        cell_locs$areax[i] <- 0
        } else {
       md2 <- spTransform(m.diff, crs.geox)
       cell_locs$garea[i] <- gArea(md2)/1e6
       cell_locs$areax[i] <- areaPolygon(m.diff)/1e6
      }
    }
    print(i); flush.console()
  }
  cell_locs$latlong <- paste(cell_locs$lt + 2.5, cell_locs$ln + 2.5, sep = "_")
  return(cell_locs)
}

EEZ_dir <- "~/../OneDrive_personal/OneDrive/Data/World EEZs"
EEZ_shape_sf <- read_sf(dsn = EEZ_dir, layer = "World_EEZ_v10_2018_0_360") # Load the shapefile for all international
EEZ_shape_sf2 <- as(EEZ_shape_sf, "Spatial")  # convert to spatial polygon format
crs.geo <- CRS("+init=EPSG:4326")  # geographical, datum WGS84

grid_edges <- list(xlim=c(0, 360), ylim = c(-50, 50)) # Define the space in which to estimate cell areas
grid_edges <- list(xlim=c(100, 120), ylim = c(-55, -30)) # Define the space in which to estimate cell areas
windows()
m <- map(database = "world2Hires", fill = TRUE, xlim=grid_edges$xlim, ylim = grid_edges$ylim)  # This is the old way, the world without EEZs.
# define the boundaries used to clip the space out of the EEZ shapefile
clp <- with(grid_edges, paste("POLYGON((",
                              xlim[1], ylim[1], ",",
                              xlim[1], ylim[2], ",",
                              xlim[2], ylim[2], ",",
                              xlim[2], ylim[1], ",",
                              xlim[1], ylim[1], "))"))
bnds <- readWKT(clp)
projection(bnds) <- crs.geo; projection(EEZ_shape_sf2) <- crs.geo # Apply the same projection to both
EEZ_bounded <- gIntersection(EEZ_shape_sf2, bnds, byid = TRUE, drop_lower_td = TRUE) # clip out the intersecting areas, i.e. the shapefile area that lies within the boundaries
projection(EEZ_bounded) <- crs.geo
windows()
plot(EEZ_bounded)
# Set up the cells to measure
cell2_locs <- with(grid_edges, expand.grid(ln = seq(xlim[1], xlim[2] - 5, 5), lt = seq(ylim[1], ylim[2] - 5, 5), garea = NA, areax = NA))
cell_areas_EEZ <- make_cell_areas(cell_locs = cell2_locs, m=m, EEZ = EEZ_bounded)
save(cell_areas_EEZ, file = "cell_areas_EEZ")
cell_areas <- make_cell_areas(cell_locs = cell2_locs, m=m, EEZ = NA)
save(cell_areas, file = "cell_areas")


########-------------------------------------------------------
# Development and testing
EEZ_dir <- "~/../OneDrive_personal/OneDrive/Data/World EEZs"
#EEZ_dir <- "~/../OneDrive/Data/World EEZs"
EEZ_shape_sf <- read_sf(dsn = EEZ_dir, layer = "World_EEZ_v10_2018_0_360") # Load the shapefile for all international EEZs plus land
#EEZ_shape_sf <- read_sf(dsn = EEZ_dir, layer = "World_EEZ_boundaries_v10_2018_0_360") # Load the shapefile for all international EEZs plus land
#EEZ_shape_sf <- read_sf(dsn = "~/../OneDrive/Data/EEZ", layer = "eez_v10") # Load the shapefile for all international EEZs plus land
EEZ_shape_sf2 <- as(EEZ_shape_sf, "Spatial")  # convert to spatial polygon format
EEZm.sp_sf2_1 <- gSimplify(EEZ_shape_sf2, tol = 1) # simplify at various resolutions
EEZm.sp_sf2_0.1 <- gSimplify(EEZ_shape_sf2, tol = 0.1)
EEZm.sp_sf2_0.09 <- gSimplify(EEZ_shape_sf2, tol = 0.09)
crs.geo <- CRS("+init=EPSG:4326")  # geographical, datum WGS84
projection(EEZm.sp_sf2_1) <- crs.geo # apply the same projection to each
projection(EEZm.sp_sf2_0.1) <- crs.geo
projection(EEZm.sp_sf2_0.09) <- crs.geo


grid_edges <- list(xlim=c(150, 180), ylim = c(-30, 5)) # Define the space in which to estimate cell areas
windows()
m <- map(database = "world2Hires", fill = TRUE, xlim=grid_edges$xlim, ylim = grid_edges$ylim)  # This is the old way, the world without EEZs.

# define the boundaries which will be used to clip the space out of the EEZ shapefile
clp <- with(grid_edges, paste("POLYGON((",
             xlim[1], ylim[1], ",",
             xlim[1], ylim[2], ",",
             xlim[2], ylim[2], ",",
             xlim[2], ylim[1], ",",
             xlim[1], ylim[1], "))"))
bnds <- readWKT(clp)
projection(bnds) <- crs.geo; projection(EEZ_shape_sf2) <- crs.geo # Apply the same projection to both

EEZ_bounded <- gIntersection(EEZ_shape_sf2, bnds, byid = TRUE, drop_lower_td = TRUE) # clip out the intersecting areas, i.e. the shapefile area that lies within the boundaries
plot(EEZ_bounded)
# Simplify the shapefile definitions at various tolerances.
EEZm.bounded_1 <- gSimplify(EEZ_bounded, tol = 1)
EEZm.bounded_0.1 <- gSimplify(EEZ_bounded, tol = 0.1)
EEZm.bounded_0.05 <- gSimplify(EEZ_bounded, tol = 0.05)
EEZm.bounded_0.01 <- gSimplify(EEZ_bounded, tol = 0.01)
EEZm.bounded_0.005 <- gSimplify(EEZ_bounded, tol = 0.005)
projection(EEZ_bounded) <- crs.geo
projection(EEZm.bounded_1) <- crs.geo
projection(EEZm.bounded_0.1) <- crs.geo
projection(EEZm.bounded_0.05) <- crs.geo
projection(EEZm.bounded_0.01) <- crs.geo
projection(EEZm.bounded_0.005) <- crs.geo

# Set up the cells to measure
cell2_locs <- with(grid_edges, expand.grid(ln = seq(xlim[1], xlim[2], 5), lt = seq(ylim[1], ylim[2], 5), garea = NA, areax = NA))
# Estimate cell areas at each tolerance level
cell2_areas_EEZ_1 <- make_cell_areas(cell_locs = cell2_locs, m=m, EEZ = EEZm.bounded_1)
cell2_areas_EEZ_0.1 <- make_cell_areas(cell2_locs, m, EEZ = EEZm.bounded_0.1)
cell2_areas_EEZ_0.05 <- make_cell_areas(cell2_locs, m, EEZ = EEZm.bounded_0.05)
cell2_areas_EEZ_0.01 <- make_cell_areas(cell2_locs, m, EEZ = EEZm.bounded_0.01)
cell2_areas_EEZ_0.005 <- make_cell_areas(cell2_locs, m, EEZ = EEZm.bounded_0.005)
cell2_areas_EEZ_nosimp <- make_cell_areas(cell_locs = cell2_locs, m=m, EEZ = EEZ_bounded)
cell2_areas_m <- make_cell_areas(cell2_locs, m, EEZ = NA)
save(cell2_areas_EEZ_1, cell2_areas_EEZ_0.1, cell2_areas_EEZ_0.05, cell2_areas_EEZ_0.01, cell2_areas_EEZ_0.005, cell2_areas_EEZ_nosimp, file="cell2_areas_testing.RData")
load(file="cell2_areas_testing.RData")

# Examine how much the tolerance affects the cell area estimates
cbind(cell_areas_EEZ[,1:3], cell_areas_EEZ$garea / cell_areas_m$garea)
cbind(cell_areas_EEZ_1[,1:3], cell_areas_EEZ_0.1$garea / cell_areas_EEZ_1$garea)
cbind(cell_areas_EEZ_0.1[,1:3], cell_areas_EEZ_0.05$garea / cell_areas_EEZ_0.1$garea)
cbind(cell_areas_EEZ_0.05[,1:3], cell_areas_EEZ_0.01$garea / cell_areas_EEZ_0.05$garea)
cbind(cell_areas_EEZ_0.01[,1:3], cell_areas_EEZ_0.005$garea / cell_areas_EEZ_0.01$garea)
cbind(cell_areas_EEZ_0.005[,1:3], rat = cell_areas_EEZ_nosimp$garea / cell_areas_EEZ_0.005$garea, diff = cell_areas_EEZ_nosimp$garea - cell_areas_EEZ_0.005$garea)
cbind(cell_areas_EEZ_0.005[,1:3], rat = cell_areas_EEZ_nosimp$garea / cell_areas_EEZ_0.005$garea, diff = cell_areas_EEZ_nosimp$garea - cell_areas_EEZ_0.005$garea)

cbind(cell2_areas_EEZ[,1:3], cell2_areas_EEZ$garea / cell2_areas_m$garea)
cbind(cell2_areas_EEZ_1[,1:3], cell2_areas_EEZ_0.1$garea / cell2_areas_EEZ_1$garea)
cbind(cell2_areas_EEZ_0.1[,1:3], cell2_areas_EEZ_0.05$garea / cell2_areas_EEZ_0.1$garea)
cbind(cell2_areas_EEZ_0.05[,1:3], cell2_areas_EEZ_0.01$garea / cell2_areas_EEZ_0.05$garea)
cbind(cell2_areas_EEZ_0.01[,1:3], cell2_areas_EEZ_0.005$garea / cell2_areas_EEZ_0.01$garea)
cbind(cell2_areas_EEZ_0.005[,1:3], rat = cell2_areas_EEZ_nosimp$garea / cell2_areas_EEZ_0.005$garea, diff = cell2_areas_EEZ_nosimp$garea - cell2_areas_EEZ_0.005$garea)
cbind(cell2_areas_EEZ_0.005[,1:3], rat = cell2_areas_EEZ_nosimp$garea / cell2_areas_EEZ_0.005$garea, diff = cell2_areas_EEZ_nosimp$garea - cell2_areas_EEZ_0.005$garea)

