#load packages
library(TOC)
library(rasterVis)
library(magrittr)

#load data
lc_1971 <- raster("Landcover01/1971Landcover01.rst")#1 = built,
                                                    #2 = barren
                                                    #3 = forest
                                                    #4 = water

lc_1985 <- raster("Landcover01/1985Landcover01.rst")

#Plot index variables
levels(lc_1971)[[1]]["Value"] <- levels(lc_1971)[[1]]["Class_name"]
levelplot(lc_1971, main = "Landcover: 1971", xlab = "Meters", ylab = "Meters") + 
  layer({
    SpatialPolygonsRescale(layout.north.arrow(),
                           offset = c(230020,926520),
                           scale = 220)
    SpatialPolygonsRescale(layout.scale.bar(),
                           offset = c(229500, 926595),
                           scale = 500, txt = "500", fill = c("transparent", "black"), col = "black", at = 250)
  })

levels(lc_1985)[[1]]["Value"] <- levels(lc_1971)[[1]]["Class_name"]
levelplot(lc_1985, main = "Landcover: 1985", xlab = "Meters", ylab = "Meters") + 
  layer({
    SpatialPolygonsRescale(layout.north.arrow(),
                           offset = c(230020,926520),
                           scale = 220)
    SpatialPolygonsRescale(layout.scale.bar(),
                           offset = c(229500, 926595),
                           scale = 500, txt = "500", fill = c("transparent", "black"), col = "black", at = 250)
  })



#Data prep for Q1:
########################################################


# create boolean image of "Built": 
# reclassification matrix: is, becomes
built <- c(1L, 1)
not_built <- rbind(c(2L, 0L),
                   c(3L, 0L),
                   c(4L, 0L))

my_built_reclass <- rbind(built, not_built)

#reclassify to boolean and ready for categorical mapping
built_bool_71 <- reclassify(lc_1971, rcl = my_built_reclass) %>% 
  as.factor()

built_bool_85 <- reclassify(lc_1985, rcl = my_built_reclass) %>% 
  as.factor()

rat <- levels(built_bool_71)[[1]]
rat$landcover <- c("Not Built", "Built")
levels(built_bool_71) <- rat
levels(built_bool_85) <- rat

levelplot(built_bool_71,
          main = "Boolean Image of Built Landcover: 1971",
          ylab = "Meters",
          xlab = "Meters") + 
  layer({
    SpatialPolygonsRescale(layout.north.arrow(),
                           offset = c(230020,926520),
                           scale = 220,
                           col = "darkgrey",
                           fill = "darkgrey")
    SpatialPolygonsRescale(layout.scale.bar(),
                           offset = c(229500, 926595),
                           scale = 500, txt = "500", fill = c("darkgrey", "white"), col = "darkgrey", at = 250)
  })
levelplot(built_bool_85,
          main = "Boolean Image of Built Landcover: 1985",
          ylab = "Meters",
          xlab = "Meters")



#create mask
built <- c(1L, 0)
not_built <- rbind(c(2L, 1L),
                   c(3L, 1L), 
                   c(4L, 1L))
my_nonbuilt_reclass <- rbind(built, not_built)
not_built_bool_71 <- reclassify(lc_1971, rcl = my_nonbuilt_reclass) %>% 
  as.factor() #masked image
rat <- levels(not_built_bool_71)[[1]]
rat$landcover <- c("Built", "Not Built")
levels(not_built_bool_71) <- rat

levelplot(not_built_bool_71, margin=F, main = "Non-Built Areas: 1971", ylab = "Meters", xlab = "Meters") + 
  layer({
    SpatialPolygonsRescale(layout.north.arrow(),
                           offset = c(230020,926520),
                           scale = 220,
                           col = "black",
                           fill = "black")
    SpatialPolygonsRescale(layout.scale.bar(),
                           offset = c(229500, 926595),
                           scale = 500, txt = "500", fill = c("transparent", "black"), col = "black", at = 250)
  })
#distance calculation
distance_71 <- gridDistance(built_bool_71, 1) #calculates distance from all cells with val of 1
levelplot(distance_71,
     main = "Distance from Built Areas: 1971",
     margin = F,
     xlab = "Meters",
     ylab = "Meters") + 
  layer({
    SpatialPolygonsRescale(layout.north.arrow(),
                           offset = c(230020,926520),
                           scale = 220,
                           col = "black",
                           fill = "white")
    SpatialPolygonsRescale(layout.scale.bar(),
                           offset = c(229500, 926595),
                           scale = 500, txt = "500", fill = c("transparent", "white"), col = "black", at = 250)
  })

# gain of built -- will be boolean image
gain_of_built <- (built_bool_85 != built_bool_71) %>% 
  as.factor()
rat <- levels(gain_of_built)[[1]]
rat$landcover <- c("No Gain", "Gain")
levels(gain_of_built) <- rat


levelplot(gain_of_built,
     main = "Gain of Built Areas: 1971-1985", xlab = "Meters", ylab = "Meters") +
  layer({
    SpatialPolygonsRescale(layout.north.arrow(),
                           offset = c(230020,926520),
                           scale = 220,
                           col = "darkgrey",
                           fill = "darkgrey")
    SpatialPolygonsRescale(layout.scale.bar(),
                           offset = c(229500, 926595),
                           scale = 500, txt = "500", fill = c("darkgray", "white"), col = "darkgrey", at = 250)
  })

# Q1 Analysis:
#######################################################

# TOC Curve for Q1: Use TOC to describe the relationship between Gain of 
# Built between 71-85 and distance from Built at 71. Uses (-) on the index
# bercause the sequential order of values needs to start at LOW and go to HIGH
my_TOC1 <- TOC(index = -distance_71, 
               boolean = gain_of_built,
               mask = not_built_bool_71,
               NAval = 0,
               progress = T) %>% 
  scaling(scalingFactor = 10000, newUnits = "Hectares")
plot(my_TOC1,
     main = "Relationship between Gain of Built 1971-85 and Distance from Built in 1971")

# Q2 Data Prep:
######################################################

# reclassification order is important for this package
reclassed_1971_matrix <- rbind(c(1L, 0L), #Built
                               c(2L, 3L), #Water
                               c(3L, 2L), #Forest
                               c(4L, 1L)) #Barren
reclassed_1971 <- reclassify(lc_1971, rcl = reclassed_1971_matrix) %>% 
                      as.factor()

#just to remember classification codes...
rat <- levels(reclassed_1971)[[1]]
rat$landcover <- c("Built",
                   "Water",
                   "Forest",
                   "Baren")
levels(reclassed_1971) <- rat
levelplot(reclassed_1971, xlab = "Meters", ylab = "Meters", main = "Reclassified Landcover for TOC: 1971") + 
  layer({
    SpatialPolygonsRescale(layout.north.arrow(),
                           offset = c(230020,926520),
                           scale = 220)
    SpatialPolygonsRescale(layout.scale.bar(),
                           offset = c(229500, 926595),
                           scale = 500, txt = "500", fill = c("transparent", "black"), col = "black", at = 250)
  })
  
# Q2 Analysis:
#####################################################

# Use the TOC to describe the relationship between Gain of Built
# during 71-85 and land cover in 71

my_TOC2 <- TOC(index = reclassed_1971, 
               boolean = gain_of_built,
               mask = not_built_bool_71,
               NAval = 0,
               progress = T) %>% 
  scaling(scalingFactor = 10000, newUnits = "Hectares")


plot(my_TOC2, labelThres = T, posL = 4,
     main = "Relationship Between Gain of Built 1971-1985 and Landuse in 1971")






