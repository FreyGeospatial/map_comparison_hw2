#load packages
library(TOC)

#load data
lc_1971 <- raster("Landcover01/1971Landcover01.rst")
lc_1985 <- raster("Landcover01/1985Landcover01.rst")

# create boolean image of "Built": 
# reclassification matrix: is, becomes
built <- c(1L, 1)
not_built <- rbind(c(2L, 0L),
                   c(3L, 0L),
                   c(4L, 0L))

my_built_reclass <- rbind(built, not_built)
built_bool_71 <- reclassify(lc_1971, rcl = my_built_reclass)
built_bool_85 <- reclassify(lc_1985, rcl = my_built_reclass)

#create mask
built <- c(1L, 0)
not_built <- rbind(c(2L, 1L),
                   c(3L, 1L), 
                   c(4L, 1L))
my_nonbuilt_reclass <- rbind(built, not_built)
not_built_bool_71 <- reclassify(lc_1971, rcl = my_nonbuilt_reclass)

#distance calculation
distance_71 <- gridDistance(built_bool_71, 1)
plot(distance_71) #calculates distance from all cells with val of 1

# gain of built

gain_of_built <- built_bool_85 != built_bool_71
plot(gain_of_built)


my_TOC1 <- TOC(index = distance_71, 
               boolean = gain_of_built,
               mask = not_built_bool_71,
               NAval = 0,
               progress = T)
plot(my_TOC1)


