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

my_reclass <- rbind(built, not_built)
built_bool_71 <- reclassify(lc_1971, rcl = my_reclass)
built_bool_85 <- reclassify(lc_1985, rcl = my_reclass)


#distance calculation
plot(gridDistance(built_bool_71, 1)) #calculates distance from all cells with val of 1
