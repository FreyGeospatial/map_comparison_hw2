#load packages
library(TOC)

#load data
lc_1971 <- raster("Landcover01/1971Landcover01.rst")
lc_1985 <- raster("Landcover01/1985Landcover01.rst")

#Data prep for Q1:
########################################################

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
not_built_bool_71 <- reclassify(lc_1971, rcl = my_nonbuilt_reclass) # will be masked image

#distance calculation
distance_71 <- gridDistance(built_bool_71, 1) #calculates distance from all cells with val of 1
plot(distance_71,
     main = "Distance from Built Areas: 1971")

# gain of built -- will be boolean image
gain_of_built <- built_bool_85 != built_bool_71
plot(gain_of_built,
     main = "Gain of Built Areas: 1971-1985")

# Q1 Analysis:
#######################################################

# TOC Curve for Q1: Use TOC to describe the relationship between Gain of 
# Built between 71-85 and distance from Built at 71
my_TOC1 <- TOC(index = distance_71, 
               boolean = gain_of_built,
               mask = not_built_bool_71,
               NAval = 0,
               progress = T)
plot(my_TOC1)

# Q2 Data Prep:
######################################################

# reclassification order is important for this package
reclassed_1971_matrix <- rbind(c(1L, 0L),
                               c(2L, 1L),
                               c(3L, 2L),
                               c(4L, 3L))
reclassed_1971 <- reclassify(lc_1971, rcl = reclassed_1971_matrix) #new index image

# Q2 Analysis:
#####################################################

# Use the TOC to describe the relationship between Gain of Built
# during 71-85 and land cover in 71

my_TOC2 <- TOC(index = reclassed_1971, 
               boolean = gain_of_built,
               mask = not_built_bool_71,
               NAval = 0,
               progress = T)

plot(my_TOC2)






