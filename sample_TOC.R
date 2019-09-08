# SAMPLE RUN OF TOC PACKAGE

#load packages
library(TOC)

# data:
index <- raster(system.file("external/Prob_Map2.rst", package = "TOC"))
boolean <- raster(system.file("external/change_map2b.rst", package = "TOC"))
mask <- raster(system.file("external/mask4.rst", package = "TOC"))

# TOC Curve
tocd_1 <- TOC(index = index, 
              boolean = boolean, 
              mask = mask, 
              nthres = 100) #100 thresholds

plot(tocd_1, main = "TOC Curve")

tocd_2 <- TOC(index = index, 
              boolean = boolean, 
              mask = mask, 
              nthres = 10) #10 thresholds

plot(tocd_2, 
     labelThres = T, #label the thresholds
     main = "TOC Curve",
     cex = 0.8, #reduces font size
     posL = 4) #position specifier for text labels; values 1-4
