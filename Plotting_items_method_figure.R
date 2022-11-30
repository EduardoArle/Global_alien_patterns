#load libraries
library(rgdal); library(raster); library(rgeos)


###### CHECKLISTS #######


#make polygons to represent checklist regions

p1 <- Polygon(cbind(c(10,08,09,34,35,26,23,10),
                    c(10,11,13,26,23,09,08,10)))
ps1 <- Polygons(list(p1), 1)

p2 <- Polygon(cbind(c(80,90,103,105,110,100,88,80),
                    c(60,75,80,57,50,55,57,60)))
ps2 <- Polygons(list(p2), 2)

p3 <- Polygon(cbind(c(05,12,25,10,12,08,05),
                    c(63,75,87,99,85,87,63)))
ps3 <- Polygons(list(p3), 3)

p4 <- Polygon(cbind(c(27,40,34,20,27),
                    c(53,35,66,59,53)))
ps4 <- Polygons(list(p4), 4)

p5 <- Polygon(cbind(c(67,74,84,60,48,67),
                    c(13,35,46,59,33,13)))
ps5 <- Polygons(list(p5), 5)

p6 <- Polygon(cbind(c(40,50,50,88,60,40),
                    c(90,95,99,87,70,90)))
ps6 <- Polygons(list(p6), 6)

sps <- SpatialPolygons(list(ps1, ps2, ps3, ps4, ps5, ps6))

par(mar =c(5,5,5,5))
plot(sps, lwd = 4, col = "white", bg = "gray", ylim = c(-5,115))

#save default size


###### POINTS #######


#make a polygon to represent the native range

p <- Polygon(cbind(c(10,08,09,12,15,16,14,12,14,20,25,32,34,35,31,29,29,26,23,20,15,12,10),
                   c(10,11,13,13,15,15,17,18,23,26,29,30,26,23,14,12,10,09,08,05,07,08,10)))

ps <- Polygons(list(p), 1)
sps <- SpatialPolygons(list(ps))
sps2 <- gBuffer(sps, width = 1)

plot(sps2, lwd = 4, col = "white", bg = "gray", ylim = c(0,35))


#seed random points within the polygon to represent native occ

occ <- spsample(sps, n = 40, type = "random")
plot(occ, pch = 19, col = "darkorange", add = T) #save fig


#save default size

