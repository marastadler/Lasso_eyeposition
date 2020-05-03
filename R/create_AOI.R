library(XML)
#library(RCurl)




### car cornfield (Dynamic AOI)

parsed.url <- xmlTreeParse(file = "Video-Stimuli-EP-AG-Export_190806_tw/Video-18_Car-Cornfield_AOI.xml",
                           error = function(...) {
                           }, useInternalNodes = T)

DynamicAOI1 <- xpathSApply(doc = parsed.url, path = "//KeyFrames")[[1]] # Endpunkt
DynamicAOI2 <- xpathSApply(doc = parsed.url, path = "//KeyFrames")[[2]] # Auto


# Bringe AOI2_Frame in dataframe-Form:
my_list <- list()
for(i in 1:6){
  AOI2_Frame <- DynamicAOI2[i]
  AOI2_unlist_Frame <- unlist(xmlToList(AOI2_Frame$KeyFrame)$Points)
  Timestamp <- as.numeric(xmlToList(AOI2_Frame$KeyFrame)$Timestamp)
  Points <- as.numeric(AOI2_unlist_Frame)
  Points <- matrix(Points, nrow = length(Points)/2, byrow = T)
  Points[, 1] <- Points[, 1] - 258 # Verschiebung linker Rand
  Points <- split(Points, 1:nrow(Points)) 
  
  my_list[[i]] <- list(timestamp = Timestamp, points = Points)
}

source('poly_interpolate.R')

interpolate_AOI2 <- interpolate_poly_list(my_list, t = seq(0, 6640000,
                                                           length.out = 6.64*25))
plot_pl(interpolate_AOI2) # 25 Hz
summary(interpolate_AOI2)

# Erstellung der binären Matrizen
library(xROI)

List_AOI_car.cornfield_dyn <- list()

for(i in 1 : (6.64 * 25)){
interpolate_unlist <- unlist(interpolate_AOI2[[i]]$points)

AOI2_Ant <- matrix(interpolate_unlist, nrow = length(interpolate_unlist)/2, byrow = T)
AOI2_Ant[, 1] <- AOI2_Ant[, 1]/1350
AOI2_Ant[, 2] <- AOI2_Ant[, 2]/1080

pnts <- AOI2_Ant
imgSize <- c(720, 576)

m <- rasterizeROI(pnts, imgSize)
m <- abs(m - 1)



mirror.matrix <- function (x)
{
  xx <- as.data.frame(t(x))
  xx <- rev(xx)
  xx <- t(as.matrix(xx))
  xx
}

List_AOI_car.cornfield_dyn[[i]] <- mirror.matrix(m)
}


str(List_AOI_car.cornfield_dyn)

xROI::addMask(List_AOI_car.cornfield_dyn[[2]], add = FALSE)

save(List_AOI_car.cornfield_dyn, file = 'car.corn_dynamic_AOI.Rdata')



# dim(List_AOI_car.cornfield_dyn[[1]])
# str(m)
# table(m)


### car cornfield (Static AOI)
# Da nicht dynamisch, reicht eine AOI-Map für alle Frames:


AOI1_Frame <- DynamicAOI1[1]
AOI1_unlist_Frame <- unlist(xmlToList(AOI1_Frame$KeyFrame)$Points)
Timestamp <- as.numeric(xmlToList(AOI1_Frame$KeyFrame)$Timestamp)
Points <- as.numeric(AOI1_unlist_Frame)
Points <- matrix(Points, nrow = length(Points)/2, byrow = T)
Points[, 1] <- Points[, 1] - 258 # Verschiebung linker Rand
Points <- split(Points, 1:nrow(Points)) 

my_list <- list(timestamp = Timestamp, points = Points)

# Erstellung der binären Matrizen

AOI1_Ant <- matrix(unlist(my_list$points), nrow = 
                     length(unlist(my_list$points))/2, byrow = T)
AOI1_Ant[, 1] <- AOI1_Ant[, 1]/1350
AOI1_Ant[, 2] <- AOI1_Ant[, 2]/1080
  
pnts <- AOI1_Ant
imgSize <- c(720, 576)
imgSize1 <- c(1350, 1080)

m <- rasterizeROI(pnts, imgSize)
m <- abs(m - 1)


m1 <- rasterizeROI(pnts, imgSize1)
m1 <- abs(m1 - 1)

AOI_car.cornfield_stat <- mirror.matrix(m)
AOI_car.cornfield_stat1350_1080 <- mirror.matrix(m1)

xROI::addMask(AOI_car.cornfield_stat, add = FALSE)
xROI::addMask(AOI_car.cornfield_stat1350_1080, add = FALSE)
dim(AOI_car.cornfield_stat)
dim(AOI_car.cornfield_stat1350_1080)

save(AOI_car.cornfield_stat, file = 'car.corn_static_AOI.Rdata')
save(AOI_car.cornfield_stat1350_1080, 
     file = 'car.corn_static_AOI1350_1080.Rdata')

table(AOI_car.cornfield_stat)
xROI::addMask(AOI_car.cornfield_stat, add = FALSE)





### walking market (Dynamic AOI)

parsed.url <- xmlTreeParse(file = "Video-Stimuli-EP-AG-Export_190806_tw/Video-26_Walking-Market_AOI.xml",
                           error = function(...) {
                           }, useInternalNodes = T)

DynamicAOI1 <- xpathSApply(doc = parsed.url, path = "//KeyFrames")[[1]] # Frau
DynamicAOI2 <- xpathSApply(doc = parsed.url, path = "//KeyFrames")[[2]] # Marktstand


# Bringe AOI1_Frame in dataframe-Form:
my_list <- list()
for(i in 1:7){
  AOI1_Frame <- DynamicAOI1[i]
  AOI1_unlist_Frame <- unlist(xmlToList(AOI1_Frame$KeyFrame)$Points)
  Timestamp <- as.numeric(xmlToList(AOI1_Frame$KeyFrame)$Timestamp)
  Area <- as.numeric(xmlToList(AOI1_Frame$KeyFrame)$Area)
  Points <- as.numeric(AOI1_unlist_Frame)
  Points <- matrix(Points, nrow = length(Points)/2, byrow = T)
  Points[, 1] <- Points[, 1] - 258 # Verschiebung linker Rand
  Points <- split(Points, 1:nrow(Points)) 
  
  my_list[[i]] <- list(timestamp = Timestamp, area = Area, points = Points)
}

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

library(spatstat)
my_list_new <- list()

for(i in 1:7){

  c1 <- my_list[[i]]$points$`1`
  c2 <- my_list[[i]]$points$`2`
  
  a <- euc.dist(c1, c2)/2
  centre <- c((c1[1] + c2[1])/2, (c1[2] + c2[2])/2 )
  Area <-  my_list[[i]]$area

  # Area = pi * a * b
  # d.h. b = Area/(pi * a)

  b <- Area / (pi * a)
  
  W <- ellipse(a = b, b = a, centre = centre, phi = 0)
  W_points <- cbind(W$bdry[[1]]$x, W$bdry[[1]]$y)
  
  Timestamp <- my_list[[i]]$timestamp
  
  my_list_new[[i]] <- list(timestamp = Timestamp, 
                           points = split(t(W_points), rep(1:nrow(W_points), 
                                                        each = ncol(W_points))))

  
}

source('poly_interpolate.R')

interpolate_AOI1 <- interpolate_poly_list(my_list_new, t = seq(0, 5960000,
                                                           length.out = 5.96*25))

summary(interpolate_AOI1)
plot_pl(interpolate_AOI1) # 25 Hz

# Erstellung der binären Matrizen
library(xROI)
List_AOI_walking.market_dyn <- list()

for(i in 1 : (5.96 * 25)){
  interpolate_unlist <- unlist(interpolate_AOI1[[i]]$points)
  
  AOI1_Ant <- matrix(interpolate_unlist, nrow = length(interpolate_unlist)/2, byrow = T)
  AOI1_Ant[, 1] <- AOI1_Ant[, 1]/1350
  AOI1_Ant[, 2] <- AOI1_Ant[, 2]/1080
  
  pnts <- AOI1_Ant
  imgSize <- c(720, 576)
  m <- rasterizeROI(pnts, imgSize)
  m <- abs(m - 1)
  mirror.matrix <- function (x)
  {
    xx <- as.data.frame(t(x))
    xx <- rev(xx)
    xx <- t(as.matrix(xx))
    xx
  }
  List_AOI_walking.market_dyn[[i]] <- mirror.matrix(m)
}
str(List_AOI_walking.market_dyn)

xROI::addMask(List_AOI_walking.market_dyn[[149]], add = FALSE)

save(List_AOI_walking.market_dyn, file = 'walking.market_dynamic_AOI.Rdata')



### walking market (Static AOI)


# Erstellung der Marktstand-AOI:
# Da nicht dynamisch, reicht eine AOI-Map für alle Frames:

AOI2_Frame <- DynamicAOI2[1]
AOI2_unlist_Frame <- unlist(xmlToList(AOI2_Frame$KeyFrame)$Points)
Timestamp <- as.numeric(xmlToList(AOI2_Frame$KeyFrame)$Timestamp)
Points <- as.numeric(AOI2_unlist_Frame)
Points <- matrix(Points, nrow = length(Points)/2, byrow = T)
Points[, 1] <- Points[, 1] - 258 # Verschiebung linker Rand
Points <- split(Points, 1:nrow(Points)) 

my_list <- list(timestamp = Timestamp, points = Points)

# Erstellung der binären Matrizen

AOI2_Ant <- matrix(unlist(my_list$points), nrow = 
                     length(unlist(my_list$points))/2, byrow = T)
AOI2_Ant[, 1] <- AOI2_Ant[, 1]/1350
AOI2_Ant[, 2] <- AOI2_Ant[, 2]/1080

pnts <- AOI2_Ant
imgSize <- c(720, 576)
m <- rasterizeROI(pnts, imgSize)
xROI::addMask(m, add = FALSE)
m <- abs(m - 1)

mirror.matrix <- function (x)
{
  xx <- as.data.frame(t(x))
  xx <- rev(xx)
  xx <- t(as.matrix(xx))
  xx
}

AOI_walking.market_stat <- mirror.matrix(m)
save(AOI_walking.market_stat, file = 'walking.market_static_AOI.Rdata')
table(AOI_walking.market_stat)
xROI::addMask(AOI_walking.market_stat, add = FALSE)

