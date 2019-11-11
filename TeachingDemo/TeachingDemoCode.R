### Teaching Demo Code - KNN regression for house prices
# Presenter: Dr. Karsten Maurer

#--------------------------------------------------------------------------------------
### Load a few helpful libraries for working with the data and building knn models
library(tidyverse)
library(caret)
library(ggvoronoi)
# If you don't have them installed, then uncomment and run the following:
# install.packages(c("tidyverse","caret","ggvoronoi"))

#--------------------------------------------------------------------------------------
### Load up Oxford Ohio house prices data scraped from Zillow webpages (accessed: 11/3/19) 
# Data gathering and processing used tools from the following R packages: 
#     rvest, XML, xml2, ZillowR, stringr, dplyr
real_estate <- read.csv("http://kmaurer.github.io/documents/data/oxford_houses.csv", 
                        stringsAsFactors = FALSE)
head(real_estate)

ggplot() +
  geom_point(aes(x=std_sqft, y=std_lot, color=sale_price), 
             data=real_estate) +
  scale_color_viridis_c("Price", limits=c(0,400000),
                        breaks=seq(0,400000, by= 100000),
                        labels=paste0("$",seq(0,400,by=100),"k"))+
  labs(x="Floor Size (standarized)", y="Lot Size (Standardized)")+
  theme_bw()

#--------------------------------------------------------------------------------------
### Start coding knn for predictions here:

# Define a 10-nearest neighbor model
price_knn <- knnreg(sale_price ~ std_sqft + std_lot,
                   data=real_estate, k=10)
# Use the 10-NN regression to predict for a new house
new_house <- data.frame(std_sqft=1, std_lot=0)
predict(price_knn, newdata = new_house)

# Use the 10-NN regression to predict for MANY new houses
# each with a unique combination of characteristic values
new_houses <- expand.grid(std_sqft=seq(-1.5,4,.02), 
                          std_lot=seq(-1.5,4,.02))
new_houses$pred_price <- predict(price_knn, newdata = new_houses)
head(new_houses)

# What do those predictions look like at all unique combinations
ggplot() +
  geom_tile(aes(x=std_sqft, y=std_lot, fill=pred_price), 
             data=new_houses)+
  geom_point(aes(x=std_sqft, y=std_lot), data=real_estate, shape=1) +
  scale_fill_viridis_c("Price", limits=c(0,400000),
                        breaks=seq(0,400000, by= 100000),
                        labels=paste0("$",seq(0,400,by=100),"k"))+
  labs(x="Floor Size (standarized)", y="Lot Size (Standardized)")+
  theme_bw()


# Note: one nearest neightbor is a voronoi tesselation
ggplot() +
  geom_voronoi(aes(x=std_sqft, y=std_lot, fill=sale_price), 
             data=real_estate) +
  scale_fill_viridis_c("Price", limits=c(0,400000),
                        breaks=seq(0,400000, by= 100000),
                        labels=paste0("$",seq(0,400,by=100),"k"))+
  labs(x="Floor Size (standarized)", y="Lot Size (Standardized)")+
  theme_bw()


### Make an animation of how the predictions change from 1NN to 20NN
# Build the data for the animation
compare_knns <- NULL
for(k in 1:20){
  price_knn <- knnreg(sale_price ~ std_sqft + std_lot,
                      data=real_estate, k=k)
  price_knn
  
  new_house <- data.frame(std_sqft=1, std_lot=0)
  predict(price_knn, newdata = new_house)
  
  new_houses <- expand.grid(std_sqft=seq(-1.5,4,.02), 
                            std_lot=seq(-1.5,4,.02),
                            k=k)
  
  new_houses$pred_price <- predict(price_knn, newdata = new_houses)
  compare_knns <- rbind(compare_knns,new_houses)
}
head(compare_knns)

# Run the animation with gganimate
library(gganimate)
p1 <- ggplot() +
  geom_tile(aes(x=std_sqft, y=std_lot, fill=pred_price), 
            data=compare_knns)+
  geom_point(aes(x=std_sqft, y=std_lot), data=real_estate, shape=1) +
  scale_fill_viridis_c("Price", limits=c(0,400000),
                       breaks=seq(0,400000, by= 100000),
                       labels=paste0("$",seq(0,400,by=100),"k"))+
  labs(x="Floor Size (standarized)", y="Lot Size (Standardized)")+
  theme_bw()

anim <- animate( p1 +
  transition_states(k) )
anim

