library(terra) # rasts and vects
library(tidyterra) # terra + ggplot
library(sf) # shapefiles
library(rnaturalearth) # boundary data
library(tidyverse) # everything else


p_radar <- function(conf,colmap){

    rname1<-conf$data1
    rname2<-conf$data2

    rast1<-rast(rname1)
    rast2<-rast(rname2)

    # Create country lines
    basemap_sf <- ne_countries(
        country = "Finland",
        scale = "large", 
        returnclass = "sf") %>% 
        dplyr::select(1) %>% 
        st_transform(crs = crs(rast1))  # make sure crs is same as raster

    basemap_vect <- basemap_sf %>% 
        vect()

    # Read in lakes and seas
    lakes <- read_sf("data/ne_10m_lakes.shp")
    seas <- read_sf("data/ne_10m_ocean.shp")

    # Need to set a name for the raster data, was not able to access
    # it otherwise
    set.names(rast1,"testi")
    set.names(rast2,"testi")

    # Do a difference between the datas
    rast3 <- setValues(rast1,values(rast2)-values(rast1))

    # Generate a layered map, starting from seas, country lines,
    # lakes, and finally the data itself (the order matters a lot)
    fh<-ggplot(seas) + 
        geom_sf(data = basemap_sf,
                fill = "white") +
        geom_sf(data = seas,
                fill = "lightblue")+ 
        geom_sf(data = lakes,
                fill = "lightblue") + 
        geom_spatraster(
            data = rast3,
            aes(fill = testi)) +
        colmap +
        coord_sf(xlim = c(22.2, 30.45), ylim = c(60.7, 64.4))+
        theme_linedraw()+
        theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        labs(fill = NULL)

    return(fh)
}
