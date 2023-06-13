
get_nice_map <- function(place, plot = T){
  
  bbox <- getbb(place)
  
  library(osmdata)
  streets <- bbox %>%
    opq()%>%
    add_osm_feature(key = "highway",
                    value = c("motorway", "primary",
                              "secondary", "tertiary")) %>%
    osmdata_sf()

  small_streets <- bbox %>%
    opq()%>%
    add_osm_feature(key = "highway",
                    value = c("residential", "living_street",
                              "unclassified",
                              "service", "footway")) %>%
    osmdata_sf()
  
  river <- bbox %>%
    opq()%>%
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_sf()
  
  streets <- list(streets, small_streets, river)
  
  if(plot){
    
    ggplot()+geom_sf(data = streets[[1]]$osm_lines, inherit.aes = FALSE, color = "black", size = .4, alpha = .6) +
      geom_sf(data = streets[[2]]$osm_lines,
              inherit.aes = FALSE,
              color = "black",
              size = .4,
              alpha = .5)+
      geom_sf(data = streets[[3]]$osm_lines,
              inherit.aes = FALSE,
              color = "black",
              size = .2,
              alpha = .3)+theme_void()+ggtitle(place)+theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Futura", color = '#333333'))
  } else {
  return(streets)}
  }

get_nice_map(place = 'Volda')
