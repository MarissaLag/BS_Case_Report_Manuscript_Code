#Map making
#Baynes Sound, BC

#Load packages ----
install.packages(c("maps", "ggplot2", "mapdata", "cowplot"))
library(maps)
library(mapdata)
library(ggplot2)
library(cowplot)

#Set theme ----
theme.marissa <- function() {
  theme_classic(base_size = 14) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16, face = "bold"))
}

theme_set(theme.marissa())

# Get map data
bc_map <- map_data("worldHires", region = "Canada")

# Study site coordinates
site <- data.frame(lon = -124.8, lat = 49.5, name = "Baynes Sound")

# Main BC map
ggplot(bc_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray70", color = "black") +
  coord_map(xlim = c(-130, -122), ylim = c(47.7, 52.85)) +
  annotate("rect", xmin = -125, xmax = -124.1, 
           ymin = 49.3, ymax = 49.8,
           fill = NA, color = "red", linewidth = 1.5) +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),      
        axis.ticks = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 1.5, fill = NA)
        )  

# Zoomed inset
ggplot(bc_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray70", color = "black") +
  coord_map(xlim = c(-125.1, -123.9), ylim = c(49.1, 49.8)) +
  theme(panel.border = element_rect(color = "black", linewidth = 1.5, fill = NA)) +
  labs(x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", width_hint = 0.2)


