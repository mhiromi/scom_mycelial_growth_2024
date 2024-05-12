### Plotting a map with strain IDs

library(ggplot2)
library(rnaturalearth)
library(ggrepel)  

japan_map <- ne_countries(scale = "medium", country = "japan", returnclass = "sf")
points_df <- data.frame (
  ## FC6170==IFM65656
  Strain = c("FC8191 (Minami-Torishima)", "FC8192 (Minami-Torishima)", "FC8152 (Akita)", "FC8125 (Ibaraki)", "FC8172 (Saga)","FC6141 (Kanagawa)",  "FC6170/IFM65656 (unknown)"),
  lon = c(154.50, 153.98, 140.10,140.45, 130.30, 139.64, 123.0),
  lat = c(24.70, 24.29, 39.72,  36.34, 33.25, 35.45,45.0)
)
shp <- c(16, 16, 16, 16, 16, 16, 16)
clr <- c("#84919e", "#1f77b4", "#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#e377c2")


p_map <- ggplot(data = japan_map) +
  geom_sf(fill = "white", color = "black") + 
  geom_point(data = points_df, aes(x = lon, y = lat, color=Strain, shape=Strain), size = 5, alpha=1) +
  geom_label_repel(data = points_df, aes(x=lon, y=lat, label=Strain, color=Strain), 
                   box.padding = 0.5, point.padding = 0.5, segment.color ='grey50', size = 7) + 
  
  scale_color_manual(values = clr) +
  scale_shape_manual(values = shp) +
  guides(color=FALSE, shape=FALSE) + 
  theme_minimal() +
  theme(    axis.title = element_text(size = 20),  # Size for axis titles
            axis.text = element_text(size = 20),  # Size for axis text
            plot.title = element_text(size = 20, face = "bold")
  )   +
  labs(title = "Sampling Location", x="Longitude", y="Latitude", position="center") 
plot(p_map)
ggsave("Fig1_map.pdf", p_map, width=8.27, height=11.69, units="in")
