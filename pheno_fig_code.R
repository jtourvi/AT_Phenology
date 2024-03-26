library(sf)
library(ggplot2)
library(tidyverse)
library(rgdal)
library(maps)
library(raster)
library(cowplot)

# The input file geodatabase
fgdb1 <- "C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Documents/ArcGIS/Projects/MyProject1.gdb/"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb1)
print(fc_list)

# Read the feature class
states1 <- readOGR(dsn=fgdb1,layer="cb_2018_us_sta_ExportF_Union")
app_line = readOGR(dsn=fgdb1,layer="app_center")

# Determine the FC extent, projection, and attribute information
summary(states)

# View the feature class
plot(states)

dma.df1 <- fortify(states1)
al = fortify(app_line)
summary(al)
#dma.df <- rename(dma.df, DMA = id)

#num of total observations

point_density <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Desktop/p_dens")
test_spdf <- as(point_density, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

#GAP/LADFIRE Forests

point_density1 <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Desktop/gaplf2011lc_v30_lcc_10.tif")
test_spdf1 <- as(point_density1, "SpatialPixelsDataFrame")
test_df1 <- as.data.frame(test_spdf1)
colnames(test_df1) <- c("value", "x", "y")

big_map = ggplot() + 
  geom_polygon(data=dma.df1, aes(x=long, y=lat, group=group), color = "grey", linewidth = 0.5, fill = "white") +
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.7) +
  geom_line(data = al, aes(x=long, y=lat, group=group), color = "red", linewidth = 0.5, lty = "dashed") +
  #coord_map(clip = "off") +
  #geom_point(data = a_point, aes(x = long, y = lat), color = "red", size = 5, pch = 17) +
  coord_cartesian(xlim = c(-86, -67), ylim = c(31, 48)) +
  scale_fill_viridis_b(name = "Observations", option = "plasma", breaks = c(0,10,100,1000,10000,100000)) +
  #xlim(-92,-66)+
  #ylim(30,47.5) +
  annotate("text", label = "Canopy Trees (n = 72,957) ", x = -75, y = 32, size = 3, colour = "black", hjust = 0) +
  annotate("text", label = "Understory Plants (n = 45,293)", x = -75, y = 31, size = 3, colour = "black", hjust = 0) +
  annotate("text", label = "Northern AT", x = -86.8, y = 45.5, size = 4, colour = "black", hjust = 0) +
  annotate("text", label = "Mid-Atlantic AT", x = -86.8, y = 41.5, size = 4, colour = "black", hjust = 0) +
  annotate("text", label = "Southern AT", x = -86.8, y = 37.5, size = 4, colour = "black", hjust = 0) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_hline(yintercept = 34, lwd = 1) +
  geom_hline(yintercept = 38, lwd = 1) +
  geom_hline(yintercept = 42, lwd = 1) +
  geom_hline(yintercept = 46, lwd = 1) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
big_map

tiff(file = "big_map.tiff", width = 6.5, height = 5, units = 'in', res = 600, pointsize = 11)
big_map
dev.off()

diff_n <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Desktop/n_ras")
test_n <- as(diff_n, "SpatialPixelsDataFrame")
n_df <- as.data.frame(test_n)
colnames(n_df) <- c("value", "x", "y")

n_mask <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Desktop/n_mask")
test_mn <- as(n_mask, "SpatialPixelsDataFrame")
test_mn <- as.data.frame(test_mn)
colnames(test_mn) <- c("value", "x", "y")

north_map = ggplot() + 
  geom_polygon(data=dma.df1, aes(x=long, y=lat, group=group), color = "grey", linewidth = 0.5, fill = "white") +              
  geom_tile(data=n_df, aes(x=x, y=y, fill=value), alpha=0.7) +
  #geom_tile(data=test_df1, aes(x=x, y=y, fill=value), alpha=0.7) +
  #geom_tile(data=test_mn, aes(x=x, y=y, fill=value), alpha=0.9) +
  geom_line(data = al, aes(x=long, y=lat, group=group), color = "red", linewidth = 0.5, lty = "dashed") +
  #coord_map(clip = "off") +
  coord_cartesian(xlim = c(-73.9, -68), ylim = c(41.5, 47)) +
  scale_fill_viridis_c(name = "\u0394 Window \nSensitivity (days) \nUnderstory - Canopy", 
                       option = "plasma", limits = c(-0.5,4.5)) +
  #xlim(-92,-66)+
  #ylim(30,47.5) +
  #annotate("text", label = "Canopy Trees (n = 72,957) ", x = -75, y = 34, size = 3, colour = "black", hjust = 0) +
  #annotate("text", label = "Understory Plants (n = 45,293)", x = -75, y = 33, size = 3, colour = "black", hjust = 0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Northern AT") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        title = element_text(size = 14))
north_map

diff_m <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Desktop/mid_new")
test_m <- as(diff_m, "SpatialPixelsDataFrame")
m_df <- as.data.frame(test_m)
colnames(m_df) <- c("value", "x", "y")

mid_map = ggplot() + 
  geom_polygon(data=dma.df1, aes(x=long, y=lat, group=group), color = "grey", linewidth = 0.5, fill = "white") +
  geom_tile(data=m_df, aes(x=x, y=y, fill=value), alpha=0.7) +
  geom_line(data = al, aes(x=long, y=lat, group=group), color = "red", linewidth = 0.5, lty = "dashed") +
  #coord_map(clip = "off") +
  coord_cartesian(xlim = c(-79.5, -72.5), ylim = c(37.5, 42.5)) +
  scale_fill_viridis_c(name = "\u0394 Window \nSensitivity (days) \nUnderstory - Canopy", 
                       option = "plasma", limits = c(-0.5,4.5)) +
  #xlim(-92,-66)+
  #ylim(30,47.5) +
  #annotate("text", label = "Canopy Trees (n = 72,957) ", x = -75, y = 34, size = 3, colour = "black", hjust = 0) +
  #annotate("text", label = "Understory Plants (n = 45,293)", x = -75, y = 33, size = 3, colour = "black", hjust = 0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Mid-Atlantic AT") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        title = element_text(size = 14))
mid_map

diff_s <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Desktop/low_diff1")
test_s <- as(diff_s, "SpatialPixelsDataFrame")
s_df <- as.data.frame(test_s)
colnames(s_df) <- c("value", "x", "y")

south_map = ggplot() + 
  geom_polygon(data=dma.df1, aes(x=long, y=lat, group=group), color = "grey", linewidth = 0.5, fill = "white") +
  geom_tile(data=s_df, aes(x=x, y=y, fill=value), alpha=0.7) +
  geom_line(data = al, aes(x=long, y=lat, group=group), color = "red", linewidth = 0.5, lty = "dashed") +
  #coord_map(clip = "off") +
  coord_cartesian(xlim = c(-84.5, -78), ylim = c(33.5, 38.5)) +
  scale_fill_viridis_c(name = "\u0394 Window \nSensitivity (days) \nUnderstory - Canopy", 
                       option = "plasma", limits = c(-0.5,4.5)) +
  #xlim(-92,-66)+
  #ylim(30,47.5) +
  #annotate("text", label = "Canopy Trees (n = 72,957) ", x = -75, y = 34, size = 3, colour = "black", hjust = 0) +
  #annotate("text", label = "Understory Plants (n = 45,293)", x = -75, y = 33, size = 3, colour = "black", hjust = 0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Southern AT") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        title = element_text(size = 14))
south_map

tiff(file = "3_map_vert.tiff", width = 5.5, height = 10, units = 'in', res = 600, pointsize = 11)

all_sec = plot_grid(north_map, mid_map, south_map, ncol = 1, labels = c("A", "B", "C"))
all_sec

dev.off()

###myco maps

# The input file geodatabase
fgdb <- "C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Documents/ArcGIS/Projects/MyProject2/MyProject2.gdb/"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
states <- readOGR(dsn=fgdb,layer="cb_2018_us_sta_VT")
app_point = readOGR(dsn=fgdb,layer="green_points_ExportFeatures1")
a_point = read.csv("vt_points.csv", header = TRUE)

# Determine the FC extent, projection, and attribute information
summary(states)

# View the feature class
plot(states)
plot(app_point)

dma.df <- fortify(states)
al = fortify(app_point)
#summary(al)
dma.df <- rename(dma.df, DMA = id)

point_density <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Documents/ArcGIS/Projects/MyProject2/vt_elev")
test_spdf <- as(point_density, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")
library(ggrepel)

vt_map = ggplot() + 
  geom_polygon(data=dma.df, aes(x=long, y=lat, group=group), color = "grey", linewidth = 0.5, fill = "white") +
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.7) +
  geom_point(data = a_point, aes(x = long, y = lat), color = "black", size = 5, pch = 17) +
  #coord_map(clip = "off") +
  coord_cartesian(xlim = c(-73.5, -71), ylim = c(42.75, 45)) +
  scale_fill_viridis(name = "Elevation \n(m a.s.l.)", option = "C", breaks = c(0,300,600,900,1200,1500),
                       limits = c(0, 1500)) +
  #ylim(30,47.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  #ggspatial::annotation_scale(plot_unit = "m",
    #location = "br",
    #bar_cols = c("grey60", "white")
  #) +
  scalebar(data = dma.df, location = "bottomright", dist = 30,
           dist_unit = "km", transform = TRUE,  model = "WGS84", 
           st.size = 3, anchor = c(x = -70.97, y = 42.75)) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    ))

vt_map

vt_map1 = vt_map + geom_label_repel(data = a_point, aes(label = Site, x = long, y = lat),
                          box.padding   = 0.7, 
                          point.padding = 0.3,
                          segment.color = 'black')
vt_map1

main_map <- 
  vt_map +
  geom_rect(
    xmin = 1869227,
    ymin = 5086142,
    xmax = 1887557,
    ymax = 5104660,
    fill = NA, 
    colour = "black",
    size = 0.6
  )

main_map %>% 
  ggdraw() +
  draw_plot(
    {
      main_map + 
        coord_sf(
          xlim = c(1869227, 1887557),
          ylim = c(5086142, 5104660),
          expand = FALSE) +
        theme(legend.position = "none")
    },
    x = 0.58, 
    y = 0,
    width = 0.46, 
    height = 0.46)

main_map

#install.packages("png")             # Install png package
library(png)# Load png package
library(patchwork)
#install.packages("ggspatial")
library(ggspatial)

my_image <- readPNG("C:/Users/jtourville/Downloads/myco_fig1.draw.png", native = TRUE)
my_image1 <- readPNG("C:/Users/jtourville/Downloads/Picture_b.png", native = TRUE)

ggp_image <- vt_map1 +                  # Combine plot & image
  inset_element(p = my_image1,
                left = 0.58,
                bottom = 0.23,
                right = 0.98,
                top = 0.6)
ggp_image  

ggp_image1 <- ggp_image +                  # Combine plot & image
  inset_element(p = my_image1,
                left = 0.8,
                bottom = 0.6,
                right = 0.98,
                top = 0.98)
ggp_image1 

tiff(file = "vt_map_note.tif", width = 6.5, height = 5, units = 'in', res = 600, pointsize = 11)
ggp_image
dev.off()

install.packages('ggsn')
library(ggsn)

vt_map2 = vt_map1 +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
vt_map2

tiff(file = "vt_map2.tiff", width = 6.5, height = 5, units = 'in', res = 600, pointsize = 11)
vt_map2
dev.off()

###########################
library(sf)
library(ggplot2)
library(tidyverse)
library(rgdal)
library(maps)
library(raster)

# The input file geodatabase
fgdb1 <- "C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Documents/ArcGIS/Projects/MyProject1.gdb/"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb1)
print(fc_list)

# Read the feature class
states1 <- readOGR(dsn=fgdb1,layer="cb_2018_us_sta_ExportF_Union")
app_line = readOGR(dsn=fgdb1,layer="nf_sites_XY")
a_p = read.csv("nf_sites.csv")

# Determine the FC extent, projection, and attribute information
summary(states)

# View the feature class
plot(states1)

dma.df1 <- fortify(states1)
al = fortify(app_line)
summary(al)
#dma.df <- rename(dma.df, DMA = id)

#num of total observations

point_density <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Documents/ArcGIS/Projects/MyProject1.gdb/elevation_1KMmn_GMTE_1_Clip")
test_spdf <- as(point_density, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

#GAP/LADFIRE Forests

point_density1 <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Desktop/gaplf2011lc_v30_lcc_10.tif")
test_spdf1 <- as(point_density1, "SpatialPixelsDataFrame")
test_df1 <- as.data.frame(test_spdf1)
colnames(test_df1) <- c("value", "x", "y")

big_map = ggplot() + 
  geom_polygon(data=dma.df1, aes(x=long, y=lat, group=group), color = "grey", linewidth = 0.5, fill = "white") +
  #geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.7) +
  #geom_line(data = al, aes(x=long, y=lat, group=group), color = "red", linewidth = 0.5, lty = "dashed") +
  #coord_map(clip = "off") +
  geom_point(data = a_p, aes(x = Longitude, y = Latitude), color = "red", size = 5, pch = 17) +
  coord_cartesian(xlim = c(-75, -68.5), ylim = c(42.5, 46)) +
  #scale_fill_viridis_b(name = "Observations", option = "plasma", breaks = c(0,10,100,1000,10000,100000)) +
  #xlim(-92,-66)+
  #ylim(30,47.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
big_map

big_map1 = big_map + geom_label_repel(data = a_p, aes(label = mountain, x = Longitude, y = Latitude),
                                    box.padding   = 0.7, 
                                    point.padding = 0.3,
                                    segment.color = 'black')
big_map1

tiff(file = "nsf_map.tiff", width = 6.5, height = 5, units = 'in', res = 600, pointsize = 11)
big_map1
dev.off()
