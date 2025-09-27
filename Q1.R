library(readxl)
library(sf)
library(ggplot2)

xls_path <- "C:/Users/zofia/OneDrive/Dokumenty/aaschool/y2/q1/statistical computing/MapV1bombs_xycoordinates.xlsx"
data <- read_excel(xls_path)

pts <- st_as_sf(data, coords = c("xcoordinates","ycoordinates"), crs = 4326)
pts_proj <- st_transform(pts, 27700)

bb <- st_bbox(
  c(xmin = 521693.2, xmax = 547285.1, ymin = 169883.8, ymax = 188149.3),
  crs = st_crs(pts_proj)
)
rect <- st_as_sfc(bb)
rect <- st_sf(geometry = rect)

ggplot() +
  geom_sf(data = pts_proj, size = 0.4, color = "blue") +
  geom_sf(data = rect, fill = NA, color = "red", linewidth = 1)


