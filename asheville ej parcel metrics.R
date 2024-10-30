library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(scales)


# Read in county's parcel data
st_path <- "C:\\Users\\jpco\\OneDrive - University of North Carolina at Chapel Hill\\assets\\geodata\\Buncombe_County_Parcels\\Buncombe_County_Parcels.shp"
parcels_county <- st_read(st_path)

# Read in waste sites
st_path <- "C:\\Users\\jpco\\OneDrive - University of North Carolina at Chapel Hill\\projects\\asheville helene ej analysis\\waste sites\\POINT.shp"
waste_sites <- st_read(st_path) %>% 
  st_transform(crs = 2264)

# Read in zoning
st_path <- "C:\\Users\\jpco\\OneDrive - University of North Carolina at Chapel Hill\\projects\\asheville helene ej analysis\\Asheville_Zoning\\Asheville_Zoning.shp"
zoning <- st_read(st_path) %>% 
  st_transform(crs = 2264)


### Make geoms valid

parcels_county <- st_make_valid(parcels_county)
zoning <- st_make_valid(zoning)


### Construct set of plausible public parcels in Asheville

parcels_candidate <- parcels_county %>% 
  st_drop_geometry() %>% 
  filter(Exempt == 'EXM' & CityName == 'ASHEVILLE')

table(parcels_candidate$Owner)

# decided to only filter to owned by City of Asheville. Including County of Buncombe could also be valid.

parcels_candidate <- parcels_county %>%
  filter(Owner == 'CITY OF ASHEVILLE')


### Map to inspect

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(data = st_transform(parcels_candidate, 4326), fillColor = "orange", color = "black", 
              weight = 1, opacity = 0.6, fillOpacity = 0.3, 
              group = "Parcels") %>%
  addMarkers(data = st_transform(waste_sites, 4326),
              group = "sites") %>%
  addLayersControl(
    overlayGroups = c("Parcels", "Sites"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  setView(lng = mean(st_coordinates(st_transform(waste_sites, 4326))[,1]), 
          lat = mean(st_coordinates(st_transform(waste_sites, 4326))[,2]), 
          zoom = 12)  # Center the map on the parcels



### Get current waste site parcels

parcels_permitted <- parcels_county[waste_sites, ]

ggplot() +
  geom_sf(data=parcels_permitted)+
  geom_sf(data=waste_sites)


### Buffer parcels

parcels_permitted_buff_025_mi <- st_buffer(st_centroid(parcels_permitted), 1320)
parcels_permitted_buff_05_mi <- st_buffer(st_centroid(parcels_permitted), 2640)
parcels_permitted_buff_1_mi <- st_buffer(st_centroid(parcels_permitted), 5280)

# decided to use centroid since golf course is quite variable

ggplot() +
  geom_sf(data= parcels_permitted_buff_1_mi ) +
  geom_sf(data= parcels_permitted_buff_05_mi ) +
  geom_sf(data= parcels_permitted_buff_025_mi ) +
  geom_sf(data=parcels_permitted)+
  geom_sf(data=waste_sites)


### Bring in zoning

zoning <- zoning %>% 
  select(districts)

parcels_county <- st_join(parcels_county, zoning)

table(parcels_county$districts)


### Attribute parcels with buffer distances
parcels_county <- st_join(parcels_county, 
                          parcels_permitted_buff_1_mi %>% 
                            mutate(WASTE_SITE_BUFFER_1 = TRUE) %>% 
                            select(WASTE_SITE_BUFFER_1),
                          left = T)

parcels_county <- st_join(parcels_county, 
                          parcels_permitted_buff_05_mi %>% 
                            mutate(WASTE_SITE_BUFFER_05 = TRUE) %>% 
                            select(WASTE_SITE_BUFFER_05),
                          left = T)

parcels_county <- st_join(parcels_county, 
                          parcels_permitted_buff_025_mi %>% 
                            mutate(WASTE_SITE_BUFFER_025 = TRUE) %>% 
                            select(WASTE_SITE_BUFFER_025),
                          left = T)

parcels_county <- parcels_county %>% 
  mutate(WASTE_SITE_BUFFER = case_when(
    WASTE_SITE_BUFFER_025 ~ "0.25 mile",
    WASTE_SITE_BUFFER_05 ~ "0.5 mile",
    WASTE_SITE_BUFFER_1 ~ "1 mile",
    .default = "More than 1 mile"
  ))

table(parcels_county$WASTE_SITE_BUFFER)


### Duplicate PINS exist
remove_dup_PINS <- function (df) {
  df %>%
    filter(!Acreage %in% c('0','0.0')) %>% # remove personal prop with no land
    group_by(PIN) %>% 
    mutate(id = row_number()) %>% 
    filter(id == 1) %>% 
    select(-id)
}


### Area by land value per acre relative to city average

# note -- Dearview apts in RES EXP district

parcels_county %>% 
  st_drop_geometry() %>% 
  remove_dup_PINS() %>%
  filter(grepl("RM",districts) | grepl("RS",districts) | grepl("RES EXP",districts) | grepl("URD",districts)) %>% 
  mutate(
    `Land Value per Acre` = as.numeric(LandValue) / Acreage
  ) %>% 
  group_by(`WASTE_SITE_BUFFER`) %>%
  summarise(`Land Value per Acre` = median(`Land Value per Acre`, na.rm = TRUE), .groups = 'drop') %>% 
  ggplot(aes(`WASTE_SITE_BUFFER`, `Land Value per Acre`)) +
  geom_bar(aes(), #aes(fill = `districts`), 
           position = "dodge", stat = "identity") +
  geom_text(aes(label = dollar(`Land Value per Acre`)), #fill = `Land Use`, color = `Land Use`), 
            vjust = -0.5, size = 4, angle = 0, position = position_dodge(width = 0.9)) +
  scale_y_continuous(labels = label_number(suffix = " K", prefix = "$ ", scale = 1e-3)) +
  labs(y = "", x = "distance to debris site") +
  theme_minimal() + 
  ggtitle("Land values of residential parcels near permitted Hurricane Helene debris sites",
          subtitle = "Median appraised land value per acre")

parcels_county %>% 
  st_drop_geometry() %>% 
  remove_dup_PINS() %>%
  filter(grepl("RM",districts) | grepl("RS",districts) | grepl("RES EXP",districts) | grepl("URD",districts)) %>% 
  mutate(
    `Market Value per Acre` = as.numeric(TotalMarke) / as.numeric(Acreage)
  ) %>% 
  group_by(`WASTE_SITE_BUFFER`) %>%
  summarise(`Market Value per Acre` = mean(`Market Value per Acre`, na.rm = TRUE), .groups = 'drop') %>% 
  ggplot(aes(`WASTE_SITE_BUFFER`, `Market Value per Acre`)) +
  geom_bar(aes(), #fill = `Land Use`), 
           position = "dodge", stat = "identity") +
  geom_text(aes(label = dollar(`Market Value per Acre`)), #fill = `Land Use`, color = `Land Use`), 
            vjust = -0.5, size = 4, angle = 0, position = position_dodge(width = 0.9)) +
  scale_y_continuous(labels = label_number(suffix = " K", prefix = "$ ", scale = 1e-3)) +
  labs(y = "", x = "distance to debris site") +
  theme_minimal() + 
  ggtitle("Market values of residential parcels near permitted Hurricane Helene debris sites",
          subtitle = "Median appraised matket value per acre")


### Area by zoning class

parcels_county %>% 
  st_drop_geometry() %>% 
  remove_dup_PINS() %>% 
  filter(districts != 'NA') %>% 
  mutate(
    `Zoning Group` = case_when(
      grepl("EXP",districts) ~ "Exempt",
      grepl("RS",districts) ~ "Single family",
      grepl("RM",districts) ~ "Multifamily",
      grepl("UV",districts) | grepl("UP",districts) | grepl("URD",districts) ~ "Mixed Use",
      grepl("CB",districts) | districts %in% c("COM","NB","O2","OFFICE","OB","HB","RB","NCD") ~ "Business",
      grepl("INST",districts) ~ "Institutional",
      grepl("HR",districts) ~ "Haywood Road",
      grepl("RAD",districts) ~ "River Arts District",
      grepl("CI",districts) | grepl("LI",districts) | grepl("IND",districts) ~ "Industrial",
      grepl("HCU",districts) | grepl("RESORT",districts) ~ "Historic or Resort",
      districts == "RIVER" ~ "River",
      .default = "Not zoned"
    ),
    `Market Value per Acre` = as.numeric(TotalMarke) / Acreage
  ) %>% 
  group_by(`WASTE_SITE_BUFFER`, `Zoning Group`) %>%
  summarise(Acreage = sum(Acreage, na.rm = TRUE), .groups = 'drop') %>% 
  ggplot(aes(`WASTE_SITE_BUFFER`, Acreage)) +
  geom_bar(aes(fill = `Zoning Group`), 
           position = "fill", stat = "identity") +
  # geom_text(aes(label = Acreage, fill = `districts`, color = `districts`), 
  #           vjust = -0.5, size = 2.5, angle = 0, position = position_dodge(width = 0.9)) +
  scale_y_continuous(labels = label_number(suffix = "%", prefix = "", scale = 100)) +
  labs(y = "", x = "distance to debris site") +
  theme_minimal() + 
  ggtitle("Acreage near permitted Hurricane Helene debris sites by zoning group")


### Leaflet
map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = parcels_county %>%
                filter(districts != "NA" & WASTE_SITE_BUFFER == "More than 1 mile") %>%
                remove_dup_PINS() %>% 
                st_transform(4326),
              fillColor = "lightgrey", color = "black",
              weight = 1, opacity = 1, fillOpacity = 0.5,
              group = "Parcels beyond 1 mile") %>%
  addPolygons(data = parcels_county %>% 
                filter(WASTE_SITE_BUFFER == "1 mile") %>%
                remove_dup_PINS() %>% 
                st_transform(4326), 
              fillColor = "yellow", color = "black",
              weight = 1, opacity = 1, fillOpacity = 0.5,
              group = "Parcels within 1 mile") %>%
  addPolygons(data = parcels_county %>% 
                filter( WASTE_SITE_BUFFER == "0.5 mile") %>%
                remove_dup_PINS() %>% 
                st_transform(4326), 
              fillColor = "orange", color = "black",
              weight = 1, opacity = 1, fillOpacity = 0.5,
              group = "Parcels within 0.5 mile") %>%
  addPolygons(data = parcels_county %>% 
                filter(WASTE_SITE_BUFFER == "0.25 mile") %>%
                remove_dup_PINS() %>% 
                st_transform(4326), 
              fillColor = "red", color = "black",
              weight = 1, opacity = 1, fillOpacity = 0.5,
              group = "Parcels within 0.25 mile") %>%
  addMarkers(data = waste_sites %>%
                st_transform(4326),
              group = "Debris sites") %>%
  addLayersControl(
    overlayGroups = c("Parcels beyond 1 mile","Parcels within 1 mile",
                      "Parcels within 0.5 mile","Parcels within 0.25 mile",
                      "Debris sites"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  setView(lng = mean(st_coordinates(waste_sites %>% st_transform(4326))[,1]), 
          lat = mean(st_coordinates(waste_sites %>% st_transform(4326))[,2]), 
          zoom = 12)

r# save map
library(htmlwidgets)
saveWidget(map, "asheville_helene_debris_sites.html", selfcontained = TRUE)

# save parcels
output_path = ".\\shp\\asheville_ej_parcels.shp"
st_write(parcels_county %>% remove_dup_PINS(), output_path, delete_dsn = TRUE)

