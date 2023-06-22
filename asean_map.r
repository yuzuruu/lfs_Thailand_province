# ##################################################################
# Map of Thailand and ASEAN
# Rev. 22nd. June 2023
# Yuzuru Utsunomiya, Ph. D.
# Faculty of Economics, Nagaski University
# ##################################################################
# read library
library(tidyverse)
library(sf)
library(khroma)
library(viridis)
library(ggimage)
library(ggrepel)

# make map of ASEAN and surroundings
# read shapefiles
# obtain file list by country
file_names_shape_0 <- 
  dplyr::tibble(
    file_names = list.files(
      "../shapefiles/shapefiles_asean+4_merged"
      )
    ) %>% 
  # administrative boundaries of countries
  # tail No.: 0
  dplyr::filter(
    stringr::str_detect(
      .$file_names, 
      "0.shp"
      )
    ) %>% 
  # for reading files continuously
  dplyr::mutate(
    file_names = paste0(
      "../shapefiles/shapefiles_asean+4_merged/", 
      file_names
      )
    )
# read shapefiles
asean_4_boundary <- 
  file_names_shape_0 %>% 
  dplyr::mutate(
    boundary_map =   purrr::map_dfr(
      .x = file_names,
      ~
        sf::read_sf(.)
      )
    ) %>% 
  dplyr::mutate(
    country = dplyr::pull(.$boundary_map, var = 1),
    ID_0 = dplyr::pull(.$boundary_map, var = 2),
    geometry = dplyr::pull(.$boundary_map, var = 3)
  ) %>% 
  dplyr::select(-file_names, -boundary_map) %>% 
  # obtain centroid of provinces
  # We use them to place provinces' names.
  dplyr::mutate(
    lon = purrr::map_dbl(
      .x = geometry,
      ~
        sf::st_coordinates(sf::st_centroid(.))[,1]
    ),
    lat = purrr::map_dbl(
      .x = geometry,
      ~
        sf::st_coordinates(sf::st_centroid(.))[,2]
    )
    
  ) 
# plot
asean_4_boundary_map <- 
  asean_4_boundary %>% 
  ggplot(
    aes(
      geometry = geometry
      )
    ) + 
  geom_sf(
    fill = "transparent",
    color = "black"
    ) +
  geom_sf(
    data = dplyr::filter(asean_4_boundary, country == "Thailand"), 
    aes(
      geometry = geometry
    ),
    fill = "black",
    color = "black",
    size = 0.5
  ) +
  # place provinces' name
  ggrepel::geom_text_repel(
    # highlight Thailand
    data = dplyr::filter(
      asean_4_boundary, 
      country == "Thailand"
      ),
    aes(x = lon, y = lat, label = country), 
    # Paint in black
    colour = "black",
    nudge_x = -20,
    nudge_y = -25,
    size = 8
    ) + 
  # place ocean's name
  annotate("text", 
           label = "Pacific \n ocean", 
           x = 135, 
           y = 15, 
           size = 5, 
           colour = "black"
  ) +
  # 南シナ海と書きましょう。
  annotate("text", 
           label = "South \n China \n sea", 
           x = 115, 
           y = 15, 
           size = 3, 
           colour = "black"
  ) +
  lims(x = c(85, 145), y = c(-15, 45)) +
  theme_void()
# Province-level map of Thailand
Thailand_boundary_province <- 
  # read shapefiles
  sf::st_read("../shapefiles/shapefiles_asean+4_separated/gadm40_THA_shp/gadm40_THA_1.shp") %>% 
  # select necessary variables
  dplyr::select(
    COUNTRY, 
    NAME_1, 
    NL_NAME_1, 
    geometry
    ) %>% 
  # dplyr::mutate(
  #   NL_NAME_1 = stringr::str_replace_all(
  #     .$NL_NAME_1, 
  #     "จังหวัด",
  #     ""
  #     )
  # ) %>% 
  dplyr::mutate(
    lon = purrr::map_dbl(
      .x = geometry,
      ~
        sf::st_coordinates(sf::st_centroid(.))[,1]
    ),
    lat = purrr::map_dbl(
      .x = geometry,
      ~
        sf::st_coordinates(sf::st_centroid(.))[,2]
    )
  )
# plot
Thailand_boundary_province_map <- 
  Thailand_boundary_province %>% 
  ggplot2::ggplot() +
  geom_sf(
    fill = "transparent"
  ) +
  # add provinces' names
  ggrepel::geom_text_repel(
    aes(x = lon, y = lat, label = NAME_1), 
    colour = "black",
    size = 2
  ) +
  # separate ASEAN＋5 and Thailand
  geom_segment(x = 102, y = 11.25, xend = 106, yend = 11.25, size = 0.5) + 
  geom_segment(x = 102, y = 11.25, xend = 102, yend = 7, size = 0.5) + 
  geom_segment(x = 102, y = 7, xend = 103, yend = 6.5, size = 0.5) + 
  geom_segment(x = 103, y = 6.5, xend = 106, yend = 6.5, size = 0.5) + 
  labs(x = "Longitude", y = "Latitude") +
  # add scale bar
  ggsn::scalebar(
    x.min = 104,
    x.max = 106.0,
    y.min = 5.8,
    y.max = 6.5,
    dist_unit = "km",
    dist = 150, 
    st.size = 5,
    st.dist = 0.5,
    height = 0.1, # height of scalebar
    model = "WGS84",
    transform = TRUE,
    location = "bottomright",
    box.fill = c("grey30", "white"), 
    box.color = "grey28",
    st.color = "grey28"
    ) +
    theme_minimal()
# https://dichika.hateblo.jp/entry/20110116/1295183973
# save
# resolution
# 240: daily use
# 2400: for paper
png(
  filename = "Thailand_boundary_province_map.png",
  width = 300,
  height = 300,
  units = "mm",
  res = 240     # adjust resolution here
)
print(Thailand_boundary_province_map)
print(
  asean_4_boundary_map,
  vp = grid::viewport(
    width = 0.25, 
    height = 0.25, 
    x = 0.675, 
    y = 0.275
    )
)
dev.off()
# 
