# ##################################################################
# タイとASEAN重ね書き地図だよーん
# 2022年6月27日　
# 宇都宮　譲
# （長崎大学経済学部）
# めんどうくさいから、ぜんぶコメントは日本語だよーん
# ##################################################################
# ライブラリを読み込む
library(tidyverse)
library(sf)
library(khroma)
library(viridis)
library(ggimage)
library(ggrepel)
library(tmap)
library(tmaptools)

# ASEANと周辺国地図をつくる
# シェープファイルを読み込む
# シェープファイルがあるフォルダ内にあるファイルリスト取得
file_names_shape_0 <- 
  data_frame(
    file_names = list.files(
      "../shapefiles/shapefiles_asean+4_merged"
      )
    ) %>% 
  # 国境線レベルのシェープファイルだけを取り出す
  # 末尾が0であるファイルが該当する
  dplyr::filter(
    # stringr::str_detect()は大変使い勝手がよい関数だす
    stringr::str_detect(
      .$file_names, 
      "0.shp"
      )
    ) %>% 
  # 連続読込用にパスとなる文字列を作成
  dplyr::mutate(
    file_names = paste0(
      "../shapefiles/shapefiles_asean+4_merged/", 
      file_names
      )
    )
# シェープファイル連続読込
asean_4_boundary <- 
  file_names_shape_0 %>% 
  # purrr:map()はとっても便利な繰り返し処理用関数だす
  dplyr::mutate(
    # map_dfr()はデータフレームを生成する
    # Listでもらっても大変だから、はじめからデータフレームを
    # 返してもらえば便利～
    boundary_map =   purrr::map_dfr(
      .x = file_names,
      ~
        sf::read_sf(.)
      )
    ) %>% 
  # 返ってきたデータフレームから取り出す
  # map_dfr()が返した結果は、そのままだと直接変数指定できない。
  # 変数として再度引っ張り出さないといけない
  dplyr::mutate(
    # dplyr::pull()がとっても役立つよ。
    # varで列を指定。左側から1、2、と指定。
    country = dplyr::pull(.$boundary_map, var = 1),
    ID_0 = dplyr::pull(.$boundary_map, var = 2),
    geometry = dplyr::pull(.$boundary_map, var = 3)
  ) %>% 
  # 使わない変数を除く
  dplyr::select(-file_names, -boundary_map) %>% 
  # 都県毎に中心座標を取得する
  # 都県名を配置するため
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
# 作図だよ
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
  # 都県名を配置する
  ggrepel::geom_text_repel(
    # タイだけ取り出す
    # タイを塗りつぶさないといけないからねぇ。
    data = dplyr::filter(
      asean_4_boundary, 
      country == "Thailand"
      ),
    aes(x = lon, y = lat, label = country), 
    # 黒で塗りつぶす。カラーにする必要ないですな。
    colour = "black",
    nudge_x = -20,
    nudge_y = -25,
    size = 8
    ) + 
  # 太平洋と書きましょう。
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
# タイ都県別地図だす
Thailand_boundary_province <- 
  # シェープファイルを読み込む
  sf::st_read("../shapefiles/shapefiles_asean+4_separated/gadm40_THA_shp/gadm40_THA_1.shp") %>% 
  # 使う変数だけ選ぶ
  # シェープファイルには、使わない変数が含まれることがけっこうある
  dplyr::select(
    COUNTRY, 
    NAME_1, 
    NL_NAME_1, 
    geometry
    ) %>% 
  # タイ語県名から、タイ語で「県」と書いてある文字列だけを削除する
  # 地図に書き込む予定だった。が、やめておいたほうがよさそう。
  dplyr::mutate(
    NL_NAME_1 = stringr::str_replace_all(
      .$NL_NAME_1, 
      "จังหวัด",
      ""
      )
  ) %>% 
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
# タイ地図描画
Thailand_boundary_province_map <- 
  Thailand_boundary_province %>% 
  ggplot2::ggplot() +
  geom_sf(
    fill = "transparent"
  ) +
  # 県名書き込む
  ggrepel::geom_text_repel(
    aes(x = lon, y = lat, label = NAME_1), 
    colour = "black",
    size = 2
  ) +
  # ASEAN＋5地図書き込む領域周辺に線をひく
  # そのほうがかっこういいから。
  geom_segment(x = 102, y = 11.25, xend = 106, yend = 11.25, size = 0.5) + 
  geom_segment(x = 102, y = 11.25, xend = 102, yend = 7, size = 0.5) + 
  geom_segment(x = 102, y = 7, xend = 103, yend = 6.5, size = 0.5) + 
  geom_segment(x = 103, y = 6.5, xend = 106, yend = 6.5, size = 0.5) + 
  labs(x = "Longitude", y = "Latitude") +
  # 縮尺
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
# 保存
# いつものggsave()は使えなさそう
# 高解像度なpngファイルにしておけば、毎度コンパイルしなくて済む
# ただし、高解像度(2,400dpi)は本番のみ。日頃は200dpiで十分
png(
  filename = "Thailand_boundary_province_map.png",
  width = 300,
  height = 300,
  units = "mm",
  res = 240     # ここで解像度を調整する
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
# おしまい

