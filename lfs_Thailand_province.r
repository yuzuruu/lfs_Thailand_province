##########################################################################
# Labour force survey Thailand by Province
# Yuzuru Utsunomiya
# First make: 21st. February 2022
# Revised: 
# In detail of the revised history, refer to my Github

### ----- read.library.province -----
# read necessary library
library(tidyverse)
library(viridis)
library(khroma)
library(cmdstanr)
library(bayesplot)
library(patchwork)
library(ggridges)
library(gt)
library(vroom)
library(rsample)
# set concurrent computing plan
# multisession: use CPUs as many as possible
library(furrr)
plan(multisession)
#
### ----- read.data.province -----
# read.data
# male
clf_male <- 
  readxl::read_excel(
    "lfs_province_2022.xlsx",
    sheet = "male",
    range = "A1:CB98",
    col_names = TRUE
  ) %>% 
  tidyr::pivot_longer(
    cols = c(
      -status, 
      -year_month, 
      -gender
      ),
    names_to = "province", 
    values_to = "number"
  )
# female
clf_female <- 
  readxl::read_excel(
    "lfs_province_2022.xlsx",
    sheet = "female",
    range = "A1:CB98",
    col_names = TRUE
  ) %>% 
  tidyr::pivot_longer(
    cols = c(
      -status, 
      -year_month, 
      -gender
    ),
    names_to = "province", 
    values_to = "number"
  )
# combine the both data above
clf_fm <- 
  clf_female %>% 
  dplyr::bind_rows(clf_male) %>% 
  dplyr::mutate(
    dplyr::across(
      .cols = c(status, gender, province),
      .fns = factor
    ),
    number = as.numeric(number)
  )
# save the combined data
readr::write_excel_csv(
  clf_fm, 
  "clf_fm.csv"
  )
# plot the results for description
clf_fm_line <-
  clf_fm %>%
  ggplot2::ggplot(
   aes(
     x = year_month,
     y = number,
     colour = gender
   )
  ) +
  geom_line() +
  scale_color_okabeito() +
  labs(x = "Year (Q1/1994-Q4-2020)", y = "N. of current labour force (Unit: 1,000 persons)")+
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )+
  facet_wrap(~ province, scales = "free_y")
# save the line plot 
ggsave(
  "clf_fm_line.pdf",
  plot = clf_fm_line,
  width = 500,
  height = 500,
  units = "mm"
)
# 
# make a whole-kingdom line plot using aggregate data
# NOTE
# The plotting results might be different from aggregated by NSO.
# In particular, befor 2002.
clf_aggregate_line <- 
  # sum up the clf by quarter
  clf_fm %>% 
  dplyr::group_by(
    status, 
    year_month
    ) %>% 
  dplyr::summarise(
    N = sum(
      number, 
      na.rm = TRUE
      )
    ) %>%
  # replace unreliable numbers into NA
  dplyr::mutate(
    N = replace(
      N, 
      N < 22000000, 
      NA
      )
    ) %>% 
  # plot the figure
  ggplot2::ggplot(
    aes(
      x = year_month,
      y = N
    )
  ) +
  geom_line() +
  geom_point() + 
  labs(x = "Year", y = "N. of current labor force (Unit: person)") +
  theme_classic() 
ggsave(
  "clf_aggregate_line.pdf",
  plot = clf_aggregate_line,
  width = 500,
  height = 500,
  units = "mm"
)
# 
# ### ---- clf.fm.line.original ----
# clf_fm_line_province_original <- 
#   clf_fm %>%
#   group_by(province) %>% 
#   nest() %>% 
#   dplyr::mutate(
#     line = purrr::map(
#       data,
#       ~
#         ggplot2::ggplot(
#           data = .,
#           aes(
#             x = year_month,
#             y = number,
#             colour = gender
#           ) 
#         ) +
#         geom_line() +
#         geom_point() +
#         labs(
#           title = province,
#           x = "Year",
#           y = "N. of current labour force (Unit: person)"
#         ) +
#         scale_color_okabeito() +
#         theme_classic() +
#         theme(
#           legend.position = "bottom"
#         )
#     )
#   )
# # save the figure
# # The object includes 77 provinces and it takes 
# # a few minutes.
# pdf("clf_fm_line_province_original.pdf")
# clf_fm_line_province_original$line  
# dev.off()  
# #
#

### ---- state.space.01 ----
# make a dataset
# by summing up
clf_fm_selected <-
  clf_fm %>%
  dplyr::group_by(status, year_month, province) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::ungroup() %>%
  # select necessary province and gender for trial
  # For trial, select some (4 or 5 in maximum) province to save computation period
  # dplyr::filter(
  #   province %in%
  #     c("Amnat_Charoen", "Chiang_Mai", "Kalasin", "Bangkok_Metropolis")
  #   ) %>%
  dplyr::select(
    year_month,  province, number#, gender
  ) %>%
  tidyr::pivot_wider(
    names_from = province,
    values_from = number
  )
# # Trans the data set and make it matrix
# Y <-
#   clf_fm_selected %>%
#   dplyr::select(-year_month) %>%
#   as.matrix(.) %>%
#   t(.)
# # Pick up non-NA data
# ypos <- Y[!is.na(Y)]
# # count the N. of non-NA data
# n_pos <- length(ypos)  # number on non-NA ys
# # obtain indices of the non-NA data
# indx_pos <- which(!is.na(Y), arr.ind = TRUE)  # index on the non-NAs
# col_indx_pos <- as.vector(indx_pos[, "col"])
# row_indx_pos <- as.vector(indx_pos[, "row"])
# # make a list data for stan
# data_volume <-
#   list(
#     y = ypos,
#     TT = ncol(Y),
#     N = nrow(Y),
#     n_pos = n_pos,
#     col_indx_pos = col_indx_pos,
#     row_indx_pos = row_indx_pos
#   )
#
# compile the stan model
# NOTE
# model list
# model_clf_01 (lfs_Thailand_province_01.stan):
# local level with change point detection using cauchy distribution
# model_clf_02:
#
#
# model_clf_01 <-
#   cmdstan_model(
#     # NOTE
#     # write the stan model separately
#     stan_file = "lfs_Thailand_province_01.stan",
#     compile = TRUE
#   )
# fit the stan model
# Here needs some computation periods.
# NOTE
# For trial, adjust iteration / warmup number to save computation periods.
# fit_clf_01 <-
#   model_clf_01$sample(
#     data_volume,
#     # max_treedepth = 20,
#     # adapt_delta = 0.9,
#     iter_warmup = 1000,    # here!
#     iter_sampling = 1000,  # here!
#     chains = 4,
#     parallel_chains = 4,
#     refresh = 200
#   )
# save the results
# NOTE
# To read, use base::readRDS()
#
# 計算結果を保存
# fit_clf_01$save_object(file = "fit_clf_01.rds")
# 保存した計算結果を読み込む
# fit_clf_01 <- readRDS("fit_clf_01.rds")
# save the summary statistics
# fit_clf_01_summary <- fit_clf_01$summary()
# write_excel_csv(fit_clf_01_summary, "fit_clf_01_summary.csv")
# saveRDS(fit_clf_01_summary, "fit_clf_01_summary.rds")
# fit_clf_01_summary_00 <- readRDS("fit_clf_01_summary.rds")

# ### ---- results.summary ----
# 折れ線グラフをつくるよ
# 都県別推定値と実測値
# 実測値；Yに入ってる
# 推定値：
# yhat：実測値とセットで
# x：実測値とセットで
# pro_dev：都県別に色を変えつつ1枚で結構
# u：密度プロットがあればいいです。summaryは使えませぬな。サンプルを取り出さないと。
# 作図用推論結果データ作成
# データ作成に数分かかる。
# fit_clf_01_summary <-
#   fit_clf_01$summary(
#     NULL,
#     mean,
#     median,
#     sd,
#     ~ quantile(.x, probs = c(0.025, 0.975))
#     ) %>%
#   data.table::setnames(
#     c("variable","mean", "median", "sd", "ll","ul")
#   )
# write_excel_csv(fit_clf_01_summary, "fit_clf_01_summary.csv")
# # 必要な変数を取り出す。
# # 前段でつくった、処理に時間がかかるデータセットを使うから注意。
# # 必要な変数=状態xと推定したyhat、それから都県・四半期毎変動prodev
# fit_clf_01_summary_x_yhat_pd <-
#   fit_clf_01_summary %>%
#   dplyr::filter(
#     # 必要な変数に関する結果のみを取り出す
#     stringr::str_detect(variable, "(^yhat[:punct:]|^x[:punct:]|^(pro_dev)[:punct:])")
#   ) %>%
#   # pro_dev -> prodevにする。
#   # 後々ジャマになりがちなアンダースコアを取り除く
#   dplyr::mutate(
#     variable = stringr::str_replace(variable, pattern = "_", replacement = "")
#   ) %>%
#   # https://stackoverflow.com/questions/23518325/how-to-extract-substring-between-patterns-and-in-r
#   dplyr::mutate(
#     parameter = str_extract(variable, "(.+)(?=\\[)"),
#     quarter = as.numeric(str_extract(variable, "(?<=[:punct:])(.+)(?=\\,)")),
#     province_id = as.numeric(str_extract(variable, "(?<=,)(.+)(?=\\])"))
#   ) %>%
#   dplyr::mutate(
#     year_month = lubridate::ymd(
#       dplyr::case_when(
#         quarter == 1 ~ as.character(clf_fm_selected$year_month[1]),
#         quarter == 2 ~ as.character(clf_fm_selected$year_month[2]),
#         quarter == 3 ~ as.character(clf_fm_selected$year_month[3]),
#         quarter == 4 ~ as.character(clf_fm_selected$year_month[4]),
#         quarter == 5 ~ as.character(clf_fm_selected$year_month[5]),
#         quarter == 6 ~ as.character(clf_fm_selected$year_month[6]),
#         quarter == 7 ~ as.character(clf_fm_selected$year_month[7]),
#         quarter == 8 ~ as.character(clf_fm_selected$year_month[8]),
#         quarter == 9 ~ as.character(clf_fm_selected$year_month[9]),
#         quarter == 10 ~ as.character(clf_fm_selected$year_month[10]),
#         quarter == 11 ~ as.character(clf_fm_selected$year_month[11]),
#         quarter == 12 ~ as.character(clf_fm_selected$year_month[12]),
#         quarter == 13 ~ as.character(clf_fm_selected$year_month[13]),
#         quarter == 14 ~ as.character(clf_fm_selected$year_month[14]),
#         quarter == 15 ~ as.character(clf_fm_selected$year_month[15]),
#         quarter == 16 ~ as.character(clf_fm_selected$year_month[16]),
#         quarter == 17 ~ as.character(clf_fm_selected$year_month[17]),
#         quarter == 18 ~ as.character(clf_fm_selected$year_month[18]),
#         quarter == 19 ~ as.character(clf_fm_selected$year_month[19]),
#         quarter == 20 ~ as.character(clf_fm_selected$year_month[20]),
#         quarter == 21 ~ as.character(clf_fm_selected$year_month[21]),
#         quarter == 22 ~ as.character(clf_fm_selected$year_month[22]),
#         quarter == 23 ~ as.character(clf_fm_selected$year_month[23]),
#         quarter == 24 ~ as.character(clf_fm_selected$year_month[24]),
#         quarter == 25 ~ as.character(clf_fm_selected$year_month[25]),
#         quarter == 26 ~ as.character(clf_fm_selected$year_month[26]),
#         quarter == 27 ~ as.character(clf_fm_selected$year_month[27]),
#         quarter == 28 ~ as.character(clf_fm_selected$year_month[28]),
#         quarter == 29 ~ as.character(clf_fm_selected$year_month[29]),
#         quarter == 30 ~ as.character(clf_fm_selected$year_month[30]),
#         quarter == 31 ~ as.character(clf_fm_selected$year_month[31]),
#         quarter == 32 ~ as.character(clf_fm_selected$year_month[32]),
#         quarter == 33 ~ as.character(clf_fm_selected$year_month[33]),
#         quarter == 34 ~ as.character(clf_fm_selected$year_month[34]),
#         quarter == 35 ~ as.character(clf_fm_selected$year_month[35]),
#         quarter == 36 ~ as.character(clf_fm_selected$year_month[36]),
#         quarter == 37 ~ as.character(clf_fm_selected$year_month[37]),
#         quarter == 38 ~ as.character(clf_fm_selected$year_month[38]),
#         quarter == 39 ~ as.character(clf_fm_selected$year_month[39]),
#         quarter == 40 ~ as.character(clf_fm_selected$year_month[40]),
#         quarter == 41 ~ as.character(clf_fm_selected$year_month[41]),
#         quarter == 42 ~ as.character(clf_fm_selected$year_month[42]),
#         quarter == 43 ~ as.character(clf_fm_selected$year_month[43]),
#         quarter == 44 ~ as.character(clf_fm_selected$year_month[44]),
#         quarter == 45 ~ as.character(clf_fm_selected$year_month[45]),
#         quarter == 46 ~ as.character(clf_fm_selected$year_month[46]),
#         quarter == 47 ~ as.character(clf_fm_selected$year_month[47]),
#         quarter == 48 ~ as.character(clf_fm_selected$year_month[48]),
#         quarter == 49 ~ as.character(clf_fm_selected$year_month[49]),
#         quarter == 50 ~ as.character(clf_fm_selected$year_month[50]),
#         quarter == 51 ~ as.character(clf_fm_selected$year_month[51]),
#         quarter == 52 ~ as.character(clf_fm_selected$year_month[52]),
#         quarter == 53 ~ as.character(clf_fm_selected$year_month[53]),
#         quarter == 54 ~ as.character(clf_fm_selected$year_month[54]),
#         quarter == 55 ~ as.character(clf_fm_selected$year_month[55]),
#         quarter == 56 ~ as.character(clf_fm_selected$year_month[56]),
#         quarter == 57 ~ as.character(clf_fm_selected$year_month[57]),
#         quarter == 58 ~ as.character(clf_fm_selected$year_month[58]),
#         quarter == 59 ~ as.character(clf_fm_selected$year_month[59]),
#         quarter == 60 ~ as.character(clf_fm_selected$year_month[60]),
#         quarter == 61 ~ as.character(clf_fm_selected$year_month[61]),
#         quarter == 62 ~ as.character(clf_fm_selected$year_month[62]),
#         quarter == 63 ~ as.character(clf_fm_selected$year_month[63]),
#         quarter == 64 ~ as.character(clf_fm_selected$year_month[64]),
#         quarter == 65 ~ as.character(clf_fm_selected$year_month[65]),
#         quarter == 66 ~ as.character(clf_fm_selected$year_month[66]),
#         quarter == 67 ~ as.character(clf_fm_selected$year_month[67]),
#         quarter == 68 ~ as.character(clf_fm_selected$year_month[68]),
#         quarter == 69 ~ as.character(clf_fm_selected$year_month[69]),
#         quarter == 70 ~ as.character(clf_fm_selected$year_month[70]),
#         quarter == 71 ~ as.character(clf_fm_selected$year_month[71]),
#         quarter == 72 ~ as.character(clf_fm_selected$year_month[72]),
#         quarter == 73 ~ as.character(clf_fm_selected$year_month[73]),
#         quarter == 74 ~ as.character(clf_fm_selected$year_month[74]),
#         quarter == 75 ~ as.character(clf_fm_selected$year_month[75]),
#         quarter == 76 ~ as.character(clf_fm_selected$year_month[76]),
#         quarter == 77 ~ as.character(clf_fm_selected$year_month[77]),
#         quarter == 78 ~ as.character(clf_fm_selected$year_month[78]),
#         quarter == 79 ~ as.character(clf_fm_selected$year_month[79]),
#         quarter == 80 ~ as.character(clf_fm_selected$year_month[80]),
#         quarter == 81 ~ as.character(clf_fm_selected$year_month[81]),
#         quarter == 82 ~ as.character(clf_fm_selected$year_month[82]),
#         quarter == 83 ~ as.character(clf_fm_selected$year_month[83]),
#         quarter == 84 ~ as.character(clf_fm_selected$year_month[84]),
#         quarter == 85 ~ as.character(clf_fm_selected$year_month[85]),
#         quarter == 86 ~ as.character(clf_fm_selected$year_month[86]),
#         quarter == 87 ~ as.character(clf_fm_selected$year_month[87]),
#         quarter == 88 ~ as.character(clf_fm_selected$year_month[88]),
#         quarter == 89 ~ as.character(clf_fm_selected$year_month[89]),
#         quarter == 90 ~ as.character(clf_fm_selected$year_month[90]),
#         quarter == 91 ~ as.character(clf_fm_selected$year_month[91]),
#         quarter == 92 ~ as.character(clf_fm_selected$year_month[92]),
#         quarter == 93 ~ as.character(clf_fm_selected$year_month[93]),
#         quarter == 94 ~ as.character(clf_fm_selected$year_month[94]),
#         quarter == 95 ~ as.character(clf_fm_selected$year_month[95]),
#         quarter == 96 ~ as.character(clf_fm_selected$year_month[96]),
#         quarter == 97 ~ as.character(clf_fm_selected$year_month[97]),
#         TRUE ~ "hoge"
#         )
#       ),
#     province = dplyr::case_when(
#       province_id == 1 ~ as.character(rownames(Y)[1]),
#       province_id == 2 ~ as.character(rownames(Y)[2]),
#       province_id == 3 ~ as.character(rownames(Y)[3]),
#       province_id == 4 ~ as.character(rownames(Y)[4]),
#       province_id == 5 ~ as.character(rownames(Y)[5]),
#       province_id == 6 ~ as.character(rownames(Y)[6]),
#       province_id == 7 ~ as.character(rownames(Y)[7]),
#       province_id == 8 ~ as.character(rownames(Y)[8]),
#       province_id == 9 ~ as.character(rownames(Y)[9]),
#       province_id == 10 ~ as.character(rownames(Y)[10]),
#       province_id == 11 ~ as.character(rownames(Y)[11]),
#       province_id == 12 ~ as.character(rownames(Y)[12]),
#       province_id == 13 ~ as.character(rownames(Y)[13]),
#       province_id == 14 ~ as.character(rownames(Y)[14]),
#       province_id == 15 ~ as.character(rownames(Y)[15]),
#       province_id == 16 ~ as.character(rownames(Y)[16]),
#       province_id == 17 ~ as.character(rownames(Y)[17]),
#       province_id == 18 ~ as.character(rownames(Y)[18]),
#       province_id == 19 ~ as.character(rownames(Y)[19]),
#       province_id == 20 ~ as.character(rownames(Y)[20]),
#       province_id == 21 ~ as.character(rownames(Y)[21]),
#       province_id == 22 ~ as.character(rownames(Y)[22]),
#       province_id == 23 ~ as.character(rownames(Y)[23]),
#       province_id == 24 ~ as.character(rownames(Y)[24]),
#       province_id == 25 ~ as.character(rownames(Y)[25]),
#       province_id == 26 ~ as.character(rownames(Y)[26]),
#       province_id == 27 ~ as.character(rownames(Y)[27]),
#       province_id == 28 ~ as.character(rownames(Y)[28]),
#       province_id == 29 ~ as.character(rownames(Y)[29]),
#       province_id == 30 ~ as.character(rownames(Y)[30]),
#       province_id == 31 ~ as.character(rownames(Y)[31]),
#       province_id == 32 ~ as.character(rownames(Y)[32]),
#       province_id == 33 ~ as.character(rownames(Y)[33]),
#       province_id == 34 ~ as.character(rownames(Y)[34]),
#       province_id == 35 ~ as.character(rownames(Y)[35]),
#       province_id == 36 ~ as.character(rownames(Y)[36]),
#       province_id == 37 ~ as.character(rownames(Y)[37]),
#       province_id == 38 ~ as.character(rownames(Y)[38]),
#       province_id == 39 ~ as.character(rownames(Y)[39]),
#       province_id == 40 ~ as.character(rownames(Y)[40]),
#       province_id == 41 ~ as.character(rownames(Y)[41]),
#       province_id == 42 ~ as.character(rownames(Y)[42]),
#       province_id == 43 ~ as.character(rownames(Y)[43]),
#       province_id == 44 ~ as.character(rownames(Y)[44]),
#       province_id == 45 ~ as.character(rownames(Y)[45]),
#       province_id == 46 ~ as.character(rownames(Y)[46]),
#       province_id == 47 ~ as.character(rownames(Y)[47]),
#       province_id == 48 ~ as.character(rownames(Y)[48]),
#       province_id == 49 ~ as.character(rownames(Y)[49]),
#       province_id == 50 ~ as.character(rownames(Y)[50]),
#       province_id == 51 ~ as.character(rownames(Y)[51]),
#       province_id == 52 ~ as.character(rownames(Y)[52]),
#       province_id == 53 ~ as.character(rownames(Y)[53]),
#       province_id == 54 ~ as.character(rownames(Y)[54]),
#       province_id == 55 ~ as.character(rownames(Y)[55]),
#       province_id == 56 ~ as.character(rownames(Y)[56]),
#       province_id == 57 ~ as.character(rownames(Y)[57]),
#       province_id == 58 ~ as.character(rownames(Y)[58]),
#       province_id == 59 ~ as.character(rownames(Y)[59]),
#       province_id == 60 ~ as.character(rownames(Y)[60]),
#       province_id == 61 ~ as.character(rownames(Y)[61]),
#       province_id == 62 ~ as.character(rownames(Y)[62]),
#       province_id == 63 ~ as.character(rownames(Y)[63]),
#       province_id == 64 ~ as.character(rownames(Y)[64]),
#       province_id == 65 ~ as.character(rownames(Y)[65]),
#       province_id == 66 ~ as.character(rownames(Y)[66]),
#       province_id == 67 ~ as.character(rownames(Y)[67]),
#       province_id == 68 ~ as.character(rownames(Y)[68]),
#       province_id == 69 ~ as.character(rownames(Y)[69]),
#       province_id == 70 ~ as.character(rownames(Y)[70]),
#       province_id == 71 ~ as.character(rownames(Y)[71]),
#       province_id == 72 ~ as.character(rownames(Y)[72]),
#       province_id == 73 ~ as.character(rownames(Y)[73]),
#       province_id == 74 ~ as.character(rownames(Y)[74]),
#       province_id == 75 ~ as.character(rownames(Y)[75]),
#       province_id == 76 ~ as.character(rownames(Y)[76]),
#       province_id == 77 ~ as.character(rownames(Y)[77]),
#       province_id == 78 ~ as.character(rownames(Y)[78]),
#       province_id == 79 ~ as.character(rownames(Y)[79]),
#       province_id == 80 ~ as.character(rownames(Y)[80]),
#       province_id == 81 ~ as.character(rownames(Y)[81]),
#       province_id == 82 ~ as.character(rownames(Y)[82]),
#       province_id == 83 ~ as.character(rownames(Y)[83]),
#       province_id == 84 ~ as.character(rownames(Y)[84]),
#       province_id == 85 ~ as.character(rownames(Y)[85]),
#       province_id == 86 ~ as.character(rownames(Y)[86]),
#       province_id == 87 ~ as.character(rownames(Y)[87]),
#       province_id == 88 ~ as.character(rownames(Y)[88]),
#       province_id == 89 ~ as.character(rownames(Y)[89]),
#       province_id == 90 ~ as.character(rownames(Y)[90]),
#       province_id == 91 ~ as.character(rownames(Y)[91]),
#       province_id == 92 ~ as.character(rownames(Y)[92]),
#       province_id == 93 ~ as.character(rownames(Y)[93]),
#       province_id == 94 ~ as.character(rownames(Y)[94]),
#       province_id == 95 ~ as.character(rownames(Y)[95]),
#       province_id == 96 ~ as.character(rownames(Y)[96]),
#       province_id == 97 ~ as.character(rownames(Y)[97]),
#       TRUE ~ "hoge"
#       )
#     ) %>%
#   dplyr::select(-quarter, -province_id) %>%
#   dplyr::mutate(
#     parameter = factor(parameter),
#     province = factor(province),
#   )
# saveRDS(fit_clf_01_summary_x_yhat_pd,"fit_clf_01_summary_x_yhat_pd.rds")
# # 保存しておいたデータセットを読み込む
fit_clf_01_summary_x_yhat_pd <- 
  readRDS("fit_clf_01_summary_x_yhat_pd.rds")
# readr::write_excel_csv(fit_clf_01_summary_x_yhat_pd, "fit_clf_01_summary_x_yhat_pd.csv")
# 作図する
# 状態x変化を、都県毎に描く。95%CIつき!
# line_fit_clf_01_summary_x <- 
#   fit_clf_01_summary_x_yhat_pd %>% 
#   dplyr::filter(
#     parameter == "x" 
#   ) %>% 
#   ggplot2::ggplot(
#     aes(
#       x = year_month,
#       y = median,
#       color = province
#     )
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = ll,
#       ymax = ul,
#     ),
#     fill = "grey",
#     colour = "transparent"
#   ) +
#   geom_line() +
#   geom_point() +
#   labs(
#     x = "Year (Q1/1994 - Q4/2020)",
#     y = "Human resource abundance (Unit: persons)",
#     title = "Human resource abundance",
#     subtitle = "Line indicates estimated abundance of human resource. Areas filled in grey indicate 95 percent credible intervals."
#   ) +
#   scale_color_viridis(discrete = TRUE) +
#   facet_wrap(~ province, scales = "free_y") +
#   theme_classic() +
#   theme(
#     legend.position = "none",
#     strip.background = element_blank()
#   )
# line_fit_clf_01_summary_x
# save results
# enlarge plot area up to 500mm * 500mm
# ggsave(
#   "line_fit_clf_01_summary_x.pdf", 
#   plot = line_fit_clf_01_summary_x, 
#   width = 500, 
#   height = 500, 
#   units = "mm"
#   )
# # prodev
# line_fit_clf_01_summary_prodev <- 
#   fit_clf_01_summary_x_yhat_pd %>% 
#   dplyr::filter(
#     parameter == "prodev" 
#   ) %>% 
#   ggplot2::ggplot(
#     aes(
#       x = year_month,
#       y = median,
#       color = province
#     )
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = ll,
#       ymax = ul,
#     ),
#     fill = "grey",
#     colour = "transparent"
#   ) +
#   geom_line() +
#   geom_point() +
#   labs(
#     x = "Year (Q1/1994 - Q4/2020)",
#     y = "Deviance by province (Unit: persons)",
#     title = "Deviance by province",
#     subtitle = "Line indicates estimated deviance. Areas filled in grey indicate 95 percent credible intervals."
#   ) +
#   scale_color_viridis(discrete = TRUE) +
#   facet_wrap(~ province, scales = "free_y") +
#   theme_classic() +
#   theme(
#     legend.position = "none",
#     strip.background = element_blank()
#   )
# line_fit_clf_01_summary_prodev
# ggsave(
#   "line_fit_clf_01_summary_prodev.pdf", 
#   plot = line_fit_clf_01_summary_prodev, 
#   width = 500, 
#   height = 500, 
#   units = "mm"
# )
# # yhat
# line_fit_clf_01_summary_yhat <- 
#   fit_clf_01_summary_x_yhat_pd %>% 
#   dplyr::filter(
#     parameter == "yhat" 
#   ) %>% 
#   ggplot2::ggplot(
#   aes(
#     x = year_month,
#     y = median,
#     color = province
#   )
# ) +
#   geom_ribbon(
#     aes(
#       ymin = ll,
#       ymax = ul,
#     ),
#     fill = "grey",
#     colour = "transparent"
#   ) +
#   geom_line() +
#   geom_point() +
#   labs(
#     x = "Year (Q1/1994 - Q4/2020)",
#     y = "Estimated Current Labour Force by province (Unit: persons)",
#     title = "Estimated Current Labour Force",
#     subtitle = "Line indicates estimated current labour force. Areas filled in grey indicate 95 percent credible intervals."
#   ) +
#   scale_color_viridis(discrete = TRUE) +
#   facet_wrap(~ province, scales = "free_y") +
#   theme_classic() +
#   theme(
#     legend.position = "none",
#     strip.background = element_blank()
#   )
# line_fit_clf_01_summary_yhat
# ggsave(
#   "line_fit_clf_01_summary_yhat.pdf", 
#   plot = line_fit_clf_01_summary_yhat, 
#   width = 500, 
#   height = 500, 
#   units = "mm"
# )
# Combinedだよーん
# remake the data
# x
data_x <- 
  fit_clf_01_summary_x_yhat_pd %>% 
  dplyr::filter(
    parameter == "x" 
  ) 
# yhat
data_yhat <- 
  fit_clf_01_summary_x_yhat_pd %>% 
  dplyr::filter(
    parameter == "yhat" 
  ) 
# Y
data_Y <- 
  clf_fm_selected %>% 
  tidyr::pivot_longer(
    cols = -c(year_month),
    names_to = "province",
    values_to = "number"
  ) %>% 
  dplyr::mutate(
    year_month = lubridate::date(year_month)
  )
# plot
line_fit_clf_01_summary_combined <- 
  data_x %>% 
  ggplot2::ggplot(
    aes(
      x = year_month,
      y = median,
      color = province
    )
  ) +
  geom_ribbon(
    aes(
      ymin = ll,
      ymax = ul,
    ),
    fill = "grey",
    colour = "transparent"
  ) +
  geom_line() +
  geom_point(
    data = data_yhat,
    aes(
      x = year_month,
      y = median,
      color = province
    ),
    shape = 21 # circle
  ) +
  geom_point(
    data = data_Y,
    aes(
      x = year_month,
      y = number,
      color = province
    ),
    shape = 15 # square
  ) +
  labs(
    x = "Year (Q1/1994 - Q4/2020)",
    y = "Observed CLF, estimated CLF, and HRA (Unit: persons)",
    title = "Observed current labour force (CLF), estimated Current Labour Force, and human resource abundance (HRA) by province",
    subtitle = "Line indicates the human resource abundance. Areas filled in grey indicate 95 percent credible intervals of the HRA. White circles indicate the estimated CLF, and colored squares indicate the observed CLF."
  ) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~ province, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )
# save the figure
ggsave(
  "line_fit_clf_01_summary_combined.pdf",
  plot = line_fit_clf_01_summary_combined,
  width = 500,
  height = 500,
  units = "mm"
)
#
# 観測ノイズと状態ノイズ密度プロットを描く
# 分析結果を読み込む
# fit_clf_01 <- readRDS("fit_clf_01.rds")
# # サンプルをデータフレーム形式で得る。
# # もともと行列形式であるものをデータフレームにしただけ。
# # なのでちっともtidyではないのよ
# fit_clf_01_sample <- 
#   fit_clf_01$draws(format = "df")
# # tidyなデータセットに整形する
# fit_clf_01_sample_df <- 
#   fit_clf_01_sample %>% 
#   tidyr::pivot_longer(
#     # 対象外にする列名はあらかじめ調べておく
#     cols = -c(".chain", ".iteration", ".draw"),
#     names_to = "variable",
#     values_to = "number"
#   ) %>% 
#   # 累積対数確率密度は使わないから外す
#   dplyr::filter(variable != "lp__") %>% 
#   dplyr::mutate(
#     variable = factor(variable)
#   ) %>% 
#   data.table::setnames(
#     c("chain","iteration","draw","variable","number")
#   )
# 作図する
# そのまえに必要なデータを取り出す
# 今回はs_x（状態ノイズ）とs_r（観測誤差）
# fit_clf_01_sample_df_sd <- 
#   fit_clf_01_sample_df %>% 
#   dplyr::filter(
#     stringr::str_detect(variable, "(^s[:punct:])")
#   ) %>% 
#   dplyr::mutate(
#     variable = stringr::str_replace(variable, pattern = "_", replacement = "")
#     ) %>%
#     dplyr::mutate(
#       parameter = str_extract(variable, "(.+)(?=\\[)"),
#       province_id = as.numeric(str_extract(variable, "(?<=[:punct:])(.+)(?=\\])"))
#     ) %>%
#   dplyr::mutate(
#     province = dplyr::case_when(
#       province_id == 1 ~ as.character(rownames(Y)[1]),
#       province_id == 2 ~ as.character(rownames(Y)[2]),
#       province_id == 3 ~ as.character(rownames(Y)[3]),
#       province_id == 4 ~ as.character(rownames(Y)[4]),
#       province_id == 5 ~ as.character(rownames(Y)[5]),
#       province_id == 6 ~ as.character(rownames(Y)[6]),
#       province_id == 7 ~ as.character(rownames(Y)[7]),
#       province_id == 8 ~ as.character(rownames(Y)[8]),
#       province_id == 9 ~ as.character(rownames(Y)[9]),
#       province_id == 10 ~ as.character(rownames(Y)[10]),
#       province_id == 11 ~ as.character(rownames(Y)[11]),
#       province_id == 12 ~ as.character(rownames(Y)[12]),
#       province_id == 13 ~ as.character(rownames(Y)[13]),
#       province_id == 14 ~ as.character(rownames(Y)[14]),
#       province_id == 15 ~ as.character(rownames(Y)[15]),
#       province_id == 16 ~ as.character(rownames(Y)[16]),
#       province_id == 17 ~ as.character(rownames(Y)[17]),
#       province_id == 18 ~ as.character(rownames(Y)[18]),
#       province_id == 19 ~ as.character(rownames(Y)[19]),
#       province_id == 20 ~ as.character(rownames(Y)[20]),
#       province_id == 21 ~ as.character(rownames(Y)[21]),
#       province_id == 22 ~ as.character(rownames(Y)[22]),
#       province_id == 23 ~ as.character(rownames(Y)[23]),
#       province_id == 24 ~ as.character(rownames(Y)[24]),
#       province_id == 25 ~ as.character(rownames(Y)[25]),
#       province_id == 26 ~ as.character(rownames(Y)[26]),
#       province_id == 27 ~ as.character(rownames(Y)[27]),
#       province_id == 28 ~ as.character(rownames(Y)[28]),
#       province_id == 29 ~ as.character(rownames(Y)[29]),
#       province_id == 30 ~ as.character(rownames(Y)[30]),
#       province_id == 31 ~ as.character(rownames(Y)[31]),
#       province_id == 32 ~ as.character(rownames(Y)[32]),
#       province_id == 33 ~ as.character(rownames(Y)[33]),
#       province_id == 34 ~ as.character(rownames(Y)[34]),
#       province_id == 35 ~ as.character(rownames(Y)[35]),
#       province_id == 36 ~ as.character(rownames(Y)[36]),
#       province_id == 37 ~ as.character(rownames(Y)[37]),
#       province_id == 38 ~ as.character(rownames(Y)[38]),
#       province_id == 39 ~ as.character(rownames(Y)[39]),
#       province_id == 40 ~ as.character(rownames(Y)[40]),
#       province_id == 41 ~ as.character(rownames(Y)[41]),
#       province_id == 42 ~ as.character(rownames(Y)[42]),
#       province_id == 43 ~ as.character(rownames(Y)[43]),
#       province_id == 44 ~ as.character(rownames(Y)[44]),
#       province_id == 45 ~ as.character(rownames(Y)[45]),
#       province_id == 46 ~ as.character(rownames(Y)[46]),
#       province_id == 47 ~ as.character(rownames(Y)[47]),
#       province_id == 48 ~ as.character(rownames(Y)[48]),
#       province_id == 49 ~ as.character(rownames(Y)[49]),
#       province_id == 50 ~ as.character(rownames(Y)[50]),
#       province_id == 51 ~ as.character(rownames(Y)[51]),
#       province_id == 52 ~ as.character(rownames(Y)[52]),
#       province_id == 53 ~ as.character(rownames(Y)[53]),
#       province_id == 54 ~ as.character(rownames(Y)[54]),
#       province_id == 55 ~ as.character(rownames(Y)[55]),
#       province_id == 56 ~ as.character(rownames(Y)[56]),
#       province_id == 57 ~ as.character(rownames(Y)[57]),
#       province_id == 58 ~ as.character(rownames(Y)[58]),
#       province_id == 59 ~ as.character(rownames(Y)[59]),
#       province_id == 60 ~ as.character(rownames(Y)[60]),
#       province_id == 61 ~ as.character(rownames(Y)[61]),
#       province_id == 62 ~ as.character(rownames(Y)[62]),
#       province_id == 63 ~ as.character(rownames(Y)[63]),
#       province_id == 64 ~ as.character(rownames(Y)[64]),
#       province_id == 65 ~ as.character(rownames(Y)[65]),
#       province_id == 66 ~ as.character(rownames(Y)[66]),
#       province_id == 67 ~ as.character(rownames(Y)[67]),
#       province_id == 68 ~ as.character(rownames(Y)[68]),
#       province_id == 69 ~ as.character(rownames(Y)[69]),
#       province_id == 70 ~ as.character(rownames(Y)[70]),
#       province_id == 71 ~ as.character(rownames(Y)[71]),
#       province_id == 72 ~ as.character(rownames(Y)[72]),
#       province_id == 73 ~ as.character(rownames(Y)[73]),
#       province_id == 74 ~ as.character(rownames(Y)[74]),
#       province_id == 75 ~ as.character(rownames(Y)[75]),
#       province_id == 76 ~ as.character(rownames(Y)[76]),
#       province_id == 77 ~ as.character(rownames(Y)[77]),
#       province_id == 78 ~ as.character(rownames(Y)[78]),
#       province_id == 79 ~ as.character(rownames(Y)[79]),
#       province_id == 80 ~ as.character(rownames(Y)[80]),
#       province_id == 81 ~ as.character(rownames(Y)[81]),
#       province_id == 82 ~ as.character(rownames(Y)[82]),
#       province_id == 83 ~ as.character(rownames(Y)[83]),
#       province_id == 84 ~ as.character(rownames(Y)[84]),
#       province_id == 85 ~ as.character(rownames(Y)[85]),
#       province_id == 86 ~ as.character(rownames(Y)[86]),
#       province_id == 87 ~ as.character(rownames(Y)[87]),
#       province_id == 88 ~ as.character(rownames(Y)[88]),
#       province_id == 89 ~ as.character(rownames(Y)[89]),
#       province_id == 90 ~ as.character(rownames(Y)[90]),
#       province_id == 91 ~ as.character(rownames(Y)[91]),
#       province_id == 92 ~ as.character(rownames(Y)[92]),
#       province_id == 93 ~ as.character(rownames(Y)[93]),
#       province_id == 94 ~ as.character(rownames(Y)[94]),
#       province_id == 95 ~ as.character(rownames(Y)[95]),
#       province_id == 96 ~ as.character(rownames(Y)[96]),
#       province_id == 97 ~ as.character(rownames(Y)[97]),
#       TRUE ~ "hoge"
#       )
#     ) %>%
#   dplyr::select(-province_id) %>%
#   dplyr::mutate(
#     province = factor(province),
#   )
# fit_clf_01_sample_df_sd
# saveRDS(fit_clf_01_sample_df_sd,"fit_clf_01_sample_df_sd.rds")
# # 保存した結果を読み込む
fit_clf_01_sample_df_sd <- readRDS("fit_clf_01_sample_df_sd.rds")
# 作図する
density_fit_clf_01_sample_df_sx <-
  fit_clf_01_sample_df_sd %>%
  dplyr::filter(parameter == "sx" & iteration >= 500) %>%
  ggplot(
    aes(
      x = number, y = province, fill = province)
  ) +
  geom_density_ridges() +
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Estimated variance by province",
    x = "Estimated SD of state error",
    y = "Province"
  ) +
  theme_ridges() +
  theme(legend.position = "none")
ggsave(
  "density_fit_clf_01_sample_df_sx.pdf",
  plot = density_fit_clf_01_sample_df_sx,
  width = 500,
  height = 500,
  units = "mm"
  )
# 県別SDを使って塗り分け地図
# yとxとでSDが違うのよ。
# y：観測空間の誤差。まさに観測誤差。統計調査上の誤差や採用に由来する誤差。
# x：状態空間の誤差。人的資源量にみる都県に由来する誤差。
# 
# 各SDが示す分布について、中心傾向を求める
fit_clf_01_sample_df_sd <- 
  readRDS("fit_clf_01_sample_df_sd.rds")
fit_clf_01_sample_df_sd_summary <- 
  fit_clf_01_sample_df_sd %>% 
  dplyr::group_by(
    province, 
    parameter
    ) %>% 
  dplyr::summarize(
    mean = mean(number),
    median = median(number)
    )
# 各SDを取り出す
# 観測空間の県別誤差
fit_clf_01_sample_df_sd_summary_sr <- 
  fit_clf_01_sample_df_sd_summary %>% 
  dplyr::filter(parameter == "sr")
# 状態空間の県別誤差
fit_clf_01_sample_df_sd_summary_sx <- 
  fit_clf_01_sample_df_sd_summary %>% 
  dplyr::filter(parameter == "sx")
# 表をつくる
fit_clf_01_sample_df_sd_summary_table <-
  fit_clf_01_sample_df_sd_summary %>%
  # グループ化解除。しないとうまく作表できない
  ungroup() %>%
  # 中央値はいいかな
  dplyr::select(-median) %>%
  dplyr::mutate(
    parameter = factor(parameter)
    ) %>%
  # gt()に適合するよう、横長データセットをつくる
  # たぶん縦長なままつくる方法もある。が、いまひとつ方法がわからないからいまはこれで
  tidyr::pivot_wider(
    names_from = parameter,
    values_from = mean
  ) %>%
  # 小数点以下桁数が大きすぎるから適宜調整
  dplyr::mutate(
    sr = round(sr, digits = 1),
    sx = round(sx, digits = 1),
  ) %>%
  dplyr::mutate(
    difference = sr-sx
  ) %>%
  # 基本の作表
  gt() %>%
  tab_spanner(label = "deviation", columns = c(sr, sx)) %>%
  # 上記関数を用いてAPAっぽく作表
  # benelib()ライブラリが必要ですよ
  theme_gt_apa()
  # 保存
  # フォーマットはPDF以外でもいける
fit_clf_01_sample_df_sd_summary_table %>%
  gtsave("fit_clf_01_sample_df_sd_summary.pdf")

# 塗り分け地図用シェープファイルをロード
# sfをGADMで配らなくなってしまったこの頃です。
# 仕方がないので、シェープファイルを読む
tha_shp_01 <-
  sf::read_sf("../shapefiles/gadm40_THA_shp/gadm40_THA_1.shp") %>%
  # これまで使った都県名にあわせて、空白はアンダースコアに置換
  dplyr::mutate(
    NAME_1 = stringr::str_replace_all(
      NAME_1,
      " ", # 空白を
      "_"  # アンダースコアにすべて置き換える
      )
    )
# シェープファイルとSD中心傾向とをマージ
tha_shp_01_sr <-
  tha_shp_01 %>%
  dplyr::left_join(fit_clf_01_sample_df_sd_summary_sr, by =c("NAME_1" = "province"))
tha_shp_01_sx <-
  tha_shp_01 %>%
  dplyr::left_join(fit_clf_01_sample_df_sd_summary_sx, by =c("NAME_1" = "province"))
# 描画
# y
map_tha_shp_01_sr <-
  ggplot(tha_shp_01_sr) +
  geom_sf(aes(fill = mean))+
  scale_fill_viridis(limits = c(0, 100000), begin = 1, end = 0, oob = scales::squish) +
  labs(
    title = "Estimated SD of Y (Sampling error)"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.2),
    legend.text = element_text(size = 8),
    legend.key.size = unit(5, "mm")
  )
# x
map_tha_shp_01_sx <-
  ggplot(tha_shp_01_sx) +
  geom_sf(aes(fill = mean))+
  scale_fill_viridis(limits = c(0, 100000), begin = 1, end = 0, oob = scales::squish) +
  labs(
    title = "Estimated SD of x (State error)"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.2),
    legend.text = element_text(size = 8),
    legend.key.size = unit(5, "mm")
  )
# 2つの地図を並べて描く
# patchwork()が必要
map_tha_shp_01_sx_sr_mean <- map_tha_shp_01_sx + map_tha_shp_01_sr
# 保存
# ggsave(
#   "map_tha_shp_01_sx_mean.pdf",
#   plot = map_tha_shp_01_sx_sr_mean,
#   width = 200,
#   height = 200,
#   units = "mm"
# )

#
##
### END --- ###
##
#

hoge <- 
  fit_clf_01_sample_df_sd_summary %>%
  # グループ化解除。しないとうまく作表できない
  ungroup() %>%
  # 中央値はいいかな
  dplyr::select(-median) %>%
  dplyr::mutate(
    parameter = factor(parameter)
  ) %>%
  # gt()に適合するよう、横長データセットをつくる
  # たぶん縦長なままつくる方法もある。が、いまひとつ方法がわからないからいまはこれで
  tidyr::pivot_wider(
    names_from = parameter,
    values_from = mean
  ) %>%
  # 小数点以下桁数が大きすぎるから適宜調整
  dplyr::mutate(
    sr = round(sr, digits = 1),
    sx = round(sx, digits = 1),
  ) 
hoge <- 
  fit_clf_01_summary_x_yhat_pd %>% 
  filter(parameter == "x")




# タイ都県名地図と周辺国地図


