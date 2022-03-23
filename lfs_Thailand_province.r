##########################################################################
# Labour force survey Thailand by Province
# Yuzuru Utsunomiya
# First make: 21st. February 2022
# Revised: 
# In detail of the revised history, refer to my Github

### ----- read.library ----
# read necessary library
library(tidyverse)
library(viridis)
library(khroma)
library(cmdstanr)
#
#
##
### END ###
##
#

### ---- read.data ----
# read.data
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
  facet_wrap(~ province, scales = "free_y")
clf_fm_line
#
#
##
### END ###
##
#
#
### ---- clf.fm.line.original ----
clf_fm_line_province_original <- 
  clf_fm %>%
  group_by(province) %>% 
  nest() %>% 
  dplyr::mutate(
    line = purrr::map(
      data,
      ~
        ggplot2::ggplot(
          data = .,
          aes(
            x = year_month,
            y = number,
            colour = gender
          ) 
        ) +
        geom_line() +
        geom_point() +
        labs(
          title = province,
          x = "Year",
          y = "N. of current labour force (Unit: person)"
        ) +
        scale_color_okabeito() +
        theme_classic() +
        theme(
          legend.position = "bottom"
        )
    )
  )
# save the figure
# The object includes 77 provinces and it takes 
# a few minutes.
pdf("clf_fm_line_province_original.pdf")
clf_fm_line_province_original$line  
dev.off()  
#
#
##
### END ###
##
#
#
### ---- clf.fm.line.wrap ----
# Draw line plots wrapped by province
clf_fm_line_province_wrap <- 
  clf_fm %>% 
  ggplot2::ggplot(
    data = .,
    aes(
      x = year_month,
      y = number,
      colour = gender
    ) 
  ) +
  geom_line() +
  geom_point() +
  labs(
    x = "Quarter (Q1/1994-Q4/2020)",
    y = "N. of current labour force (Unit: person)"
  ) +
  scale_color_okabeito() +
  theme_classic() +
  theme(
    legend.position = "bottom"
  ) + 
  facet_wrap(~ province, scales = "free_y")
clf_fm_line_province_wrap
# save the figure
# Note
# The plot includes 77 line plots.
# We need to adjust plot area accordingly. 
ggsave(
  "clf_fm_line_province_wrap.pdf",
  plot = clf_fm_line_province_wrap,
  width = 500,
  height = 500,
  units = "mm"
)
#
#
##
### END ###
##
#


### ---- state.space.01 ----
# make a dataset
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
# Trans the data set and make it matrix
Y <-
  clf_fm_selected %>%
  dplyr::select(-year_month) %>%
  as.matrix(.) %>%
  t(.)
# Pick up non-NA data
ypos <- Y[!is.na(Y)]
# count the N. of non-NA data
n_pos <- length(ypos)  # number on non-NA ys
# obtain indices of the non-NA data
indx_pos <- which(!is.na(Y), arr.ind = TRUE)  # index on the non-NAs
col_indx_pos <- as.vector(indx_pos[, "col"])
row_indx_pos <- as.vector(indx_pos[, "row"])
# make a list data for stan
data_volume <-
  list(
    y = ypos,
    TT = ncol(Y),
    N = nrow(Y),
    n_pos = n_pos,
    col_indx_pos = col_indx_pos,
    row_indx_pos = row_indx_pos
  )
#
# compile the stan model
# NOTE
# model list
# model_clf_01 (lfs_Thailand_province_01.stan):
# local level with change point detection using cauchy distribution
# model_clf_02:
#
#
model_clf_01 <-
  cmdstan_model(
    # NOTE
    # write the stan model separately
    stan_file = "lfs_Thailand_province_01.stan",
    compile = TRUE
  )
# fit the stan model
# Here needs some computation periods.
# NOTE
# For trial, adjust iteration / warmup number to save computation periods.
fit_clf_01 <-
  model_clf_01$sample(
    data_volume,
    # max_treedepth = 20,
    # adapt_delta = 0.9,
    iter_warmup = 500,    # here!
    iter_sampling = 500,  # here!
    chains = 4,
    parallel_chains = 4,
    refresh = 200
  )
# save the results
# NOTE
# To read, use base::readRDS()
#
fit_clf_01$save_object(file = "fit_clf_01.rds")
# save the summary statistics
fit_clf_01_summary <- fit_clf_01$summary()
write_excel_csv(fit_clf_01_summary, "fit_clf_01_summary.csv")


# ここから先はこれから検討する箇所
# 実行しても時間がかかるばかりでいい結果は出ないから注意。
# くれぐれもよーく再検討してね。
# stanfit <- rstan::read_stan_csv(fit_clf_01$output_files())
# shinystan::launch_shinystan(stanfit)
### ---- state.space.02 ----
# # make a dataset
# clf_fm_female <- 
#   clf_fm %>%
#   # select necessary province and gender for trial
#   # For trial, select some (4 or 5 in maximum) province to save computation period
#   dplyr::filter(
#     province %in%
#       c("Amnat_Charoen", "Chiang_Mai", "Kalasin", "Bangkok_Metropolis") & gender == "Female"
#   ) %>%
#   dplyr::select(
#     year_month,  province, number#, gender 
#   ) %>% 
#   tidyr::pivot_wider(
#     names_from = province,
#     values_from = number
#   ) 
# clf_fm_male <- 
#   clf_fm %>%
#   # select necessary province and gender for trial
#   # For trial, select some (4 or 5 in maximum) province to save computation period
#   dplyr::filter(
#     province %in%
#       c("Amnat_Charoen", "Chiang_Mai", "Kalasin", "Bangkok_Metropolis") & gender == "Male"
#   ) %>%
#   dplyr::select(
#     year_month,  province, number#, gender 
#   ) %>% 
#   tidyr::pivot_wider(
#     names_from = province,
#     values_from = number
#   ) 
# 
# # Trans the data set and make it matrix
# Y_female <- 
#   clf_fm_female %>% 
#   dplyr::select(-year_month) %>% 
#   as.matrix(.) %>% 
#   t(.)
# Y_male <- 
#   clf_fm_male %>% 
#   dplyr::select(-year_month) %>% 
#   as.matrix(.) %>% 
#   t(.)
# # Pick up non-NA data
# ypos_female <- Y_female[!is.na(Y_female)]
# ypos_male <- Y_male[!is.na(Y_male)]
# # count the N. of non-NA data
# n_pos_female <- length(ypos_female)  # number on non-NA ys
# n_pos_male <- length(ypos_male)  # number on non-NA ys
# # obtain indices of the non-NA data
# indx_pos_female <- which(!is.na(Y_female), arr.ind = TRUE)  # index on the non-NAs
# indx_pos_male <- which(!is.na(Y_male), arr.ind = TRUE)  # index on the non-NAs
# col_indx_pos_female <- as.vector(indx_pos_female[, "col"])
# row_indx_pos_female <- as.vector(indx_pos_female[, "row"])
# col_indx_pos_male <- as.vector(indx_pos_male[, "col"])
# row_indx_pos_male <- as.vector(indx_pos_male[, "row"])
# 
# ypos <- c(ypos_female, ypos_male)
# n_pos <- c(n_pos_female + n_pos_male)
# col_indx_pos <- c(col_indx_pos_female, col_indx_pos_male)
# row_indx_pos <- c(row_indx_pos_female, row_indx_pos_male)
# gender <- rep(c(1,2), each = (n_pos/2))
# y <- data.frame(gender = gender, y = ypos)
# make a list data for stan
# data_volume <-
#   list(
#     y = ypos,
#     gender = gender,
#     n_gender = length(levels(factor(gender))),
#     TT = ncol(Y_female), 
#     N = nrow(Y_female), 
#     n_pos = n_pos, 
#     col_indx_pos = col_indx_pos,
#     row_indx_pos = row_indx_pos
#   )


# Y_data <- 
#   data.frame(Y_original = c(Y_female, Y_male)) %>% 
#   dplyr::mutate(
#     Missing = dplyr::case_when(
#       !is.na(.$Y_original) ~ 0,
#       TRUE ~ 1
#       ),
#     Y = dplyr::case_when(
#       is.na(.$Y_original) ~ 0,
#       TRUE ~ .$Y_original
#     )
#   )
# 
# N <- nrow(Y_female)
# T <- ncol(Y_female)
# G <- length(levels(factor(gender)))
# 
# data_volume <- list()
# # N. of gender
# data_volume$G <- G # 2
# # N. of time period
# data_volume$T <- T # 97
# # N. of province
# data_volume$N <- N # 4 
# data_volume$T_N <- array(data = ncol(Y_female), dim = 4)
# #observed data
# data_volume$Y <- 
#   array(
#     data = Y_data$Y, 
#     dim = c(
#       N, # N 4
#       T, # T, 97
#       G# G, 2
#     )
#   )
# data_volume$Missing <- 
#   array(
#     data = Missing$Missing, 
#     dim = c(
#       N, # N
#       T, # T,
#       G# G,
#     )
#   )
# # To check 
# data_volume
# 
# compile the stan model
# NOTE
# model list
# model_clf_01 (lfs_Thailand_province_01.stan): 
# local level with change point detection using cauchy distribution
# model_clf_02:
# 
# 
# model_clf_03 <-
#   cmdstan_model(
#     # NOTE
#     # write the stan model separately
#     stan_file = "lfs_Thailand_province_03.stan",
#     compile = TRUE
#   )
# fit the stan model
# Here needs some computation periods.
# NOTE
# For trial, adjust iteration / warmup number to save computation periods.
# fit_clf_03 <-
#   model_clf_03$sample(
#     data_volume,
#     # max_treedepth = 20,
#     # adapt_delta = 0.9,
#     iter_warmup = 500,    # here!
#     iter_sampling = 500,  # here!
#     chains = 4,
#     parallel_chains = 4,
#     refresh = 200
#   )
# save the results
# NOTE
# To read, use base::readRDS()
# 
# fit_clf_02$save_object(file = "fit_clf_02.rds")
# # save the summary statistics
# fit_clf_02_summary <- fit_clf_02$summary()
# write_excel_csv(fit_clf_02_summary, "fit_clf_02_summary.csv")


# 男女差を考慮したモデル
# 収束しなさすぎるので、そのうち再度検討
# model_clf_04 <-
#   cmdstan_model(
#     # NOTE
#     # write the stan model separately
#     stan_file = "lfs_Thailand_province_04.stan",
#     compile = TRUE
#   )
# # fit the stan model
# # Here needs some computation periods.
# # NOTE
# # For trial, adjust iteration / warmup number to save computation periods.
# fit_clf_04 <-
#   model_clf_04$sample(
#     data_volume,
#     # max_treedepth = 20,
#     # adapt_delta = 0.9,
#     iter_warmup = 500,    # here!
#     iter_sampling = 500,  # here!
#     chains = 4,
#     parallel_chains = 4,
#     refresh = 200
#   )
# 
# fit_clf_04_summary <- fit_clf_04$summary()
# write_excel_csv(fit_clf_04_summary, "fit_clf_04_summary.csv")







