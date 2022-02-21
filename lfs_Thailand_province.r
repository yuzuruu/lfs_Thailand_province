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

