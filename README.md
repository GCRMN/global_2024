
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **Status of coral reefs of the world: 2025**

## 1. Introduction

## 2. Code

### A. Cleaning and selection

- `a1_extract-reef-aca.js` Export [Allen Coral
  Atlas](https://developers.google.com/earth-engine/datasets/catalog/ACA_reef_habitat_v2_0)
  coral reef distribution data as .shp at a 500 m resolution from Google
  Earth Engine (GEE)
- `a2_clean-reefs.R` Add GCRMN region to coral reef distribution and
  create 100 km coral reef buffer

### B. Extraction

### C. Models

### D. Figures and tables

### E. Functions

- `data_descriptors.R` Get number of sites, surveys, datasets, first and
  last year of monitoring.
- `graphical_par.R` Graphical parameters, including colors and fonts.
- `theme_graph.R` Main ggplot theme for the plots of the reports.
- `theme_map.R` Main ggplot theme for the maps of the reports.

## 3. Reproducibility parameters

    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.4.0 (2024-04-24 ucrt)
     os       Windows 10 x64 (build 18363)
     system   x86_64, mingw32
     ui       RTerm
     language (EN)
     collate  French_France.utf8
     ctype    French_France.utf8
     tz       Europe/Paris
     date     2024-06-07
     pandoc   3.1.11 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────
     package       * version    date (UTC) lib source
     askpass         1.2.0      2023-09-03 [1] CRAN (R 4.4.0)
     backports       1.5.0      2024-05-23 [1] CRAN (R 4.4.0)
     base64enc       0.1-3      2015-07-28 [1] CRAN (R 4.4.0)
     bit             4.0.5      2022-11-15 [1] CRAN (R 4.4.0)
     bit64           4.0.5      2020-08-30 [1] CRAN (R 4.4.0)
     blob            1.2.4      2023-03-17 [1] CRAN (R 4.4.0)
     broom           1.0.6      2024-05-17 [1] CRAN (R 4.4.0)
     bslib           0.7.0      2024-03-29 [1] CRAN (R 4.4.0)
     cachem          1.1.0      2024-05-16 [1] CRAN (R 4.4.0)
     callr           3.7.6      2024-03-25 [1] CRAN (R 4.4.0)
     cellranger      1.1.0      2016-07-27 [1] CRAN (R 4.4.0)
     class           7.3-22     2023-05-03 [1] CRAN (R 4.4.0)
     classInt        0.4-10     2023-09-05 [1] CRAN (R 4.4.0)
     cli             3.6.2      2023-12-11 [1] CRAN (R 4.4.0)
     clipr           0.8.0      2022-02-22 [1] CRAN (R 4.4.0)
     clock           0.7.0      2023-05-15 [1] CRAN (R 4.4.0)
     codetools       0.2-20     2024-03-31 [1] CRAN (R 4.4.0)
     colorspace      2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
     conflicted      1.2.0      2023-02-01 [1] CRAN (R 4.4.0)
     cpp11           0.4.7      2023-12-02 [1] CRAN (R 4.4.0)
     crayon          1.5.2      2022-09-29 [1] CRAN (R 4.4.0)
     curl            5.2.1      2024-03-01 [1] CRAN (R 4.4.0)
     data.table      1.15.4     2024-03-30 [1] CRAN (R 4.4.0)
     DBI             1.2.3      2024-06-02 [1] CRAN (R 4.4.0)
     dbplyr          2.5.0      2024-03-19 [1] CRAN (R 4.4.0)
     diagram         1.6.5      2020-09-30 [1] CRAN (R 4.4.0)
     dials           1.2.1      2024-02-22 [1] CRAN (R 4.4.0)
     DiceDesign      1.10       2023-12-07 [1] CRAN (R 4.4.0)
     digest          0.6.35     2024-03-11 [1] CRAN (R 4.4.0)
     doFuture        1.0.1      2023-12-20 [1] CRAN (R 4.4.0)
     dplyr           1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
     dtplyr          1.3.1      2023-03-22 [1] CRAN (R 4.4.0)
     e1071           1.7-14     2023-12-06 [1] CRAN (R 4.4.0)
     ellipsis        0.3.2      2021-04-29 [1] CRAN (R 4.4.0)
     evaluate        0.23       2023-11-01 [1] CRAN (R 4.4.0)
     fansi           1.0.6      2023-12-08 [1] CRAN (R 4.4.0)
     farver          2.1.2      2024-05-13 [1] CRAN (R 4.4.0)
     fastmap         1.2.0      2024-05-15 [1] CRAN (R 4.4.0)
     fontawesome     0.5.2      2023-08-19 [1] CRAN (R 4.4.0)
     forcats         1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
     foreach         1.5.2      2022-02-02 [1] CRAN (R 4.4.0)
     fs              1.6.4      2024-04-25 [1] CRAN (R 4.4.0)
     furrr           0.3.1      2022-08-15 [1] CRAN (R 4.4.0)
     future          1.33.2     2024-03-26 [1] CRAN (R 4.4.0)
     future.apply    1.11.2     2024-03-28 [1] CRAN (R 4.4.0)
     gargle          1.5.2      2023-07-20 [1] CRAN (R 4.4.0)
     generics        0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
     ggplot2         3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
     globals         0.16.3     2024-03-08 [1] CRAN (R 4.4.0)
     glue            1.7.0      2024-01-09 [1] CRAN (R 4.4.0)
     googledrive     2.1.1      2023-06-11 [1] CRAN (R 4.4.0)
     googlesheets4   1.1.1      2023-06-11 [1] CRAN (R 4.4.0)
     gower           1.0.1      2022-12-22 [1] CRAN (R 4.4.0)
     GPfit           1.0-8      2019-02-08 [1] CRAN (R 4.4.0)
     gtable          0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
     hardhat         1.4.0      2024-06-02 [1] CRAN (R 4.4.0)
     haven           2.5.4      2023-11-30 [1] CRAN (R 4.4.0)
     highr           0.11       2024-05-26 [1] CRAN (R 4.4.0)
     hms             1.1.3      2023-03-21 [1] CRAN (R 4.4.0)
     htmltools       0.5.8.1    2024-04-04 [1] CRAN (R 4.4.0)
     httr            1.4.7      2023-08-15 [1] CRAN (R 4.4.0)
     ids             1.0.1      2017-05-31 [1] CRAN (R 4.4.0)
     infer           1.0.7      2024-03-25 [1] CRAN (R 4.4.0)
     ipred           0.9-14     2023-03-09 [1] CRAN (R 4.4.0)
     isoband         0.2.7      2022-12-20 [1] CRAN (R 4.4.0)
     iterators       1.0.14     2022-02-05 [1] CRAN (R 4.4.0)
     jquerylib       0.1.4      2021-04-26 [1] CRAN (R 4.4.0)
     jsonlite        1.8.8      2023-12-04 [1] CRAN (R 4.4.0)
     KernSmooth      2.23-22    2023-07-10 [1] CRAN (R 4.4.0)
     knitr           1.47       2024-05-29 [1] CRAN (R 4.4.0)
     labeling        0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
     lattice         0.22-6     2024-03-20 [1] CRAN (R 4.4.0)
     lava            1.8.0      2024-03-05 [1] CRAN (R 4.4.0)
     lhs             1.1.6      2022-12-17 [1] CRAN (R 4.4.0)
     lifecycle       1.0.4      2023-11-07 [1] CRAN (R 4.4.0)
     listenv         0.9.1      2024-01-29 [1] CRAN (R 4.4.0)
     lubridate       1.9.3      2023-09-27 [1] CRAN (R 4.4.0)
     magrittr        2.0.3      2022-03-30 [1] CRAN (R 4.4.0)
     MASS            7.3-60.2   2024-04-24 [1] local
     Matrix          1.7-0      2024-03-22 [1] CRAN (R 4.4.0)
     memoise         2.0.1      2021-11-26 [1] CRAN (R 4.4.0)
     mgcv            1.9-1      2023-12-21 [1] CRAN (R 4.4.0)
     mime            0.12       2021-09-28 [1] CRAN (R 4.4.0)
     modeldata       1.3.0      2024-01-21 [1] CRAN (R 4.4.0)
     modelenv        0.1.1      2023-03-08 [1] CRAN (R 4.4.0)
     modelr          0.1.11     2023-03-22 [1] CRAN (R 4.4.0)
     munsell         0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
     nlme            3.1-164    2023-11-27 [1] CRAN (R 4.4.0)
     nnet            7.3-19     2023-05-03 [1] CRAN (R 4.4.0)
     numDeriv        2016.8-1.1 2019-06-06 [1] CRAN (R 4.4.0)
     openssl         2.2.0      2024-05-16 [1] CRAN (R 4.4.0)
     parallelly      1.37.1     2024-02-29 [1] CRAN (R 4.4.0)
     parsnip         1.2.1      2024-03-22 [1] CRAN (R 4.4.0)
     patchwork       1.2.0      2024-01-08 [1] CRAN (R 4.4.0)
     pillar          1.9.0      2023-03-22 [1] CRAN (R 4.4.0)
     pkgconfig       2.0.3      2019-09-22 [1] CRAN (R 4.4.0)
     prettyunits     1.2.0      2023-09-24 [1] CRAN (R 4.4.0)
     processx        3.8.4      2024-03-16 [1] CRAN (R 4.4.0)
     prodlim         2023.08.28 2023-08-28 [1] CRAN (R 4.4.0)
     progress        1.2.3      2023-12-06 [1] CRAN (R 4.4.0)
     progressr       0.14.0     2023-08-10 [1] CRAN (R 4.4.0)
     proxy           0.4-27     2022-06-09 [1] CRAN (R 4.4.0)
     ps              1.7.6      2024-01-18 [1] CRAN (R 4.4.0)
     purrr           1.0.2      2023-08-10 [1] CRAN (R 4.4.0)
     R6              2.5.1      2021-08-19 [1] CRAN (R 4.4.0)
     ragg            1.3.2      2024-05-15 [1] CRAN (R 4.4.0)
     rappdirs        0.3.3      2021-01-31 [1] CRAN (R 4.4.0)
     RColorBrewer    1.1-3      2022-04-03 [1] CRAN (R 4.4.0)
     Rcpp            1.0.12     2024-01-09 [1] CRAN (R 4.4.0)
     readr           2.1.5      2024-01-10 [1] CRAN (R 4.4.0)
     readxl          1.4.3      2023-07-06 [1] CRAN (R 4.4.0)
     recipes         1.0.10     2024-02-18 [1] CRAN (R 4.4.0)
     rematch         2.0.0      2023-08-30 [1] CRAN (R 4.4.0)
     rematch2        2.1.2      2020-05-01 [1] CRAN (R 4.4.0)
     reprex          2.1.0      2024-01-11 [1] CRAN (R 4.4.0)
     rlang           1.1.3      2024-01-10 [1] CRAN (R 4.4.0)
     rmarkdown       2.27       2024-05-17 [1] CRAN (R 4.4.0)
     rpart           4.1.23     2023-12-05 [1] CRAN (R 4.4.0)
     rsample         1.2.1      2024-03-25 [1] CRAN (R 4.4.0)
     rstudioapi      0.16.0     2024-03-24 [1] CRAN (R 4.4.0)
     rvest           1.0.4      2024-02-12 [1] CRAN (R 4.4.0)
     s2              1.1.6      2023-12-19 [1] CRAN (R 4.4.0)
     sass            0.4.9      2024-03-15 [1] CRAN (R 4.4.0)
     scales          1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
     selectr         0.4-2      2019-11-20 [1] CRAN (R 4.4.0)
     sf              1.0-16     2024-03-24 [1] CRAN (R 4.4.0)
     shape           1.4.6.1    2024-02-23 [1] CRAN (R 4.4.0)
     slider          0.3.1      2023-10-12 [1] CRAN (R 4.4.0)
     SQUAREM         2021.1     2021-01-13 [1] CRAN (R 4.4.0)
     stringi         1.8.3      2023-12-11 [1] CRAN (R 4.4.0)
     stringr         1.5.1      2023-11-14 [1] CRAN (R 4.4.0)
     survival        3.5-8      2024-02-14 [1] CRAN (R 4.4.0)
     sys             3.4.2      2023-05-23 [1] CRAN (R 4.4.0)
     systemfonts     1.1.0      2024-05-15 [1] CRAN (R 4.4.0)
     textshaping     0.4.0      2024-05-24 [1] CRAN (R 4.4.0)
     tibble          3.2.1      2023-03-20 [1] CRAN (R 4.4.0)
     tidymodels      1.2.0      2024-03-25 [1] CRAN (R 4.4.0)
     tidyr           1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
     tidyselect      1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
     tidyverse       2.0.0      2023-02-22 [1] CRAN (R 4.4.0)
     timechange      0.3.0      2024-01-18 [1] CRAN (R 4.4.0)
     timeDate        4032.109   2023-12-14 [1] CRAN (R 4.4.0)
     tinytex         0.51       2024-05-06 [1] CRAN (R 4.4.0)
     tune            1.2.1      2024-04-18 [1] CRAN (R 4.4.0)
     tzdb            0.4.0      2023-05-12 [1] CRAN (R 4.4.0)
     units           0.8-5      2023-11-28 [1] CRAN (R 4.4.0)
     utf8            1.2.4      2023-10-22 [1] CRAN (R 4.4.0)
     uuid            1.2-0      2024-01-14 [1] CRAN (R 4.4.0)
     vctrs           0.6.5      2023-12-01 [1] CRAN (R 4.4.0)
     viridisLite     0.4.2      2023-05-02 [1] CRAN (R 4.4.0)
     vroom           1.6.5      2023-12-05 [1] CRAN (R 4.4.0)
     warp            0.2.1      2023-11-02 [1] CRAN (R 4.4.0)
     withr           3.0.0      2024-01-16 [1] CRAN (R 4.4.0)
     wk              0.9.1      2023-11-29 [1] CRAN (R 4.4.0)
     workflows       1.1.4      2024-02-19 [1] CRAN (R 4.4.0)
     workflowsets    1.1.0      2024-03-21 [1] CRAN (R 4.4.0)
     xfun            0.44       2024-05-15 [1] CRAN (R 4.4.0)
     xml2            1.3.6      2023-12-04 [1] CRAN (R 4.4.0)
     yaml            2.3.8      2023-12-11 [1] CRAN (R 4.4.0)
     yardstick       1.3.1      2024-03-21 [1] CRAN (R 4.4.0)

     [1] C:/Users/jwicquart/AppData/Local/Programs/R/R-4.4.0/library

    ──────────────────────────────────────────────────────────────────────────────
