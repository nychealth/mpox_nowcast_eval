#Mpox nowcasting evaluation packages

#Analyst: Rebecca Rohrer
#Code review: Allegra Wilson
#Last updated 12/7/2023

#This code loads required packages
#The code requires installing NobBS and JAGS

if (!require(pacman))install.packages("pacman")
library(pacman)

p_load(NobBS, MLmetrics, dplyr, conflicted, tidyverse, lubridate, data.table, performance, glmmTMB, ggsci, lemon, odbc, pals, parallel,
       scales, DBI, dbplyr, stringr, haven, ggplot2)
p_loaded()

conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("wday", "lubridate")

# R version 4.2.3 (2023-03-15)
# Platform: x86_64-redhat-linux-gnu (64-bit)
# Running under: Red Hat Enterprise Linux 8.7 (Ootpa)
# 
# Matrix products: default
# BLAS/LAPACK: /usr/lib64/libopenblaso-r0.3.15.so
# 
# locale:
#   [1] LC_CTYPE=en_US.UTF-8          LC_NUMERIC=C                  LC_TIME=en_US.UTF-8           LC_COLLATE=en_US.UTF-8        LC_MONETARY=en_US.UTF-8      
# [6] LC_MESSAGES=en_US.UTF-8       LC_PAPER=en_US.UTF-8          LC_NAME=en_US.UTF-8           LC_ADDRESS=en_US.UTF-8        LC_TELEPHONE=en_US.UTF-8     
# [11] LC_MEASUREMENT=en_US.UTF-8    LC_IDENTIFICATION=en_US.UTF-8
# 
# attached base packages:
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] lemon_0.4.6        ggsci_2.9          glmmTMB_1.1.5      performance_0.10.2 sf_1.0-9           rgeos_0.6-1        scatterpie_0.1.8  
# [8] htmlTable_2.4.1    gridExtra_2.3      gtable_0.3.1       RColorBrewer_1.1-3 rjson_0.2.21       leaflet_2.1.1      rgdal_1.6-2       
# [15] sp_1.5-1           ggrepel_0.9.2      plotly_4.10.1      crosstalk_1.2.0    png_0.1-8          htmlwidgets_1.5.4  highcharter_0.9.4 
# [22] sparkline_2.0      kableExtra_1.3.4   magrittr_2.0.3     readxl_1.4.1       DT_0.26            reshape2_1.4.4     gdata_2.18.0.1    
# [29] chron_2.3-58       officer_0.5.1      haven_2.5.1        xlsx_0.6.5         dbplyr_2.2.1       RJDBC_0.2-10       rJava_1.0-6       
# [36] DBI_1.1.3          scales_1.2.1       mailR_0.8          pals_1.7           odbc_1.3.3         data.table_1.14.6  lubridate_1.9.0   
# [43] timechange_0.1.1   forcats_0.5.2      stringr_1.5.0      purrr_1.0.0        readr_2.1.3        tidyr_1.2.1        tibble_3.1.8      
# [50] ggplot2_3.4.0      tidyverse_1.3.2    conflicted_1.1.0   dplyr_1.0.10       MLmetrics_1.1.1    NobBS_0.1.0       
# 
# loaded via a namespace (and not attached):
#   [1] uuid_1.1-0              backports_1.4.1         systemfonts_1.0.4       plyr_1.8.8              igraph_1.3.5            lazyeval_0.2.2         
# [7] splines_4.2.3           TMB_1.9.1               digest_0.6.30           htmltools_0.5.3         leaflet.providers_1.9.0 fansi_1.0.3            
# [13] checkmate_2.1.0         memoise_2.0.1           googlesheets4_1.0.1     tzdb_0.3.0              modelr_0.1.10           R.utils_2.12.2         
# [19] xts_0.12.2              svglite_2.1.0           askpass_1.1             colorspace_2.0-3        blob_1.2.3              rvest_1.0.3            
# [25] xfun_0.35               crayon_1.5.2            jsonlite_1.8.4          lme4_1.1-31             zoo_1.8-11              glue_1.6.2             
# [31] polyclip_1.10-4         gargle_1.2.1            webshot_0.5.4           quantmod_0.4.20         maps_3.4.1              Rcpp_1.0.9             
# [37] viridisLite_0.4.1       units_0.8-1             proxy_0.4-27            bit_4.0.5               mapproj_1.2.9           httr_1.4.4             
# [43] ellipsis_0.3.2          pkgconfig_2.0.3         R.methodsS3_1.8.2       farver_2.1.1            sass_0.4.4              utf8_1.2.2             
# [49] tidyselect_1.2.0        labeling_0.4.2          rlang_1.0.6             munsell_0.5.0           cellranger_1.1.0        tools_4.2.3            
# [55] cachem_1.0.6            cli_3.4.1               generics_0.1.3          broom_1.0.2             evaluate_0.18           fastmap_1.1.0          
# [61] yaml_2.3.6              knitr_1.41              bit64_4.0.5             fs_1.5.2                zip_2.2.2               nlme_3.1-162           
# [67] R.oo_1.25.0             xml2_1.3.3              compiler_4.2.3          rstudioapi_0.14         curl_4.3.3              e1071_1.7-12           
# [73] reprex_2.0.2            tweenr_2.0.2            bslib_0.4.1             stringi_1.7.8           lattice_0.20-45         Matrix_1.5-3           
# [79] nloptr_2.0.3            classInt_0.4-8          vctrs_0.5.1             pillar_1.8.1            lifecycle_1.0.3         jquerylib_0.1.4        
# [85] insight_0.19.0          R6_2.5.1                KernSmooth_2.23-20      rjags_4-13              dichromat_2.0-0.1       boot_1.3-28.1          
# [91] MASS_7.3-58.2           gtools_3.9.4            assertthat_0.2.1        xlsxjars_0.6.1          openssl_2.0.5           withr_2.5.0            
# [97] rlist_0.4.6.2           hms_1.1.2               ggfun_0.0.9             grid_4.2.3              minqa_1.2.5             class_7.3-21           
# [103] coda_0.19-4             rmarkdown_2.18          googledrive_2.0.0       TTR_0.24.3              ggforce_0.4.1           numDeriv_2016.8-1.1  