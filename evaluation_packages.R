#Mpox nowcasting evaluation packages

#Analyst: Rebecca Rohrer
#Code review: Allegra Wilson
#Last updated 1/18/2024

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
conflict_prefer("mutate", "dplyr")

# R version 4.2.3 (2023-03-15)
# Platform: x86_64-redhat-linux-gnu (64-bit)
# Running under: Red Hat Enterprise Linux 8.9 (Ootpa)
# 
# Matrix products: default
# BLAS/LAPACK: /usr/lib64/libopenblaso-r0.3.15.so
# 
# locale:
#   [1] LC_CTYPE=en_US.UTF-8          LC_NUMERIC=C                  LC_TIME=en_US.UTF-8          
# [4] LC_COLLATE=en_US.UTF-8        LC_MONETARY=en_US.UTF-8       LC_MESSAGES=en_US.UTF-8      
# [7] LC_PAPER=en_US.UTF-8          LC_NAME=en_US.UTF-8           LC_ADDRESS=en_US.UTF-8       
# [10] LC_TELEPHONE=en_US.UTF-8      LC_MEASUREMENT=en_US.UTF-8    LC_IDENTIFICATION=en_US.UTF-8
# 
# attached base packages:
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] haven_2.5.1        xlsx_0.6.5         dbplyr_2.2.1       RJDBC_0.2-10       rJava_1.0-6       
# [6] DBI_1.1.3          scales_1.2.1       mailR_0.8          pals_1.7           odbc_1.3.3        
# [11] lemon_0.4.6        ggsci_2.9          glmmTMB_1.1.5      performance_0.10.2 data.table_1.14.6 
# [16] lubridate_1.9.0    timechange_0.1.1   forcats_0.5.2      stringr_1.5.0      purrr_1.0.0       
# [21] readr_2.1.3        tidyr_1.2.1        tibble_3.1.8       ggplot2_3.4.0      tidyverse_1.3.2   
# [26] conflicted_1.1.0   dplyr_1.0.10       MLmetrics_1.1.1    NobBS_0.1.0        pacman_0.5.1      
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-162        fs_1.5.2            bit64_4.0.5         insight_0.19.0      httr_1.4.4         
# [6] numDeriv_2016.8-1.1 tools_4.2.3         TMB_1.9.1           backports_1.4.1     utf8_1.2.2         
# [11] R6_2.5.1            colorspace_2.0-3    withr_2.5.0         tidyselect_1.2.0    gridExtra_2.3      
# [16] bit_4.0.5           compiler_4.2.3      cli_3.4.1           rvest_1.0.3         xml2_1.3.3         
# [21] minqa_1.2.5         R.utils_2.12.2      dichromat_2.0-0.1   pkgconfig_2.0.3     lme4_1.1-31        
# [26] maps_3.4.1          fastmap_1.1.0       rlang_1.0.6         readxl_1.4.1        rstudioapi_0.14    
# [31] generics_0.1.3      jsonlite_1.8.4      R.oo_1.25.0         googlesheets4_1.0.1 magrittr_2.0.3     
# [36] Matrix_1.5-3        Rcpp_1.0.9          munsell_0.5.0       fansi_1.0.3         R.methodsS3_1.8.2  
# [41] lifecycle_1.0.3     stringi_1.7.8       MASS_7.3-58.2       plyr_1.8.8          grid_4.2.3         
# [46] blob_1.2.3          crayon_1.5.2        lattice_0.20-45     splines_4.2.3       xlsxjars_0.6.1     
# [51] mapproj_1.2.9       hms_1.1.2           knitr_1.41          pillar_1.8.1        boot_1.3-28.1      
# [56] reprex_2.0.2        glue_1.6.2          modelr_0.1.10       vctrs_0.5.1         nloptr_2.0.3       
# [61] tzdb_0.3.0          cellranger_1.1.0    gtable_0.3.1        assertthat_0.2.1    cachem_1.0.6       
# [66] xfun_0.35           broom_1.0.2         coda_0.19-4         googledrive_2.0.0   rjags_4-13         
# [71] gargle_1.2.1        memoise_2.0.1       ellipsis_0.3.2