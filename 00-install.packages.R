#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Install packages needed for analysis
#######################################

install.packages("Librarian")
library(Librarian)

shelf(here, dplyr, ggcorrplot, tidyr, reshape2, ggplot2,
      grid, gridExtra, ben-arnold/washb, lubridate, stringr,
      ncdf4, boxr, assertthat, mgcv, gamm4, DHARMa, purrr,
      viridis, caret, wrapr, pgirmess, tidyverse, kableExtra,
      rmarkdown, tictoc, doParallel, doRNG, parallel, foreach,
      magrittr, broom, lme4, rcartocolor, RColorBrewer, MetBrewer,
      cowplot, patchwork)