# set options
options(datatable.verbose = FALSE) 

# load libraries ------------------------------------------

warn_conflicts <- TRUE

# library(autoEDA         , warn.conflicts = warn_conflicts)
# library(citr            , warn.conflicts = warn_conflicts)
library(data.table       , warn.conflicts = warn_conflicts) # No Dep!! 
library(DT               , warn.conflicts = warn_conflicts) # many 
# library(esquisse        , warn.conflicts = warn_conflicts)
library(forcats          , warn.conflicts = warn_conflicts) 
library(formattable      , warn.conflicts = warn_conflicts) # many
library(flexdashboard    , warn.conflicts = warn_conflicts) # many
# library(fst             , warn.conflicts = warn_conflicts)
#library(ggiraph           , warn.conflicts = warn_conflicts)
library(plotly           , warn.conflicts = warn_conflicts)
library(ggplot2          , warn.conflicts = warn_conflicts)
library(glue             , warn.conflicts = warn_conflicts)  # No Dep!
# library(gtable          , warn.conflicts = warn_conflicts)
# library(grid            , warn.conflicts = warn_conflicts)
library(knitr            , warn.conflicts = warn_conflicts)  # many
# library(lineupjs        , warn.conflicts = warn_conflicts) 
library(lubridate        , warn.conflicts = warn_conflicts)  # "generics" "Rcpp" 
library(magrittr         , warn.conflicts = warn_conflicts)  # No Dep!
# library(manipulateWidget, warn.conflicts = warn_conflicts)
library(openxlsx        , warn.conflicts = warn_conflicts)
library(purrr            , warn.conflicts = warn_conflicts)  # "magrittr" "rlang" 
# library(readr            , warn.conflicts = warn_conflicts)  # CHECK TO REMOVE 
# library(RColorBrewer    , warn.conflicts = warn_conflicts)
library(RCurl            , warn.conflicts = warn_conflicts)  # bitops
# library(rticles         , warn.conflicts = warn_conflicts)
# library(servr           , warn.conflicts = warn_conflicts)
# library(scales          , warn.conflicts = warn_conflicts)
# library(styler          , warn.conflicts = warn_conflicts)
library(stringr          , warn.conflicts = warn_conflicts)  # "glue", "magrittr", "stringi"
# library(tictoc          , warn.conflicts = warn_conflicts)    
# library(tidyr           , warn.conflicts = warn_conflicts)     
# library(tidyverse       , warn.conflicts = warn_conflicts)    
# library(tufte           , warn.conflicts = warn_conflicts)
# library(utils           , warn.conflicts = warn_conflicts)

# Environment ---------------------------------------------- 
Sys.setenv(TZ='CET')

# Options -------------------------------------------------
# options("esquisse.viewer" = "browser")
scipen <- options("scipen")[["scipen"]]
options(scipen = 999)

# Path's --------------------------------------------------
p_enx <- file.path("C:", "PW", "OneDrive", "DS", "ENEXIS")
p_raw <- file.path(p_enx, "GIT", "data")
p_prp <- file.path(p_enx, "GIT", "data", "prep")
p_scr <- file.path("~", "RW", "ENEXIS", "00-scripts")

# Colors
enx_green  <- "#FE5000"
enx_purple <- "#F68946"

# Own Coding  ---------------------------------------------
# invisible(source(file.path(p_scr, "10_import.R")))       # file handling
# invisible(source(file.path(p_scr, "20_tidy.R")))         # cleaning and structure
# invisible(source(file.path(p_scr, "30_transform.R")))    # 
# invisible(source(file.path(p_scr, "40_visualize.R")))    #
# invisible(source(file.path(p_scr, "50_model.R")))        #
# invisible(source(file.path(p_scr, "60_communicate.R")))  #
# invisible(source(file.path(p_scr, "90_general.R")))      #

