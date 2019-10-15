
# ---
# List of libraries need it for the analysis: 
## As we are using packrat to manage the dependecies of this analysis, all libraries
## should be installed. But to avoid confusion we check that this is the case: 
# ---

# the following function was copied from: 
# https://gist.github.com/stevenworthington/3178163


#ipak <- function(pkg){
#  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#  if (length(new.pkg)) 
#    install.packages(new.pkg, dependencies = TRUE)
#  sapply(pkg, require, character.only = TRUE)
#}

# usage
#packages <- c("tidyverse", "ggridges", 
#              "ggpubr", "cowplot", "sp", "egg", 
#              "psych", "xtable", "grid", "Hmisc", "jtrans", 
#              "nortest", "boot", "emmeans")
#ipak(packages)

suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("ggridges"))
suppressPackageStartupMessages(library("ggpubr"))
suppressPackageStartupMessages(library("cowplot"))
suppressPackageStartupMessages(library("sp"))
suppressPackageStartupMessages(library("egg"))
suppressPackageStartupMessages(library("psych"))
suppressPackageStartupMessages(library("xtable"))
suppressPackageStartupMessages(library("grid"))
suppressPackageStartupMessages(library("Hmisc"))
suppressPackageStartupMessages(library("jtrans"))
suppressPackageStartupMessages(library("nortest"))
suppressPackageStartupMessages(library("boot"))
suppressPackageStartupMessages(library("emmeans"))
suppressPackageStartupMessages(library("vegan"))

#---
# Import homebrew functions and colour pallete: 
#---

source("R/corstar.R")
source("R/Qfly_palette .R")

#---
# Import Climate data 
#---

source("R/weather_variables.R")

#if(!exists("weather_variables")){
#  if(!file.exists("data/weather_variables.Rdata")){
#    source("R/weather_variables.R")
#  }
#}
