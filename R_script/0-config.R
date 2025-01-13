#config file
rm(list = ls()) # Clear the environment
#Load libraries
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(stringr)
library(geosphere)
library(patchwork)
library(tidyr)
library(scales)
####
set.seed(617)
#Setting variables for paths
box.path <- Sys.getenv("BOX")
#Loading the script where functions are located
source(file.path(here::here(),"0-functions.R"))


