## Utility Functions for MTJ2W Projects
## Author: Zheng Zhou
## Date: Feb 27 2022
## Update: Apr 21 2022

# Global Settings----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(seed)

# Load Packages------
library(dplyr)
library(stringr)

# Functions--------

FullName <- function(
  first,                   # a vector of strings
  last                    # a vector of strings
){
  full <- str_c(first," ", last)
  return(full)
}


