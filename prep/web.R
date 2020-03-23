library(tibble)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)

ano <- 2015
cdg_url <- sprintf('http://www.chancedegol.com.br/br%02d.htm', ano - 2000)
cdg_url
