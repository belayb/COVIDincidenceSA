#' This script loads data from git.hub -- rep 
#' Train a model to predict 1, 2, 3, 4, 5 days a head 
#' rw1 and ar1 model with poisson and negative binomial distribution will be used 
#' Model fitting will be based on inla 

#'---------------Load packages 
library(ggplot2)
library(visreg)
library(INLA)
#'library(visibly)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(patchwork)

#'------------- Load data 
tests_sa <- read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv",header=TRUE)
cases_sa <- read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv",header=TRUE)
head(tests_sa)
head(cases_sa)
#'------------- Prepare the two data sets to merge  
tests_sa<-tests_sa%>%
  mutate(date <- as.Date(date, "%d-%m-%y"))
cases_sa<-cases_sa%>%
  mutate(date <- as.Date(date, "%d-%m-%y"))
variable_keep_test<-c("date","cumulative_tests")
variable_keep_cases<-c("date","total")

sa_cov_dat<-left_join(cases_sa[,variable_keep_cases], tests_sa[,variable_keep_test], by="date")
head(sa_cov_dat)
dim(sa_cov_dat[is.na(sa_cov_dat$cumulative_tests),])# There are unknown number of tests in the first 4 days 05,08,09,10-03-2020 
# sa_cov_dat[18:35,] also unknown number of tests on 25-03, and 07-04 
sa_cov_dat<-sa_cov_dat[sa_cov_dat$date>'09-03-2020'&!is.na(sa_cov_dat$cumulative_tests),]
head(sa_cov_dat)
typeof(sa_cov_dat$date)

ggplot(sa_cov_dat, aes(x=as.Date(date, "%d-%m-%y"), y=total,group=1))+geom_line()

