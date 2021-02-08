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
library(lubridate)

#'------------- Load data 
tests_sa <- read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv",header=TRUE)
cases_sa <- read.csv("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv",header=TRUE)
head(tests_sa)
head(cases_sa)
#'------------- Prepare the two data sets to merge  
tests_sa<-tests_sa%>%mutate(date2=lubridate::ymd(YYYYMMDD))
cases_sa<-cases_sa%>%mutate(date2=lubridate::ymd(YYYYMMDD))

variable_keep_test<-c("date2","cumulative_tests")
variable_keep_cases<-c("date2","total")

sa_cov_dat<-left_join(cases_sa[,variable_keep_cases], tests_sa[,variable_keep_test], by="date2")
head(sa_cov_dat)
dim(sa_cov_dat[is.na(sa_cov_dat$cumulative_tests),])# There are unknown number of tests in the first 4 days 05,08,09,10-03-2020 
# sa_cov_dat[18:35,] also unknown number of tests on 25-03, and 07-04 
sa_cov_dat<-sa_cov_dat[sa_cov_dat$date2>'2020-03-09'&!is.na(sa_cov_dat$cumulative_tests),]


# You need to start from 2020-03-12 in order to properly calaculate daily cases and tests
# Compute daily cases and tests 
sa_cov_dat<-sa_cov_dat%>%mutate(Dialy_cases=-1*(lag(total)-total), Daily_tests=-1*(lag(cumulative_tests)-cumulative_tests))
sa_cov_dat<-sa_cov_dat[-1, ]
#Plot of observed data- cummulative cases, daily cases, cummulative tests 

p1<-ggplot(sa_cov_dat, aes(x=date2, y=Dialy_cases,group=1))+geom_point()+xlab("Date")+ylab("Daily cases")+theme_bw()

p2<-ggplot(sa_cov_dat, aes(x=date2, y=total,group=1))+geom_line(size = 1.5)+xlab("Date")+ylab("Cummulative cases")+theme_bw()


p3<-ggplot(sa_cov_dat, aes(x=date2, y=cumulative_tests,group=1))+geom_line(size = 1.5)+xlab("Date")+ylab("Cummulative tests")+theme_bw()

p1_3<-p1/p2/p3


# Modeling Daily Cases - Negaive Binomial - AR1 and RW1 model - INLA
# crate a new process time column from date2

sa_cov_dat<-sa_cov_dat%>%mutate(time=cumsum(c(0,diff.Date(date2))))

fomula_ar1<-Dialy_cases ~ 1 + f(time, model = "ar1",
                               hyper = list(prec = list(param = c(0.1, 0.1))))
fomula_rw1<-Dialy_cases ~ 1 + f(time, model = "rw1",
                               hyper = list(prec = list(param = c(0.1, 0.1))))

nb.ar1 <- inla(fomula_ar1,                   
               data = sa_cov_dat,
               family = "nbinomial",
               control.predictor = list(compute = TRUE),
               control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))
nb.rw1 <- inla(fomula_rw1,                   
               data = sa_cov_dat,
               family = "nbinomial",
               control.predictor = list(compute = TRUE),
               control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))


summary(nb.ar1)
summary(nb.rw1)

newdata_ar1<-cbind(sa_cov_dat[,c("date2","Dialy_cases")],nb.ar1$summary.fitted.values)
newdata_ar1 <- reshape:::rename(newdata_ar1, c("0.025quant"="lower", "0.975quant"="upper"))
p_ar1_inc<-ggplot(newdata_ar1, 
                  aes(y=mean, x=date2)) +geom_blank()+geom_point(aes(y=Dialy_cases, x=date2)) +
                  geom_ribbon(aes(ymin=lower, ymax=upper), fill='blue', alpha=0.6) +
                  geom_line(aes(y=mean, x=date2)) +xlab("Date")+
                  ylab("Daily observed and fitted cases")+
                  labs(title = "Negaive-Binomial with AR(1) model" )+
                  theme_bw()


newdata_rw1<-cbind(sa_cov_dat[,c("date2","Dialy_cases")],nb.rw1$summary.fitted.values)
newdata_rw1 <- reshape:::rename(newdata_rw1, c("0.025quant"="lower", "0.975quant"="upper"))
p_rw1_inc<-ggplot(newdata_rw1, aes(y=mean, x=date2)) +
  geom_blank()+
  geom_point(aes(y=Dialy_cases, x=date2)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='blue', alpha=0.6) +
  geom_line(aes(y=mean, x=date2)) +xlab("Date")+ylab("Daily observed and fitted cases")+
  labs(title = "Negaive-Binomial with RW(1) model" )+
  theme_bw()

p_ar1_cum<-newdata_ar1%>%
  mutate(Cumm_fitted=cumsum(mean), Cumm_fitted_lower=cumsum(lower),
         Cumm_fitted_upper=cumsum(upper),Cumm_observed=cumsum(Dialy_cases))%>%
  ggplot(aes(y=Cumm_fitted, x=date2)) +
  geom_blank()+
  geom_point(aes(y=Cumm_observed, x=date2)) +
  geom_ribbon(aes(ymin=Cumm_fitted_lower, ymax=Cumm_fitted_upper), fill='blue', alpha=0.6) +
  geom_line(aes(y=Cumm_fitted, x=date2)) +xlab("Date")+
  ylab("cummulative observed and fitted cases")+
  labs(title = "Negaive-Binomial with AR(1) model" )+
  theme_bw()
p_rw1_cum<-newdata_rw1%>%
  mutate(Cumm_fitted=cumsum(mean), Cumm_fitted_lower=cumsum(lower),
         Cumm_fitted_upper=cumsum(upper),Cumm_observed=cumsum(Dialy_cases))%>%
  ggplot(aes(y=Cumm_fitted, x=date2)) +
  geom_blank()+
  geom_point(aes(y=Cumm_observed, x=date2)) +
  geom_ribbon(aes(ymin=Cumm_fitted_lower, ymax=Cumm_fitted_upper), fill='blue', alpha=0.6) +
  geom_line(aes(y=Cumm_fitted, x=date2)) +xlab("Date")+
  ylab("cummulative observed and fitted cases")+
  labs(title = "Negaive-Binomial with RW(1) model" )+
  theme_bw()

#plot of incidence over time 
p_ar1_inc/p_rw1_inc

# plot of cummulative overtime
p_ar1_cum/p_rw1_cum


# Do Forcasting 


# BRMS option





