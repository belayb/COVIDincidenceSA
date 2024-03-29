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


dataa<-gather(sa_cov_dat[,c("date2", "total", "cumulative_tests")], Outcome, count, total:cumulative_tests, factor_key=TRUE)

  p2<-ggplot(dataa,aes(x=date2, y=count,group=Outcome))+
  geom_line(aes(color=Outcome))+
  xlab("Date")+ylab("Cummulative cases")+
theme_bw()

p2<-ggplot(sa_cov_dat, aes(x=date2, y=total,group=1))+geom_line(size = 1.5)+xlab("Date")+ylab("Cummulative cases")+theme_bw()


p3<-ggplot(sa_cov_dat, aes(x=date2, y=cumulative_tests,group=1))+geom_line(size = 1.5)+xlab("Date")+ylab("Cummulative tests")+theme_bw()

p1_3<-p1/p2


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
                  labs(title = "Negative-Binomial with AR(1) model" )+
                  theme_bw()


newdata_rw1<-cbind(sa_cov_dat[,c("date2","Dialy_cases")],nb.rw1$summary.fitted.values)
newdata_rw1 <- reshape:::rename(newdata_rw1, c("0.025quant"="lower", "0.975quant"="upper"))
p_rw1_inc<-ggplot(newdata_rw1, aes(y=mean, x=date2)) +
  geom_blank()+
  geom_point(aes(y=Dialy_cases, x=date2)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='blue', alpha=0.6) +
  geom_line(aes(y=mean, x=date2)) +xlab("Date")+ylab("Daily observed and fitted cases")+
  labs(title = "Negative-Binomial with RW(1) model" )+
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
  labs(title = "Negative-Binomial with AR(1) model" )+
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
  labs(title = "Negative-Binomial with RW(1) model" )+
  theme_bw()

#plot of incidence over time 
p_ar1_inc/p_rw1_inc

# plot of cummulative overtime
p_ar1_cum/p_rw1_cum


# Do Forcasting 
tail(sa_cov_dat)
sa_cov_dat$Dialy_cases3<-sa_cov_dat$Dialy_cases
sa_cov_dat$Dialy_cases5<-sa_cov_dat$Dialy_cases
sa_cov_dat$Dialy_cases7<-sa_cov_dat$Dialy_cases

sa_cov_dat$Dialy_cases3[sa_cov_dat$time>339]<-NA
sa_cov_dat$Dialy_cases5[sa_cov_dat$time>337]<-NA
sa_cov_dat$Dialy_cases7[sa_cov_dat$time>335]<-NA

fomula_ar1_3<-Dialy_cases3 ~ 1 + f(time, model = "ar1",
                                hyper = list(prec = list(param = c(0.1, 0.1))))
fomula_rw1_3<-Dialy_cases3 ~ 1 + f(time, model = "rw1",
                                hyper = list(prec = list(param = c(0.1, 0.1))))
fomula_ar1_5<-Dialy_cases5 ~ 1 + f(time, model = "ar1",
                                 hyper = list(prec = list(param = c(0.1, 0.1))))
fomula_rw1_5<-Dialy_cases5 ~ 1 + f(time, model = "rw1",
                                 hyper = list(prec = list(param = c(0.1, 0.1))))
fomula_ar1_7<-Dialy_cases7 ~ 1 + f(time, model = "ar1",
                                 hyper = list(prec = list(param = c(0.1, 0.1))))
fomula_rw1_7<-Dialy_cases7 ~ 1 + f(time, model = "rw1",
                                 hyper = list(prec = list(param = c(0.1, 0.1))))

nb.ar1_3 <- inla(fomula_ar1_3,                   
               data = sa_cov_dat,
               family = "nbinomial",
               control.predictor = list(compute = TRUE, link = 1),
               control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))
nb.rw1_3 <- inla(fomula_rw1_3,                   
               data = sa_cov_dat,
               family = "nbinomial",
               control.predictor = list(compute = TRUE, link = 1),
               control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))
nb.ar1_5 <- inla(fomula_ar1_5,                   
                 data = sa_cov_dat,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE, link = 1),
                 control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))
nb.rw1_5 <- inla(fomula_rw1_5,                   
                 data = sa_cov_dat,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE, link = 1),
                 control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))
nb.ar1_7 <- inla(fomula_ar1_7,                   
                 data = sa_cov_dat,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE, link = 1),
                 control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))
nb.rw1_7 <- inla(fomula_rw1_7,                   
                 data = sa_cov_dat,
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE, link = 1),
                 control.compute=list(cpo=TRUE,waic=TRUE,dic=TRUE))

nb.rw1_3_pred<-nb.rw1_3$summary.fitted.values[,c(1,3,5)]
nb.rw1_5_pred<-nb.rw1_5$summary.fitted.values[,c(1,3,5)]
nb.rw1_7_pred<-nb.rw1_7$summary.fitted.values[,c(1,3,5)]
nb.ar1_3_pred<-nb.ar1_3$summary.fitted.values[,c(1,3,5)]
nb.ar1_5_pred<-nb.ar1_5$summary.fitted.values[,c(1,3,5)]
nb.ar1_7_pred<-nb.ar1_7$summary.fitted.values[,c(1,3,5)]

nb.rw1_3_pred <- reshape:::rename(nb.rw1_3_pred, c("0.025quant"="RW1_lower", "0.975quant"="RW1_upper","mean"="RW1_mean"))
nb.ar1_3_pred <- reshape:::rename(nb.ar1_3_pred, c("0.025quant"="AR1_lower", "0.975quant"="AR1_upper","mean"="AR1_mean"))
Day3_pred<-cbind(sa_cov_dat,nb.rw1_3_pred,nb.ar1_3_pred)

nb.rw1_5_pred <- reshape:::rename(nb.rw1_5_pred, c("0.025quant"="RW1_lower", "0.975quant"="RW1_upper","mean"="RW1_mean"))
nb.ar1_5_pred <- reshape:::rename(nb.ar1_5_pred, c("0.025quant"="AR1_lower", "0.975quant"="AR1_upper","mean"="AR1_mean"))
Day5_pred<-cbind(sa_cov_dat,nb.rw1_5_pred,nb.ar1_5_pred)

nb.rw1_7_pred <- reshape:::rename(nb.rw1_7_pred, c("0.025quant"="RW1_lower", "0.975quant"="RW1_upper","mean"="RW1_mean"))
nb.ar1_7_pred <- reshape:::rename(nb.ar1_7_pred, c("0.025quant"="AR1_lower", "0.975quant"="AR1_upper","mean"="AR1_mean"))
Day7_pred<-cbind(sa_cov_dat,nb.rw1_7_pred,nb.ar1_7_pred)

#----Present the result as a table as well as plot 
Day7_pred<-Day7_pred%>%
  mutate(Cum_pred_ar1=cumsum(AR1_mean),Cum_pred_ar1_lower=cumsum(AR1_lower),Cum_pred_ar1_upper=cumsum(AR1_upper),
         Cum_pred_rw1=cumsum(RW1_mean),Cum_pred_rw1_lower=cumsum(RW1_lower),Cum_pred_rw1_upper=cumsum(RW1_upper))
Day5_pred<-Day5_pred%>%
  mutate(Cum_pred_ar1=cumsum(AR1_mean),Cum_pred_ar1_lower=cumsum(AR1_lower),Cum_pred_ar1_upper=cumsum(AR1_upper),
         Cum_pred_rw1=cumsum(RW1_mean),Cum_pred_rw1_lower=cumsum(RW1_lower),Cum_pred_rw1_upper=cumsum(RW1_upper))
Day3_pred<-Day3_pred%>%
  mutate(Cum_pred_ar1=cumsum(AR1_mean),Cum_pred_ar1_lower=cumsum(AR1_lower),Cum_pred_ar1_upper=cumsum(AR1_upper),
         Cum_pred_rw1=cumsum(RW1_mean),Cum_pred_rw1_lower=cumsum(RW1_lower),Cum_pred_rw1_upper=cumsum(RW1_upper))





forcast_cum<-Day7_pred%>%
  ggplot(aes(y=total, x=date2)) +
  geom_blank()+
  geom_point(aes(y=total, x=date2)) +
  geom_line(aes(y=Cum_pred_rw1, x=date2),linetype = "dashed", color="red") +
  geom_line(aes(y=Cum_pred_ar1, x=date2),color="blue") +
  xlab("Date")+
  ylab("Cummulative observed and fitted cases")+
  labs(title = "Negative-Binomial model - 7 days a head forcasting")+
  theme_bw()

forcast_cum<-forcast_cum+annotate("segment", x = as.Date("2020-10-06","%Y-%m-%d"), xend = as.Date("2021-02-06","%Y-%m-%d"), y = 1500000, yend = 1500000,
         colour = "blue", size = 1, arrow = arrow())

forcast_cum_zoom<-Day7_pred[Day7_pred$time>324,]%>%
  ggplot(aes(y=total, x=date2)) +
  geom_blank()+
  geom_point(aes(y=total, x=date2)) +
  geom_line(aes(y=Cum_pred_rw1, x=date2),linetype = "dashed", color="red") +
  geom_line(aes(y=Cum_pred_ar1, x=date2),color="blue") +
  ylab(" ")+
  xlab("Date")+
  theme_bw()

forcast_cum_all<-forcast_cum + inset_element(forcast_cum_zoom, left =0.3, right =0.7, bottom = 0.6, top =0.99)
forcast_cum_all
# table 
Frocasting7<-Day7_pred[Day7_pred$time>335,c("date2","total","Cum_pred_rw1", "Cum_pred_rw1_lower", "Cum_pred_rw1_upper"
                                            ,"Cum_pred_ar1", "Cum_pred_ar1_lower", "Cum_pred_ar1_upper")]

Frocasting7$Pred_Int_rw1<-paste0("(",round(Frocasting7$Cum_pred_rw1_lower,digits=2), "-", round(Frocasting7$Cum_pred_rw1_upper,digits=2),")")
Frocasting7$Pred_Int_ar1<-paste0("(",round(Frocasting7$Cum_pred_ar1_lower,digits=2), "-", round(Frocasting7$Cum_pred_ar1_upper,digits=2),")")

Frocasting7[,c("date2","total","Cum_pred_rw1","Pred_Int_rw1","Cum_pred_ar1","Pred_Int_ar1")]

F1<-Frocasting7[,c("date2","total","Cum_pred_rw1","Pred_Int_rw1")]
F2<-Frocasting7[,c("date2","total","Cum_pred_ar1","Pred_Int_ar1")]
names(F1)<-c("Date","Total","Prediction","Prediction Interval")
names(F2)<-c("Date","Total","Prediction","Prediction Interval")
#dd_tab<-as.data.frame(rbind(F1,F2))
dd_tab<-rbind(F1,F2)
class(dd_tab)
