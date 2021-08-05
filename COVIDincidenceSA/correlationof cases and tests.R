p1<-ggplot(sa_cov_dat, aes(x=date2, y= Dialy_cases/Daily_tests,group=1))+
  geom_point(size = 1.5, stroke = 0, shape = 16)+
  xlab("Date")+ylab("Proportion of positive testing")+
  theme_classic()+
  labs(title="C")

p2<-ggplot(dataa,aes(x=date2, y=count,group=Type))+
  geom_line(aes(color=Type))+
  xlab("Date")+ylab("Cumulative count")+
  theme_classic()+theme(legend.position=c(0.2, 0.8))+
  labs(color='Cumulative count') +
  labs(title="B")
p2

p3<-ggplot(sa_cov_dat, aes(x=date2, y=Dialy_cases,group=1))+geom_point()+
  xlab("Date")+ylab("Daily cases")+theme_classic()+
  labs(title="A")
p3

p4<-ggplot(sa_cov_dat, aes(Dialy_cases, Daily_tests))+geom_point()+stat_cor(method = "pearson")+
  theme_classic()+ylab("Daily tests")+xlab("Daily cases")+
  labs(title="D")


library(patchwork)
p_all<-p3+p2+p1+p4

ggsave("C:/Users/user/Dropbox (The University of Manchester)/Thesis_Belay/Fig_Fold/covidplotall_review.png",p_all)
