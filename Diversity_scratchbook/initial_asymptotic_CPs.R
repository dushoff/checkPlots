###########
# take at look at some diveristy checkplots

asy_rich_large<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_1_inds_10000_outer_1_.csv")
checkplot(asy_rich_large)
asy_rich_small<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_1_inds_100_outer_1_.csv")
checkplot(asy_rich_small)




mycv<-function(x){sd(x)/mean(x)}
asy_rich_large %>% summarize(mycv(log(obsD)))
asy_rich_small%>% summarize(mycv(log(obsD)))

asy_sim_large %>% summarize(mycv(log(obsD)))
asy_sim_small%>% summarize(mycv(log(obsD)))
asy_sim_med %>% summarize(mycv(log(obsD)))

map(c("small", "large"), function(ss){
  map(c("rich", "sim"), function(l){
  myests<-get(paste0("asy_", l,"_", ss)) %>% mutate(est=chaoest)
  rangePlot(myests, orderFun=slug, target=mean(myests$truediv), opacity=0.05)+
    theme_classic()
  })
})

####################
# looks like Simspon might work fine with this community, need more reps to compare fairly
asy_sim_large<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_-1_inds_10000_outer_1_.csv")
checkplot(asy_sim_large)

asy_sim_small<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_-1_inds_100_outer_1_.csv")
checkplot(asy_sim_small)

asy_sim_med<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_-1_inds_1000_outer_1_.csv")
checkplot(asy_sim_med)
