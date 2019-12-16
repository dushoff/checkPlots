###########
# take at look at some diveristy checkplots

asy_rich_large<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_1_inds_10000_outer_1_.csv")
checkplot(asy_rich_large)
asy_rich_small<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_1_inds_100_outer_1_.csv")
checkplot(asy_rich_small)







####################
# looks like Simspon might work fine with this community, need more reps to compare fairly
asy_sim_large<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_-1_inds_10000_outer_1_.csv")
checkplot(asy_sim_large)

asy_sim_small<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_-1_inds_100_outer_1_.csv")
checkplot(asy_sim_small)

asy_sim_med<-read.csv("/Rstudio_Git/Dushoff_diversity/data/SAD_7_l_-1_inds_1000_outer_1_.csv")
checkplot(asy_sim_med)
