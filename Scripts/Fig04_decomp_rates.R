## ----setup, include=FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=F, echo=F-------------------------------------------------------------------------
require(tidyverse)
require(cowplot)
require(readxl)
require(lubridate)
require(ggpubr)
require(grid)
require(gridExtra)
require(nlme)
require(ggtext)

# Set working directory
# setwd('C:/Users/hougn001/OneDrive - Wageningen University & Research/Current Downloads/Last chapter/Scripts')
# rm(list=ls())

# Retrieve data from processed folder
load('Data/Processed/femo.Rdata')
load('Data/Processed/newfemo.Rdata')

#####data wrangling

# Decomposition data
set1=Decomposition %>%
  filter(Location!='Akowonjo-Akoko-A') %>%
  select(Date,Location,Code,Duplicate,Position,IniDry_g,Residual_g) %>%
  mutate(Code=substr(Code,15,nchar(Code)),
         Location=substr(Location,1,nchar(Location)-2),
         Time=as.numeric(Date-as_datetime('2020-01-10'))/86400) %>%
  mutate(across(where(is.character),as.factor)) %>%
  rename(InitialBag_g=IniDry_g,ResidualBag_g=Residual_g)

set2=Disappearance %>%
  select(Date,Location,Code,Duplicate,Position,IniDry_g,Residual_g) %>%
  mutate(Code=substr(Code,15,nchar(Code)),
         Location=substr(Location,1,nchar(Location)-2),
         Time=as.numeric(Date-as_datetime('2020-01-10'))/86400) %>%
  mutate(across(where(is.character),as.factor)) %>%
  rename(InitialFrame_g=IniDry_g,ResidualFrame_g=Residual_g)

decomp=merge(set1,set2) %>%
  mutate(Resbag=ResidualBag_g/InitialBag_g,
         Resframe=ResidualFrame_g/InitialFrame_g)

decomp1=pivot_longer(decomp,cols=c(Resbag,Resframe),
                     names_to='Medium',values_to='Residual') %>%
  mutate(Medium=as.factor(ifelse(Medium=='Resbag','Litterbag','Frame'))) %>%
  mutate(Tree=as.factor(case_when(as.numeric(substr(Code,2,3)) %in% 1:4~ 'Tree1',
                                  as.numeric(substr(Code,2,3)) %in% 5:8~ 'Tree2',
                                  as.numeric(substr(Code,2,3)) %in% 9:12~ 'Tree3',
                                  as.numeric(substr(Code,2,3)) %in% 13:16~ 'Tree4',
                                  as.numeric(substr(Code,2,3)) %in% 17:20~ 'Tree5')))

moredate=cbind.data.frame(Time=0:max(set2$Time,na.rm=T),Medium='Litterbag')


###############Analysis
# res03=nlme(Residual~exp(-k*Time),
#            fixed=k~Medium,
#            random=k~1|Location/Duplicate/Position,
#            method='REML',
#            start=c(0.001,0),
#            decomp1 %>% filter(Time<=400))
# summary(res03)
# intervals(res03,which='fixed')  # I think I need to bootstrap
# performance::icc(res03,by_group=T)
# multilevelTools::iccMixed('Residual',c('Time','Location','Duplicate','Medium'),decomp1)
# multilevelTools::iccMixed('Residual',c('Location','Duplicate','Medium'),decomp1)
#
# fixef(res03)
# ranef(res03)
# k03_frame=summary(res03)[['coefficients']][['fixed']][1]
# k03_bag=k03_frame+summary(res03)[['coefficients']][['fixed']][2]
# R03=paste('italic(R)^2==', round(nlraa::R2M(res03)[['R2.marginal']],2))
# lbl03_bag=paste('M=={M[0]}* e^{ ~.(-round(k03_bag,4)) t}')
# lbl03_frame=paste('M=={M[0]}* e^{ ~.(-round(k03_frame,4)) t}')
# fractionnation_coef=k03_frame/k03_bag
# # Mass loss differnce at end of incubation was (%)
# exp(-k03_bag* max(decomp1$Time))-exp(-k03_frame* max(decomp1$Time))
#
# graph05_decomp=ggplot(decomp1,aes(Time,100*Residual,colour=Medium))+
#   geom_point()+
#   geom_line(data=moredate,aes(y=100*exp(-k03_bag*Time)),colour='red2',size=0.8)+
#   geom_line(data=moredate,aes(y=100*exp(-k03_frame*Time)),colour='blue',size=0.8)+
#   scale_colour_manual(values=c('blue','red2'))+
#   labs(x='Time, days after incubation',y='Residual cocoa leaf litter mass, %',colour='Access')+
#   scale_colour_manual(labels=c('with','without'),values=c('blue','red2'))+
#   annotate('text',x=30,y=60,
#            label=bquote(M==M[0] * e^{ ~.(round(k03_bag,4)) *t} ),colour='red2')+
#   annotate('text',x=30,y=50,
#            label=bquote(M==M[0] * e^{ ~.(round(k03_frame,4)) *t}  ),colour='blue')+
#   annotate('text',x=30,y=75,label=R03,parse=T)+
#   theme_test()+
#   theme(legend.position=c(0.25,0.1),
#         legend.direction='horizontal')
#
# # tiff('../Prez_Graphs/graph05_decomp.tiff',height=7.5,width=12.5,units='cm',res=600,compression='lzw')
# # graph05_decomp
# # ggsave('../Prez_Graphs/graph05_decomp.tiff')
# # dev.off()
#
# graph05bis_decomp=ggplot(decomp1,aes(Time,100*Residual,colour=Medium))+
#   geom_point(aes(shape=Location))+
#   geom_line(data=moredate,aes(y=100*exp(-k03_bag*Time)),colour='red2',size=0.8)+
#   geom_line(data=moredate,aes(y=100*exp(-k03_frame*Time)),colour='blue',size=0.8)+
#   scale_colour_manual(values=c('blue','red2'))+
#   labs(x='Time, days after incubation',y='Residual cocoa leaf litter mass, %',colour='Access')+
#   scale_colour_manual(labels=c('with','without'),values=c('blue','red2'))+
#   annotate('text',x=30,y=60,
#            label=bquote(M==M[0] * e^{ ~.(round(k03_bag,4)) *t} ),colour='red2')+
#   annotate('text',x=30,y=50,
#            label=bquote(M==M[0] * e^{ ~.(round(k03_frame,4)) *t}  ),colour='blue')+
#   annotate('text',x=30,y=75,label=R03,parse=T)+
#   theme_test()+
#   theme(legend.position=c(0.2,0.1),
#         legend.direction='horizontal')+
#   guides(shape=F)
# # tiff('../Prez_Graphs/graph05bis_decomp.tiff',height=7.5,width=15,units='cm',res=600,compression='lzw')
# # graph05bis_decomp
# # ggsave('../Prez_Graphs/graph05bis_decomp.tiff')
# # dev.off()
#
# graph05ter_decomp=ggplot(decomp1,aes(Time,100*Residual,colour=Medium))+
#   geom_point()+
#   geom_line(data=moredate,aes(y=100*exp(-k03_bag*Time)),colour='red2',size=0.8)+
#   geom_line(data=moredate,aes(y=100*exp(-k03_frame*Time)),colour='blue',size=0.8)+
#   scale_colour_manual(values=c('blue','red2'))+
#   labs(x='Time, days after incubation',y='Residual cocoa leaf litter mass, %',colour='Access')+
#   scale_colour_manual(labels=c('with','without'),values=c('blue','red2'))+
#   facet_grid(~Location)+
#   theme_test()
# tiff('../Prez_Graphs/graph05ter_decomp.tiff',height=7.5,width=15,units='cm',res=600,compression='lzw')
# graph05ter_decomp
# ggsave('../Prez_Graphs/graph05ter_decomp.tiff')
# dev.off()

# res04=nlme(-log(Residual)~k*Time,
#            fixed=k~Medium,
#            random=k~1|Location/Duplicate/Position,
#            method='REML',
#            start=c(0.01,0),
#            decomp1 %>% filter(Time<=400))
# summary(res04)
# intervals(res04,which='fixed')  # I think I need to bootstrap
# performance::icc(res04,by_group=T)
# multilevelTools::iccMixed('Residual',c('Time','Location','Duplicate','Medium'),decomp1)
# multilevelTools::iccMixed('Residual',c('Location','Duplicate','Medium'),decomp1)
#
# fixef(res04)
# ranef(res04)
# k04_frame=summary(res04)[['coefficients']][['fixed']][1]
# k04_bag=k04_frame+summary(res04)[['coefficients']][['fixed']][2]
# R04=paste('italic(R)^2==', round(nlraa::R2M(res04)[['R2.marginal']],2))
# lbl04_bag=paste('M=={M[0]}* e^{ ~.(-round(k04_bag,4)) t}')
# lbl04_frame=paste('M=={M[0]}* e^{ ~.(-round(k04_frame,4)) t}')
k_values=c(NULL,NULL,NULL)
quick_lin_reg=function(lieu){
  print(lieu)
  res00=lm(-log(Residual)~0+Time,
           decomp1 %>% filter(Time<=400,Medium=='Frame',Location==lieu))
  k_with=coef(res00)[1]
  res01=lm(-log(Residual)~0+Time,
           decomp1 %>% filter(Time<=400,Medium=='Litterbag',Location==lieu))
  k_without=coef(res01)[1]
  k_val=cbind.data.frame(Location=levels(as.factor(decomp1$Location))[lieu],
                         k_with=round(k_with,4),
                         k_without=round(k_without,4))
  k_values=rbind(k_values,k_val)
  assign('k_values',k_values,envir=.GlobalEnv)
}
sapply(unique(decomp1$Location),quick_lin_reg)
rownames(k_values)=NULL

Fig04=ggplot(decomp1,aes(Time,-log(Residual),colour=Medium))+
  geom_point()+
  geom_smooth(method=lm,formula='y~0+x',se=F)+
  scale_colour_manual(values=c('blue','red2'))+
  labs(x='Time, days after incubation',colour='Access',
       y=expression(paste('-ln(', italic(m[t]), ' / ', italic(m[i]),')')) )+
  scale_colour_manual(labels=c('+ macrofauna','- macrofauna'),values=c('blue','red2'))+
  stat_cor(aes(label=paste(..rr.label..)),
           r.accuracy=0.01,
           label.x=0, label.y=c(1.1,0.7)) +
  annotate('text',x=10,y=0.8,colour='red2',
           label=expression(paste(italic(k['-']),'=')) )+
  annotate('text',x=10,y=1.2,colour='blue',
           label=expression(paste(italic(k['+']),'=')) )+
  geom_text(data=k_values,aes(x=100,y=0.8,label=k_without),colour='red2')+
  geom_text(data=k_values,aes(x=100,y=1.2,label=k_with),colour='blue')+
  facet_grid(~Location)+
  theme_test()+
  theme(legend.position=c(0.75,0.85),
        axis.text = element_text(family = 'serif',face = 'bold',colour = 'black'),
        axis.title = element_text(family = 'serif',face = 'bold',colour = 'black'),
        axis.title.y = element_text(family = 'serif',face = 'bold',colour = 'black'),
        strip.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),
        strip.background = element_rect(fill = 'white',colour = NULL))



# Save the Plot -----------------------------------------------------------

tiff('Paper_Graphs/Fig04.tiff',height=10,width=20,units='cm',res=600,compression='lzw')
Fig04
#ggsave('../Paper_Graphs/Fig04.tiff')

ggsave(filename = here::here('Paper_Graphs/Fig04.tiff'))

dev.off()


all_k=k_values %>%
  pivot_longer(cols=starts_with('k'),names_to='Medium',values_to='k_val')
res00=lmerTest::lmer(k_val~Medium+(1|Location),all_k)
anova(res00)
emmeans::emmeans(res00,pairwise~Medium)
