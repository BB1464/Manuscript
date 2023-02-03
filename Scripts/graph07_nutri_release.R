## ----setup, include=FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=F, echo=F-------------------------------------------------------------------------
require(tidyverse)
require(cowplot)
require(readxl)
require(lubridate)
require(grid)
require(gridExtra)

# Set working directory
setwd('C:/Users/hougn001/OneDrive - Wageningen University & Research/Current Downloads/Last chapter/Scripts')
rm(list=ls())

# Retrieve data from processed folder
load('../Data/Processed/femo.Rdata')
load('../Data/Processed/newfemo.Rdata')

######
# Estimated nutrient contents of the annual litterfall volume

# First, replacing a very low C concentration which was not replaced in raw data file
Nutrient_Released$`Carbon.Conc.(mg/g)`[Nutrient_Released$`Carbon.Conc.(mg/g)`<400]=542.1

# Getting a better formatting of the raw data
nutri_check=Nutrient_Released %>%
  rename(Location=location,Medium=Incubation,
         IniDM=`Initial.Dry.Matter.Weight`,FinDM=`Final.Dry.Matter.Weight`,
         IniNconc=`Initial.N.Concentration.(mg/g)`,
         IniPconc=`Initial.P.Concentration.(mg/g)`,
         IniKconc=`Initial.K.Concentration.(mg/g)`,
         FinCconc=`Carbon.Conc.(mg/g)`,
         FinNconc=`Nitrogen.Conc(mg/g)`,
         FinPconc=`Phosphorus.Conc(mg/g)`,
         FinKconc=`Potassium.Conc(mg/g)`) %>%
  mutate(N=100*(FinDM*FinNconc/(IniDM*IniNconc)),
         P=100*(FinDM*FinPconc/(IniDM*IniPconc)),
         K=100*(FinDM*FinKconc/(IniDM*IniKconc)))

nutri=nutri_check %>%
  group_by(Period,Location,Position,Medium) %>%
  select(N,P,K) %>%
  filter(!is.na(Period)) %>%
  pivot_longer(cols=c(N,P,K),names_to='Nutrient',values_to='Residual') %>%
  mutate(Nutrient=factor(Nutrient,levels=c('N','P','K')),
         Medium=factor(ifelse(Medium=='Bag','without','with'),
                       levels=c('with','without'),ordered=F)) %>%
  rename(Access=Medium)
  

graph07_nutri=ggplot(nutri %>%
                       mutate(Newdate=case_when(Period=='P0'~0,
                                                Period=='P1'~180,
                                                Period=='P2'~248,
                                                Period=='P3'~314,
                                                Period=='P4'~388)),
                     aes(Newdate,Residual,fill=NULL,colour=Access,group=paste0(Access,Newdate)))+
  geom_hline(yintercept=100,size=0.5,linetype=3)+
  geom_boxplot(width=30,position=position_dodge())+
  labs(x='Time, days after incubation',y='Residual nutrient in cocoa leaf litter, %')+
  scale_colour_manual(values=c('blue','red2'))+
  facet_wrap(~Nutrient,scales = 'free_y')+
  theme_test()+
  theme(legend.position=c(0.085,0.25))
graph07_nutri

tiff('../Prez_Graphs/graph07_resid_nutri.tiff',height=7.5,width=12.5,units='cm',res=600,compression='lzw')
graph07_nutri
ggsave('../Prez_Graphs/graph07_resid_nutri.tiff')
dev.off()


graph07bis_nutri=ggplot(nutri,aes(Period,Residual,fill=NULL,colour=Access))+
  geom_hline(yintercept=100,size=0.5,linetype=3)+
  geom_boxplot(width=0.3,position=position_dodge())+
  labs(x='Time, days after incubation',y='Residual nutrient in cocoa leaf litter, %')+
  scale_colour_manual(values=c('blue','red2'))+
  scale_x_discrete(labels=c(0,180,248,314,388))+
  facet_grid(~Nutrient,scales = 'free_y')+
  theme_test()+
  theme(legend.position=c(0.1,0.85))
graph07bis_nutri

tiff('../Prez_Graphs/graph07bis_resid_nutri.tiff',height=7.5,width=10,units='cm',res=600,compression='lzw')
graph07bis_nutri
ggsave('../Prez_Graphs/graph07bis_resid_nutri.tiff')
dev.off()

graph07ter_nutri=ggplot(nutri %>%
                          mutate(Newdate=case_when(Period=='P0'~0,
                                                   Period=='P1'~180,
                                                   Period=='P2'~248,
                                                   Period=='P3'~314,
                                                   Period=='P4'~388)),
                        aes(Newdate,Residual,fill=NULL,colour=Access,group=paste0(Access,Newdate)))+
  geom_hline(yintercept=100,size=0.5,linetype=3)+
  geom_boxplot(width=30,position=position_dodge())+
  labs(x='Time, days after incubation',y='Residual nutrient in cocoa leaf litter, %')+
  scale_colour_manual(values=c('blue','red2'))+
  facet_grid(~Nutrient,scales = 'free_y')+
  theme_test()+
  theme(legend.position=c(0.1,0.85))

tiff('../Prez_Graphs/graph07ter_resid_nutri.tiff',height=7.5,width=10,units='cm',res=600,compression='lzw')
graph07ter_nutri
ggsave('../Prez_Graphs/graph07ter_resid_nutri.tiff')
dev.off()



graph07qua_nutri=ggplot(nutri,aes(Period,Residual,fill=NULL,colour=Access))+
  geom_hline(yintercept=100,size=0.5,linetype=3)+
  geom_boxplot(width=0.3,position=position_dodge())+
  labs(x='Sampling period, days after incubation',y='Residual nutrient in cocoa leaf litter, %')+
  scale_colour_manual(values=c('blue','red2'))+
  scale_x_discrete(labels=c('Jan','Jun','Aug','Nov','Jan 2021'))+
  facet_grid(~Nutrient,scales = 'free_y')+
  theme_test()+
  theme(legend.position=c(0.1,0.85))

tiff('../Prez_Graphs/graph07qua_resid_nutri.tiff',height=7.5,width=10,units='cm',res=600,compression='lzw')
graph07qua_nutri
ggsave('../Prez_Graphs/graph07qua_resid_nutri.tiff')
dev.off()

graph07quin_nutri=ggplot(nutri %>%
                           mutate(Newdate=case_when(Period=='P0'~0,
                                                    Period=='P1'~180,
                                                    Period=='P2'~248,
                                                    Period=='P3'~314,
                                                    Period=='P4'~388)),
                         aes(Newdate,Residual,fill=NULL,colour=Access,group=paste0(Access,Newdate)))+
  geom_hline(yintercept=100,size=0.5,linetype=3)+
  geom_boxplot(width=30,position=position_dodge())+
  labs(x='Time, days after incubation',y='Residual nutrient in cocoa leaf litter, %')+
  scale_colour_manual(values=c('blue','red2'))+
  facet_grid(Nutrient~Location,scales='free_y')+
  theme_test()+
  theme(legend.position=c(0.11,0.55))


tiff('../Prez_Graphs/graph07quin_resid_nutri.tiff',height=10.5,width=10.5,units='cm',res=600,compression='lzw')
graph07quin_nutri
ggsave('../Prez_Graphs/graph07quin_resid_nutri.tiff')
dev.off()


graph08_carbon=ggplot(nutri_check %>%
                        mutate(Newdate=case_when(Period=='P0'~0,
                                                 Period=='P1'~180,
                                                 Period=='P2'~248,
                                                 Period=='P3'~314,
                                                 Period=='P4'~388),
                               Medium=factor(Medium,levels=c('Frames','Bag'),ordered=F)),
                      aes(Newdate,FinCconc,fill=NULL,colour=Medium,group=paste0(Medium,Newdate)))+
  geom_boxplot(width=30,position=position_dodge())+
  labs(x='Time, days after incubation',
       y=expression(paste('C concentration in cocoa leaf litter, mg ',g^{-1})))+
  scale_colour_manual(name='Access',values=c('blue','red2'),labels=c('with','without'))+
  facet_grid(~Location,scales='free_y')+
  theme_test()+
  theme(legend.position=c(0.1,0.2))


tiff('../Prez_Graphs/graph08_resid_carb.tiff',height=7.5,width=12.5,units='cm',res=600,compression='lzw')
graph08_carbon
ggsave('../Prez_Graphs/graph08_resid_carb.tiff')
dev.off()

res00=lmerTest::lmer(FinCconc~Period*Access+(1|Location/Position),
                     nutri_check %>% 
                       mutate(Medium=factor(ifelse(Medium=='Bag','without','with'),
                                            levels=c('with','without'),ordered=F)) %>%
                       rename(Access=Medium))

graph08bis_carbon=sjPlot::plot_model(res00,'int')+
  labs(x='Time, days after incubation',title=NULL,
       y=expression(paste('C concentration in cocoa leaf litter, mg ',g^{-1})))+
  scale_x_continuous(labels=c(0,180,248,314,388))+
  theme_test()+
  theme(legend.position=c(0.11,0.15))

tiff('../Prez_Graphs/graph08bis_resid_carb.tiff',height=10,width=15,units='cm',res=600,compression='lzw')
graph08bis_carbon
ggsave('../Prez_Graphs/graph08bis_resid_carb.tiff')
dev.off()
