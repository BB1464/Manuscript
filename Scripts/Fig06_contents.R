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
         FinNconc=`Nitrogen.Conc(mg/g)`,
         FinPconc=`Phosphorus.Conc(mg/g)`,
         FinKconc=`Potassium.Conc(mg/g)`,
         FinCconc=`Carbon.Conc.(mg/g)`) %>%
  mutate(N=FinDM*FinNconc,
         P=FinDM*FinPconc,
         K=FinDM*FinKconc,
         C=FinDM*FinCconc)

nutri_01=nutri_check %>%
  group_by(Period,Location,Position,Medium) %>%
  select(C,N,P,K) %>%
  filter(!is.na(Period)) %>%
  pivot_longer(cols=c(C,N,P,K),names_to='Nutrient',values_to='ResidAmount') %>%
  mutate(Nutrient=factor(Nutrient,levels=c('C','N','P','K')),
         Medium=factor(ifelse(Medium=='Bag','- macrofauna','+ macrofauna'),
                       levels=c('+ macrofauna','- macrofauna'),ordered=F)) %>%
  rename(Access=Medium)

nutri_02=nutri_check %>%
  group_by(Period,Location,Position,Medium) %>%
  select(FinCconc,FinNconc,FinPconc,FinKconc) %>%
  filter(!is.na(Period)) %>%
  pivot_longer(cols=c(FinCconc,FinNconc,FinPconc,FinKconc),
               names_to='Nutrient',values_to='Concentration') %>%
  mutate(Nutrient=substr(Nutrient,4,4),
         Nutrient=factor(Nutrient,levels=c('C','N','P','K')),
         Medium=factor(ifelse(Medium=='Bag','- macrofauna','+ macrofauna'),
                       levels=c('+ macrofauna','- macrofauna'),ordered=F)) %>%
  rename(Access=Medium)

nutri_stat=inner_join(nutri_01,nutri_02); rm(nutri_01,nutri_02)

nutri_stat=nutri_stat %>%
  inner_join(nutri_stat %>%
               filter(Period=='P0') %>%
               ungroup() %>%
               select(Location,Position,Access,Nutrient,ResidAmount) %>%
               rename(IniAmount=ResidAmount)) %>%
  mutate(Losses=100*ResidAmount/IniAmount)

nutri_03=nutri_stat %>% 
  filter(Access=='+ macrofauna') %>%
  ungroup() %>%
  group_by(Period,Location,Position) %>%
  select(!c(Access,Concentration),ResidAmount) %>%
  rename(Resid_with=ResidAmount)

nutri_04=nutri_stat %>% 
  filter(Access=='- macrofauna') %>%
  ungroup() %>%
  group_by(Period,Location,Position) %>%
  select(!c(Access,Concentration),ResidAmount) %>%
  rename(Resid_without=ResidAmount)

nutri_diff=inner_join(nutri_03,nutri_04) %>%
  mutate(Resid_diff=Resid_without-Resid_with) %>%
  select(!c(Resid_with,Resid_without)); rm(nutri_03,nutri_04)

nutri_ratio=nutri_check %>%
  group_by(Period,Location,Position,Medium) %>%
  select(FinCconc,FinNconc,FinPconc,FinKconc) %>%
  mutate(C_N=FinCconc/FinNconc,C_P=FinCconc/FinPconc,
         N_P=FinNconc/FinPconc,N_K=FinNconc/FinKconc,P_K=FinPconc/FinKconc,
         Medium=factor(ifelse(Medium=='Bag','- macrofauna','+ macrofauna'),
                       levels=c('+ macrofauna','- macrofauna'),ordered=F)) %>%
  rename(Access=Medium)


Fig05=ggplot(nutri_stat %>%
               mutate(Newdate=case_when(Period=='P0'~0,
                                        Period=='P1'~180,
                                        Period=='P2'~248,
                                        Period=='P3'~314,
                                        Period=='P4'~388)),
             aes(Newdate,Concentration,fill=NULL,colour=Access,group=paste0(Access,Newdate)))+
  geom_boxplot(width=30,position=position_dodge())+
  labs(x='Time, days after incubation',
       y=expression(paste('Elemental concentration in cocoa leaf litter, mg ', g^{-1})) )+
  scale_colour_manual(values=c('blue','red2'))+
  facet_grid(Nutrient~Location,scales='free_y')+
  theme_test()+
  theme(legend.title=element_text(size=9),
        legend.text=element_text(size=7),
        legend.position=c(0.46,0.5),
        legend.key=element_rect(fill='grey90'),
        legend.background=element_rect(fill='grey90'))

# tiff('../Paper_Graphs/Fig05.tiff',height=12.5,width=12.5,units='cm',res=600,compression='lzw')
# Fig05
# ggsave('../Paper_Graphs/Fig05.tiff')
# dev.off()

Fig06=ggplot(nutri_stat %>%
               mutate(Newdate=case_when(Period=='P0'~0,
                                        Period=='P1'~180,
                                        Period=='P2'~248,
                                        Period=='P3'~314,
                                        Period=='P4'~388)),
             aes(Newdate,Losses,fill=NULL,colour=Access,group=paste0(Access,Newdate)))+
  geom_hline(yintercept=100,size=0.5,linetype=3)+
  geom_boxplot(width=30,position=position_dodge())+
  labs(x='Time, days after incubation',y='Residual amount in cocoa leaf litter, % initial')+
  scale_colour_manual(values=c('blue','red2'))+
  facet_grid(Nutrient~Location,scales='free_y')+
  theme_test()+
  theme(legend.title=element_text(size=9),
        legend.text=element_text(size=7),
        legend.position=c(0.46,0.5),
        legend.key=element_rect(fill='grey90'),
        legend.background=element_rect(fill='grey90'))

tiff('../Paper_Graphs/Fig06.tiff',height=12.5,width=12.5,units='cm',res=600,compression='lzw')
Fig06
ggsave('../Paper_Graphs/Fig06.tiff')
dev.off()


# Comparing the % losses between elements (especially at end of experiment)
# If possible slope of losses would be compared! (but non linear process)
res00=lmerTest::lmer(Losses~Nutrient*Period*Access+(1|Location/Position),nutri_stat)
anova(res00)
emmeans::emmeans(res00,pairwise~Nutrient|Period,adj='bonferroni')
