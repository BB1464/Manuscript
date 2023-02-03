## --setup, include=FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --warning=F, echo=F-------------------------------------------------------------------------
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
         Medium=factor(ifelse(Medium=='Bag','without','with'),
                       levels=c('with','without'),ordered=F)) %>%
  rename(Access=Medium)

nutri_02=nutri_check %>%
  group_by(Period,Location,Position,Medium) %>%
  select(FinCconc,FinNconc,FinPconc,FinKconc) %>%
  filter(!is.na(Period)) %>%
  pivot_longer(cols=c(FinCconc,FinNconc,FinPconc,FinKconc),
               names_to='Nutrient',values_to='Concentration') %>%
  mutate(Nutrient=substr(Nutrient,4,4),
         Nutrient=factor(Nutrient,levels=c('C','N','P','K')),
         Medium=factor(ifelse(Medium=='Bag','without','with'),
                       levels=c('with','without'),ordered=F)) %>%
  rename(Access=Medium)

nutri_stat=inner_join(nutri_01,nutri_02); rm(nutri_01,nutri_02)

nutri_03=nutri_stat %>% 
  filter(Access=='with') %>%
  ungroup() %>%
  group_by(Period,Location,Position) %>%
  select(!c(Access,Concentration),ResidAmount) %>%
  rename(Resid_with=ResidAmount)

nutri_04=nutri_stat %>% 
  filter(Access=='without') %>%
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
         Medium=factor(ifelse(Medium=='Bag','without','with'),
                       levels=c('with','without'),ordered=F)) %>%
  rename(Access=Medium)

concen_quick=function(x){
  res00=lmerTest::lmer(Concentration~Period*Access+(1|Location/Position),
                       nutri_stat %>% filter(Nutrient==x))
  print(sjPlot::plot_model(res00,'int')+
          labs(title=NULL,y=paste0(x,' concentration in residual litter, mg/g'))+
          theme_test())
  print(paste('=============Concentration of ',x,'===================='))
  print(anova(res00))
  print(emmeans::emmeans(res00,pairwise~Period))
  print(emmeans::emmeans(res00,pairwise~Access|Period,adj='bonferroni'))
}

amount_quick=function(x){
  res01=lmerTest::lmer(ResidAmount~Period*Access+(1|Location/Position),
                       nutri_stat %>% filter(Nutrient==x))
  print(sjPlot::plot_model(res01,'int')+
          labs(title=NULL,y=paste0(x,' in residual litter, mg per sample'))+
          theme_test())
  print(paste('========Amounts of ',x,'===================='))
  print(anova(res01))
  print(emmeans::emmeans(res01,pairwise~Period))
  print(emmeans::emmeans(res01,pairwise~Access|Period,adj='bonferroni'))
  
}


ratio_quick=function(x){
  res02=lmerTest::lmer(nutri_ratio[[x]]~Period*Access+(1|Location/Position),nutri_ratio)
  print(sjPlot::plot_model(res02,'int')+
          labs(title=x,y='Ratio in residual litter')+
          theme_test())
  print(paste('====================',x,'ratio======================'))
  print(anova(res02))
  print(summary(res02))
  print(emmeans::emmeans(res02,pairwise~Period,adj='bonferroni'))
  print(emmeans::emmeans(res02,pairwise~Access|Period,adj='bonferroni'))
  
}

amount_diff=function(x){
  res03=lmerTest::lmer(Resid_diff~Period+(1|Location/Position),
                       nutri_diff %>% filter(Nutrient==x))
  print(sjPlot::plot_model(res03,'pred')+
          labs(title=NULL,y=paste0(x,' in residual litter, mg per sample'))+
          theme_test())
  print(paste('========Amounts of ',x,'related to macrofauna============='))
  print(anova(res03))
  print(summary(res03))
  print(emmeans::emmeans(res03,pairwise~Period,adj='bonferroni'))
  print(emmeans::emmeans(res03,pairwise~1|Period,adj='bonferroni'))
}

amount_diff_per_period=function(x){
  for(pp in unique(nutri_diff$Period)){
    res04=t.test(nutri_diff %>%
                   ungroup() %>%
                   filter(Nutrient==x,Period==pp) %>%
                   select(Resid_diff))
    print(paste('========Amounts of ',x,'related to macrofauna','in period',pp,' ============='))
    print(res04)
  }
}

sapply(c('C','N','P','K'),concen_quick)
sapply(c('C_N','C_P','N_P','N_K','P_K'),ratio_quick)
sapply(c('C','N','P','K'),amount_quick)
sapply(c('C','N','P','K'),amount_diff)
sapply(c('C','N','P','K'),amount_diff_per_period)

## Only C concentration was significantly affected by macrofauna
## Consequently C:N ratio significantly decreased with access of macrofauna

