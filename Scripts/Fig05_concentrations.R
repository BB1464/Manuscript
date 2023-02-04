## ----setup, include=FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=F, echo=F-------------------------------------------------------------------------
require(tidyverse)
require(cowplot)
require(readxl)
require(lubridate)
require(grid)
require(gridExtra)
require(ggtext)

# Set working directory
# setwd('C:/Users/hougn001/OneDrive - Wageningen University & Research/Current Downloads/Last chapter/Scripts')
# rm(list=ls())

# Retrieve data from processed folder
load('Data/Processed/femo.Rdata')
load('Data/Processed/newfemo.Rdata')

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


Old_Fig05=ggplot(nutri_stat %>%
               mutate(Newdate=case_when(Period=='P0'~0,
                                        Period=='P1'~180,
                                        Period=='P2'~248,
                                        Period=='P3'~314,
                                        Period=='P4'~388)),
             aes(Newdate,Concentration,fill=NULL,colour=Access,group=paste0(Access,Newdate)))+
  geom_boxplot(width=30,position=position_dodge())+
  labs(x='Time, days after incubation',
       #y=expression(paste('Elemental concentration in cocoa leaf litter, mg ', g^{-1})),
       y='Elemental concentration in cocoa leaf litter, mg g^-1')+
  scale_colour_manual(values=c('blue','red2'))+
  facet_grid(Nutrient~Location,scales='free_y')+
  theme_test()+
  theme(legend.title=element_text(size=12,family = 'serif',face = 'bold',colour = 'black'),
        legend.text=element_text(size=9,family = 'serif',face = 'bold',colour = 'black'),
        legend.position=c(0.42,0.7),
        legend.key=element_rect(fill = alpha("white", .5)),
        legend.background=element_rect(fill = alpha("white", 0.5)),
        axis.title.y = element_markdown(family = 'serif',face = 'bold'),
        axis.title.x = element_text(family = 'serif',face = 'bold'),
        axis.text.x = element_text(family = 'serif',face = 'bold'),
        axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black'),
        strip.text = element_text(family = 'serif',face = 'bold',colour = 'black'),
        strip.background = element_rect(fill = 'white',colour = NULL))


Old_Fig05


 tiff('Paper_Graphs/Old_Fig05.tiff',height=15.5,width=18.5,units='cm',res=600,compression='lzw')

 ggsave(filename = here::here('Paper_Graphs/Fig05_concentrations.tiff'))

Old_Fig05
# ggsave('../Paper_Graphs/Old_Fig05.tiff')
# dev.off()

allSE=c(NULL,NULL,NULL,NULL,NULL)
concen_extract=function(x){
  res00=lmerTest::lmer(Concentration~Period*Access+(1|Location/Position),
                       nutri_stat %>% filter(Nutrient==x))
  print(sjPlot::plot_model(res00,'int')+
          labs(title=NULL,y=paste0(x,' concentration in residual litter, mg/g'))+
          theme_test())
  print(paste('=============Concentration of ',x,'===================='))
  print(anova(res00))
  print(emmeans::emmeans(res00,pairwise~Period))
  print(emmeans::emmeans(res00,pairwise~Access|Period,adj='bonferroni'))
  oneSE=emmeans::emmeans(res00,pairwise~Period|Access,adj='bonferroni')$emmeans %>%
    as_tibble() %>%
    select(Access,Period,emmean,SE) %>%
    mutate(Nutrient=x)
  allSE=rbind(allSE,oneSE)
  assign('allSE',allSE,envir=.GlobalEnv)
}

sapply(c('C','N','P','K'),concen_extract)
allSE$Nutrient=factor(allSE$Nutrient,levels=c('C','N','P','K'))
Fig05=ggplot(allSE %>%
               mutate(Newdate=case_when(Period=='P0'~0,
                                        Period=='P1'~180,
                                        Period=='P2'~248,
                                        Period=='P3'~314,
                                        Period=='P4'~388)),
             aes(Newdate,emmean,colour=Access,group=Access))+  # if needed, add shape=Access
  geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),
                width=10,position=position_dodge(width=3))+
  geom_line(position=position_dodge(width=3))+
  geom_point(position=position_dodge(width=3))+
  labs(x='Time, days after incubation',
       y=expression(paste('Concentration in cocoa leaf litter, mg ', g^{-1})) )+
  scale_colour_manual(values=c('blue','red2'))+
  facet_wrap(~Nutrient,scales='free_y')+
  theme_test()+
  theme(legend.title=element_text(size=9,family = 'serif',face = 'bold',colour = 'black'),
        legend.text=element_text(size=7,family = 'serif',colour = 'black'),
        legend.position=c(0.12,0.25),
        legend.key=element_rect(fill='white'),
        legend.background=element_rect(fill='white'),
        strip.background = element_rect(fill = 'white',colour = NULL),
        strip.text.x = element_text(family = 'serif',face = 'bold',colour = 'black'))

tiff('Paper_Graphs/Fig05.tiff',height=7.5,width=12.5,units='cm',res=600,compression='lzw')
Fig05

ggsave('../Paper_Graphs/Fig05.tiff')
dev.off()
