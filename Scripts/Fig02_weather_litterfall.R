## ----setup, include=FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=F, echo=F-------------------------------------------------------------------------
require(tidyverse)
require(cowplot)
require(readxl)
require(lubridate)
require(grid)
require(gridExtra)
require(patchwork)

# Set working directory
setwd('C:/Users/hougn001/OneDrive - Wageningen University & Research/Current Downloads/Last chapter/Scripts')
rm(list=ls())

# Retrieve data from processed folder
load('../Data/Processed/femo.Rdata')
load('../Data/Processed/newfemo.Rdata')

#####
meteoNASA=meteoNASA %>% 
  mutate(Year=substr(Month,1,4),
         Month=factor(format(Month,'%b'),levels=month.abb,ordered=F))

myNASA=meteoRH %>%
  mutate(Year=as.character(YEAR),
         Month=factor(month.abb[MO],levels=month.abb)) %>%
  group_by(Location,Year,Month) %>%
  select(Rainfall_mm,T2M) %>%
  summarize(Rainfall_mm=sum(Rainfall_mm,na.rm=T),
            Temp_2m=mean(T2M,na.rm=T)) %>%
  left_join(Lfall2 %>%
              mutate(Year=as.character(year(Month)),
                     Month=factor(month.abb[month(Month)],
                                  levels=month.abb)) %>%
              group_by(Location,Year,Month) %>%
              select(Total_g_fn1,Total_g_fn2) %>%
              summarize(Total_mean=0.01*Total_g_fn1/0.4,
                        Total_sd=0.01*Total_g_fn2/0.4)) %>%
  mutate(Date=as_date(paste0(Year,'-',Month,'-',01)))

P00_ugly=ggplot(myNASA)+
  geom_col(aes(x=Date,y=Rainfall_mm*2),colour='black',fill='white')+
  geom_ribbon(aes(x=Date,ymin=Total_mean-Total_sd,
                  ymax=Total_mean+Total_sd),
              fill='pink',alpha=0.95)+
  geom_line(aes(Date,Total_mean),colour='brown')+
  scale_x_date(name=NULL,date_labels='%b %Y',date_breaks='4 month')+
  scale_y_continuous(name=expression(paste('Total litterfall, kg ',ha^{-1})),
                     sec.axis=sec_axis(~./2,name='Rainfall, mm'))+
  facet_wrap(~Location,scales='free_y')+
  theme_test()



P01a=ggplot(myNASA,aes(x=as.factor(Date),group=1))+
  geom_col(aes(y=Rainfall_mm/15),colour='black',fill='white')+
  geom_path(aes(x=as.factor(Date),y=Temp_2m))+
  scale_x_discrete(name=NULL)+
  scale_y_continuous(name=expression(paste('Temperature, ',degree*C)),
                     sec.axis=sec_axis(~.*15,name='Rainfall, mm'))+
  facet_wrap(~Location)+
  theme_test()+
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

P01b=ggplot(myNASA,aes(as.factor(Date),Total_mean))+
  geom_errorbar(aes(ymin=Total_mean-Total_sd,ymax=Total_mean+Total_sd))+
  geom_col(fill='grey50')+
  scale_x_discrete(name='Period',
               labels=c('Jan 2020',rep('',5),'Jul 2020',rep('',5),
                        'Jan 2021',rep('',5),'Jul 2021',rep('',5)))+
  scale_y_continuous(name=expression(paste('Litterfall, Mg ',ha^{-1})) )+
  facet_wrap(~Location)+
  theme_test()+
  theme(axis.text.x=element_text(angle=-60,hjust=-0.2,vjust=1.5),
        strip.text.x=element_blank(),
        strip.background=element_blank(),
        panel.border=element_rect(fill = NA))

graph1=P01a/P01b

tiff('../Paper_Graphs/Fig02.tiff',width=20,height=10,units='cm',res=600,compression='lzw')
graph1
ggsave('../Paper_Graphs/Fig02.tiff')
dev.off()
