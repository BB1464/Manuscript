
## Load Required Packages

require(tidyverse)
require(cowplot)
require(readxl)
require(lubridate)
require(grid)
require(gridExtra)
require(patchwork)
require(ggtext)


# Retrieve data from processed folder
load('Data/Processed/femo.Rdata')
load('Data/Processed/newfemo.Rdata')

#Lfall_4 <- read_csv('Data/Lfall_4.csv')

#####
meteoNASA=meteoNASA %>%
  mutate(Year=substr(Month,1,4),
         Month=factor(format(Month,'%b'),levels=month.abb,ordered=F))


## I dont want to drop Covid and End Sars
Lfall_1=Quantification %>%
  filter(Location!='Akowonjo-Akoko-J') %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(across(Location, function(x) ifelse(x=='Akowonjo-Akoko-A','Akowonjo-Akoko',x))) %>%
  group_by(Location,Date) %>%
  summarise_at(c('Cocoa_g','Shade_g','Total_g'),c(mean,sd),na.rm=T)

Lfall_2=Lfall_1 %>%
  mutate(Month=floor_date(Date,'month')) %>%
  group_by(Location,Month) %>%
  summarise(across(where(is.numeric),sum,na.rm=F))


myNASA= meteoRH %>%
  mutate(Year=as.character(YEAR),
         Month=factor(month.abb[MO],levels=month.abb)) %>%
  group_by(Location,Year,Month) %>%
  select(Rainfall_mm,T2M) %>%
  summarize(Rainfall_mm=sum(Rainfall_mm,na.rm=T),
            Temp_2m=mean(T2M,na.rm=T)) %>%
  left_join(Lfall_2 %>%
              mutate(Year=as.character(year(Month)),
                     Month=factor(month.abb[month(Month)],
                                  levels=month.abb)) %>%
              group_by(Location,Year,Month) %>%
              select(Total_g_fn1,Total_g_fn2) %>%
              summarize(Total_mean=0.01*Total_g_fn1/0.4,
                        Total_sd=0.01*Total_g_fn2/0.4)) %>%
  mutate(Date=as_date(paste0(Year,'-',Month,'-',01))) %>%
  ## Add situation of covid
  mutate(Total_mean=case_when(Location=='Ago-Owu' & is.na(Total_mean) & Date=='2021-02-01'~1.292500,TRUE~Total_mean)) %>%
  mutate(Situation=case_when(Location=='Akowonjo-Akoko' & Date ==c('2020-04-01')~'Period with disruption',
                             Location=='Akowonjo-Akoko' & Date ==c('2020-05-01')~'Period with disruption',
                             Location=='Akowonjo-Akoko' & Date ==c('2020-06-01')~'Period with disruption',
                             Location=='Akowonjo-Akoko' & Date %in% c('2021-03-01','2021-04-01','2021-05-01')~'Period with disruption',
                             Location=='Ago-Owu' & Date %in% c('2020-04-01','2020-05-01','2020-06-01')~'Period with disruption',
                             Location=='Ijebu-Itele' & Date %in% c('2020-04-01','2020-05-01','2020-06-01')~'Period with disruption',TRUE~'Period without disruption'))




myNASA <-
  myNASA %>%
  ungroup() %>%
  mutate(Situation=factor(Situation,levels=c('Period without disruption','Covid-19 pandemic','Period with disruption'))) %>%
  mutate(Total_mean=case_when(Location=='Ago-Owu' & is.na(Total_mean) & Date=='2021-02-01'~1.292500,TRUE~Total_mean)) %>%
  mutate(Total_sd=case_when(Location=='Ago-Owu'& is.na(Total_sd)& Date=='2021-02-01'~0.4116650, TRUE~Total_sd)) %>%
  mutate(Total_mean=case_when(Location=='Ijebu-Itele' & is.na(Total_mean)& Date=='2021-02-01' ~0.957266,TRUE~Total_mean)) %>%
  mutate(Total_sd=case_when(Location=='Ijebu-Itele'& is.na(Total_sd)& Date=='2021-02-01'~0.3859466, TRUE~Total_sd)) %>%
  mutate(Total_mean=case_when(Location=='Akowonjo-Akoko' & is.na(Total_mean) & Date=='2021-02-01'~0.814750,TRUE~Total_mean)) %>%
  mutate(Total_sd=case_when(Location=='Akowonjo-Akoko'& is.na(Total_sd)& Date=='2021-02-01'~0.3488202, TRUE~Total_sd))






## Write myNASA dataset out so that I can make some adjustment

#write.csv(x = myNASA,file = 'Data/myNASA.csv',row.names = F)

## read the data back to R

#myNASA <- read_csv(file = 'Data/myNASA.csv')


## Second Plot


# Lfall_4 %>%
#   mutate(month=month(Month),
#          year=year(Month)) %>%
#   mutate(month=case_when(month==1~'Jan',month==2~'Feb',
#                          month==3~'Mar',month==4~'Apr',month==5~'May',
#                          month==6~'Jun',month==7~'Jul',
#                          month==8~'Aug',month==9~'Sep',
#                          month==10~'Oct',month==11~'Nov',TRUE~'Dec')) %>%
#   mutate(Period=paste0(month, ' ', year)) %>%
#   mutate(Situation=case_when(Month ==c('2020-06-01','2021-03-01')~'Civil-unrest',TRUE~'Period without unrest')) %>%
#   mutate(Situation=case_when(Month ==c(2021-04-01,2021-05-01)|Location=='Akowonjo-Akoko'~'Civil-unrest',TRUE~'Period without unrest')) %>%
#   group_by(Period,Location,Situation) %>%
#   summarise(Total_mean=0.01**Total_g/0.4) %>%
#   ungroup() %>%
#   ggplot(aes(x=Period,y=Total_mean,fill=Situation))+
#   stat_summary(geom = 'errorbar')+
#   stat_summary(geom = 'col',fun = mean)
#



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
  geom_path(aes(x=as.factor(Date),y=Temp_2m),size=0.8)+
  scale_x_discrete(name=NULL)+ #\u00B0C Temp code
  scale_y_continuous(
                    sec.axis=sec_axis(~.*15,name='Rainfall, mm'),expand = expansion(mult = c(0,0),add = c(0,0.6)))+
  facet_wrap(~Location)+
  labs(y = "Temperature \u00B0C")+
  theme_test()+
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y  = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
        axis.title.y.left   = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
        axis.text.y.right = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
        axis.text.y.left = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
        axis.title.y.right = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
        strip.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
        strip.background = element_rect(fill = 'white',colour = NULL))


P01a

P01b=ggplot(myNASA,aes(as.factor(Date),Total_mean,fill=Situation))+
  geom_errorbar(aes(ymin=Total_mean-ifelse(is.na(Total_sd),0.2620138,Total_sd),ymax=Total_mean+ifelse(is.na(Total_sd),0.2620138,Total_sd)),colour='black')+
  geom_col()+ # '#3B3B3B'
  scale_x_discrete(name='Period',
                   labels=c('Jan 2020',rep('',5),'Jul 2020',rep('',5),
                            'Jan 2021',rep('',5),'Jul 2021',rep('',5)))+
  scale_fill_manual(values = c('Period with disruption'='gray80','Period without disruption'='#3B3B3B'))+
  #scale_fill_manual(values=c('grey40','grey65','grey90'))
  #labs(y=expression(paste('Litterfall, Mg ',ha^{-1})))+
  labs(y='Litterfall,  Mg ha^-1')+
  scale_y_continuous(expand = expansion(mult = c(0,0),add = c(0,0.6)) )+
  facet_wrap(~Location)+
  theme_test()+
  theme(axis.text.x=element_text(angle=45,hjust=0.97,vjust=1,family = 'serif',face = 'bold',colour = 'black',size = 18),
        strip.text.x=element_blank(),
        strip.background=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 15),
        legend.position = c(.2,.8),
        panel.border=element_rect(fill = NA),
        axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
        axis.title.y = element_markdown(family = 'serif',face = 'bold',colour = 'black',size=18),
        axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',margin = margin(t = 10,b = 10),size=18),
        axis.ticks.x = element_line(colour = 'black',linewidth = .6,linetype = 'solid',lineend = 'butt'))

P01b

graph1=P01a/P01b

graph1

# Save the final Graph

ggsave(
  filename = 'Paper_Graphs/Fig02a.png',
  plot = last_plot(),
  width = 12.5,
  height = 8,
  dpi = 600
)

 tiff('../Paper_Graphs/Fig02.tiff',width=25,height=16,units='cm',res=600,compression='lzw')

 graph1

 ggsave(filename = here::here('Paper_Graphs/Fig02.tiff'))

dev.off()


### Second Approach for the Graph

myNASA2 <-
  myNASA %>%
  mutate(Total_mean=case_when(Location=='Ago-Owu' & is.na(Total_mean) & Date=='2020-05-01'~0.3633833,
                              Location=='Ago-Owu' & is.na(Total_mean) & Date=='2020-04-01'~0.3633833,TRUE~Total_mean)) %>%
  mutate(across(.cols=c(Total_mean),function(x) ifelse(Location=='Ago-Owu' & Date=='2020-06-01',0.3633833,x))) %>%
  mutate(Total_sd=case_when(Location=='Ago-Owu' & is.na(Total_sd) & Date=='2020-05-01'~0.1479865,
                            Location=='Ago-Owu' & is.na(Total_sd) & Date=='2020-04-01'~0.1479865,TRUE~Total_sd)) %>%
  mutate(across(.cols=c(Total_sd),function(x) ifelse(Location=='Ago-Owu' & Date=='2020-06-01',0.1479865,x))) %>%
  mutate(Total_mean=case_when(Location=='Akowonjo-Akoko' & is.na(Total_mean) & Date=='2020-05-01'~0.9539583,
                              Location=='Akowonjo-Akoko' & is.na(Total_mean) & Date=='2020-04-01'~0.9539583,TRUE~Total_mean)) %>%
  mutate(across(.cols=c(Total_mean),function(x) ifelse(Location=='Akowonjo-Akoko' & Date=='2020-06-01',0.9539583,x))) %>%
  mutate(Total_sd=case_when(Location=='Akowonjo-Akoko' & is.na(Total_sd) & Date=='2020-05-01'~0.4030115,
                            Location=='Akowonjo-Akoko' & is.na(Total_sd) & Date=='2020-04-01'~0.4030115,TRUE~Total_sd)) %>%
  mutate(across(.cols=c(Total_sd),function(x) ifelse(Location=='Akowonjo-Akoko' & Date=='2020-06-01',0.4030115,x))) %>%

  mutate(Total_mean=case_when(Location=='Akowonjo-Akoko' & is.na(Total_mean) & Date=='2021-03-01'~0.8673333,
                              Location=='Akowonjo-Akoko' & is.na(Total_mean) & Date=='2021-04-01'~0.8673333,TRUE~Total_mean)) %>%
  mutate(across(.cols=c(Total_mean),function(x) ifelse(Location=='Akowonjo-Akoko' & Date=='2021-05-01',0.8673333,x))) %>%
  mutate(Total_sd=case_when(Location=='Akowonjo-Akoko' & is.na(Total_sd) & Date=='2021-03-01'~0.34436,
                            Location=='Akowonjo-Akoko' & is.na(Total_sd) & Date=='2021-04-01'~0.34436,TRUE~Total_sd)) %>%
  mutate(across(.cols=c(Total_sd),function(x) ifelse(Location=='Akowonjo-Akoko' & Date=='2021-05-01',0.34436,x))) %>%
  mutate(Total_mean=case_when(Location=='Ijebu-Itele' & is.na(Total_mean) & Date=='2020-05-01'~0.6034583,
                              Location=='Ijebu-Itele' & is.na(Total_mean) & Date=='2020-04-01'~0.6034583,TRUE~Total_mean)) %>%
  mutate(across(.cols=c(Total_mean),function(x) ifelse(Location=='Ijebu-Itele' & Date=='2020-06-01',0.6034583,x))) %>%
  mutate(Total_sd=case_when(Location=='Ijebu-Itele' & is.na(Total_sd) & Date=='2020-05-01'~0.2437378,
                            Location=='Ijebu-Itele' & is.na(Total_sd) & Date=='2020-04-01'~0.2437378,TRUE~Total_sd)) %>%
  mutate(across(.cols=c(Total_sd),function(x) ifelse(Location=='Ijebu-Itele' & Date=='2020-06-01',0.2437378,x)))


## Plot


P01b=ggplot(myNASA2,aes(as.factor(Date),Total_mean,fill=Situation))+
  geom_errorbar(aes(ymin=Total_mean-ifelse(is.na(Total_sd),0.2620138,Total_sd),ymax=Total_mean+ifelse(is.na(Total_sd),0.2620138,Total_sd)),colour='black')+
  geom_col()+ # '#3B3B3B'
  scale_x_discrete(name='Period',
                   labels=c('Jan 2020',rep('',5),'Jul 2020',rep('',5),
                            'Jan 2021',rep('',5),'Jul 2021',rep('',5)))+
  scale_fill_manual(values = c('Period with disruption'='gray80','Period without disruption'='#3B3B3B'))+
  #labs(y=expression(paste('Litterfall, Mg ',ha^{-1})))+
  labs(y='Litterfall,  Mg ha^-1')+
  scale_y_continuous(expand = expansion(mult = c(0,0),add = c(0,0.6)) )+
  facet_wrap(~Location)+
  theme_test()+
  theme(axis.text.x=element_text(angle=45,hjust=0.97,vjust=1,family = 'serif',face = 'bold',colour = 'black',size = 18),
        strip.text.x=element_blank(),
        strip.background=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 15),
        legend.position = c(.2,.8),
        panel.border=element_rect(fill = NA),
        axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
        axis.title.y.left  = element_markdown(family = 'serif',face = 'bold',colour = 'black',size=18),
        axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',margin = margin(t = 10,b = 10),size=18),
        axis.ticks.x = element_line(colour = 'black',linewidth = .6,linetype = 'solid',lineend = 'butt'))

graph1=P01a/P01b

graph1

# Save the final Graph

ggsave(
  filename = 'Paper_Graphs/Fig02b.png',
  plot = last_plot(),
  width = 12.5,
  height = 8,
  dpi = 600
)

tiff('../Paper_Graphs/Fig02.tiff',width=25,height=16,units='cm',res=600,compression='lzw')

graph1

ggsave(filename = here::here('Paper_Graphs/Fig02.tiff'))

dev.off()









