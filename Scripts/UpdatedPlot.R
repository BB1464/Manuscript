
# Setup -------------------------------------------------------------------


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



# Theme Setup -------------------------------------------------------------

theme_set(theme_test())


theme_update(axis.ticks.x=element_blank(),
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y  = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
                   axis.title.y.left   = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
                   axis.text.y.right = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
                   axis.text.y.left = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
                   axis.title.y.right = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
                   strip.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
                   strip.background = element_rect(fill = 'white',colour = NULL))



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
  facet_wrap(~Location,scales='free_y')



P01a=ggplot(myNASA,aes(x=as.factor(Date),group=1))+
  geom_col(aes(y=Rainfall_mm/15),colour='black',fill='white')+
  geom_path(aes(x=as.factor(Date),y=Temp_2m),linewidth=0.8)+
  scale_x_discrete(name=NULL)+ #\u00B0C Temp code
  scale_y_continuous(
    sec.axis=sec_axis(~.*15,name='Rainfall, mm'),expand = expansion(mult = c(0,0),add = c(0,0.6)))+
  facet_wrap(~Location)+
  labs(y = "Temperature \u00B0C")


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



# Fig03 -------------------------------------------------------------------

#####
perc_leaf=Lfall5 %>%
  filter(Annual!='2022-01-01') %>%
  mutate(Year=year(Annual)) %>%
  group_by(Year) %>%
  summarize(label=paste0(round(100*mean(Cocoa_g/Total_g),0),'%')) %>%
  mutate(Annual='')

graph_interannual=ggplot(Lfall5[Lfall5$Annual!='2022-01-01',],
                         aes(as.factor(Annual),0.01*Total_g/0.4,group=Annual))+
  geom_boxplot()+
  scale_x_discrete(labels=c('2020','2021'))+
  labs(x='Year',y=expression(paste('Total litterfall, Mg ',ha^{-1})))+
  geom_text(data=perc_leaf,mapping=aes(x=ifelse(Year==2020,1,2),y=2,label=label))

graph_interannual

tiff('../Paper_Graphs/graph03bis_interannual.tiff',height=7.5,width=10,units='cm',res=600,compression='lzw')
graph_interannual
ggsave('Paper_Graphs/graph03bis_interannual.tiff')
dev.off()

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

nutri_conc00=nutri_check %>%
  select(Location,Position,IniNconc,IniPconc,IniKconc) %>%
  inner_join(nutri_check %>%
               filter(Period=='P0') %>%
               select(Location,Position,FinCconc) %>%
               rename(IniCconc=FinCconc)) %>%
  distinct()

nutri_Lfall=Lfall5 %>%
  filter(Annual!='2022-01-01') %>%
  ungroup() %>%
  inner_join(nutri_conc00) %>%
  mutate(C=IniNconc*Cocoa_g,
         N=IniNconc*Cocoa_g,
         P=IniPconc*Cocoa_g,
         K=IniKconc*Cocoa_g) %>%
  select(Location,Annual,Position,N,P,K) %>%
  pivot_longer(cols=c(N,P,K),names_to='Nutrient',values_to='Content') %>%
  mutate(Nutrient=factor(Nutrient,levels=c('N','P','K'),ordered=F))

nutri_litter=nutri_Lfall %>%
  mutate(Annual=factor(year(Annual))) %>%
  group_by(Location,Annual,Nutrient) %>%
  summarize(MeanContent=mean(0.01*Content/0.4,na.rm=T),
            SDContent=sd(0.01*Content/0.4,na.rm=T))

carbon_Lfall5=Lfall5 %>%
  filter(Annual!='2022-01-01') %>%
  mutate(Annual=factor(year(Annual))) %>%
  ungroup() %>%
  inner_join(nutri_conc00) %>%
  mutate(C=IniCconc*Cocoa_g/1000) %>%
  select(Location,Annual,Position,C)


graph_c_locat_annual=ggplot(carbon_Lfall5,
                            aes(as.factor(Annual),0.01*C/0.4,group=Annual))+
  geom_boxplot()+
  scale_x_discrete(labels=c('2020','2021'))+
  labs(x='Year',y='C in cocoa leaf litterfall, Mg , ha^-1')+
  facet_wrap(~Location)+
  theme_test()+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black'),
        strip.background = element_rect(fill = 'white',colour = NULL),
        axis.title.y = ggtext::element_markdown(family = 'serif',face = 'bold',colour = 'black'))

graph_c_locat_annual

tiff('../Paper_Graphs/graph03_carbon_annual.tiff',height=7.5,width=10,units='cm',res=600,compression='lzw')
graph_c_locat_annual
ggsave('Paper_Graphs/graph03_carbon_annual.tiff')
dev.off()

graph_carbon_annual=ggplot(carbon_Lfall5,
                           aes(as.factor(Annual),0.01*C/0.4,group=Annual))+
  geom_boxplot()+
  scale_x_discrete(labels=c('2020','2021'))+
  labs(x='Year',y='Carbon in cocoa leaf litterfall, Mg , ha-1')+
  theme_test()+
  theme(axis.title.y = ggtext::element_markdown(family = 'serif',face = 'bold',colour = 'black'),
        axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black'),
        axis.text = element_text(family = 'serif',face = 'bold',colour = 'black'))

graph_carbon_annual

tiff('../Paper_Graphs/graph03bis_carbon_annual.tiff',height=7.5,width=10,units='cm',res=600,compression='lzw')
graph_carbon_annual
ggsave('Paper_Graphs/graph03bis_carbon_annual.tiff')
dev.off()

graph04_Lcont=ggplot(nutri_litter,
                     aes(Annual,MeanContent,fill=Nutrient))+
  geom_col(position=position_dodge(width=0.8))+
  geom_errorbar(aes(ymin=MeanContent-SDContent,
                    ymax=MeanContent+SDContent),
                width=0.3,position=position_dodge(width=0.8))+
  labs(x='Year',fill=NULL,
       y='Leaf litter nutrient ,  kg , ha-1')+
  scale_fill_manual(values=c('grey40','grey65','grey90'))+
  scale_x_discrete(labels=c('2020','2021'))+
  scale_y_continuous(expand = expansion(mult = c(0,0.1),add = c(0,0.9)))+
  facet_grid(~Location)+
  theme_test()+
  theme(legend.position=c(0.15,0.9),
        legend.direction='horizontal',
        legend.text = element_text(family = 'serif',face = 'bold',colour = 'black'),
        axis.title.y = ggtext::element_markdown(family = 'serif',face = 'bold',colour = 'black'),
        axis.title.x = ggtext::element_markdown(family = 'serif',face = 'bold',colour = 'black'),
        axis.text = element_text(family = 'serif',face = 'bold',colour = 'black'),
        strip.background = element_rect(fill = 'white',colour = NULL),
        strip.text = element_text(family = 'serif',face = 'bold',colour = 'black'))

tiff('../Paper_Graphs/graph04_nutrientinlitter.tiff',height=7.5,width=15,units='cm',res=600,compression='lzw')
graph04_Lcont
ggsave('Paper_Graphs/graph04_nutrientinlitter.tiff')
dev.off()




## Original Figure for Paper

graph04bis_Lcont=ggplot(nutri_litter %>%
                          mutate(MeanContent=ifelse(Nutrient=='P',10*MeanContent,MeanContent),
                                 SDContent=ifelse(Nutrient=='P',10*SDContent,SDContent)),
                        aes(Annual,MeanContent,fill=Nutrient))+
  geom_errorbar(aes(ymin=MeanContent-SDContent,
                    ymax=MeanContent+SDContent),
                width=0.3,position=position_dodge(width=0.8))+
  geom_col(position=position_dodge(width=0.8))+
  labs(x='Year',fill=NULL,
       y=expression(paste('Leaf litter nutrient, kg ',ha^{-1})) )+
  scale_fill_manual(values=c('grey90','#7F7F7F','#3B3B3B'))+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.4)),expression(paste('N and K, kg ',ha^{-1})),
                     sec.axis=sec_axis(~./10,name=expression(paste('P, kg ',ha^{-1}))) )+
  scale_x_discrete(labels=c('2020','2021'))+
  facet_grid(~Location)+
  theme_test()+
  theme(legend.position=c(0.16,0.9),
        legend.direction='horizontal',
        legend.text = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
        axis.title.y.left  = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
        axis.title.y.right  = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
        axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
        axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
        strip.background = element_rect(fill = 'white',colour = NULL),
        strip.text = element_text(family = 'serif',face = 'bold',colour = 'black',size=18))

tiff('../Paper_Graphs/graph04bis_nutrientinlitter.tiff',height=7.5,width=15,units='cm',res=600,compression='lzw')
graph04bis_Lcont

ggsave('Paper_Graphs/graph04bis_nutrientinlitter.tiff')
dev.off()


# Save the final Graph

ggsave(
  filename = 'Paper_Graphs/Fig04.png',
  plot = last_plot(),
  width = 12.5,
  height = 8,
  dpi = 600
)


#######################
# Analysis of total litterfall
res00=lmerTest::lmer(0.01*Total_g/0.4~Position+Annual+(1|Location/Duplicate/Code),
                     Lfall5[Lfall5$Annual!='2022-01-01',])
anova(res00)
sjPlot::plot_model(res00,'est')
performance::icc(res00,by_group=T,tolerance=exp(-1000))
emmeans::emmeans(res00,pairwise~Position,adj='bonferroni')
emmeans::emmeans(res00,pairwise~Annual,adj='bonferroni')

# Analysis of cocoa leaf litterfall
res01=lmerTest::lmer(0.01*Cocoa_g/0.4~Position+Annual+(1|Location/Duplicate/Code),
                     Lfall5[Lfall5$Annual!='2022-01-01',])
anova(res01)
sjPlot::plot_model(res01,'est')
performance::icc(res01,by_group=T,tolerance=exp(-1000))
emmeans::emmeans(res01,pairwise~Position,adj='bonferroni')
emmeans::emmeans(res01,pairwise~Annual,adj='bonferroni')

# Analysis of shade-tree leaf litterfall
res02=lmerTest::lmer(0.01*Shade_g/0.4~Position+Annual+(1|Location/Duplicate/Code),
                     Lfall5[Lfall5$Annual!='2022-01-01',])
anova(res02)
sjPlot::plot_model(res02,'est')
performance::icc(res02,by_group=T,tolerance=exp(-1000))
emmeans::emmeans(res02,pairwise~Position,adj='bonferroni')
emmeans::emmeans(res02,pairwise~Annual,adj='bonferroni')

# Analysis of other litterfall
res03=lmerTest::lmer(0.01*Other_g/0.4~Position+Annual+(1|Location/Duplicate/Code),
                     Lfall5[Lfall5$Annual!='2022-01-01',])
anova(res03)
sjPlot::plot_model(res03,'est')
performance::icc(res03,by_group=T,tolerance=exp(-1000))
emmeans::emmeans(res03,pairwise~Position,adj='bonferroni')
emmeans::emmeans(res03,pairwise~Annual,adj='bonferroni')

# Descriptive stat for Table 2 and text
# Mean + std dev per position
Lfall5[Lfall5$Annual!='2022-01-01',] %>%
  ungroup() %>%
  group_by(Position) %>%
  mutate(across(where(is.numeric),~.*0.01/0.4)) %>%
  summarize(across(where(is.numeric),c(mean,sd)))

# Proportion per position
Lfall5[Lfall5$Annual!='2022-01-01',] %>%
  ungroup() %>%
  group_by(Position) %>%
  mutate(across(where(is.numeric),~.*0.01/0.4)) %>%
  mutate(across(where(is.numeric),~./Total_g)) %>%
  summarize(across(where(is.numeric),mean))

# Overall C flows through litterfall (mean +/- sd)
carbon_Lfall5 %>%
  ungroup() %>%
  mutate(across(where(is.numeric),~.*0.01/0.4)) %>%
  summarize(across(where(is.numeric),c(mean,sd)))

# Overall nutrient flows through litterfall (mean, sd, min, max),
nutri_Lfall %>%
  mutate(Annual=factor(year(Annual))) %>%
  group_by(Location,Annual,Nutrient) %>%
  ungroup() %>%
  group_by(Nutrient) %>%
  mutate(across(where(is.numeric),~.*0.01/0.4)) %>%
  summarize_at('Content',c(mean,sd,min,max))

# The tree that received most of shade tree leaves under its canopy
Lfall5[Lfall5$Annual!='2022-01-01',] %>%
  ungroup() %>%
  group_by(Annual,Code) %>%
  arrange(Shade_g,by_group=T) %>%
  View()  # is Code=='Quantification-A05-AA-A'
# How much did it receive from shade tree leaves close and far from the stem
Lfall5[Lfall5$Annual!='2022-01-01',] %>%
  filter(Code %in% c('Quantification-A05-AA-A','Quantification-A06-AA-A')) %>%
  mutate(across(where(is.numeric),~.*0.01/0.4)) %>%
  group_by(Position) %>%  #add Annual, for details
  summarize(across(where(is.numeric),c(mean,sd)))


