
## ----warning=F, echo=F-------------------------------------------------------------------------
require(tidyverse)
require(cowplot)
require(readxl)
require(lubridate)
require(grid)
require(gridExtra)


# Retrieve data from processed folder
load('Data/Processed/femo.Rdata')
load('Data/Processed/newfemo.Rdata')

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
  geom_text(data=perc_leaf,mapping=aes(x=ifelse(Year==2020,1,2),y=2,label=label))+
  theme_test()

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
        legend.text = element_text(family = 'serif',face = 'bold',colour = 'black'),
        axis.title.y = element_text(family = 'serif',face = 'bold',colour = 'black'),
        axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black'),
        axis.text = element_text(family = 'serif',face = 'bold',colour = 'black'),
        strip.background = element_rect(fill = 'white',colour = NULL),
        strip.text = element_text(family = 'serif',face = 'bold',colour = 'black'))

tiff('../Paper_Graphs/graph04bis_nutrientinlitter.tiff',height=7.5,width=15,units='cm',res=600,compression='lzw')
graph04bis_Lcont
ggsave('Paper_Graphs/graph04bis_nutrientinlitter.tiff')
dev.off()


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
