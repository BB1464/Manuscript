# GUESTIMATE THE NUmber of required litter traps per plot
# Number of litter traps per plot used in previous studies
# Sari=2, Dawoe=4,Mughaly=20, Saj=3, Yao=3

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
load('Data/Processed/femo.Rdata')
load('Data/Processed/newfemo.Rdata')

# x is the number of simulations to run, must be < C3_10~~120
compiled=c(NULL,NULL,NULL,NULL,NULL)
bobi=function(x){
  for(i in 3:10){
    Lcut=Lfall5 %>%
      ungroup() %>%
      filter(Annual!='2022-01-01') %>%
      group_by(Annual,Location,Duplicate) %>%
      filter(Code %in% sample(Code,i)) %>%
      summarize(N=n(),SD=sd(Total_g),SE=SD/sqrt(N))
    Lcut=cbind(Simul=x,Lcut)
    compiled=rbind(compiled,Lcut)
  }
  assign('compiled',compiled,envir=.GlobalEnv)
}
sapply(1:100,bobi)

compiled %>%
  group_by(Annual,Location,N) %>%
  summarize(vv=mean(SE))

ggplot(compiled,aes(N,SE,colour=Duplicate))+
  geom_point()+
  facet_grid(Annual~Location)+
  theme_test()

ggplot(compiled %>%
         group_by(Annual,Location,Duplicate,N) %>%
         summarize(across(where(is.numeric),mean)),
       aes(N,SE,colour=Duplicate))+
  geom_line()+
  geom_point()+
  labs(x='Number of litter traps',y='Averaged SE')+
  facet_grid(Annual~Location)+
  theme_bw()
