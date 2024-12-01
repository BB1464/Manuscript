#################################################
############Chap 3-Nutrient leaching#############
#################################################

require(MatchIt)
require(lattice)
require(predictmeans)
require(lmerTest)
require(xlsx)
require(nlme)
require(ggplot2)
require(cowplot)
require(lmtest)
require(data.table)
require(BSDA)
require(car)


#clear environment
rm(list = ls(all.names = TRUE))

#For a nice background in ggplot
cleanup <-theme(panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                panel.background=element_blank(),
                axis.line=element_line(color="black"))


# for plotting residuals +  histogram + normal quantile plot
resid.inspect<-function(lmm1,col="black"){
  par(mfrow=c(1,3))
  hist(residuals(lmm1)/sd(residuals(lmm1),na.rm=T),30,main="")
  plot(fitted(lmm1),residuals(lmm1)/sd(residuals(lmm1),na.rm=T),col=col)
  qqnorm(residuals(lmm1)/sd(residuals(lmm1),na.rm=T))
  abline(coef=c(0,1))
  return(residuals(lmm1)/sd(residuals(lmm1),na.rm=T))
}

##Combined legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

###Unload a package
detach_package<-function(pkg,character.only=F)
{
  if(!character.only)
  {
    pkg<-deparse(substitute(pkg))
  }
  search_item<-paste('package',pkg,sep=':')
  while(search_item %in% search())
  {
    detach(search_item,unload=T,character.only=T)
  }
}


#######Tricks to fix failure to converge in lme
# basically  it is about increasing the number of iterations with different optimizers
# For nlme, change lmeControl to nlmeControl, and include pnlsmaxIter
ctrl0=lmeControl(opt='optim',maxIter=50)
ctrl1=lmeControl(opt='optim',maxIter=200,msMaxIter=200)
ctrl2=lmeControl(optimizer='Nelder_Mead')
ctrl3=lmeControl(optimizer='bobyqa')
ctrl4=ctrl2;ctrl4$maxIter=200;ctrl4$msMaxIter=200
ctrl5=ctrl3;ctrl5$maxIter=200;ctrl5$msMaxIter=200
ctrl6=nlmeControl(optimizer='optim')
ctrl7=nlmeControl(optimizer='nlminb',maxIter=200,msMaxIter=200,pnlsMaxIter=50)
ctrl8=nlmeControl(optimizer='optim',maxIter=200,msMaxIter=200,pnlsMaxIter=50)



####Examples of  tricks to reduce heterosedasticity in othernut (later use weights=vivo_n as argument)
# The function models the structure of the variance (weights var according to formula)
# But identifying the cause will be more efficient than randomly choosing to fix the problem
vivo0=varFixed(~Day)
vivo1=varIdent(form=~fitted(.))
vivo2=varExp(form=~Loss)
vivo3=varExp(form=~Loss|Day)
vivo4=varExp(form=~fitted(.))
vivo5=varPower(form=~fitted(.))
vivo6=varPower(form~Nutrient)
vivo7=varExp(form=~Day)
vivo8=varExp(form=~Loss|Nutrient*Day)
############################################################################################
############################################################################################

###The dataset###

#set the working directory


check04=read.csv(file='2019-10-10 check04.csv')      # Residual K from leaching trial- exp1
check05=read.csv(file='2019-10-10 check05.csv')      # to see whether residual K at the end of
                                                     # the experiment follows the predicted decomposition rate
Kside=read.csv(file='2019-10-10 Kside.csv')          # K leaching in waterlogging- exp2
Kdecom=read.csv(file='2019-10-10 Kdecom.csv')        # decomposition rate- exp3
othernutri=read.csv(file='2019-10-10 othernutri.csv') #all the nutrients


###############################################
###ANALYSIS OF DECOMPOSITION   CURVE (EXP 1)###

dato=Kdecom
dato$Frac=dato$ResidualDM/dato$InitialDM
dato=dato[which(dato$ResidualDM<dato$InitialDM),]      # Removing 1 value with residual higher than initial
# Further removing 1 value with residual (Pod P27, Batch B2, Chunck C34, Time 6) unexpectedly higher than previous days
dato$Frac[dato$Time>5 & dato$Frac>0.9]=NA
dato=na.omit(dato)

#Olson mdel
decomp1=nlme(Frac~exp(-k*Time),   # using nlme for the random effefct from pod
             data=dato,
             fixed=k~1,
             random=k~1,
             groups=~Pod,
             method='REML',
             start=1)
summary(decomp1)
RMSE_decomp1=sqrt(sum(residuals(decomp1)^2)/nrow(as.data.frame(decomp1$residuals)))
R_ols=summary(lm(predict(decomp1)~dato$Frac))$r.squared

#Yang & Janssen model
decomp2=nlme(Frac~exp(-k*(Time^a)),
             data=dato,
             fixed=k+a~1,
             random=k+a~1,
             groups=~Pod,
             method='REML',
             control=ctrl4,
             start=c(1,0.5))      #Beware of local optimum, using a=0.3 gives similar estimates but higher Rsquare (0.91)
summary(decomp2)
RMSE_decomp2=sqrt(sum(residuals(decomp2)^2)/nrow(as.data.frame(decomp2$residuals)))
R_yan=summary(lm(predict(decomp2)~dato$Frac))$r.squared

intervals(decomp1,which='fixed')   # for 95% CI Olson model
intervals(decomp2,which='fixed')   # for 95% CI Yang & Janssen model
RMSE_decomp1
RMSE_decomp2
R_ols
R_yan

#prediction of C loss at days 14, 30, 365
t14=data.frame(Time=c(0,14,30, 365))
1-predict(decomp2,newdata=t14,level=0)

Time=seq(0,70,by=0.5)    # replace 70 with 365 if one wants to predict over 1 year
Frac=exp(-fixef(decomp2)[1]*(Time^fixef(decomp2)[2]))
gag1=data.frame(cbind(Time,Frac))

P04=ggplot(dato,aes(x=Time,y=100*Frac))+
  geom_point(aes(shape=paste(Batch,Pod)),
             size=2.5)+
  geom_line(data=gag1,aes(x=Time,100*Frac),
            colour='black',
            size=1,
            linetype=1)+
  xlab('Time (days)')+
  ylab('Residual DM weight (%)')+
  #ggtitle('CPH decay at 20deg C 100% RH')+
  theme(legend.position='none')+
  cleanup

#P04 #Decomposition curve with Yang & Janssen
label0=round(fixef(decomp2)[1],2)
label1=round(fixef(decomp2)[2],2)
label1=paste('C==e^{-round(fixef(decomp2)[1],2)*t^{round(fixef(decomp2)[2],2)}}')
label2=paste('R^2==', round(R_yan,2))
P04_1=P04+
  annotate('text',x=60,y=80,colour='black',
           label=label2,parse=T)+
  annotate('text',x=60,y=90,colour='black',
           label=as.character(expression('C==e^{-0.09* t^{0.42}}')),parse=T)  #label0=0.09 and label1=0.42

pdf('fig2.pdf')
P04_1     # Fig:02 in result section
ggsave('fig2.pdf',height=5,width=5*1.5)

knitr::stitch_rmd('2020-07-03_fig 2.R')
######################################################################################
######################################################################################
######################################################################################
