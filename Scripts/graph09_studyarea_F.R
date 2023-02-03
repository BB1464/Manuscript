require(naijR)


# Set working directory
setwd('C:/Users/hougn001/OneDrive - Wageningen University & Research/Current Downloads/Last chapter/Scripts')
rm(list=ls())

tiff('../Prez_Graphs/graph09_studyarea_F.tiff',height=20,width=30,units='cm',res=600,compression='lzw')
map_ng('Nigeria')
map_ng(c('Ogun','Ondo','Osun'),fill=T,col='grey90',add=T)
points(x=c(4.5,5.75,4),y=c(7.33,7.4,6.8),pch=20,cex=2) #Not the exact locations!
# x=c(4.2031,5.6203,4.3667) ; y=c(7.1482,7.4383,6.9833)

dev.off()