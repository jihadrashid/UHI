#importing required library
library(raster)

#importing raster layers
img90=raster('.../1990.tif')
img00=raster('.../2000.tif')
img10=raster('.../2010.tif')
img20=raster('.../2020.tif')

#setting colors based on defined classes of the raster layers
#breakpoint=c(0,0.005,0.01,0.015,0.02) #for UTFVI classes
#color=c('darkred','wheat','steelblue',...)
color=hcl.colors(5)

#setting fonts to times new roman
windowsFonts(A = windowsFont("Times New Roman"))

#plotting multiple graphs in one page
par(mfrow=c(2,2),mar=c(3,0.1,1,0), oma=c(0,0.001,0,0), family='A')

plot(img90,axes=F,box=F,main='(a)',col=color,legend=F,cex.main=1, font.main = 1)
plot(img00,axes=F,box=F,main='(b)',col=color,legend=F,cex.main=1, font.main = 1)
plot(img10,axes=F,box=F,main='(c)',col=color,legend=F,cex.main=1, font.main = 1)
plot(img20,axes=F,box=F,main='(d)',col=color,legend=F,cex.main=1, font.main = 1)

legend('bottom', legend = c('A','B','C','...'),
       fill = color,xpd=TRUE,inset = c(0,-0.248),bty='n', ncol=1)


#plotted graph were exported as pdf than converted to image for better quality
library(pdftools)
bitmap <- pdf_render_page(".../Rplot.pdf", page = 1, dpi = 300)
jpeg::writeJPEG(bitmap, ".../Rplot.jpeg")
