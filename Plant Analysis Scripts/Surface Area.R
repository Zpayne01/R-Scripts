## Image processing ##
## Requires magick and pixmap package to operate ##

library('magick')
library('pixmap')

## Choose folder for processing ##

file <- image_read(file.choose())
PNM <- image_convert(file, 'PNM')

image_write(PNM, 'C:/Users/Zachary/Desktop/PNM.pnm')
pic <- read.pnm('C:/Users/Zachary/Desktop/PNM.pnm')
str(pic)

## Pixels per inch ##

pixels <- dim(pic@grey)[1] * dim(pic@grey)[2]
pixpercm2 <- round(pixels / ((8.5*2.54) * (2.54 * 11)))

## Take a subsection to get rid of the black in the corners ##

pic2 <- pic[c(100:3200), c(50:2452)]

white <- sum(pic2@grey == 1)
black <- sum(pic2@grey == 0)

## Area of the leaf in cm^2 ##

area <- black/pixpercm * 2
print(area)
