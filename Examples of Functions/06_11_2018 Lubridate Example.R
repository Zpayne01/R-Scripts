library(lubridate)

## Choose your two files, file 1 will be your main file, file 2 will be your humidity file

file1 <- file.choose()
file2 <- file.choose()

## Convert them to tables

table1 <- read.csv(file = file1, stringsAsFactors = FALSE)
table2 <- read.csv(file = file2, stringsAsFactors = FALSE)

## Make sure to change the column denotation to the correct in the following line to match your column 
## Lubridate needs them as POSIXct or POSIXlt

table1$Time <- as.POSIXct(table1$Time, format = "%m/%d/%Y %H:%M:%S")
table2$Time <- as.POSIXct(table2$Time, format = "%m/%d/%Y %H:%M:%S")

## This is the lubridate code that will floor each time by minute, meaning it rounds down. This is useful for both aggregating or matching
## You can also use round_date, which will round up or down depending on the second, or ceiling_date which rounds up

table1$Time <- floor_date(table1$Time, unit = "minute")
table2$Time <- floor_date(table2$Time, unit = "minute")

## This will match your times with the correctly, you want to do the more numerous (the per second reading in your case) first.

matchedtimes <- match(table1$Time, table2$Time)

## Finally you can do whatever math you need to do by uncommenting the following

## table1$calculation <- table1$Data + table2$X[matchedtimes]
