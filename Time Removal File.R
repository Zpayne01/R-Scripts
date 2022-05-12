library(lubridate)

## Time Removal for ambient IUTRP ##

file <- file.choose()
csv <- read.csv(file, header = TRUE)
csv$datetime <- as.POSIXct(csv$datetime, format = "%m/%d/%Y %H:%M")

## Add times to be removed (for all instruments) ##

csv$datetime <- as.integer(csv$datetime)
x <- csv[csv$datetime != 1501690500 & csv$datetime != 1501680600 & csv$datetime != 1501793100 & csv$datetime != 1501866000 & csv$datetime != 1501866900 & csv$datetime != 15018661800 & csv$datetime != 15018662700
		& csv$datetime != 1501943400 & csv$datetime != 1502028000 & csv$datetime != 1502028900 & csv$datetime != 1502222400 & csv$datetime != 1502223300 & csv$datetime != 1502224200 & csv$datetime != 1502225100
		& csv$datetime != 1502226000 & csv$datetime != 1502226900 & csv$datetime != 1502227800 & csv$datetime != 1502228700 & csv$datetime != 1502290800 & csv$datetime != 1502291700 & csv$datetime != 1502292600 
		& csv$datetime != 1502293500 & csv$datetime != 1502376300 & csv$datetime != 1502381700 & csv$datetime != 1502382600 & csv$datetime != 1502383500 & csv$datetime != 1502652600 & csv$datetime != 1502653500
		& csv$datetime != 1502731800 & csv$datetime != 1502732700 & csv$datetime != 1502733600 & csv$datetime != 1502892000 & csv$datetime != 1502892900 & csv$datetime != 1502911800 & csv$datetime != 1502912700
		& csv$datetime != 1502913600 & csv$datetime != 1502996400 & csv$datetime != 1502997300,]

y <- as.POSIXct(as.integer(x$datetime), origin = "1970-01-01")
x$datetime <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)


## Times to be removed for NOx Analyzer ##

file <- file.choose()
csv <- read.csv(file, header = TRUE)
csv$timeamb <- as.POSIXct(csv$timeamb, format = "%Y-%m-%d %H:%M:%S")

## Add times to be removed (NOx Analzyer) ##

csv$timeamb <- as.integer(csv$timeamb)
x <- csv[csv$timeamb != 1501690500 & csv$timeamb != 1501680600 & csv$timeamb != 1501793100 & csv$timeamb != 1501866000 & csv$timeamb != 1501866900 & csv$timeamb != 15018661800 & csv$timeamb != 15018662700
		& csv$timeamb != 1501943400 & csv$timeamb != 1502028000 & csv$timeamb != 1502028900 & csv$timeamb != 1502222400 & csv$timeamb != 1502223300 & csv$timeamb != 1502224200 & csv$timeamb != 1502225100
		& csv$timeamb != 1502226000 & csv$timeamb != 1502226900 & csv$timeamb != 1502227800 & csv$timeamb != 1502228700 & csv$timeamb != 1502290800 & csv$timeamb != 1502291700 & csv$timeamb != 1502292600 
		& csv$timeamb != 1502293500 & csv$timeamb != 1502376300 & csv$timeamb != 1502381700 & csv$timeamb != 1502382600 & csv$timeamb != 1502383500 & csv$timeamb != 1502652600 & csv$timeamb != 1502653500
		& csv$timeamb != 1501855200 & csv$timeamb != 1501856100 & csv$timeamb != 1501857000 & csv$timeamb != 1501857900 & csv$timeamb != 1501858800 & csv$timeamb != 1501659700 & csv$timeamb != 1502660600
		& csv$timeamb != 1501861500 & csv$timeamb != 1501862400 & csv$timeamb != 1501863300 & csv$timeamb != 1501864200 & csv$timeamb != 1501865100 & csv$timeamb != 1502029800 & csv$timeamb != 1502030700
		& csv$timeamb != 1502031600 & csv$timeamb != 1502032500 & csv$timeamb != 1502033400 & csv$timeamb != 1502034300 & csv$timeamb != 1502035200 & csv$timeamb != 1502036100 & csv$timeamb != 1502207100
		& csv$timeamb != 1502208000 & csv$timeamb != 1502208900 & csv$timeamb != 1502550000,]

y <- as.POSIXct(as.integer(x$timeamb), origin = "1970-01-01")
x$timeamb <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)

x <- as.POSIXct("08/16/2017 10:00", format = "%m/%d/%Y %H:%M", tmz = "EST")
as.integer(x, origin = "1970-01-01")

## Time removal for Chambers not NOx ##

## CHAMBER A 

file <- file.choose()
csv <- read.csv(file, header = TRUE)

csv <- na.omit(csv)

csv$datetime_a <- as.POSIXct(csv$datetime_a, format = "%m/%d/%Y %H:%M")
csv$datetime_a <- as.integer(csv$datetime_a, origin = "1970-01-01")

x <- csv[csv$datetime_a != 1501866000 & csv$datetime_a != 1502028000 &  csv$datetime_a != 150222400 & csv$datetime_a != 1502226000
         & csv$datetime_a != 1502290800 & csv$datetime_a != 1502892000,]

y <- as.POSIXct(as.integer(x$datetime_a), origin = "1970-01-01")
x$datetime_a <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)

x <- as.POSIXct("08/16/2017 10:00", format = "%m/%d/%Y %H:%M", tmz = "EST")
as.integer(x, origin = "1970-01-01")

## CHAMBER B ##

file <- file.choose()
csv <- read.csv(file, header = TRUE)

csv <- na.omit(csv)

csv$datetime_a <- as.POSIXct(csv$datetime_b, format =  "%Y-%m-%d %H:%M:%S")
csv$datetime_a <- as.integer(csv$datetime_b, origin = "1970-01-01")

x <- csv[csv$datetime_b != 1501690500 & csv$datetime_b != 1501866000 & csv$datetime_b != 1502028000  & csv$datetime_b != 1502222400 & csv$datetime_b != 1502226000
         & csv$datetime_b != 1502290800 & csv$datetime_b != 1502467200 & csv$datetime_b != 15026400000  & csv$datetime_b != 1502892000,]

x_a <- as.POSIXct("08/16/2017 10:00", format = "%m/%d/%Y %H:%M", tmz = "EST")
as.integer(x_a, origin = "1970-01-01")

y <- as.POSIXct(as.integer(x$datetime_b), origin = "1970-01-01")
x$datetime_a <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)

## CHAMBER C

file <- file.choose()
csv <- read.csv(file, header = TRUE)

csv <- na.omit(csv)

csv$datetime_c <- as.POSIXct(csv$datetime_c, format =  "%m/%d/%Y %H:%M")
csv$datetime_c <- as.integer(csv$datetime_c, origin = "1970-01-01")

x <- csv[csv$datetime_c != 1501680600 & csv$datetime_c != 1501866000 & csv$datetime_c != 1501941600
         & csv$datetime_c != 1502222400 & csv$datetime_c != 1502226000 & csv$datetime_c != 1502290800
         & csv$datetime_c != 1502467200  & csv$datetime_c != 1502650800 & csv$datetime_c != 1502730000
         & csv$datetime_c != 1501678800,]

x_c <- as.POSIXct("08/05/2017 10:00", format = "%m/%d/%Y %H:%M", tmz = "EST")
as.integer(x_c, origin = "1970-01-01")

y <- as.POSIXct(as.integer(x$datetime_c), origin = "1970-01-01")
x$datetime_c <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)

## CHAMBER D ##

file <- file.choose()
csv <- read.csv(file, header = TRUE)

csv <- na.omit(csv)

csv$datetime_d <- as.POSIXct(csv$datetime_d, format =  "%m/%d/%Y %H:%M")
csv$datetime_d <- as.integer(csv$datetime_d, origin = "1970-01-01")

x <- csv[csv$datetime_d != 1501790400 & csv$datetime_d != 1501866000 & csv$datetime_d != 1502222400
         & csv$datetime_d != 1502226000 & csv$datetime_d != 1502290800 & csv$datetime_d != 1502373600
         & csv$datetime_d != 1502650800 & csv$datetime_d != 1502730000,]

x_a <- as.POSIXct("08/14/2017 13:00", format = "%m/%d/%Y %H:%M", tmz = "EST")
as.integer(x_a, origin = "1970-01-01")

y <- as.POSIXct(as.integer(x$datetime_d), origin = "1970-01-01")
x$datetime_d <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)


## Time removal for Chambers for NOx ##

## Time removal for Chamber A ##

file <- file.choose()
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

csv <- na.omit(csv)

csv$timea <- as.POSIXct(csv$timea, format = "%Y-%m-%d %H:%M:%S")
csv$timea <- floor_date(csv$timea, unit = 'hour')

csv$timea <- as.integer(csv$timea)

x <- csv[(csv$timea != 1501855200 & csv$timea != 1501858800 & csv$timea != 1501862400 & csv$timea != 1501866000 & csv$timea != 1501948800 & csv$timea != 1501869600
          & csv$timea != 1502028000 & csv$timea != 1502031600 & csv$timea != 1502035200 & csv$timea != 150220800 & csv$timea != 1502222400  & csv$timea != 1502226000
          & csv$timea != 1502290800  & csv$timea != 1502892000),]

y <- as.POSIXct(as.integer(x$timea), origin = "1970-01-01")
x$timea <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)

## Time removal for Chamber B ##

file <- file.choose()
csv <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

csv <- na.omit(csv)

csv$timeb <- as.POSIXct(csv$timeb, format = "%Y-%m-%d %H:%M")
csv$timeb <- floor_date(csv$timeb, unit = 'hour')

csv$timeb <- as.integer(csv$timeb)

x <- csv[(csv$timeb != 1501690500 & csv$timeb != 1501855200 & csv$timeb != 1501858800 & csv$timeb != 1501862400 & csv$timeb != 1501866000 & csv$timeb != 1501948800 
          & csv$timeb != 1501869600 & csv$timeb != 1502028000 & csv$timeb != 1502031600 & csv$timeb != 1502035200 & csv$timeb != 150220800 & csv$timeb != 1502222400  
          & csv$timeb != 1502226000 & csv$timeb != 1502290800  & csv$timeb != 1502892000),]


y <- as.POSIXct(as.integer(x$timeb), origin = "1970-01-01")
x$timeb <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)

## Time removal for Chamber C ##

file <- file.choose()
csv <- read.csv(file, header = TRUE)

csv <- na.omit(csv)

csv$timec <- as.POSIXct(csv$timec, format =  "%Y-%m-%d %H:%M")

csv$timec <- floor_date(csv$timec, unit = 'hour')
csv$timec <- as.integer(csv$timec, origin = "1970-01-01")

x <- csv[csv$timec != 1501680600 & csv$timec != 1501866000 & csv$timec != 1501941600
         & csv$timec != 1502222400 & csv$timec != 1502226000 & csv$timec != 1502290800
         & csv$timec != 1502467200  & csv$timec != 1502650800 & csv$timec != 1502730000
         & csv$timec != 1501678800  & csv$timec != 1501855200 & csv$timec != 1501858800
         & csv$timec != 1501948800 & csv$timec != 1501952400  & csv$timec != 1502031600
         & csv$timec != 1502028000  & csv$timec != 1502910000,]

y <- as.POSIXct(as.integer(x$timec), origin = "1970-01-01")
x$timec <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)

## Time removal for Chamber D ##

file <- file.choose()
csv <- read.csv(file, header = TRUE)

csv <- na.omit(csv)

csv$timed <- as.POSIXct(csv$timed, format =  "%Y-%m-%d %H:%M")
csv$timed <- floor_date(csv$timed, unit = 'hour')

csv$timed <- as.integer(csv$timed, origin = "1970-01-01")

x <- csv[csv$timed != 1501790400 & csv$timed != 1501866000 & csv$timed != 1502222400
         & csv$timed != 1502226000 & csv$timed != 1502290800 & csv$timed != 1502373600
         & csv$timed != 1502650800 & csv$timed != 1502730000 & csv$timed != 1501855200 
         & csv$timed != 1501858800 & csv$timed != 1501948800 & csv$timed != 1501952400  
         & csv$timed != 1502031600 & csv$timed != 1502028000  & csv$timed != 1502910000
         & csv$timed != 1502204400 & csv$timed != 1502546400,]

x_a <- as.POSIXct("08/12/2017 10:00", format = "%m/%d/%Y %H:%M", tmz = "EST")
as.integer(x_a, origin = "1970-01-01")

y <- as.POSIXct(as.integer(x$timed), origin = "1970-01-01")
x$timed <- y

name <- "C:/Users/Zachary/Desktop/Correct_Values.csv"

write.csv(x, file = name, row.names = FALSE)


