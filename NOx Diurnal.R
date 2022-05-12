## Dirunal Averaging for NOx

file <- file.choose()
compile <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

colnames(compile) <- c('time', 'no_ave', 'no_sd', 'no2_ave', 'no2_sd', 'hono_ave', 'hono_sd')

compile$time <- strptime(compile$time, format = "%m/%d/%Y %H:%M", tz = "EST")
## compile$time <- strptime(compile$time, format = "%m/%d/%Y %H:%M", tz = "EST")

datetime <- compile$time
compile$hour <- datetime$hour

## Create hours column

start <- compile[compile$hour == 0,]

## Using Equation from statistics website

ave_no <- mean(c(na.omit(start$no_ave)))

ES <- na.omit(start$no_sd) ^ 2 * (44)
GS <- (na.omit(start$no_ave) - (ave_no[1])) ^ 2 * 45
sd_no <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$no_ave)) - 1))

ave_no2 <- mean(c(na.omit(start$no2_ave)))

ES <- na.omit(start$no2_sd) ^ 2 * (44)
GS <- (na.omit(start$no2_ave) - (ave_no2[1])) ^ 2 * 45
sd_no2 <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$no2_ave)) - 1))

ave_hono <- mean(c(na.omit(start$hono_ave)))

ES <- na.omit(start$hono_sd) ^ 2 * (44)
GS <- (na.omit(start$hono_ave) - (ave_hono[1])) ^ 2 * 45
sd_hono <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$hono_ave)) - 1))

## Nummber of observations for determine confidence

observations <- nrow(start) * 45
CE95_no <- 1.96 * sd_no/(observations)  
CE95_no2 <- 1.96 * sd_no2/(observations)
CE95_hono <- 1.96 * sd_hono/(observations)

export <- data.frame(start$hour[1], ave_no, sd_no, CE95_no, ave_no2, sd_no2, CE95_no2, 
                     ave_hono, sd_hono, CE95_hono)
colnames(export) <- c('hour', 'ave_no', 'sd_no', 'CE95_no', 'ave_no2', 'sd_no2', 'CE95_no2', 
                      'ave_hono', 'sd_hono', 'CE95_hono')

# Loop through each hour

for(i in 1:23)	{
  start <- compile[compile$hour == i,]
  
  ave_no <- mean(c(na.omit(start$no_ave)))
  
  ES <- na.omit(start$no_sd) ^ 2 * (44)
  GS <- (na.omit(start$no_ave) - (ave_no[1])) ^ 2 *45
  sd_no <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$no_ave)) - 1))
  
  ave_no2 <- mean(c(na.omit(start$no2_ave)))
  
  ES <- na.omit(start$no2_sd) ^ 2 * (44)
  GS <- (na.omit(start$no2_ave) - (ave_no2[1])) ^ 2 * 45
  sd_no2 <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$no2_ave)) - 1))
  
  ave_hono <- mean(c(na.omit(start$hono_ave)))
  
  ES <- na.omit(start$hono_sd) ^ 2 * (44)
  GS <- (na.omit(start$hono_ave) - (ave_hono[1])) ^ 2 * 45
  sd_hono <- sqrt((sum(ES)+sum(GS))/(45 * length(na.omit(start$hono_ave)) - 1))
  
  ## Nummber of observations for determine confidence
  
  observations <- nrow(start) * 45
  CE95_no <- 1.96 * sd_no/(observations)  
  CE95_no2 <- 1.96 * sd_no2/(observations)
  CE95_hono <- 1.96 * sd_hono/(observations)
  
  add <- data.frame(start$hour[1], ave_no, sd_no, CE95_no, ave_no2, sd_no2, CE95_no2, 
                    ave_hono, sd_hono, CE95_hono)
  colnames(add) <- c('hour', 'ave_no', 'sd_no', 'CE95_no', 'ave_no2', 'sd_no2', 'CE95_no2', 
                     'ave_hono', 'sd_hono', 'CE95_hono')
  
  export <- rbind(export, add)

}

write.table(export, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)

