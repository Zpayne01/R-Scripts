file <- file.choose()
tab <- read.csv(file)

head(tab)
tail(tab)
#tab <- tab[-c(1,2),] #Glucose
#tab <- tab[-nrow(tab),]
tab <- tab[-c(nrow(tab), nrow(tab) - 1),] #No Glucose

blank <- tab[seq(1,nrow(tab), 3),]
measurement <- tab[seq(3,nrow(tab), 3), ]

blank <- blank %>%
  mutate(no_ppb = (no_counts - 20)*(1738)^-1) %>%
  mutate(no_sd = (no_countssd)/1738) %>%
  mutate(no2_ppb = (blc2_counts - 20)/1738/0.9) %>%
  mutate(no2_sd = (blc2_countssd)/1738/.9) %>%
  mutate(hono_ppb = (HONO_counts - 20)*(1738)^-1) %>%
  mutate(hono_sd = (HONO_countssd)/1738)

measurement <- measurement %>%
  mutate(no_ppb = (no_counts - 20)*(1738)^-1) %>%
  mutate(no_sd = (no_countssd)/1738) %>%
  mutate(no2_ppb = (blc2_counts - 20)/1738/0.9) %>%
  mutate(no2_sd = (blc2_countssd)/1738/.9) %>%
  mutate(hono_ppb = (HONO_counts - 20)*(1738)^-1) %>%
  mutate(hono_sd = (HONO_countssd)/1738)

par(mfrow = c(3, 1))

plot(measurement$no_ppb, type = 'l', ylim = c(0, 5))
lines(1:nrow(blank),blank$no_ppb, col = 'red')

plot(measurement$no2_ppb, type = 'l', ylim = c(0, 0.5))
lines(1:nrow(blank),blank$no2_ppb, col = 'red')

plot(measurement$hono_ppb, type = 'l', ylim = c(0, 0.25))
lines(1:nrow(blank),blank$hono_ppb, col = 'red')

dev.off()

output <- data.frame(
  hour = seq(0,(nrow(measurement)-1)*.25,.25),
  no_ppb = measurement$no_ppb - blank$no_ppb,
  no_sd = measurement$no_sd,
  no2_ppb = measurement$no2_ppb - blank$no2_ppb,
  no2_sd = measurement$no2_sd,
  hono_ppb = measurement$hono_ppb - blank$hono_ppb,
  hono_sd = measurement$hono_sd
)

 output$ceil <- ceiling(output$hour)
 output <- output %>%
   filter(!hour %in% c(5.75, 6.25, 6.5))


outputagg <- group_by(output, ceil) %>%
                summarise(no_conc_ppb = mean(no_ppb),
                          no_sd = sd(no_ppb),
                          no2_conc_ppb = mean(no2_ppb),
                          no2_sd = sd(no2_ppb),
                          hono_conc_pbb = mean(hono_ppb),
                          hono_sd = sd(hono_ppb))

write.csv(outputagg, file = 'C:/Users/zacpayne/Desktop/Glucose agg.csv', row.names = F)
