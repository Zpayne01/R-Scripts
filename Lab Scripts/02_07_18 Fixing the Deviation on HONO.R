## Correct files HONO SD
## List all of the correct files in the designated folder

f <- list.files(path = "C:/Users/Zachary/Documents/Lab Data/Flow Tube Studies/NO_and_HONO", pattern = "HONO", full.names = TRUE)

## Assign a number to each of the files

for (i in 1:length(f))
{	assign(f[i], read.csv(f[i], header = TRUE))	}

for (i in 1:length(f)){
  data <- read.csv(f[i], header = TRUE, stringsAsFactors = FALSE)
  data$hono_sd <- (((data$hono_sd*100.12)-72.605)/100.12)
  end <- substr(f[i],78,89)
  filename <- paste0('C:/Users/Zachary/Documents/Lab Data/Flow Tube Studies/NO_and_HONO/NO_and_HONO_fixed_',end)
  write.csv(data, file = filename, row.names = FALSE)
}
