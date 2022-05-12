# Script to process CIMS files
# and plot them
# (improved version)

# Select a file and open it...
# Change the default path as needed...
fn.inp <- choose.files(default="I:\\Raff_Lab_Data_Backup\\CIMS\\Raw Data\\*.*",
	caption="Select file(s) for analysis", multi = FALSE)
cims.raw <- read.table(fn.inp, header = TRUE)

# Delete extraneous columns...
cims.p <- cims.raw[-c(12:15,17,20,23,26,29,32,35,38,41,44,47,49:80)]
# Consolidate times into a time column
cims.p$dtd <- ISOdatetime(cims.p$yr,cims.p$mo,cims.p$dm,
			cims.p$hr,cims.p$mn,cims.p$sc)

# Extract analog readouts
cims.sf5 <- cims.raw$A01		# SF5- readout
cims.press <- cims.raw$A07/1000	# Pressure readout
cims.n2 <- cims.raw$A08/1000		# N2 readout

# Make a data frame
# from relevant columns

dtd <- cims.p$dtd
m46 <- cims.p$Hz046
m62 <- cims.p$Hz062
m174 <- cims.p$Hz174
m235 <- cims.p$Hz235
m127 <- cims.p$Hz127
m55 <- cims.p$Hz055
m66 <- cims.p$Hz066
m82 <- cims.p$Hz082
press <- cims.press
n2 <- cims.n2

cims.df <- data.frame(dtd,m46,m62,m174,m235,m127,m55,
		m66,m82,press,n2, row.names = NULL)