

parameters <- c(a = -8/3, b = -10, c = 28)
state <- c(X = 1, Y = 1, Z = 1)

Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # rate of change
    dX <- a*X + Y*Z
    dY <- b * (Y-Z)
    dZ <- -X*Y + c*Y - Z
    
    # return the rate of change
    list(c(dX, dY, dZ))
  }) #end with(as. list...
}

times <- seq(0, 100, by = 0.01)

library(deSolve)

out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

head(out)

par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "X"], out[, "Z"], pch = ".")
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)



scaleseq <- seq(0,1,.01)


statelist <- list()
for(i in 1:length(scaleseq)) {
  statelist[[i]] = c(NO = mixingratio_to_percc(.2, temp = 291), O3 = mixingration_to_percc(100, temp = 291)*scaleseq, NO2 = 0)
}

parameters = list()

for(i in 1:length(scaleseq)) {
  parameters[[i]] = c(k1 = 1.7E-14, k2 = 1.03e-19, j1 = 0)
}

OzoneCycle <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         #Rate of change
         
         dNO <- -k1*NO*O3 + j1*NO2 - k2*NO*NO
         dO3 <- -k1*NO*O3 + j1*NO2
         dNO2 <- k1*NO*O3 - j1*NO2 + k2*NO*NO
         
         #return the rate of change
         list(c(dNO, dO3, dNO2))
       })
}

## Doing a single run
NO <- .5 #ppb
O3 <- 40 #ppb

nafcorrection <- 6.8E-4 * O3 + 0.59

times <- seq(0, 10, by = .1)
NO <- mixingratio_to_percc(NO, temp = 291)
O3 <- mixingratio_to_percc(O3, temp = 291)


state = c(NO = NO, O3 = O3, NO2 = 0)
parameterstart = c(k1 = 1.8E-14,k2 = 1.03e-19, j1 = 0)

out <- data.frame(ode(y = state, times = times, func = OzoneCycle, parms = parameterstart, method = 'euler'))
out[,2:4] <- percc_to_mixingratio(out[,2:4], temp = 291)

write.table(out, file = 'clipboard', sep = '\t', row.names = F)

#Oh baby

NO <- mixingratio_to_percc(c(.2,5,50,200), temp = 291)
O3 <- mixingratio_to_percc( seq(0,100,1), temp = 291)

#Mixing Ratio Naf
O3 <- seq(0,100,1)
nafcorrection <- 6.8E-4 * O3 + 0.59

O3 <- mixingratio_to_percc( seq(0,100,1), temp = 291) * nafcorrection


statelist <- list()

for(i in 1:length(NO)) {
  for(j in 1:length(O3)) {
    statelist[[(i-1)*101 + j]] <- c(NO = NO[i], O3 = O3[j], NO2 = 0)
  }
}


out2 <- lapply(statelist, function(x) data.frame(ode(y = x, times = times, func = OzoneCycle, parms = parameterstart, method = 'euler')))
#nonafout2 <- out2
#nafout2 <- out2

output1 <- unlist(lapply(nonafout2, function(x) x$NO[62]))
output2 <- unlist(lapply(nafout2, function(x) x$NO[62]))

output3 <- output2/output1

output4 <- matrix(output3, ncol = 4, nrow = 101)
write.table(output4, 'clipboard', sep = '\t')


out2 <- data.frame(ode(y = state, times = times, func = OzoneCycle, parms = parameterstart, method = 'euler'))

out2 <- lapply(parameters, function(x) data.frame(ode(y = state, times = times, func = OzoneCycle, parms = x, method = 'euler')))

plot(NO ~ time, out2[[101]], type = 'l', lwd = 2)
for(i in 1:(length(out2)-1)) {
  lines(NO ~ time, out2[[i]], col = i, lwd = 2)
}

nonaf <- unlist(lapply(out2, function(x) x$NO[33]))
naf <- unlist(lapply(out2, function(x) x$NO[33]))

output <- data.frame(
  ozone = seq(0, 100, 1),
  nonaf = nonaf * 1000000000 * 24.4/6.022E+23/0.001,
  naf = naf * 1000000000 * 24.4/6.022E+23/0.001
)

write.table(output, file = 'clipboard', sep = '\t', row.names = F)

lines(NO ~ time, out2[[2]], col = '3')

head(out2)

par(nrow = 1, ncol = 1, oma = c(0, 0, 0, 0))
plot(out2, xlab = "time", ylab = "-")
plot(out2[, "NO"], out[, "NO2"], pch = ".")
mtext(outer = TRUE, side = 3, "Ozone Cycle Model", cex = 1.5)

outclip = out[seq(0,600,5), ]
write.table(outclip, file = "clipboard", row.names = FALSE, sep = "\t")

O3 <- state[[2]] 
NO <- nonaf[[101]]
