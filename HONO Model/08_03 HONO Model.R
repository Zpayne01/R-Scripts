library(deSolve)

HONOCycle <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         #Rate of change
         
         dHONO <- 0.5 * k_het_NO2 * NO2 + k_HONO_surf - k_het_HONO * HONO - k_air * HONO - j_HONO * HONO
         dNO2 <- 0
         
         #return the rate of change
         list(c(dHONO, dNO2))
       })
}

times <- seq(0, 3600, by = 1)
state <- c(HONO = 2*2.46E10, NO2 = 2*2.46E10)
parameters <- c(k_het_NO2 = 4E-4, k_HONO_surf = 1E10, k_het_HONO = 4E-3, k_air = 1.4E-4, j_HONO = 1E-4)


out <- data.frame(ode(y = state, times = times, func = HONOCycle, parms = parameters, method = 'euler'))
outrealative <-  0.5 * 4E-4 * 2*2.46E10
plot(out$time, out$HONO)

head(out)

par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "NO"], out[, "NO2"], pch = ".")
mtext(outer = TRUE, side = 3, "HONO", cex = 1.5)