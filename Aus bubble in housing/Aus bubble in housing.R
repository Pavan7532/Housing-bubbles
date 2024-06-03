library(psymonitor)

inp_path <- "australia_df_Standardised price-income ratio.csv"
df <- read.csv(inp_path, header = TRUE, sep = ",")

year <- as.integer(df[, 1])
value <- df[, 2]

obs <- length(value)
r0 <- 0.01 + 1.8/sqrt(obs)
swindow0 <- floor(r0 * obs)
dim <- obs - swindow0 + 1

IC <- 0
adflag <- 0
yr <- 1 # year = 1
Tb <- yr + swindow0 - 1
nboot <- 1000
nCore <- 2

bsadf <- PSY(value, swindow0, IC, adflag)
max(bsadf)

quantilesBsadf <- cvPSYwmboot(value, swindow0, IC, adflag, Tb, nboot, nCore)

year <- year[swindow0:obs]
value <- value[swindow0:obs]
quantile95 <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = dim)
ind95 <- (bsadf > t(quantile95[2, ])) * 1

OT <- locate(ind95, year)
BCyear <- disp(OT, obs)
print(BCyear)

# Create the plot with lines for bubble periods
plot(year, value, xlim = c(min(year), max(year)), ylim = c(min(value), max(value)), xlab = 'year', ylab = 'Value', type = "l", lwd = 3, col = "black")

# Add the blue vertical lines for bubbles
for (i in 1:length(year)) {
  if (ind95[i] == 1) {
    abline(v = year[i], col = "blue")
  }
}
# Add indicator name as plot title
title('Yearly Standardised Price Rent Ratio')