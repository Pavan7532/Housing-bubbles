library(psymonitor)

# Set the file path and indicator name
inp_path <- "australia_df_quarterly_Standardised price-rent ratio.csv"
indicator_name <- "Quarterly Standardised Price Rent Ratio "

df <- read.csv(inp_path, header = TRUE, sep = ",")


# Extract the time and value vectors
time <- df$Time
value <- df$Value

obs <- length(value)
r0 <- 0.01 + 1.8/sqrt(obs)
swindow0 <- floor(r0 * obs)
dim <- obs - swindow0 + 1
IC <- 1
adflag <- 0
yr <- 1  # year = 1
Tb <- yr + swindow0 - 1
nboot <- 2000
nCore <- 2

bsadf <- PSY(value, swindow0, IC, adflag)
quantilesBsadf <- cvPSYwmboot(value, swindow0, IC, adflag, Tb, nboot, nCore)

time <- time[swindow0:obs]
value <- value[swindow0:obs]

quantile95 <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = dim)
ind95 <- (bsadf > t(quantile95[2, ])) * 1

OT <- locate(time, ind95)
str(OT)
BCquarter <- disp(OT, obs)
print(BCquarter)

# Create the plot with lines for bubble periods
plot(time, value, xlab = 'Time', ylab = 'Value', type = "l", lwd = 3, col = "black")

# Add the blue vertical lines for bubbles
for (j in 1:length(time)) {
  if (ind95[j] == 1) {
    abline(v = time[j], col = "blue")
  }
}

# Add indicator name as plot title
title(main = indicator_name)