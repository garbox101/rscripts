library(ggplot2)

#### - DEFINING DATA OLD VS NEW SAA - ####
i=100
j=108
Hist.Heading <- "CPI + 5.3% mandate returns"
Hist.Heading.Semi.Dev <- "CPI + 5.3% mandate downside deviation"

# Define source files by SAA index no.
ResultsTableA <- read.csv(paste0('C:\\Users\\ialgar\\OneDrive - PPS\\Investment Team work\\Results\\Latest Results\\Results', i, '.csv'), row.names=1,header=TRUE, sep=",", dec=".",stringsAsFactors = FALSE)
head(ResultsTableA)
ResultsTableB <- read.csv(paste0('C:\\Users\\ialgar\\OneDrive - PPS\\Investment Team work\\Results\\Latest Results\\Results', j, '.csv'), row.names=1,header=TRUE, sep=",", dec=".",stringsAsFactors = FALSE)
head(ResultsTableB)

# Create return series
ReturnsA <- ResultsTableA[,"Mean.Ret"]
head(ReturnsA)
ReturnsB <- ResultsTableB[,"Mean.Ret"]
head(ReturnsB)

# Create downside dev series
Semi.DevA <- ResultsTableA[,"Semi.Dev"]
head(Semi.DevA)
Semi.DevB <- ResultsTableB[,"Semi.Dev"]
head(Semi.DevB)

#creating histogram colours
col2rgb("lightblue")
col2rgb(c("lightblue", "lightgreen", "pink"))
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(205,255,103, max = 255, alpha = 80, names = "lt.green")



#### - CREATING HISTOGRAMS OLD VS NEW SAA - ####
#source: https://www.dataanalytics.org.uk/plot-two-overlapping-histograms-on-one-chart-in-r/

# Return comparison
b <- min(c(ReturnsA,ReturnsB))-0.001 # Set the minimum for the breakpoints
e <- max(c(ReturnsA,ReturnsB)) # Set the maximum for the breakpoints
ax <- pretty(range(c(ReturnsA, ReturnsB)), n=100) # Make a neat vector for the breakpoints
histA <- hist(ReturnsA, breaks = ax, plot=FALSE)
histB <- hist(ReturnsB, breaks = ax, plot=FALSE)

plot(histA, col = c1, main =Hist.Heading, xlab="Returns") # Plot 1st histogram using a transparent color
plot(histB, col = c2, add = TRUE) # Add 2nd histogram using different color
legend("topright",c(i, j), fill=c(c1, c2))

#Downside Dev comparison
b <- min(c(Semi.DevA,Semi.DevB))-0.001 # Set the minimum for the breakpoints
e <- max(c(Semi.DevA,Semi.DevB)) # Set the maximum for the breakpoints
ax <- pretty(range(c(Semi.DevA,Semi.DevB)), n=100) # Make a neat vector for the breakpoints
histA <- hist(Semi.DevA, breaks = ax, plot=FALSE)
histB <- hist(Semi.DevB, breaks = ax, plot=FALSE)

plot(histA, col = c1, main =Hist.Heading.Semi.Dev, xlab="Downside deviation") # Plot 1st histogram using a transparent color
plot(histB, col = c2, add = TRUE) # Add 2nd histogram using different color
legend("topright",c("100","108"), fill=c(c1, c2))


#### - DEFINING DATA FOR SENSITIVTY CHECKS - ####
# Define source files by SAA index no.
ResultsTableC <- read.csv(paste0('C:\\Users\\ialgar\\OneDrive - PPS\\Investment Team work\\Results\\Sensitivity checks\\Results', i, '.csv'), row.names=1,header=TRUE, sep=",", dec=".",stringsAsFactors = FALSE)
head(ResultsTableC)
ResultsTableD <- read.csv(paste0('C:\\Users\\ialgar\\OneDrive - PPS\\Investment Team work\\Results\\Sensitivity checks\\Results', j, '.csv'), row.names=1,header=TRUE, sep=",", dec=".",stringsAsFactors = FALSE)
head(ResultsTableD)

# Create return series
ReturnsC <- ResultsTableC[,"Mean.Ret"]
head(ReturnsC)
ReturnsD <- ResultsTableD[,"Mean.Ret"]
head(ReturnsD)

# Create downside dev series
Semi.DevC <- ResultsTableC[,"Semi.Dev"]
head(Semi.DevC)
Semi.DevD <- ResultsTableD[,"Semi.Dev"]
head(Semi.DevD)


#### - CREATING HISTOGRAMS NEW SAA, SENSITIVTY CHECK - ####

# Return comparison
b <- min(c(ReturnsB,ReturnsD))-0.001 # Set the minimum for the breakpoints
e <- max(c(ReturnsB,ReturnsD)) # Set the maximum for the breakpoints
ax <- pretty(range(c(ReturnsB, ReturnsD)), n=100) # Make a neat vector for the breakpoints
histB <- hist(ReturnsB, breaks = ax, plot=FALSE)
histD <- hist(ReturnsD, breaks = ax, plot=FALSE)

plot(histB, col = c2, main =Hist.Heading, xlab="Returns") # Plot 1st histogram using a transparent color
plot(histD, col = c3, add = TRUE) # Add 2nd histogram using different color
legend("topright",c("New SAA, current E(R)", "New SAA, lower OEq E(R)"), fill=c(c2, c3))

#Downside Dev comparison
b <- min(c(Semi.DevB,Semi.DevD))-0.001 # Set the minimum for the breakpoints
e <- max(c(Semi.DevB,Semi.DevD)) # Set the maximum for the breakpoints
ax <- pretty(range(c(Semi.DevB,Semi.DevD)), n=100) # Make a neat vector for the breakpoints
histA <- hist(Semi.DevB, breaks = ax, plot=FALSE)
histB <- hist(Semi.DevD, breaks = ax, plot=FALSE)

plot(histA, col = c2, main =Hist.Heading.Semi.Dev, xlab="Downside deviation") # Plot 1st histogram using a transparent color
plot(histB, col = c3, add = TRUE) # Add 2nd histogram using different color
legend("topright",c("New SAA, current E(R)", "New SAA, lower OEq E(R)"), fill=c(c2, c3))











# plot diff - WORKING ON

diff <- as.data.frame(ax,ReturnsA-ReturnsB)
head(diff)
lines(diff, col="black")

ggplot(data=diff)

