setwd("C:/Users/tate109/Documents/Packages/qFeature/windowAnimate")

set.seed(10)
measure <- round(runif(10,0,10),0)
time <- c(-3:3)
time2 <- time^2

pointColor <- c(rep("blue",4),rep("grey",3))
plotName <- "regWindow1.png"
png(plotName, height=360, width=480)
regWindow1 <- lm(measure[1:4]~time[4:7]+time2[4:7])
plot(0:6, measure[1:7],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow1$fitted.values
lines(time[4:7], regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("blue",5),rep("grey",3))
plotName <- "regWindow2.png"
png(plotName, height=360, width=480)
regWindow2 <- lm(measure[1:5]~time[3:7]+time2[3:7])
plot(-1:6, measure[1:8],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow2$fitted.values
lines(time[3:7], regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("blue",6),rep("grey",3))
plotName <- "regWindow3.png"
png(plotName, height=360, width=480)
regWindow3 <- lm(measure[1:6]~time[2:7]+time2[2:7])
plot(-2:6, measure[1:9],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow3$fitted.values
lines(time[2:7], regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("blue",7),rep("grey",3))
plotName <- "regWindow4.png"
png(plotName, height=360, width=480)
regWindow4 <- lm(measure[1:7]~time+time2)
plot(-3:6, measure[1:10],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow4$fitted.values
lines(time, regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("grey",1),rep("blue",7),rep("grey",2))
plotName <- "regWindow5.png"
png(plotName, height=360, width=480)
regWindow5 <- lm(measure[2:8]~time+time2)
plot(-4:5, measure[1:10],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow5$fitted.values
lines(time, regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("grey",2),rep("blue",7),rep("grey",1))
plotName <- "regWindow6.png"
png(plotName, height=360, width=480)
regWindow6 <- lm(measure[3:9]~time+time2)
plot(-5:4, measure[1:10],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow6$fitted.values
lines(time, regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("grey",3),rep("blue",7))
plotName <- "regWindow7.png"
png(plotName, height=360, width=480)
regWindow7 <- lm(measure[4:10]~time+time2)
plot(-6:3, measure[1:10],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow7$fitted.values
lines(time, regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("grey",3),rep("blue",6))
plotName <- "regWindow8.png"
png(plotName, height=360, width=480)
regWindow8 <- lm(measure[5:10]~time[1:6]+time2[1:6])
plot(-6:2, measure[2:10],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow8$fitted.values
lines(time[1:6], regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("grey",3),rep("blue",5))
plotName <- "regWindow9.png"
png(plotName, height=360, width=480)
regWindow9 <- lm(measure[6:10]~time[1:5]+time2[1:5])
plot(-6:1, measure[3:10],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow9$fitted.values
lines(time[1:5], regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()

pointColor <- c(rep("grey",3),rep("blue",4))
plotName <- "regWindow10.png"
png(plotName, height=360, width=480)
regWindow10 <- lm(measure[7:10]~time[1:4]+time2[1:4])
plot(-6:0, measure[4:10],
     xlim=c(-6,6),
     ylim=c(0,8),
     ylab = "Response Measure",
     xlab = "Chronological Sequence",
     las=1, pch=19,
     cex=2,
     col=pointColor)
regFitted <- regWindow10$fitted.values
lines(time[1:4], regFitted, col="blue")
rect(xleft = -3.2,
     xright = 3.2,
     ybottom = par("usr")[3],
     ytop = par("usr")[4],
     col = rgb(0,1,1,.2))
dev.off()