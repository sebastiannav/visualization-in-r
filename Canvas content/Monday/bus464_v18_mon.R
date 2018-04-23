
## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
jacoby <- read.table("data/jacoby.txt")
summary(jacoby)

## ----plot1, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
plot(jacoby$x1)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## plot(jacoby$x1, jacoby$x2)

## ----plot2, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
plot(jacoby$x1, jacoby$x2)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## plot(x2 ~ x1, data=jacoby)

## ----plot3, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
plot(x2 ~ x1, data=jacoby)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## plot(x2 ~ x1, data=jacoby, xlab="First vector",
##   ylab="Second vector", pch=16, col="#EB811B",
##   cex=1.2)

## ----plot4, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
plot(x2 ~ x1, data=jacoby, xlab="First vector", ylab="Second vector", pch=16, col="#EB811B", cex=1.2)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## plot(x2 ~ x1, data=jacoby, xlab="First vector",
##   ylab="Second vector", pch=16, col="#EB811B",
##   cex=1.2)
## abline(v=mean(jacoby$x1), h=mean(jacoby$x2),
##   lty=2, lwd=2, col="#EB811B")

## ----plot5, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
plot(x2 ~ x1, data=jacoby, xlab="First vector", ylab="Second vector", pch=16, col="#EB811B", cex=1.2)
abline(v=mean(jacoby$x1), h=mean(jacoby$x2),
  lty=2, lwd=2, col="#EB811B")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## library(lattice)
## xyplot(x2 ~ x1, jacoby)

## ----plot5l, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
library(lattice)
xyplot(x2 ~ x1, jacoby)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## xyplot(x2 ~ x1, jacoby, panel = function(x, y, ...) {
##   panel.xyplot(x, y, ...)
##   panel.abline(h = mean(y), v=mean(x), lty = 2,
##     lwd=2, col="#EB811B")
## })

## ----plot5l1, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
xyplot(x2 ~ x1, jacoby, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)  
  panel.abline(h = mean(y), v=mean(x), lty = 2,
    lwd=2, col="#EB811B")  
})

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## xyplot(x2 ~ x1, jacoby) +
##   latticeExtra::layer(panel.abline(h=mean(y), v=mean(x),
##   lty = 2, lwd=2, col="#EB811B"))

## ----plot5l1E, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
xyplot(x2 ~ x1, jacoby) +
  latticeExtra::layer(panel.abline(h=mean(y), v=mean(x),
  lty = 2, lwd=2, col="#EB811B"))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## library(ggplot2)
## ggplot(jacoby) + geom_point(aes(x=x1, y="")) + xlab("")

## ----plot1g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
library(ggplot2)
ggplot(jacoby) + geom_point(aes(y="", x=x1)) + xlab("") + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(jacoby) + geom_point(aes(x1, x2))

## ----plot2g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(jacoby) + geom_point(aes(x1, x2)) + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## p <- ggplot(jacoby) + geom_point(aes(x1, x2),
##   colour="#EB811B", size=2) + xlab("First vector") +
##   ylab("Second vector") +
##   theme(legend.position = "none")
## p

## ----plot4g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
p <- ggplot(jacoby) + geom_point(aes(x1, x2), colour="#EB811B", size=2) + xlab("First vector") + ylab("Second vector") + theme(legend.position = "none") + theme(plot.background = element_rect(fill = "transparent",colour = NA))
p

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## p + geom_hline(yintercept=mean(jacoby$x2),
##   colour="#EB811B", linetype=2) +
##   geom_vline(xintercept=mean(jacoby$x1),
##   colour="#EB811B", linetype=2)

## ----plot5g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
p + geom_hline(yintercept=mean(jacoby$x2), colour="#EB811B", linetype=2) + geom_vline(xintercept=mean(jacoby$x1),  colour="#EB811B", linetype=2) + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## stripchart(jacoby, pch=1, xlab="Data Values",
##   ylab="Variable", main="Scatterchart")

## ----plot6, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
stripchart(jacoby, pch=1, xlab="Data Values", ylab="Variable", main="Scatterchart")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## stripchart(jacoby, method="jitter",
##   jitter=0.05, pch=1,
##   xlab="Data Values",
##   ylab="Variable",
##   main="Scatterchart with jittering")

## ----plot7, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
stripchart(jacoby, method="jitter", jitter=0.05, pch=1, xlab="Data Values", ylab="Variable", main="Scatterchart with jittering")

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
jacobyS <- stack(jacoby)
str(jacobyS, width=45, strict.width="cut")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## stripplot(ind ~ values, data=jacobyS, jitter.data=TRUE)

## ----plot7l, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
stripplot(ind ~ values, data=jacobyS, jitter.data=TRUE)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## #library(reshape2)
## #jacobyS <- melt(jacoby)
## p <- ggplot(jacobyS, aes(values, ind))
## p + geom_point() + ylab("")

## ----plot6g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
#library(reshape2)
#jacobyS <- suppressMessages(melt(jacoby))
p <- ggplot(jacobyS, aes(values, ind))
p + geom_point() + ylab("") + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## set.seed(1)
## p + geom_point() + ylab("") +
##   geom_jitter(position=position_jitter(0.1))

## ----plot7g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
set.seed(1)
p + geom_point() + ylab("") + geom_jitter(position=position_jitter(0.05)) + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## boxplot(jacoby)

## ----plot8, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
boxplot(jacoby)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## bwplot(values ~ ind, data=jacobyS)

## ----plot8l, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
bwplot(values ~ ind, data=jacobyS)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## p <- ggplot(jacobyS, aes(ind, values))
## p + geom_boxplot() + xlab("")

## ----plot8g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
p <- ggplot(jacobyS, aes(ind, values))
p + geom_boxplot() + xlab("") + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## oldpar <- par(no.readonly=TRUE)
## par(mfrow=c(2,2))
## brks <- seq(15,55,by=5)
## for (i in 1:4) {
##  hist(jacoby[,i], breaks=brks, col="grey85",
##  xlab=paste("x", i, ": seq(15, 55, by=5)", sep=""),
##  freq=TRUE, main="")
## }
## par(oldpar)

## ----plot9, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
for (i in 1:4) {
 hist(jacoby[,i], breaks=seq(15,55,by=5), col="grey85",
 xlab=paste("x", i, ": seq(15, 55, by=5)", sep=""), freq=TRUE, main="")
}
par(oldpar)

## ----plot9a, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
for (i in 1:4) {
 hist(jacoby[,i], breaks=seq(17.5,52.5,by=5), col="grey85",
 xlab=paste("x", i, ": seq(17.5, 52.5, by=5)", sep=""), freq=TRUE, main="")
}
par(oldpar)

## ----plot9b, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
for (i in 1:4) {
 hist(jacoby[,i], breaks=seq(17.5,52.5,by=2.5), col="grey85",
 xlab=paste("x", i, ": seq(17.5, 52.5, by=2.5)", sep=""), freq=TRUE, main="")
}
par(oldpar)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## histogram(~ values | ind, data=jacobyS,
##   breaks=seq(15,55,by=5), type="count",
##   index.cond=list(c(3,4,1,2)))

## ----plot9l, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
histogram(~ values | ind, data=jacobyS, breaks=seq(15,55,by=5), type="count", index.cond=list(c(3,4,1,2)), main="breaks: seq(15, 55, by=5)")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(jacobyS, aes(x=values)) +
##   geom_histogram(breaks=seq(15, 55, by=5)) +
##   facet_wrap(~ ind, ncol=2)

## ----plot9g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(jacobyS, aes(x=values)) + geom_histogram(breaks=seq(15, 55, by=5)) + facet_wrap(~ ind, ncol=2) + theme(plot.background = element_rect(fill = "transparent",colour = NA)) + ggtitle("breaks: seq(15, 55, by=5)")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## oldpar <- par(no.readonly=TRUE)
## par(mfrow=c(2,2))
## for (i in 1:4) {
##   plot(density(jacoby[,i], bw=1.5), main="",
##     xlim=c(15,55), ylim=c(0, 0.15))
##   rug(jacoby[,i], ticksize=0.07, lwd=2)
##   title(main=paste("Smoothed histogram of x",
##     i, sep=""))
## }
## par(oldpar)

## ----plot10, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
for (i in 1:4) {
  plot(density(jacoby[,i], bw=1.5), main="",
    xlim=c(15,55), ylim=c(0, 0.15))
  rug(jacoby[,i], ticksize=0.07, lwd=2)
  title(main=paste("Smoothed histogram of x",
    i, sep=""))
}
par(oldpar)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## densityplot(~ values | ind, data=jacobyS, bw=1.5,
##   index.cond=list(c(3,4,1,2)))

## ----plot10l, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
densityplot(~ values | ind, data=jacobyS, bw=1.5, index.cond=list(c(3,4,1,2)))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(jacobyS, aes(x=values)) +
##   geom_density(bw=1.5) + geom_rug() +
##   facet_wrap(~ ind, ncol=2) +
##   xlim(c(15, 55))

## ----plot10g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(jacobyS, aes(x=values)) + geom_density(bw=1.5) + geom_rug() + facet_wrap(~ ind, ncol=2) + theme(plot.background = element_rect(fill = "transparent",colour = NA)) + ggtitle("bw: 1.5") + xlim(c(15, 55))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## oldpar <- par(no.readonly=TRUE)
## par(mfrow=c(2,2))
## for (i in 1:4) {
##   plot(ecdf(jacoby[,i]), main="",
##     xlim=c(15,55))
##   title(main=paste("ECDF of x",
##     i, sep=""))
## }
## par(oldpar)

## ----plot16, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
for (i in 1:4) {
  plot(ecdf(jacoby[,i]), main="",
    xlim=c(15,55))
  title(main=paste("ECDF of x",
    i, sep=""))
}
par(oldpar)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## library(latticeExtra)
## ecdfplot(~ values | ind, data=jacobyS,
##   index.cond=list(c(3,4,1,2)))
## detach(package:latticeExtra)

## ----plot16l, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
suppressPackageStartupMessages(library(latticeExtra))
ecdfplot(~ values | ind, data=jacobyS, index.cond=list(c(3,4,1,2)))
detach(package:latticeExtra)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(jacobyS, aes(x=values)) +
##   stat_ecdf() +
##   facet_wrap(~ ind, ncol=2)

## ----plot16g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(jacobyS, aes(x=values)) + stat_ecdf() + facet_wrap(~ ind, ncol=2) + theme(plot.background = element_rect(fill = "transparent",colour = NA)) + ggtitle("ECDF plots")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## oldpar <- par(no.readonly=TRUE)
## par(mfrow=c(2,2))
## x <- qunif(ppoints(100))
## for (i in 1:4) {
##   qqplot(x=x, y=jacoby[,i])
##   title(main=paste("Uniform QQ of x",
##     i, sep=""))
## }
## par(oldpar)

## ----plot17, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
x <- qunif(ppoints(100))
for (i in 1:4) {
  qqplot(x=x, y=jacoby[,i], xlab="", ylab="", ylim=c(15, 55))
  title(main=paste("Uniform QQ of x",
    i, sep=""))
}
par(oldpar)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## oldpar <- par(no.readonly=TRUE)
## par(mfrow=c(2,2))
## for (i in 1:4) {
##   qqnorm(y=jacoby[,i], xlab="", ylab="",
##     ylim=c(15, 55), main="")
##   title(main=paste("Normal QQ of x",
##     i, sep=""))
## }
## par(oldpar)

## ----plot18, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
for (i in 1:4) {
  qqnorm(y=jacoby[,i], xlab="", ylab="", ylim=c(15, 55), main="")
  title(main=paste("Normal QQ of x",
    i, sep=""))
}
par(oldpar)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## qqmath(~ values | ind, data=jacobyS,
##   distribution=qunif,
##   index.cond=list(c(3,4,1,2)))

## ----plot17l, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
qqmath(~ values | ind, data=jacobyS, distribution=qunif, main="Uniform", index.cond=list(c(3,4,1,2)))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## qqmath(~ values | ind, data=jacobyS,
##   distribution=qnorm,
##   index.cond=list(c(3,4,1,2)))

## ----plot18l, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
qqmath(~ values | ind, data=jacobyS, distribution=qnorm, main="Normal", index.cond=list(c(3,4,1,2)))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(jacobyS, aes(sample=values)) +
##   stat_qq(distribution=qunif) +
##   facet_wrap(~ ind, ncol=2)

## ----plot17g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(jacobyS, aes(sample=values)) + stat_qq(distribution=qunif) + facet_wrap(~ ind, ncol=2) + theme(plot.background = element_rect(fill = "transparent",colour = NA)) + ggtitle("Uniform QQ plots")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(jacobyS, aes(sample=values)) +
##   stat_qq(distribution=qnorm) +
##   facet_wrap(~ ind, ncol=2)

## ----plot18g, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(jacobyS, aes(sample=values)) + stat_qq(distribution=qnorm) + facet_wrap(~ ind, ncol=2) + theme(plot.background = element_rect(fill = "transparent",colour = NA)) + ggtitle("Normal QQ plots")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## QQQ <- read.spss("CY6_MS_CMB_STU_QQQ.sav", to.data.frame=TRUE)
## QQQ1 <- QQQ[,c(1,2,4,29,30,35,64,810:839,920)]
## QQQ1$math_mean <- apply(as.matrix(QQQ1[,8:17]), 1, mean)
## QQQ1$math_sd <- apply(as.matrix(QQQ1[,8:17]), 1, sd)
## QQQ1$read_mean <- apply(as.matrix(QQQ1[,18:27]), 1, mean)
## QQQ1$read_sd <- apply(as.matrix(QQQ1[,18:27]), 1, sd)
## QQQ1$sci_mean <- apply(as.matrix(QQQ1[,28:37]), 1, mean)
## QQQ1$sci_sd <- apply(as.matrix(QQQ1[,28:37]), 1, sd)
## QQQ <- read.spss("CY6_MS_CMB_STU_QQQ.sav", to.data.frame=TRUE, use.value.labels=FALSE)
## QQQ1$CNT_vl <- QQQ1$CNT
## QQQ1$CNT <- QQQ$CNT
## saveRDS(QQQ1, file="dicook/pisa_raw_subset.rds")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## QQQ1 <- readRDS("dicook/pisa_raw_subset.rds")
## recodes <- c("'QES'='ESP'", "'QCH'='CHN'", "'QAR'='ARG'", "'TAP'='TWN'")
## for (str in recodes) QQQ1$CNT <- car::recode(QQQ1$CNT, str)
## QQQ2 <- droplevels(QQQ1[!(as.character(QQQ1$CNT) %in% c("QUC", "QUD", "QUE")),])
## library(ISOcodes)
## data("ISO_3166_1")
## scores <- merge(QQQ2, ISO_3166_1[,c("Alpha_3", "Name")], by.x="CNT", by.y="Alpha_3", all.x=TRUE)
## scores$Name[scores$CNT == "KSV"] <- "Kosovo"
## saveRDS(scores, file="dicook/pisa_subset.rds")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## QQQ1 <- readRDS("dicook/pisa_raw_subset.rds")
## recodes <- c("'QES'='ESP'", "'QCH'='CHN'", "'QAR'='ARG'", "'TAP'='TWN'")
## CNT_count <- as.data.frame(table(QQQ1$CNT))
## names(CNT_count) <- c("CNT", "n")
## for(str in recodes) CNT_count$CNT <- car::recode(CNT_count$CNT, str)
## CNT_count1 <- aggregate(CNT_count$n, list(CNT_count$CNT), sum)
## CNT_count2 <- droplevels(CNT_count1[!(as.character(CNT_count1$Group.1) %in% c("QUC", "QUD", "QUE")),])
## names(CNT_count2) <- c("CNT", "n")
## library(ISOcodes)
## data("ISO_3166_1")
## countries <- merge(CNT_count2, ISO_3166_1, by.x="CNT", by.y="Alpha_3", all.x=TRUE)
## countries$Name[countries$CNT == "KSV"] <- "Kosovo"
## saveRDS(countries, file="dicook/countries.rds")

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
QQQ1 <- readRDS("dicook/pisa_subset.rds")
countries <- readRDS("dicook/countries.rds")
a0 <- split(QQQ1, list(QQQ1$CNT, QQQ1$ST004D01T))

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
math_mean <- sapply(a0, function(x) weighted.mean(x$math_mean, w=x$SENWT))
n2 <- length(math_mean)/2
country <- sapply(strsplit(names(math_mean), "\\."), "[", 1)[1:n2]
co <- match(country, countries$CNT)
nms <- countries$Name[co]
gender <- factor(sapply(strsplit(names(math_mean), "\\."), "[", 2))[1:n2]
o <- order(math_mean[1:n2])

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## dotchart(math_mean[o], label=nms[o], cex=0.5, pt.cex=0.9, pch=3, xlim=c(325, 575))
## points(math_mean[n2+o], 1:n2, cex=0.9, pch=4, col="brown1")
## legend("bottomright", legend=levels(gender), col=c("black", "brown1"), pch=3:4, pt.cex=0.9, bty="n", cex=0.8)
## title("PISA mean math PV means, age 15")

## ----plot11, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
dotchart(math_mean[o], label=nms[o], cex=0.5, pt.cex=0.9, pch=3, xlim=c(325, 575))
points(math_mean[n2+o], 1:n2, cex=0.9, pch=4, col="brown1")
legend("bottomright", legend=levels(gender), col=c("black", "brown1"), pch=3:4, pt.cex=0.9, bty="n", cex=0.8)
title("PISA mean math PV means, age 15")

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
library(matrixStats)
sqn <- sqrt(sapply(a0, function(x) sum(x$SENWT)))
math_se <- sapply(a0, function(x) weightedSd(x$math_mean, w=x$SENWT))/sqn

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## for (i in 1:n2) segments(x0=math_mean[o][i]-(2*math_s2[o][i]), y0=i,
##   x1=math_mean[o][i]+2*(math_se[o][i]), lty=2)

## ----plot12, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
dotchart(math_mean[o], label=nms[o], cex=0.5, pt.cex=0.9, pch=3, xlim=c(270, 650))
for (i in 1:n2) segments(x0=math_mean[o][i]-2*(math_se[o][i]), y0=i, x1=math_mean[o][i]+2*(math_se[o][i]), lwd=2)
points(math_mean[n2+o], (1:n2)-0.25, cex=0.9, pch=4, col="brown1")
for (i in 1:n2) segments(x0=math_mean[n2+o][i]-2*(math_se[n2+o][i]), y0=i-0.25, x1=math_mean[n2+o][i]+2*(math_se[n2+o][i]), lwd=2, col="brown1")
legend("bottomright", legend=levels(gender), col=c("black", "brown1"), pch=3:4, pt.cex=0.9, bty="n", cex=0.8)
title("PISA math PV means, age 15, +/- 2se")

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
read_mean <- sapply(a0, function(x) weighted.mean(x$read_mean, w=x$SENWT))
ro <- order(read_mean[1:n2])

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
sci_mean <- sapply(a0, function(x) weighted.mean(x$sci_mean, w=x$SENWT))
so <- order(sci_mean[1:n2])

## ----plot13, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
dotchart(read_mean[ro], label=nms[ro], cex=0.5, pt.cex=0.9, pch=3, xlim=c(325, 575))
points(read_mean[n2+ro], 1:n2, cex=0.9, pch=4, col="brown1")
legend("bottomright", legend=levels(gender), col=c("black", "brown1"), pch=3:4, pt.cex=0.9, bty="n", cex=0.8)
title("PISA mean reading PV means, age 15")

## ----plot14, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
dotchart(sci_mean[so], label=nms[so], cex=0.5, pt.cex=0.9, pch=3, xlim=c(325, 575))
points(sci_mean[n2+so], 1:n2, cex=0.9, pch=4, col="brown1")
legend("bottomright", legend=levels(gender), col=c("black", "brown1"), pch=3:4, pt.cex=0.9, bty="n", cex=0.8)
title("PISA mean science PV means, age 15")

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
math_gap <- math_mean[1:n2] - math_mean[n2+(1:n2)]
read_gap <- read_mean[1:n2] - read_mean[n2+(1:n2)]
sci_gap <- sci_mean[1:n2] - sci_mean[n2+(1:n2)]
gaps <- data.frame(math_gap, read_gap, sci_gap, iso_a3=country,
  iso_a2=countries$Alpha_2[co])

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
library(rnaturalearth)
library(rnaturalearthdata)
data(countries50)
library(sf)
countries50 <- st_as_sf(countries50)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## world_gaps <- merge(countries50, gaps, by="iso_a2", all.x=TRUE)
## library(tmap)
## tm_shape(world_gaps) + tm_fill("math_gap", palette="div", n=7, style="jenks")
## #ttm()
## #last_map()

## ----plot15, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
world_gaps <- merge(countries50, gaps, by="iso_a2", all.x=TRUE)
library(tmap)
tm_shape(world_gaps) + tm_fill("math_gap", palette="div", n=7, style="jenks") + tm_layout(bg.color="transparent")

## ----sI, echo = TRUE, mysize=TRUE, size='\\tiny'-------------------------
sessionInfo()

