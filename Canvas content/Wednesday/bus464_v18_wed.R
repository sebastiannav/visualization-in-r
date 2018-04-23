
## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
## cat(system('echo "search()" | R --no-save --vanilla', intern=TRUE)[20:23], sep="\n")
search()

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
capabilities()
grSoftVersion()

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
a11 <- readRDS("dicook/a11.rds")
png("plot.png")
dev.cur()
boxplot(sci_mean ~ ST004D01T, a11)
dev.off()

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
png(tempfile())
unlist(dev.capabilities())
dev.size("in")
dev.interactive()
dev.off()

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## opar <- par(mfrow=c(2,2))
## boxplot(sci_mean ~ ST004D01T, a11, las=0)
## boxplot(sci_mean ~ ST004D01T, a11, las=1)
## boxplot(sci_mean ~ ST004D01T, a11, lwd=2)
## boxplot(sci_mean ~ ST004D01T, a11, pch=4)
## par(opar)

## ----plot1, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
opar <- par(mfrow=c(2,2))
boxplot(sci_mean ~ ST004D01T, a11, las=0)
boxplot(sci_mean ~ ST004D01T, a11, las=1)
boxplot(sci_mean ~ ST004D01T, a11, lwd=2)
boxplot(sci_mean ~ ST004D01T, a11, pch=4)
par(opar)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## opar <- par(mfrow=c(2,2))
## boxplot(sci_mean ~ ST004D01T, a11, axes=FALSE)
## axis(2, las=1)
## axis(1, at=1:2, labels=levels(a11$ST004D01T))
## box()
## par(opar)

## ----plot2, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
opar <- par(mfrow=c(2,2))
boxplot(sci_mean ~ ST004D01T, a11, axes=FALSE)
boxplot(sci_mean ~ ST004D01T, a11, axes=FALSE)
axis(2, las=1)
boxplot(sci_mean ~ ST004D01T, a11, axes=FALSE)
axis(2, las=1)
axis(1, at=1:2, labels=levels(a11$ST004D01T))
boxplot(sci_mean ~ ST004D01T, a11, axes=FALSE)
axis(2, las=1)
axis(1, at=1:2, labels=levels(a11$ST004D01T))
box()
par(opar)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## layout(cbind(c(1, 1), c(2,2)))
## opar <- par(mar=c(3, 3, 0, 0)+0.1)
## boxplot(sci_mean ~ ST004D01T, a11)
## par(mar=c(10, 3, 4, 0)+0.1)
## boxplot(sci_mean ~ ST004D01T, a11)
## layout(1)

## ----plot3, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
layout(cbind(c(1, 1), c(2,2)))
opar <- par(mar=c(3, 3, 0, 0)+0.1)
boxplot(sci_mean ~ ST004D01T, a11)
par(mar=c(10, 3, 4, 0)+0.1)
boxplot(sci_mean ~ ST004D01T, a11)
layout(1)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
library("jpeg")
im <- as.raster(readJPEG("image001.jpg"))
prop <- dim(im)[2]/dim(im)[1]
png("plot1.png")
par("din")
plot(1, type="n", axes=FALSE, xlim=c(1, 10),
  ylim=c(1, 10), asp=1, ann=FALSE)
rasterImage(im, 1:9, 1:9, (1:9) + prop, 2:10)
dev.off()

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## par(oma=rep(3, 4), bg="gray80")
## plot(c(0, 1), c(0, 1), type="n", ann=FALSE,
##   axes=FALSE); box("outer", col="gray")
## par(xpd=TRUE)
## rect(-1, -1, 2, 2, col="gray90")
## box("figure"); par(xpd=FALSE)
## rect(-1, -1, 2, 2, col="gray80")
## box("plot", lty="dashed")
## text(.5, .5, "Plot Region")
## mtext("Figure Region", side=3, line=2)
## for (i in 1:4) mtext(paste("Outer margin", i),
##   side=i, line=1, outer=TRUE)

## ----plot7, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
par(oma=rep(3, 4), bg="gray80")
plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
box("outer", col="gray")
par(xpd=TRUE)
rect(-1, -1, 2, 2, col="gray90")
box("figure")
par(xpd=FALSE)
rect(-1, -1, 2, 2, col="gray80")
box("plot", lty="dashed")
text(.5, .5, "Plot Region")
mtext("Figure Region", side=3, line=2)
for (i in 1:4)
    mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## opar <- par(mfrow=c(2,2))
## plot(lm(math_mean ~ read_mean, data=a11), pch=".")
## par(opar)

## ----plot4, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
opar <- par(mfrow=c(2,2))
plot(lm(math_mean ~ read_mean, data=a11), pch=".")
par(opar)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny', cache=TRUE----------------
db <- tools::CRAN_package_db()
types <- c("Depends", "Imports", "Suggests")
pkgs <- c("graphics", "grid", "lattice", "ggplot2")
(tbl <- sapply(types, function(type) sapply(pkgs, 
  function(pkg) length(db[grep(pkg, db[, type]), 1]))))
class(tbl)

## ----plot5, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
library(lattice)
barchart(tbl, auto.key=TRUE, horizontal=FALSE)

## ---- echo = TRUE, fig.show='hide', mysize=TRUE, size='\\tiny'-----------
b <- barchart(tbl, auto.key=TRUE,
  horizontal=FALSE)
x11()
barplot(t(tbl), legend.text=TRUE,
  args.legend=list(x="top", bty="n",
  cex=0.8, y.intersp=3))
gridGraphics::grid.echo()
library(grid)
g <- grid.grab()
dev.off()
#grid.newpage()
#gridExtra::grid.arrange(g, b, ncol=1)

## ----plot6, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
grid.newpage()
gridExtra::grid.arrange(g, b, ncol=1)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## grid.rect(gp = gpar(lty = "dashed"))
## vp <- viewport(width = 0.5, height = 0.5)
## pushViewport(vp)
## grid.rect(gp = gpar(col = "grey"))
## grid.text("quarter of the page", y = 0.85)
## pushViewport(vp)
## grid.rect()
## grid.text("quarter of the\nprevious viewport")
## popViewport(2)

## ----plot8, fig.show='hide', fig.height=6, fig.width=6, dev.args=list(family="Fira Sans")----
grid.rect(gp = gpar(lty = "dashed"))
vp <- viewport(width = 0.5, height = 0.5)
pushViewport(vp)
grid.rect(gp = gpar(col = "grey"))
grid.text("quarter of the page", y = 0.85)
pushViewport(vp)
grid.rect()
grid.text("quarter of the\nprevious viewport")
popViewport(2)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
ggplot2:::print.ggplot

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## gg <- ggplot(broom::tidy(as.table(tbl)),
##   aes(x=Var1, y=Freq, fill=Var2)) + geom_col() +
##   xlab("") + guides(fill=guide_legend(title="")) +
##   theme(legend.position="top")
## gg2 <- ggplotGrob(gg)
## t <- gridExtra::tableGrob(as.data.frame(tbl))
## #gridExtra::grid.arrange(g, b, gg2, t, ncol=4)

## ----plot9, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans")----
library(ggplot2)
gg <- ggplot(broom::tidy(as.table(tbl)), aes(x=Var1, y=Freq, fill=Var2)) + geom_col() + xlab("") + guides(fill=guide_legend(title="")) + theme(legend.position="top")
gg2 <- ggplotGrob(gg)
t <- gridExtra::tableGrob(as.data.frame(tbl))
gridExtra::grid.arrange(g, b, gg2, t, ncol=4)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## library(rggobi)
## ggobi(a11[,c(1, 4, 39:44)])


## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
library(ggvis)
a11 %>% ggvis(~ interaction(CNT, ST004D01T), ~ math_mean) %>% layer_boxplots()


## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
library(plotly)
plot_ly(a11, x = ~math_mean, color = ~CNT, type = "box")


## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
library(googleVis)
plot(gvisHistogram(a11[a11$ST004D01T == "Male", c("math_mean", "read_mean", "sci_mean")]))
plot(gvisScatterChart(a11[, c("math_mean", "read_mean", "sci_mean")]))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
library(metricsgraphics)
a11$math_mean %>% mjs_hist()

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
library(highcharter)
## ## Highcharts (www.highcharts.com) is a Highsoft
## ## software product which is not free for
## ## commercial and Governmental use
hcboxplot(x=a11$math_mean, var2=a11$ST004D01T,
 var=a11$CNT)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
library(reticulate)
np <- import("numpy")
plt <- import("matplotlib.pyplot")
phi <- np$arange(0, 3*np$pi, 0.0025)
x <- np$cos(5*phi)
y <- np$sin(3*phi)
plt$plot(x,y)
plt$savefig('sampleFileName.png')
#plt$show()

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
library(rgl)
z <- rep(1, nrow(x))
rgl.open()
rgl.points(x, y, z)
rgl.snapshot("rgl_snapshot.png", top=TRUE)
rgl.close()

## ----sI, echo = TRUE, mysize=TRUE, size='\\tiny'-------------------------
sessionInfo()

