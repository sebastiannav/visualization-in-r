
## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
QQQ1 <- readRDS("dicook/pisa_subset.rds")
countries <- readRDS("dicook/countries.rds")
countries$Alpha_2[countries$Name == "Kosovo"] <- "XK"
a0 <- split(QQQ1, list(QQQ1$CNT, QQQ1$ST004D01T))

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
math_mean <- sapply(a0, function(x) weighted.mean(x$math_mean, w=x$SENWT))
n2 <- length(math_mean)/2
country <- sapply(strsplit(names(math_mean), "\\."), "[", 1)[1:n2]
co <- match(country, countries$CNT)
gender <- factor(sapply(strsplit(names(math_mean), "\\."), "[", 2))

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
library(matrixStats)
sqn <- sqrt(sapply(a0, function(x) sum(x$SENWT)))
math_se <- sapply(a0, function(x) weightedSd(x$math_mean, w=x$SENWT))/sqn

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
read_mean <- sapply(a0, function(x) weighted.mean(x$read_mean, w=x$SENWT))

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
sci_mean <- sapply(a0, function(x) weighted.mean(x$sci_mean, w=x$SENWT))

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
math_mean_female <- math_mean[gender == "Female"]
math_mean_male <- math_mean[gender == "Male"]
math_se_female <- math_se[gender == "Female"]
math_se_male <- math_se[gender == "Male"]
math_gap <- math_mean_female - math_mean_male
read_mean_female <- read_mean[gender == "Female"]
read_mean_male <- read_mean[gender == "Male"]
read_gap <- read_mean_female - read_mean_male
sci_mean_female <- sci_mean[gender == "Female"]
sci_mean_male <- sci_mean[gender == "Male"]
sci_gap <- sci_mean_female - sci_mean_male
df <- data.frame(math_mean_female, math_mean_male, math_se_female, math_se_male,
  read_mean_female, read_mean_male, sci_mean_female, sci_mean_male,
  math_gap, read_gap, sci_gap, iso_a3=country, iso_a2=countries$Alpha_2[co])

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'-----------------
library(rnaturalearth)
library(rnaturalearthdata)
data(countries50)
library(sf)
countries50 <- st_as_sf(countries50)
countries50$iso_a2[countries50$name == "Kosovo"] <- "XK"

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## world_gaps <- merge(countries50[!is.na(countries50$iso_a2),], df, by="iso_a2", all.x=TRUE)
## library(tmap)
## tm_shape(world_gaps) + tm_fill("math_gap", palette="RdYlGn", n=7, style="jenks",
##   auto.palette.mapping=FALSE)

## ----plot1, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
world_gaps <- merge(countries50[!is.na(countries50$iso_a2),], df, by="iso_a2", all.x=TRUE)
library(tmap)
tm_shape(world_gaps) + tm_fill("math_gap", palette="RdYlGn", n=7, style="jenks", auto.palette.mapping=FALSE) + tm_layout(bg.color="transparent")

## ---- echo=FALSE, results='hide'-----------------------------------------
options(width = 50)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
x <- na.exclude(world_gaps$math_gap)
nclass.Sturges(x)
nclass.scott(x)
(n <- nclass.FD(x))
nclass.Sturges(x[sign(x) == 1L])
nclass.Sturges(x[sign(x) == -1L])

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
n
range(x)
(p <-pretty(x, n=n))
length(p)
(p <-pretty(x, n=n, high.u.bias=3))
length(p)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
suppressPackageStartupMessages(library(classInt))
(ppd <- classIntervals(x, n=n, style="pretty",
  cutlabels=TRUE))
(pp3 <- classIntervals(x, n=n, style="pretty",
  high.u.bias=3, cutlabels=TRUE))

## ----fig1a, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
library(RColorBrewer)
pal <- brewer.pal(7, "RdYlGn")
oopar <- par(mfrow=c(1, 2))
plot(ppd, pal=pal, main="Pretty (default)", xlab="", ylab="")
plot(pp3, pal=pal, main="Pretty (compressed)", xlab="", ylab="")
par(oopar)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
(pq7 <- classIntervals(x, n=n, style="quantile",
  type=7L, dataPrecision=2))
(pq3 <- classIntervals(x, n=n, style="quantile",
  type=3L, dataPrecision=2))

## ----fig1b, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
oopar <- par(mfrow=c(1, 2))
plot(pq7, pal=pal, main="Quantile type=7", xlab="", ylab="")
plot(pq3, pal=pal, main="Quantile type=3", xlab="", ylab="")
par(oopar)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
(peq <- classIntervals(x, n=n, style="equal",
  dataPrecision=2))
#diff(peq$brks)
(psd <- classIntervals(x, n=n, style="sd", 
  high.u.bias=3, dataPrecision=2))
#(psd$brks-attr(psd, "parameters")["center"]) /
#  attr(psd, "parameters")["scale"]

## ----fig1c, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
oopar <- par(mfrow=c(1, 2))
plot(peq, pal=pal, main="Equal intervals", xlab="", ylab="")
plot(psd, pal=pal, main="Standard deviations", xlab="", ylab="")
par(oopar)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
(phc7 <- classIntervals(x, n=n, style="hclust", 
  method="complete", dataPrecision=2))
(phc5 <- getHclustClassIntervals(phc7, 5))

## ----fig1d, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
oopar <- par(mfrow=c(1, 2))
plot(phc7, pal=pal, main="Hierarchical clusters (7)", xlab="", ylab="")
plot(phc5, pal=pal, main="Hierarchical clusters (5)", xlab="", ylab="")
par(oopar)

## ---- echo = FALSE, mysize=TRUE, size='\\tiny'---------------------------
set.seed(1)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
(pk7 <- classIntervals(x, n=n, style="kmeans",
  dataPrecision=2))

## ---- echo = FALSE, mysize=TRUE, size='\\tiny'---------------------------
set.seed(1)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
(pbc7 <- classIntervals(x, n=n, style="bclust", 
  verbose=FALSE, dataPrecision=2))

## ----fig1e, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
oopar <- par(mfrow=c(1, 2))
plot(pk7, pal=pal, main="K-means clusters", xlab="", ylab="")
plot(pbc7, pal=pal, main="Bagged clusters", xlab="", ylab="")
par(oopar)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
(pj7 <- classIntervals(x, n=n, style="jenks",
  dataPrecision=2))
(pf7 <- classIntervals(x, n=n, style="fisher",
  dataPrecision=2))

## ----fig1f, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
oopar <- par(mfrow=c(1, 2))
plot(pj7, pal=pal, main="Jenks natural breaks", xlab="", ylab="")
plot(pf7, pal=pal, main="Fisher natural breaks", xlab="", ylab="")
par(oopar)

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
str(findCols(pj7))
str(findInterval(x, pj7$brks, all.inside=TRUE,
  left.open=TRUE))
str(as.integer(cut(x, breaks=pj7$brks,
  include.lowest=TRUE)))


## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
suppressPackageStartupMessages(library(ggplot2))
stat_bin

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
StatBin

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
cat(capture.output(print(StatBin$compute_group))[-(1:5)], sep="\n")

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
ggplot2:::bin_breaks_bins

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
cat(capture.output(print(ggplot2:::bin_breaks_width))[-(4:8)], sep="\n")

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
ggplot2:::bin_breaks

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
ggplot2:::bins


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny', cache=TRUE-----
library(mapview)
m <- mapview(world_gaps, zcol="math_gap",
  col.regions=pal, at=pj7$brks)
#mapshot(m, file=paste0(getwd(), "/map1.png"))
m

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## library(tmap)
## tm_shape(world_gaps) + tm_fill("math_gap",
##   n=n, style="jenks", palette=pal) +
##   tm_borders(lwd=0.5, alpha=0.4)

## ---- fig7, fig.show='hide', fig.height=6, fig.width=9, dev.args=list(family="Fira Sans", bg="transparent")----
library(tmap)
tm_shape(world_gaps) + tm_fill("math_gap", n=n, style="jenks", palette=pal, auto.palette.mapping=FALSE) + tm_borders(lwd=0.5, alpha=0.4) + tm_layout(bg.color="transparent")


## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## tm_shape(world_gaps) + tm_fill(c("math_mean_female", "math_mean_male"), n=7, style="jenks", palette=pal) +
##   tm_facets(free.scales=FALSE) + tm_borders(lwd=0.5, alpha=0.4) + tm_layout(panel.labels=c("Female", "Male"))

## ---- fig8, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
tm_shape(world_gaps) + tm_fill(c("math_mean_female", "math_mean_male"), n=7, style="jenks", title="Math PV means", palette=pal, auto.palette.mapping=FALSE) + tm_facets(free.scales=FALSE) + tm_borders(lwd=0.5, alpha=0.4) + tm_layout(panel.labels=c("Female", "Male")) + tm_layout(bg.color="transparent")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## library(RColorBrewer)
## display.brewer.all()

## ----fig4, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
library(RColorBrewer)
opar <- par(cex=0.7)
display.brewer.all()
par(opar)

## ----fig5, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
suppressPackageStartupMessages(library(viridis))
library(colorspace)
library(ggplot2)
n_col <- 10
img <- function(obj, nam) {
  image(1:length(obj), 1, as.matrix(1:length(obj)), col=obj, 
        main = nam, ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}
oopar <- par(mfrow=c(7, 1), mar=rep(1, 4))
img(rev(terrain.colors(n_col)), "terrain.colors")
img(rev(heat.colors(n_col)), "heat")
img(rev(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab")(seq(0, 1, length=n_col))), "ggplot default")
img(scales::gradient_n_pal(scales::brewer_pal(type="seq")(9))(seq(0, 1, length=n_col)), "brewer blues")
img(scales::gradient_n_pal(scales::brewer_pal(type="seq", palette = "YlGnBu")(9))(seq(0, 1, length=n_col)), "brewer yellow-green-blue")
img(rev(viridis(n_col)), "viridis")
img(rev(magma(n_col)), "magma")
par(oopar)

## ----fig6, fig.show='hide', fig.height=6, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
suppressPackageStartupMessages(library(cartography))
display.carto.all(n = 10)


## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
nordics <- c("DNK", "FIN", "ISL", "NOR", "SWE")
a1 <- a0[which(sapply(strsplit(names(a0), "\\."),
  "[", 1) %in% nordics)]
a11 <- do.call("rbind", a1)
a11$CNT <- droplevels(a11$CNT)
saveRDS(a11, "dicook/a11.rds")
library(lattice)
library(ggplot2)


## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## histogram(~ sci_mean | CNT*ST004D01T, data=a11, main="Science mean PVs", xlab="")

## ---- figl1, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
histogram(~ sci_mean | CNT*ST004D01T, data=a11, main="Science mean PVs", xlab="")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(a11, aes(x=sci_mean)) + geom_histogram() + facet_wrap(~ ST004D01T + CNT, nrow=2) +
##   ggtitle("Science mean PVs") + xlab("")

## ---- figl2, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(a11, aes(x=sci_mean)) + geom_histogram() + facet_wrap(~ ST004D01T + CNT, nrow=2) + ggtitle("Science mean PVs") + xlab("") + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## histogram(~ sci_mean | CNT*ST004D01T, data=a11, main="Science mean PVs", xlab="", type="count")

## ---- figl3, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
histogram(~ sci_mean | CNT*ST004D01T, data=a11, main="Science mean PVs", xlab="", type="count")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## densityplot(~ sci_mean | ST004D01T, groups=CNT, data=a11, auto.key=list(space="right"),
##   plot.points=FALSE, main="Science mean PVs", xlab="")

## ---- figl4, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
densityplot(~ sci_mean | ST004D01T, groups=CNT, data=a11, auto.key=list(space="right"), plot.points=FALSE, main="Science mean PVs", xlab="")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(a11, aes(x=sci_mean)) + geom_density(aes(colour=CNT)) + facet_wrap(~ ST004D01T, ncol=2) +
##   ggtitle("Science mean PVs") + xlab("")

## ---- figl5, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(a11, aes(x=sci_mean)) + geom_density(aes(colour=CNT)) + facet_wrap(~ ST004D01T, ncol=2) + ggtitle("Science mean PVs") + xlab("") + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## densityplot(~ sci_mean | ST004D01T, groups=CNT, data=a11, auto.key=list(space="right"),
##   plot.points=FALSE, main="Science mean PVs", xlab="", type=c("l", "g"))

## ---- figl6a, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
densityplot(~ sci_mean | ST004D01T, groups=CNT, data=a11, auto.key=list(space="right"),  plot.points=FALSE, main="Science mean PVs", xlab="", type=c("l", "g"))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## densityplot(~ sci_mean | CNT, groups=ST004D01T, a11, auto.key=list(space="right"),
##   plot.points=FALSE, main="Science mean PVs", xlab="")

## ---- figl6, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
densityplot(~ sci_mean | CNT, groups=ST004D01T, a11, auto.key=list(space="right"), plot.points=FALSE, main="Science mean PVs", xlab="")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(a11, aes(x=sci_mean)) + geom_density(aes(colour=ST004D01T)) + facet_wrap(~ CNT, ncol=3) +
##   ggtitle("Science mean PVs") + xlab("")

## ---- figl7, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(a11, aes(x=sci_mean)) + geom_density(aes(colour=ST004D01T)) + facet_wrap(~ CNT, ncol=3) + ggtitle("Science mean PVs") + xlab("") + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## bwplot(sci_mean ~ ST013Q01TA | ST004D01T, a11, main="Science mean PVs", xlab="books", ylab="")

## ---- figl8, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
bwplot(sci_mean ~ ST013Q01TA | ST004D01T, a11, main="Science mean PVs", xlab="books", ylab="")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(na.omit(a11), aes(ST013Q01TA, sci_mean)) + geom_boxplot(aes(group=ST013Q01TA)) +
##   facet_wrap(~ ST004D01T) + ggtitle("Science mean PVs") + xlab("books") + ylab("")

## ---- figl9, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(na.omit(a11), aes(ST013Q01TA, sci_mean)) + geom_boxplot(aes(group=ST013Q01TA)) + facet_wrap(~ ST004D01T) + ggtitle("Science mean PVs") + xlab("books") + ylab("") + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## bwplot(sci_mean ~ ST013Q01TA | ST004D01T, a11, main="Science mean PVs", xlab="", ylab="", varwidth=TRUE)

## ---- figl10, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
bwplot(sci_mean ~ ST013Q01TA | ST004D01T, a11, main="Science mean PVs", xlab="books", ylab="", varwidth=TRUE)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
## ggplot(na.omit(a11), aes(ST013Q01TA, sci_mean)) + geom_boxplot(aes(group=ST013Q01TA), varwidth=TRUE) +
##   facet_wrap(~ ST004D01T) + ggtitle("Science mean PVs") + xlab("") + ylab("")

## ---- figl11, fig.show='hide', fig.height=5, fig.width=12, dev.args=list(family="Fira Sans", bg="transparent")----
ggplot(na.omit(a11), aes(ST013Q01TA, sci_mean)) + geom_boxplot(aes(group=ST013Q01TA), varwidth=TRUE) + facet_wrap(~ ST004D01T) + ggtitle("Science mean PVs") + xlab("books") + ylab("") + theme(plot.background = element_rect(fill = "transparent",colour = NA))

## ----sI, echo = TRUE, mysize=TRUE, size='\\tiny'-------------------------
sessionInfo()

