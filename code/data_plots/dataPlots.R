# dataPlots.R
# Generate plots from bibliometric analysis of LAJAM.
#
# Daniel M. Palacios, 3-6 May 2012; 7-8 August 2013


################################################################################
# 1. Clear workspace, load required packages and set working directory.

# Clear environment:
rm(list = ls())

# Set working directory:
setwd("~/Documents/DMP/dmp_main/projects/dmp_projects/lajam_bibliometrics")

# Load required libraries:
library(RColorBrewer)
library(grid)
library(lattice)
#library(latticeExtra)
library(ggplot2)
library(reshape2) # for melt()

################################################################################
# 2. Load data sets as dataframes:
dat1 <- read.csv("data1.csv")
dat2 <- read.csv("data2.csv")
dat3 <- read.csv("data3.csv")
dat4 <- read.csv("data4.csv")
dat5 <- read.csv("data5.csv")
dat6 <- read.csv("data6a.csv")
#dat6b <- read.csv("data6b_NoAuthors.csv")

# Display the internal structure of the object in a compact manner:
#str(dat1)
#'data.frame':	13 obs. of  3 variables:
#$ Issue        : Factor w/ 13 levels "1(1)","2(1)",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ ppArts.NoArts: num  7.5 6 6.3 8.1 8.2 7.3 8.6 5.9 6.4 11 ...
#$ NoArts       : int  24 9 10 11 10 9 13 13 8 10 ...


################################################################################
# If using multiple panels in a single figure:
#par(mfrow = c(3, 2))

################################################################################
# Plot 1 - Line plot of Issue No. vs. number of articles and number of
# pages/article (on second y-axes).
# *** DOUBLE-AXES FIGURES ARE NOT RECOMMENDED

# Specify dummy x-axis for each issue:
dat1$x <- 1:13

# With traditional graphics (ggplot2 does not allow multiple y-axes):
# See:
# http://www.r-bloggers.com/r-graph-with-two-y-axes/
# http://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
# http://blog.earlh.com/index.php/2009/07/multiple-y-axes-in-r-plots-part-9-in-a-series/
png(filename = "dataPlot1.png", width = 1200, height = 800)
par(mar = c(7, 5, 4, 6) + 0.1, ps = 18) # default is c(5, 4, 4, 2) + 0.1
plot(NoArts ~ x, data = dat1, type = "b", lty = "solid", lwd = 1.5, col = "red",
     ylim = c(5, 25), xaxt = "n", yaxt = "n", ann = FALSE, frame = FALSE) # bty = "n"
axis(1, at = 1:13, labels = dat1$Issue, las = 2, col = "black",
     col.axis = "black", col.lab = "black", lwd = 1.5)
mtext("Issue", side = 1, line = 5, las = 0, cex = 1, col = "black")
axis(2, at = seq(5, 25, 5), col = "red", col.axis = "red", col.lab = "red",
     lwd = 1.5) # or yaxp = c(5, 25, 5)
mtext("No. Articles", side = 2, line = 3, las = 0, cex = 1, col = "red")
par(new = TRUE)
plot(ppArts.NoArts ~ x, data = dat1, type = "b", lty = "solid", lwd = 1.5,
     col = "blue", ylim = c(4, 12), axes = FALSE, ann = FALSE ) # ylab = "", xlab = ""
axis(4, at = seq(4, 12, 2), col = "blue", col.axis = "blue", col.lab = "blue",
     lwd = 1.5)
mtext("No. Pages/No. Articles", side = 4, line = 3, las = 0, cex = 1,
      col = "blue")
dev.off()

# Package latticeExtra was downloaded in order to use 'doubleYScale' as
# a lattice alternative to traditional graphics.
# See:
# http://latticeextra.r-forge.r-project.org/man/doubleYScale.html
#obj1 <- xyplot(NoArts ~ x, data = dat1, type = "b")
#obj2 <- xyplot(ppArts.NoArts ~ x, data = dat1, type = "b")
#png(filename = "dataPlot1alt.png", width = 1200, height = 800)
#par(mar = c(7, 5, 4, 6) + 0.1, ps = 18)
#doubleYScale(obj1, obj2, add.axis = TRUE, add.ylab2 = TRUE,
#             par.settings = simpleTheme(col = c("red", "black"), lty = 1:2))
#dev.off()
# THIS WORKS BUT STILL NEEDS SOME CUSTOMIZATION TO MAKE IT LOOK GOOD


# FACETED plot in GGPLOT2 (separate plots):
# FACETED plot in GGPLOT2 (separate plots):
# FACETED plot in GGPLOT2 (separate plots):
# Restructure dat1 for faceted plotting in ggplot2 using melt():
dat.issue <- melt(dat1, id.vars = "Issue",
                  measure.vars = c("NoArts", "ppArts.NoArts"),
                  variable.name = "group", value.name = "value")
levels(dat.issue$group) <- c("# Articles", "# Pages / # Articles")
#str(dat.issue)
#'data.frame':  26 obs. of  3 variables:
#$ Issue: Factor w/ 13 levels "1(1)","2(1)",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ group: Factor w/ 2 levels "# Articles","# Pages / # Articles": 1 1 1 1 1 1 1 1 1 1 ...
#$ value: num  24 9 10 11 10 9 13 13 8 10 ...
# Plot:
plot.issue <- ggplot(dat.issue, aes(x = Issue, y = value, group = group)) + 
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "white", size = 5) +
  geom_point(color = "blue", size = 3) +
  facet_grid(group ~ ., scales = "free_y") +
  labs(x = "Issue", y = NULL) + #"Value"
  theme(axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        strip.text = element_text(size = rel(1)))
# Save plot:
ggsave(filename = "figure1_output.tiff", plot = plot.issue,
       width = 8, height = 6, dpi = 300) # this gives 2400 x 1800 pixels



################################################################################
# Plot 2 - Pie chart of percent articles by article type.
# *** PIE CHARTS ARE NOT RECOMMENDED

# Compute the percentages:
dat2$percent <- dat2$NoArts/sum(dat2$NoArts)*100
dat2$perchar <- paste(as.character(round(dat2$percent, digits = 1)), "%",
                      sep = "")

# Plot pie chart (use ColorBrewer palette):
png(filename = "dataPlot2.png", width = 800, height = 800)
pie(dat2$percent, labels = dat2$MS.Type, clockwise = TRUE, col = brewer.pal(8,"Set1"), border = "white", lty = 1, main = "Article Type", ps = 18, cex.main = 1.5) # rainbow(9)
text(c(0.6, -0.35, -0.55, -0.1), c(0, -0.5, 0.2, 0.6), dat2$perchar[c(1, 5, 7, 8)], col = "white", font = 2, cex = 1.5)
dev.off()

# THIS CAN BE DONE IN TRELLIS TOO:
#par(new = TRUE)
#piechart(VADeaths, groups = FALSE, xlab = "")
# BELOW THIS IS PLOTTED AS A BARCHART & A DOTPLOT WITH THREE OTHER PANELS


################################################################################
# Plot 3 - Scatterplot of number of pages as a function of number of authors.
#?points
#?panel.xyplot

# Use Trellis graphics (xyplot):
png(filename = "dataPlot3.png", width = 1200, height = 800)
xyplot(PP ~ NoAuthors, data = dat3, strip = FALSE, aspect = "xy", type = c("p", "smooth"), pch = 16, cex = 1.5, lwd = 1.5, col.line = "red", xlab = list("No. Authors", fontsize = 20), ylab = list("No. Pages", fontsize = 20), scales = list(cex = 1.5), grid = TRUE)
dev.off()
#trellis.par.get()

# IN GGPLOT:
# IN GGPLOT:
# IN GGPLOT:
plot.pages <- ggplot(dat3, aes(x = NoAuthors, y = PP)) +
  geom_point(size = 2.5) +
  geom_smooth(se = FALSE, color = "blue", size = 0.75) + # loess, span = 0.75, degree = 2
  labs(x = "# Authors", y = "# Pages") +
  theme(axis.text = element_text(size = rel(1.25)),
        axis.title = element_text(size = rel(1.25)))
# *** Larger font size since this barplot will be merged with the scatterplot
# above in Photoshop.
# Save plot:
ggsave(filename = "figure3a_pages.tiff", plot = plot.pages,
       width = 6, height = 6, dpi = 300) # this gives 2400 x 1800 pixels


# ---------------------------------------------------------------
# Plot 4 - Barchart of average number of authors by article type.
# ---------------------------------------------------------------
# Plot 5 - Barchart of average number of pages by article type.

# For best display show both results as three panels in the same figure.
# Combine dat2, dat4 and dat5:
dat2.4.5 <- cbind(dat2$NoArts, dat4$AvgNoAuthors, dat5$AvgPP)
colnames(dat2.4.5) <- c("No. Articles", "No. Authors", "No. Pages")
rownames(dat2.4.5) <- dat4$MS.Type
#*** This is done below slightly differently for ggplot2

# Use Trellis graphics (barchart):
png(filename = "dataPlot2_4_5.png", width = 1200, height = 800)
barchart(dat2.4.5[c(8:1),], groups = FALSE, layout = c(3, 1), col = (brewer.pal(8,"Set1")[c(8:1)]), xlab = NULL, par.settings = list(fontsize = list(text = 24)), scales = list(x = "free"))
dev.off()

# STILL TO DO: ADD NUMBER OF ARTICLES GOING INTO AVERAGES ON THE BARS
# SOMETHING LIKE:
#panel = function(x, y, subscripts, groups) {ltext(x = x, y = y, labels = groups[subscripts], cex=1, fontfamily = "HersheySans")
# OR:
# myPanel <- function(x,y,subscripts,groups,...){
#   panel.superpose(x,y,subscripts,groups,...)
#   ltext(x-.1,y+1,as.character(groups)[subscripts],cex=0.5)}
#ltext(x = c(1,1,1,1,1,1,1,1), y = c(1,2,3,4,5,6,7,8), labels = as.character(rev(dat4.5[,1])), col = "black", font = 2, cex = 1.5, pos = 2)
#ltext(x = c(5,5,5,5,5,5,5,5), y = c(1,2,3,4,5,6,7,8), labels = as.character(rev(dat4.5[,2])), col = "black", font = 2, cex = 1.5, pos = 4)
# OR:
# panel = function(...) {
# grid.text(as.character(rev(dat4.5[,1])), unit(1, "native"), unit(1:8, "native"), just = "left"
# grid.text(as.character(rev(dat4.5[,2])), unit(5, "native"), unit(1:8, "native"), just = "left"
# }



# FACETED DOTPLOT in GGPLOT2 (separate plots):
# FACETED DOTPLOT in GGPLOT2 (separate plots):
# FACETED DOTPLOT in GGPLOT2 (separate plots):
# Restructure dat2, dat4 and dat5 for faceted plotting in ggplot2 using melt():
dat2.4.5 <- cbind(dat2[, c("MS.Type", "NoArts")], dat5$AvgPP, dat4$AvgNoAuthors)
#dat2.4.5 <- cbind(dat2[, c("MS.Type", "percent")], dat5$AvgPP, dat4$AvgNoAuthors)
names(dat2.4.5) <- c("Type", "# Articles", "# Pages", "# Authors")
#names(dat2.4.5) <- c("Type", "% Articles", "# Pages", "# Authors")
# Melt:
dat.mstype <- melt(dat2.4.5, id.vars = "Type",
                  measure.vars = c("# Articles", "# Pages", "# Authors"),
                  variable.name = "group", value.name = "value")
# Dotplot:
plot.mstype <- ggplot(dat.mstype, aes(x = Type, y = value, fill = Type)) + #, fill = Type
  geom_dotplot(binaxis = "y", dotsize = 2.5) +
  facet_grid(group ~ ., scales = "free_y") + #, space = "free_y"
  labs(x = NULL, y = NULL) + #"Value"
  theme(axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        strip.text = element_text(size = rel(1))) #,legend.position = "none"
# Save plot:
ggsave(filename = "figure2_mstype.tiff", plot = plot.mstype,
       width = 8, height = 6, dpi = 300) # this gives 2400 x 1800 pixels


################################################################################
# Plot 6 - Piechart of number articles by number of authors.
# *** NOTE THAT PLOT6 AND PLOT7 ESSENTIALLY PRESENT THE SAME INFORMATION IN A DIFFERENT FORMAT!

# Compute the percentages:
#dat6$percent <- dat6$Frequency/sum(dat6$Frequency)*100
#dat6$perchar <- paste(as.character(round(dat6$percent, digits = 1)), "%")

# Specify the percentages as characters for labeling:
dat6$perchar <- paste(as.character(round(dat6$Percentage, digits = 1)), "%",
                      sep = "")

# Plot pie chart:
png(filename = "dataPlot6.png", width = 800, height = 800)
pie(dat6$Percentage, labels = dat6$NoAuthorRange, clockwise = TRUE, col = brewer.pal(8,"YlOrRd"), border = "black", lty = 1, main = "No. Authors", ps = 18, cex.main = 1.5) # rainbow(9)
text(c(0.25, 0.6, 0.25, -0.5, -0.5), c(0.65, 0.3, -0.6, -0.45, 0.4), dat6$perchar[c(1, 2, 3, 4, 5)], col = "black", font = 2, cex = 1.5)
dev.off()

# THIS CAN BE DONE IN TRELLIS TOO:
#par(new = TRUE)
#piechart(VADeaths, groups = FALSE, xlab = "")

################################################################################
# Plot 7 - Frequency histogram of number articles by number of authors.
# *** NOTE THAT PLOT6 AND PLOT7 ESSENTIALLY PRESENT THE SAME INFORMATION IN A DIFFERENT FORMAT!

#classes <- c(1, 2, 3, 5, 7, 10, 15, 24)
#h <- hist(dat6b$NoAuthors, breaks = classes)
# Frequencies have been computed so just use barplot.

# Need to reorder the NoAuthorRange column, as R re-sorts the levels:
dat6$xax <- ordered(dat6$NoAuthorRange,
                    levels = c("1", "2", "3", "4-5", "6-7", "8-10",
                               "11-15", "16-24"))

# Use Trellis graphics:
png(filename = "dataPlot7.png", width = 800, height = 800)
barchart(Frequency ~ xax, data = dat6, xlab = "No. Authors", ylab = "No. Articles", par.settings = list(fontsize = list(text = 24)))
dev.off()

# IF ADDING A HORIZONTAL GRID AND COLORED BARS:
png(filename = "dataPlot7.png", width = 800, height = 800)
barchart(Frequency ~ xax, data = dat6, col = brewer.pal(8,"YlOrRd"), xlab = "No. Authors", ylab = "No. Articles", par.settings = list(fontsize = list(text = 24)), panel = function(...) {
	panel.grid(h = -15, v = 0, col.line = "dark grey")
	panel.barchart(...)
	})
dev.off()
# NOTE: THIS GIVES AN ERROR MESSAGE (Error: unexpected input in "‚") BUT STILL PRODUCES THE DESIRED OUTPUT



# In GGPLOT2:
# In GGPLOT2:
# In GGPLOT2:
plot.authors <- ggplot(dat6, aes(x = xax, y = Frequency)) +
  geom_bar(stat = "identity") + #, fill = "grey50"
  labs(x = "# Authors", y = "# Articles") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = rel(1.25)),
        axis.text.x = element_text(angle = 25),
        axis.title = element_text(size = rel(1.25)))
# *** Larger font size since this barplot will be merged with the scatterplot
# above in Photoshop.
# Save plot:
ggsave(filename = "figure3b_authors.tiff", plot = plot.authors,
       width = 6, height = 6, dpi = 300) # this gives 2400 x 1800 pixels

