# author_network_lajam.R
# Generate a social network visualization for the patterns of association among
# LAJAM authors using the igraph library.
#
# Daniel M. Palacios, 9-15, 20 August, 26 December 2013


################################################################################
# 1. Clear workspace, load required packages and set working directory.

# Clear environment:
rm(list = ls())

# Set working directory:
setwd("~/Documents/DMP/dmp_main/projects/dmp_projects/lajam_bibliometrics/network_analysis")
#setwd("~/Dropbox/work_newFiles")

# Load required add-on packages:
#library(reshape2) # for melt() and dcast()
#library(plyr) # for ddply(), summarize(), count(), match_df(), etc.
#library(igraph)
#library(RColorBrewer)
#library(ggplot2)
#library(scales) # for scale manipulation in ggplot2
#library(grid) # for unit in ggplot2

# Load required add-on packages:
# Provide a logical for whether packages should be installed (TRUE) or only
# loaded (FALSE) if they have already been installed:
doInstall <- FALSE
# Provide the list of packages to be loaded (or installed):
toInstall <- c("reshape2", "plyr", "igraph", "RColorBrewer",
               "ggplot2", "scales", "grid")
# Evaluate doInstall and if TRUE, run package installation:
if(doInstall){
  install.packages(toInstall, repos = "http://cran.us.r-project.org")}
# If doInstall is FALSE, then simply load the packages:
lapply(toInstall, library, character.only = TRUE)
#search() returns a list of loaded packages


################################################################################
# 2. Load data sets and prepare them.


# Specify the directory where the data files reside:
dat.path <- ("~/Documents/DMP/dmp_main/projects/dmp_projects/lajam_bibliometrics/network_analysis")
#dat.path <- ("~/Dropbox/work")
# Specify the file names:
dat.fnam1 <- "Authors168GenderAffiliation.csv"
dat.fnam2 <- "Authors404Unique.csv"
dat.fnam3 <- "AuthorsOccurrencesByArticle.csv"
# Read the csv files exported from Excel as character arrays (otherwise thet are
# read as factors by default). Fill empty cells with NAs on load.
# [Note that Excel's csv export does not encode the files as "UTF-8" but as
# "Western (Mac OS Roman)". To get the encoding right I resaved the files as
# UTF-8 using TextWrangler].
traits168 <- read.csv(file = paste(dat.path, dat.fnam1, sep = "/"),
                      header = TRUE, colClasses = "character", na.strings = "")
# traits168 is a 168x5 character array containing the ID, name, gender,
# institution type and country of the lead author for each article.
names404 <- read.csv(file = paste(dat.path, dat.fnam2, sep = "/"),
                     header = TRUE, colClasses = "character", na.strings = "")
# names404 is a 404x1 array with the unique name for each one of the 404 authors
# present in the database. This array was created for convenience in
# manipulations.
aut.art <- read.csv(file = paste(dat.path, dat.fnam3, sep = "/"),
                    head = TRUE, colClasses = "character", na.strings = "")
# aut.art is a 168x25 "sparse" matrix containing the IDs and the names of all
# first authors and coauthors for each one of the 168 articles in the database.


# Find indices to unique entries in traits168 in order to remove authors with 
# more than one article and thus obtain a list of first authors without 
# duplicates. (Authors reporting different affiliation data in different articles
# were set to the same one in Excel before export).
#names118 <- sort(unique(traits168[, 2])) # 118 unique first authors
ind.aut.u <- !duplicated(traits168[, 2]) # 168 logical
traits118 <- traits168[ind.aut.u, ] # 118x5
traits118 <- traits118[order(traits118[, 2]), ] # sort alphabetically

# Create a full traits/attributes data set for all 404 authors by merging 
# names404 with traits118. This will form the basis for the "vertices" list,
# which is the information of each author.
traits404 <- merge(names404, traits118[, 2:5], by.x = "lastNameInitial",
                   by.y = "lastNameInitial_Author1", all.x = TRUE) # 404x4
#str(traits404)
#'data.frame':  404 obs. of  4 variables:
#$ lastNameInitial: chr  "AbudC" "AcevedoGutiérrezA" "AdlerF" "AlarcónD" ...
#$ Gender         : chr  NA "Male" NA NA ...
#$ InstType1      : chr  NA "University" NA NA ...
#$ Country1       : chr  NA "USA" NA NA ...

# Add a column indicating whether author is 1st author or not:
traits404$Lead <- ifelse(is.na(traits404[, "Country1"]), "N", "Y")
# traits404 is now 404x5
# Keep in mind that authors that only had single-authored papers will need to be
# removed from this array for the network analysis, as the analysis is based on
# ties. (Catalina's analyses do keep them!).


######
# OLD
# OLD
# OLD
# Load file "Lajam_authorship_vna_tieData.csv" with ties data extracted from the
# VNA file "Lajam_authoship.vna" provided by Catalina Gomez. The data are in the
# "edges" format meaning that each row records a relationship (edge) between two
# people (vertices), and the third column gives a metric of the strength of the 
# association.
#dat <- read.csv("Lajam_authorship_vna_tieData.csv", header = TRUE,
#                sep = ",", quote = "") # 3424x3
#head(dat)
#str(dat)
#'data.frame':  3424 obs. of  3 variables:
#$ FROM: Factor w/ 371 levels "Abud","AcevedoGutiÃ©rrez",..: 74 94 128 180 258 329 ...
#$ TO: Factor w/ 371 levels "Abud","AcevedoGutiÃ©rrez",..: 1 1 1 1 1 1 2 2 2 3 ...
#$ associations: num  0.5 1 1 0.5 1 1 1 0.5 1 0.5 ...
# Create igraph object:
#colnames(dat) <- c("from", "to", "weight") # needed for internal igraph operations
#datNetwork <- graph.data.frame(dat, directed = TRUE)


################################################################################
# 3. Generate complete "Edges List" and "Traits List" from the separate pieces.
# An edgesList records the shared ties/relationship (edges) between two actors
# (vertices). A traitslist contains additional information about each actor.

# Melt the aut.art data set by article ID and Author1, which has the effect of 
# generating the ties/relationships (edges) between the first author and all its
# coauthors (these are directed ties).
rel <- melt(aut.art, id.vars = c("ID", "Author1"),
                  measure.vars = 3:25, variable.name = "coautOrder",
                  value.name = "Coauthor", na.rm = TRUE) # NOW 533x4
# Keep in mind that single authors (w/ no coauthors) are eliminated in this
# process.
#str(rel)
#'data.frame':  533 obs. of  4 variables:
#$ ID         : chr  "1" "3" "4" "5" ...
#$ Author1    : chr  "SecchiE" "SecchiE" "BordinoP" "DanilewiczD" ...
#$ coautOrder : Factor w/ 23 levels "Author2","Author3",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ Coauthor  : chr  "SicilianoS" "OttP" "SicilianoS" "RosasF" ...

# Generate the edgeslist:
edgesList <- rel[, c("Author1", "Coauthor")] # 533x2
colnames(edgesList) <- c("from", "to") # Rename columns according to igraph convention
edgesList <- edgesList[order(edgesList[, 1]), ] # sort alphabetically
#str(edgesList)
#'data.frame':  533 obs. of  2 variables:
#$ from: chr  "AcevedoGutiérrezA" "AcevedoGutiérrezA" "AcevedoGutiérrezA" ...
#$ to  : chr  "DiBerardinisA" "LarkinS" "LarkinK" "ForestellP" ...

# Manually generate a partial adjacency matrix from the ties list and compute 
# the "degree" (total number of occurrences in the matrix) for Author1 ("out",
# according to igraph convention) and for Coauthor ("in", according to igraph
# convention).
adjacencyMatrixPart <- table(edgesList) # 37962 = 111x342
degOut <- as.matrix(sort(rowSums(adjacencyMatrixPart))) # 111x2
# degOut is the total number of coauthors for Author1
degIn <- as.matrix(sort(colSums(adjacencyMatrixPart))) # 342x2
# degIn is the total number of occurrences as a coauthor
# Convert the degree matrices to dataframes:
degOut <- data.frame(from = rownames(degOut), degOut = degOut,
                         stringsAsFactors = FALSE, row.names = NULL) # 111x2
degIn <- data.frame(to = rownames(degIn), degIn = degIn,
                       stringsAsFactors = FALSE, row.names = NULL) # 342x2
# Merge degOut and degIn into a single dataframe (NAs are inserted
# automatically). Note that the two columns are still kept separate.
degOutIn <- merge(degOut, degIn, by.x = "from", by.y = "to",
                   all.x = TRUE, all.y = TRUE) # 400x3
colnames(degOutIn) <- c("name", "degOut", "degIn") # rename 1st column

# Create a final traitslist by combining these values with the traits404 array
# into a new dataframe:
traitsList <- merge(traits404, degOutIn, by.x = "lastNameInitial",
                    by.y = "name", all.y = TRUE) # NOW 400x7
colnames(traitsList) <- c("name", "gender", "instType", "country", "lead",
                          "degOut", "degIn") # rename columns
#str(traitsList)
#'data.frame':  400 obs. of  7 variables:
#$ name    : chr  "AbudC" "AcevedoGutiérrezA" "AdlerF" "AlarcónD" ...
#$ gender  : chr  NA "Male" NA NA ...
#$ instType: chr  NA "University" NA NA ...
#$ country : chr  NA "USA" NA NA ...
#$ lead    : chr  "N" "Y" "N" "N" ...
#$ degOut  : num  NA 4 NA NA 2 NA NA NA NA NA ...
#$ degIn   : num  1 NA 1 1 NA 1 1 1 1 1 ...

# Export these files as csv:
#write.csv(traitsList, file = "AuthorsTraitsList.csv")
#write.csv(edgesList, file = "AuthorsEdgesList.csv")


################################################################################
# 4. Create an igraph object to store the edges and traits lists.

# Create an igraph object from the symbolic edges list and the vertices
# metadata:
autNetwork <- graph.data.frame(edgesList, directed = TRUE,
                               vertices = traitsList)
#print(autNetwork)
#str(autNetwork)
#IGRAPH DN-- 400 533 -- 
#+ attr: name (v/c), gender (v/c), instType (v/c), country (v/c), lead (v/c), degOut
#  (v/n), degIn (v/n)
#+ edges (vertex names):
#  [1] AcevedoGutiérrezA    ->DiBerardinisA       AcevedoGutiérrezA    ->LarkinS            
#  [3] AcevedoGutiérrezA    ->LarkinK             AcevedoGutiérrezA    ->ForestellP ...
#
# Inspect the contents of the igraph object:
#V(autNetwork) # prints the list of vertices (actors): 400
#V(autNetwork)$name
#V(autNetwork)$gender
#V(autNetwork)$instType
#V(autNetwork)$country
#V(autNetwork)$lead
#V(autNetwork)$degOut
#V(autNetwork)$degIn
#E(autNetwork) # prints the list of edges (ties/relationships): 533
##E(autNetwork)$weight # NULL
#degree(autNetwork) # degree prints the total number of edges per vertex (out+in)
#summary(degree(autNetwork))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   1.000   2.665   2.250  31.000
#hist(degree(autNetwork))
#plot(degree(autNetwork))
#*** NOTE: Make sure sna library is not loaded yet, as the degree() fuction will
# be masked, returning an error!!


# NETWORK DIAGNOSTIC METRICS:
# Compute several diagnostic measures for each vertex, including "betweenness
# centrality", "eigenvector centrality", "closeness" and "coreness", following
# script "igraph_example_youtube.R". "Edge betweennes" is also calculated. Looks
# like "katz centrality" is not available in igraph.
# See: http://en.wikipedia.org/wiki/Network_science.
betVert <- betweenness(autNetwork) # 400; vertex betweennes
betEdge <- edge.betweenness(autNetwork) # 533; edge betweennes
eig <- evcent(autNetwork)$vector # 400; just the eigenvector centrality scores
close <- closeness(autNetwork, mode = "all") # 400; "in"+"out"
cores <- graph.coreness(autNetwork, mode = "all") # 400; "in"+"out"
# Combine these measures (except betEdge) into a data frame:
centVert <- data.frame(bet = betVert, eig = eig, close = close, cores = cores) # 400x4
# Fit a linear model for eig as a function of betVert and obtain the residuals
# to identify who the outliers are (i.e., the important actors in the network):
res <- lm(eig ~ betVert, data = centVert)$residuals # 400
# Rescale the residuals for ease in readability:
res <- abs(res)*10
# Add the rescaled residual value as a column to the centVert data frame:
centVert$res <- res # NOW 400x5
# Extract the "degree" variable and add it as a column for good measure:
degAll <- degree(autNetwork) # 400
centVert$degAll <- degAll # NOW 400x6
# Obtain a numeric ID value for each vertex and also add it:
vertID <- as.vector(V(autNetwork)) #+1
centVert$vertID <- vertID # NOW 400x7

# Update the traitsList by incorporating centVert into it:
traitsList <- cbind(traitsList, centVert) # NOW 400x14
# Export this file as csv:
#write.csv(traitsList, file = "AuthorsTraitsListCent.csv")


# EXPLORATORY PLOTS OF DIAGNOSTIC METRICS:
# Univariate:
#ggplot(traitsList, aes(x = vertID, y = bet)) + geom_point(aes(color = res))
#ggplot(traitsList, aes(x = vertID, y = eig)) + geom_point(aes(color = res))
#ggplot(traitsList, aes(x = vertID, y = close)) + geom_point(aes(color = res))
#ggplot(traitsList, aes(x = vertID, y = cores)) + geom_point(aes(color = res))
#ggplot(traitsList, aes(x = vertID, y = degAll)) + geom_point(aes(color = res))
#ggplot(traitsList, aes(x = vertID, y = res)) + geom_point(aes(color = res))
# Bivariate:
#ggplot(traitsList, aes(x = bet, y = eig)) + geom_point(aes(color = res))
#ggplot(traitsList, aes(x = bet, y = eig)) +
#  geom_text(aes(label = name, color = res, size = res))
# From these plots we learned that:
# - Betweennes values of 0 are candidates for elimination
# - Eigenvector values < ~0.125 are candidates for elimination
# * Closeness neatly separates two groups: <9.0e-06 and >1.5e-05. This is the
#   most promising pattern to use to separate the main groups.
# - Coreness values of 1 are candidates for elimination
# - degALL values of 1 are candidates for elimination

# Closeness centrality measures how many steps are required to access every
# other vertex from a given vertex. Find the mean closeness value for each one
# of the two groups, to report in the paper:
#ind.closeLo <- which(traitsList$close < 9.0e-06) # 137 integer vector
#ind.closeHi <- which(traitsList$close > 1.5e-05) # 263 integer vector
#closeLoAvg <- mean(traitsList$close[ind.closeLo]) # MEAN = 6.433925e-06
#closeHiAvg <- mean(traitsList$close[ind.closeHi]) # MEAN = 1.787523e-05

################################################################################
# 5. Generate some customized network plots in IGRAPH.

# Initial/basic plot (too complex with all 400 authors):
#plot(autNetwork, layout = layout.fruchterman.reingold, vertex.size = 4,
#     vertex.label.dist = 0.25, margin = c(0, 0, 0, 0))
# *** NOTE: Do NOT use tkplot(). It crashes R!!
##tkplot(autNetwork, layout = layout.kamada.kawai, edge.color = E(autNetwork)$color)


# FULL NETWORK:
# Keep in mind that many of the graphical attributes specified here by
# augmenting the traitsList can also be specified directly in the igraph object
# as, e.g.:
#V(autNetwork)$color <- ifelse(V(autNetwork)$lead == "N", "blue", "red")
#V(autNetwork)$size <- degree(autNetwork)/10
# This would be the approach to follow if no indexing for particular author
# traits was necessary.

# Generate an index to identify the lead author from coauthor, in order to color
# vertices differently and also to label only lead author vertices:
ind.aut1 <- traitsList$lead == "Y" # 400 logical

# Explicitly create a label vector with only the names of the lead authors
# (blank out the names of the coauthors):
traitsList$vertLabAut1 <- traitsList$name # 400
traitsList$vertLabAut1[!ind.aut1] <- NA
# Alternatively, explicitly create a label vector for the lead authors with low
# closeness centrality (i.e., the authors in the periphery of the network):
ind.aut1.closeLo <- traitsList$lead == "Y" & traitsList$close < 9.0e-06 # 400 logical
#ind.aut1.closeLo <- which(traitsList$lead == "Y" & traitsList$close < 9.0e-06) # 39
traitsList$vertLabAut1Per <- traitsList$name # 400
traitsList$vertLabAut1Per[!ind.aut1.closeLo] <- NA
# Or a label vector with lead authors with highest centrality measures:
ind.aut1.top <- traitsList$lead == "Y" & (traitsList$close >= 1.797e-05 | traitsList$res > 2.5)
traitsList$vertLabAut1Top <- traitsList$name # 400
traitsList$vertLabAut1Top[!ind.aut1.top] <- NA


# Explicitly create a size vector for the vertices based on one of the 
# diagnostic metrics (coreness, degree, etc.). This can be used to scale the 
# vertex size symbol directly (e.g., dividing by 10 or 1.5), or the label by
# further relativizing and translating into cex units to scale the vertex label.
#traitsList$vertSize <- traitsList$degAll # same from degree(autNetwork)
#traitsList$vertSize <- traitsList$cores # traitsList$vertSize/10 or /1.5
# *** This doesn't work too well. Instead manually assign a size to the symbols:
traitsList$vertSize <- ifelse(ind.aut1, 4, 3) # larger symbol for lead authors
# Create a vector specifying symbol frame color for lead authors:
traitsList$vertFrameCol <- ifelse(ind.aut1, "black", NA)

# Generate an index to identify the lead author's gender:
ind.genderF <- traitsList$gender == "Female" # 400 logical
# Create a vector specifying symbol frame color according to gender:
#traitsList$vertFrameCol <- ifelse(ind.genderF, "magenta", NA)
# Create a vector specifying symbol shape according to gender:
traitsList$vertSymbShape <- rep("circle", length(traitsList$gender))
traitsList$vertSymbShape[ind.genderF] <- "square"

# Create a color vector with dark gray and light gray for lead authors and
# coauthors, respectively (could be used to color symbols or labels):
traitsList$vertColGrays <- ifelse(ind.aut1, "gray25", "gray75")
# "gray25" is dark; "gray75" is light

# Assign a color to the vertex LABEL by country (for select countries only).
# First, figure out the sorted frequecy of occurrence for each country (don't
# count NAs):
countryTab <- sort(table(traitsList$country), decreasing = TRUE) # 14; useNA = "always"
countryTab <- data.frame(country = rownames(countryTab), freq = countryTab,
                         row.names = NULL)
# Which yields:
#    Brazil  Argentina     México        USA    Uruguay      Chile       Perú   Colombia    Ecuador 
#        56         10          9          8          7          5          4          3          3 
#PuertoRico  Venezuela      Italy   Portugal      Spain 
#         2          2          1          1          1
# This can also be achieved with plyr as (but NAs are counted):
#countryTab <- count(traitsList, "country")
#countryTab <- countryTab[order(countryTab[, 2], decreasing = TRUE), ]
# Based on this order, color the labels of countries with at least five
# occurrences. This is six countries: Brazil, Argentina, México, USA, Uruguay,
# and Chile. Pick six bold/darker colors that will give good contrast:
#RColorBrewer::display.brewer.all(n = 14, exact.n = FALSE)
palCtry <- brewer.pal(7, "Set1")[-6] # skip yellow, which offers poor contrast on white
palCtry <- palCtry[c(5, 3, 2, 4, 6, 1)] # a better order for the colors
# For the other counties use a medium-dark gray:
palCtry <- c(palCtry, rep("gray30", 8)) # 14 colors according to frequency
# Add this column to countryTab:
countryTab$palCtry <- palCtry # NOW 14x3
# Use "factor trick" to create a color column in traitsList based on the country
# sorting and colors in countryTab$palCtry:
traitsList$vertLabColCtry <- as.character(factor(traitsList$country,
                                                 levels = countryTab$country,
                                                 labels = countryTab$palCtry))
# *This gives a WARNING that duplicated levels in factors are deprecated because
# "gray50" is used for multiple levels, but still returns the desired vector.
# One solution would be to give a sequence of unique but very similar colors
# like "gray51", "gray52", "gray53", etc.

# Create a color vector with symbol fill color based on vertLabColCtry, but 
# replace the NAs (coauthors) with light gray, since we also want to show the
# coauthor vertices in light gray (but not the labels):
traitsList$vertSymbColCtry <- traitsList$vertLabColCtry
traitsList$vertSymbColCtry[is.na(traitsList$vertSymbColCtry)] <- "gray75"

# Create a custom country code column based on the selected top six countries.
# (For use with the faceted dotplots at the end).
traitsList$country2 <- traitsList$country
traitsList$country2[traitsList$vertLabColCtry == "gray30"] <- "Other"
traitsList$country2 <- as.character(factor(traitsList$country2,
                                           levels = c(as.character(
                                             countryTab$country)[1:6], "Other"),
                                           labels = c("BR", "AR", "ME", "US",
                                                      "UR", "CH", "Other")))

# PLOT FULL NETWORK
# PLOT FULL NETWORK
# With the following customizations:
# - Vertex symbols for lead authors are slightly larger than for coauthors
# - Vertex symbols are filled with two gray shades for lead author and coauthor
# - ***Alternatively, vertex symbols are filled with different color by country
# - Vertex borders are colored by gender of lead author
# - ***Alternatively, vertex shape is circle or square by gender of lead author
# - Vertices for lead authors are labeled, but not for coauthors
# - Vertex labels for lead authors are colored by country
# - Coauthors are shown as grayed-out vertices, but not labeled
# - ***Only lead authors of the peripheral vertices are labeled. The main/core
#   group is not labeled because it becomes unreadable. Instead, the graphic
#   below plots a separate network just for the core group.
# - ***Only the lead authors of the core group with highest centrality values
#   (n = 13) are labeled.
# Country colors are: Orange=Brazil; Green=Argentina; Blue=México; Purple=USA;
#                     Brown=Uruguay; Red=Chile; Gray30=Other Countries (9).
# The nine Other Countries are: Perú; Ecuador; Colombia; PuertoRico; Venezuela;
#                               Italy; NewZealand; Portugal; Spain.
# Set a random seed in order to be able to replicate the plot in the future:
set.seed(1501) # best seed so far by trial and error
# Specify the export device:
#png(filename = "autNetwork_full_top.png", height = 1000, width = 1000)
# 1275 is 8.5" at 150 ppi. For final tiff use: 2550x2550 px for 300 dpi
#tiff(filename = "autNetwork_full.tiff", width = 2550, height = 2550,
#     units = "px", pointsize = 8, compression = "lzw", bg = "white", res = 300)
#pdf(file = "autNetwork_full.pdf", height = 12, width = 12)
postscript(file = "autNetwork_full.eps", onefile = FALSE,
           horizontal = FALSE, height = 12, width = 12, pointsize = 8)
# Plot:
plot(autNetwork,
     margin = c(0, 0, 0, 0), # BLTR
     #layout = layout.kamada.kawai,
     layout = layout.fruchterman.reingold,
     #main = "LAJAM Author Network", # SkyBlue2
     vertex.size = traitsList$vertSize, # 4 and 3
     #vertex.color = traitsList$vertColGrays, # dark and light gray for lead vs. coauthors
     vertex.color = traitsList$vertSymbColCtry, # fill colored by country or light gray
     vertex.frame.color = traitsList$vertFrameCol, # black for lead authors
     vertex.shape = traitsList$vertSymbShape, # circle for males, square for females
     #vertex.label = traitsList$vertLabAut1, # all lead authors
     vertex.label = traitsList$vertLabAut1Per, # only lead authors with low closeness
     #vertex.label = traitsList$vertLabAut1Top, # only lead authors with high centrality measures
     vertex.label.family = "Helvetica",
     vertex.label.font = 1,
     vertex.label.cex = 2, # traitsList$vertSize/max(traitsList$vertSize)*cex
     vertex.label.dist = 0.3,
     vertex.label.degree = -pi/4,
     vertex.label.color = "black", # black may be more readable
     #vertex.label.color = traitsList$vertLabColCtry, # label colored by country
     edge.color = "gray80", # arrows; lighter for higher values
     edge.width = 2.5, # the tiff version shows it thicker that the png
     edge.arrow.size = 0.75,
     edge.curved = TRUE)
# Close the device
dev.off()
# This network shows a distinct and well-connected core and a number of small
# groups of authors around the periphery that are not connected to the core
# group.


# SUBGRAPH (JUST THE CORE GROUP):
# SUBGRAPH (JUST THE CORE GROUP):
# Since the core cluster is not readable in the full network, use the closeness
# metric, which neatly separates two groups (<9.0e-06 and >1.5e-05) of authors
# to generate a separate subnetwork with the high closeness values.
# Find the indices to the rows containing the high closeness values (a numeric
# vector is required by induced.subgraph (), not a logical):
ind.closeHi <- which(traitsList$close > 1.5e-05) # 263 numeric vector
autNetwork.sub <- induced.subgraph(autNetwork, as.vector(ind.closeHi)) #-1

# Since the traitsList has been updated after the full network was created,
# create a new version based on the vertices being used to subsample the full
# network, and follow the same approach as with the full network to identify
# authors of interest.
traitsList.sub <- traitsList[ind.closeHi, ] # 263x23


# PLOT CORE GROUP:
# Set a random seed in order to be able to replicate the plot in the future:
set.seed(333) # 1500 is also an OK seed.
# Specify the export device:
png(filename = "autNetwork_sub.png", height = 1000, width = 1000)
# 1275 is 8.5" at 150 ppi. For final tiff use: 2550x2550 px for 300 dpi
#tiff(filename = "autNetwork_sub.tiff", width = 2550, height = 2550,
#     units = "px", pointsize = 8, compression = "lzw", bg = "white", res = 300)
# Plot:
plot(autNetwork.sub,
     margin = c(0, 0, 0, 0), # BLTR
     #layout = layout.kamada.kawai,
     layout = layout.fruchterman.reingold,
     #main = "LAJAM Author Network", # SkyBlue2
     vertex.size = traitsList.sub$vertSize, # 4 and 3
     #vertex.color = traitsList.sub$vertColGrays, # dark for lead auts/light for coauthors
     vertex.color = traitsList.sub$vertSymbColCtry, # fill colored by country or light gray
     vertex.frame.color = traitsList.sub$vertFrameCol, # black for lead authors
     vertex.shape = traitsList.sub$vertSymbShape, # circle for males, square for females
     vertex.label = traitsList.sub$vertLabAut1, # all lead authors
     vertex.label.family = "Helvetica",
     vertex.label.font = 1,
     vertex.label.cex = 2, # traitsList.sub$vertSize/max(traitsList.sub$vertSize)*cex
     vertex.label.dist = 0.3,
     vertex.label.degree = -pi/4,
     vertex.label.color = "black", # black may be more readable
     #vertex.label.color = traitsList.sub$vertLabColCtry, # label colored by country
     edge.color = "gray80", # arrows; lighter for higher values
     edge.width = 2.5, # the tiff version shows it thicker that the png
     edge.arrow.size = 0.75,
     edge.curved = TRUE)
# Close the device
dev.off()



# DOTPLOTS iN GGPLOT2:
# DOTPLOTS iN GGPLOT2:
# Since the core group is still not very readable in the subnetwork, use the
# closeness metric, which neatly separates two groups (<9.0e-06 and >1.5e-05) of
# authors to generate dotplots with various centrality measures in subpanels for
# the lead authors in the main/core cluster.

# Eliminate the coauthors from the traitsList by removing the NA rows:
traitsList.sub.aut1 <- traitsList.sub[!is.na(traitsList.sub$gender), ] # 73x23

# Reorder by closeness, since that was the criterion used to generate the
# full network and the subnetwork:
traitsList.sub.aut1 <- within(traitsList.sub.aut1, {
  name <- reorder(name, close)
  vertID <- reorder(vertID, close)
})
#str(traitsList.sub.aut1)
#summary(traitsList.sub.aut1)
#range(traitsList.sub.aut1$bet) # [0.000 1655.684]
#range(traitsList.sub.aut1$eig) # [0.0002616823 1]
#range(traitsList.sub.aut1$close) # [1.775253e-05 1.800991e-05]
#range(traitsList.sub.aut1$cores) # [1 5]
#range(traitsList.sub.aut1$res) # [0.009186792 3.530579510]
#range(traitsList.sub.aut1$degAll) # [1-31]
#head(traitsList.sub.aut1[, c(2:4, 8:12)], n = 12L)

# Create a vector specifying font face for lead author gender (y-axis text):
fontFaceGender <- ifelse(traitsList.sub.aut1$gender == "Female", "italic",
                         "normal")
# Alternatively, create a vector specifying font color for lead author gender
# (y-axis text):
fontColGender <- ifelse(traitsList.sub.aut1$gender == "Female", "red", "blue")
# *** Note that neither one seems to work

# Melt the traits list for faceted plotting of the centrality measures:
traitsList.cent <- melt(traitsList.sub.aut1, #, id.vars = c()
                        measure.vars = c("close", "cores", "degAll",
                                         "bet", "eig", "res"),
                        variable.name = "Measure", value.name = "value") # 438x19
# The order of the measure is given in a different order here for plotting
# purposes.
# Variable "Measure" is now a factor, but provide nice labels using the factor()
# function:
traitsList.cent$Measure <- factor(traitsList.cent$Measure,
                                  labels = c("Closeness", "Coreness",
                                             "Degree", "Between.",
                                             "Eigenvec.", "Resid."))

# PLOT:
plot.cent <- ggplot(traitsList.cent,
                    aes(x = value, y = name, group = Measure,
                        color = factor(country2, levels = c("BR", "AR", "ME",
                                                            "US", "Other")))) +
  geom_point() + # color = "blue"
  scale_colour_manual(values = countryTab$palCtry[c(1, 2, 3, 4, 7)],
                      name = "Country") +
  facet_grid(. ~ Measure, scales = "free_x") + 
  theme(legend.position = "bottom", legend.margin = unit(-1, "cm"),
        axis.text = element_text(size = rel(1)),
        axis.ticks.x = element_line(size = 0),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = rel(1)),
        axis.text.y = element_text(color = fontColGender),
        #axis.text.y = element_text(face = fontFaceGender),
        strip.text = element_text(size = rel(1)))
# If showing the x-axis labels was desirable, I could create my own scale with
# just the extreme ranges of the data using the "scales" package, with something
# like:
#scale_x_continuous(breaks = xx)
# where xx <- trans_new(name, range(traitsList.cent$value))
# See ?scales::trans_new ## NOT HELPFUL AT ALL!
# But since this is a faceted plot, probably need to use ddply, etc.
# Save plot:
ggsave(filename = "autNetwork_centrality.tiff", plot = plot.cent,
       width = 8.5, height = 11, dpi = 300)
ggsave(filename = "autNetwork_centrality.eps", plot = plot.cent,
       width = 8.5, height = 11)
# this gives 2550 x 3300 pixels at 72 ppi
# this gives 2400 x 1800 pixels for .tiff
# Country colors are:
# Orange=Brazil; Green=Argentina; Blue=México; Purple=USA; Gray30=Other



################################################################################
# Extra stuff:

# Set to NA actors with very low rescaled residual values:
#vertID[which(centVert$res < 0.25)] <- NA
# Assign the rescaled residuals to the network as variable "size"
#V(autNetwork)$size <- cent$res

# Specify the color of the vertices by whether author is lead or not:
V(autNetwork)$color <- ifelse(V(autNetwork)$lead == "N", "blue", "red")

# Specify the color of the vertices by country:
#RColorBrewer::display.brewer.all(n = 14, exact.n=FALSE)
#countries <- unique(traitsList$country)[-1] # 1st item is NA -- 14 countries
#countryCol <- c(brewer.pal(12, "Set3"), "SkyBlue2", "black", "gray") # Accent(8)
countryCol <- c(brewer.pal(8, "Dark2"), "gray", "gray", "gray", "gray", "gray",
                "gray", "black")
V(autNetwork)$color <- as.character(factor(V(autNetwork)$country,
                                           labels = countryCol))
V(autNetwork.aut)$color <- as.character(factor(V(autNetwork.aut)$country,
                                               labels = countryCol))
# Specify the size of the vertices by the total number of co-occurrences:
V(autNetwork)$size <- degree(autNetwork)/10
V(autNetwork.aut)$size <- degree(autNetwork)[V(autNetwork)$lead == "Y"]/1.5
# Need to explicitly/manually subset degree(autNetwork) as
# degree(autNetwork.aut) will recalculate the degree without coauthors.

# Subset the data to exclude coauthors:
bad.co <- V(autNetwork)[V(autNetwork)$lead == "N"] # 288
autNetwork.aut <- delete.vertices(autNetwork, bad.co)
#print(autNetwork.aut) # shrinks the network to 112 vertices and 151 edges
# Or, subset the data to exclude vertices with less than three edges:
#bad.vs <- V(autNetwork)[degree(autNetwork) < 3] # 300
#autNetwork.deg <- delete.vertices(autNetwork, bad.vs)
#print(autNetwork.deg) # shrinks the network to 100 vertices and 213 edges


################################################################################
################################################################################
################################################################################
# See Newman (2010) for more details of these metrics: Newman, M.E.J. (2010)
# Networks: An Introduction. Oxford University Press.
# Closeness centrality determines how ‘close’ a node is to other nodes in a
# network by measuring the sum of the shortest distances (geodesic paths)
# between that node and all other nodes in the network (Closeness centrality
# measures how many steps are required to access every other vertex from a given
# vertex). Degree centrality of a node in a network is the number of links
# (vertices) incident on the node. Betweenness centrality determines the
# relative importance of a node by measuring the amount of traffic flowing
# through that node to other nodes in the network. This is done my measuring the
# fraction of paths connecting all pairs of nodes and containing the node of
# interest. Eigenvector centrality is a more sophisticated version of degree
# centrality where the centrality of a node not only depends on the number of
# links incident on the node but also the quality of those links. This quality
# factor is determined by the eigenvectors of the adjacency matrix of the
# network. Coreness: The k-core of graph is a maximal subgraph in which each
# vertex has at least degree k. The coreness of a vertex is k if it belongs to
# the k-core but not to the (k+1)-core. See: 
# http://en.wikipedia.org/wiki/Network_science.
