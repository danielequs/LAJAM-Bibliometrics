# wordcloudPlots_lajam.R
# Generate wordclound plots for bibliometric & content analysis of articles
# published in LAJAM, by taxa, field, thematic area, region where the work was
# conducted, country and affiliation (acronyms).
#
# *** IT WAS DECIDED TO GENERATE THE WORDCLOUDS IN WORDLE BECAUSE IT OFFERS MORE
#     FLEXIBILITY, AND wordcloud() DOES NOT ALLOW TO COLOR WORDS BY A 2ND VARIABLE
#     WHICH WAS ONE OF THE MAIN REASONS FOR TRYING TO DO THIS IN R.
#
# Daniel M. Palacios, 7 May 2012, 4 Jan. 2014


################################################################################
# 1. Clear workspace, load required packages and set working directory.

# Clear environment:
rm(list = ls())

# Set working directory:
#setwd("/Users/danielpalacios/Documents/DMP/dmp_main/projects/dmp_projects/lajam_bibliometrics/wordclouds")
setwd("~/Documents/DMP/dmp_main/projects/dmp_projects/lajam_bibliometrics/wordclouds")

# Load required add-on packages:
#library(wordcloud)
#library(reshape2) # for melt() and dcast()
#library(plyr) # for ddply(), summarize(), count(), match_df(), etc.
#library(RColorBrewer)
#library(ggplot2)
#library(scales) # for scale manipulation in ggplot2
#library(grid) # for unit in ggplot2
#library(lattice)

# Load required add-on packages:
# Provide a logical for whether packages should be installed (TRUE) or only
# loaded (FALSE) if they have already been installed:
doInstall <- FALSE
# Provide the list of packages to be loaded (or installed):
toInstall <- c("wordcloud", "reshape2", "plyr", "RColorBrewer", "ggplot2",
               "scales", "grid", "lattice")
# Evaluate doInstall and if TRUE, run package installation:
if(doInstall){
  install.packages(toInstall, repos = "http://cran.us.r-project.org")}
# If doInstall is FALSE, then simply load the packages:
lapply(toInstall, library, character.only = TRUE)
#search() returns a list of loaded packages


################################################################################
# 2. Load data sets and prepare them.

# Generate wordclouds as follows:
# By taxon (all these could go in one wordcloud color-coded by taxonomic level):
# - Order (Carnivora, Cetartiodactyla, Sirenia)
# - Suborder (Mysticeti, Odontoceti)
# - Family (Otariidae, Phocidae, Mustelidae, Balaenidae, Eschrichtiidae,
#   Balaenopteridae, Physeteridae, Kogiidae, Iniidae, Pontoporiidae,
#   Delphinidae, Phocoenidae, Trichechidae)
# By species (genus in blue, species in red)
# By field/thematic area (color-coded?)
# By geographic region where the work was conducted (color-coded?)
# By affiliation (acronyms, color-coded by one of four types)
# Colors are:
# University: #669933, or R102G153B51
# Government: #FFFF66, or R255G255B102
# NGO: #CC9966, or R204G153B102
# Other: #99CC99, or R153G204B153
# By country (color-coded?) -- THIS ONE IS TOO SPARSE TO BE INTERESTING!!!
# *The latter two are for "National Productivity and Affiliation Patterns", not
# for "Content Analysis"

# Specify the directory where the data files reside:
dat.path <- ("~/Documents/DMP/dmp_main/projects/dmp_projects/lajam_bibliometrics/wordclouds")
# Specify the file names:
dat.fnam1 <- "byAffiliation1_acronym.csv"
dat.fnam2 <- "bySpecies.csv"
dat.fnam3 <- "byXXX.csv"

# Read the csv files exported from Excel as character arrays (otherwise thet are
# read as factors by default). Fill empty cells with NAs on load.
# [Note that Excel's csv export does not encode the files as "UTF-8" but as
# "Western (Mac OS Roman)". To get the encoding right I resaved the files as
# UTF-8 using TextWrangler].
dat.aff <- read.csv(file = paste(dat.path, dat.fnam1, sep = "/"),
                    header = TRUE, colClasses = "character", na.strings = "")
dat.aff2 <- read.csv(file = paste(dat.path, dat.fnam1, sep = "/"),
                     header = TRUE, na.strings = "")
# dat.aff is a 168x5 data frame containing the acronym, institution type, city,
# state (Brazil only) and country of the lead author for each article.





# Prepare data:
colnames(dat1) <- c("Name", "Sex")
#v <- sort(rowSums(dat1), decreasing = TRUE)
#d <- data.frame(Words = names(v), Freq = v)
#dat6$xax <- ordered(dat6$NoAuthorRange, levels = c("1", "2", "3", "4-5", "6-7", "8-10", "11-15", "16-24"))
freq <- as.matrix(summary(dat1$Name))


# Get info:
str(dat1)
#'data.frame':	167 obs. of  2 variables:
# $ Name: Factor w/ 117 levels "Acevedo-Guti\x8errez",..: 99 99 12 28 56 99 82 24 5 12 ...
# $ Sex  : Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 2 2 2 2 ...
summary(dat1)
#             Name        Sex    
# Santos         :  8   Female: 63  
# Secchi         :  6   Male  :104  
# Flores         :  5               
# Molina-Schiller:  4               
# Rosas          :  4               
# F<8e>lix       :  3               
# (Other)        :137               


################################################################################
# Plot 1 - Wordcloud by affiliation acronym, color-coded by institution type.
png(filename = "byAuthor1st_gender.png", width = 1200, height = 800)

wordcloud(dat.aff$Acronym1, scale = c(4, 0.5),
          min.freq = 1, max.words = Inf, random.order = FALSE,
          random.color = FALSE, rot.per = 0,
          ordered.colors = FALSE, fixed.asp = FALSE)
# levels(dat.aff2$Acronym1), colors = c("blue", "red"),

dev.off()

# Plot 1 - Wordcloud by 1st author name color-coded by gender.

