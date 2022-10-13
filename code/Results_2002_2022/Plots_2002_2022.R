################################################################################
################################################################################
#Packages

library(readr)
library(ggplot2)
library(tidyr)

################################################################################
################################################################################

#Trend of the number of publications

#Import data
issue <- read_csv("data/Data_2002_2022/Data_plots/Issue.csv")

#Create data
#Notes:
#ART = article, COM = commentary, EDI = editorial, INT = Introduction, 
#NOT = note, REV = review, SHO = short communication, and WKS = workshop report
#PRO = protocols, OPI = opinions
issue_data <- issue %>%
  gather(key = "MS_Type", value = "Total", -c(Issue,Period))
issue_data
issue_data$Issue<-factor(issue_data$Issue, levels=issue$Issue)

#Colors by type of document
colors_issue<-c("#ffbe4f","#6bd2db","#0ea7b5","#0c457d","#e8702a","#d3290f",
                "#52d053","#0a5d00","#765898","#000000")

#Plot
issue_plot<-ggplot(issue_data, aes(x=Issue, y=Total, fill=MS_Type))+
  geom_bar(stat = 'identity')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1),
        text=element_text(size=11))+
  ylab("Number of publications")+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=colors_issue)+
  labs(fill="Type")+
  facet_grid(~Period,scales="free_x", space = "free_x")
issue_plot

#Export
png(filename = "graphics/Data_2002_2022/Data_plots/issue_plot.png", 
    width = 1700, height = 1000, units = "px",  type = "cairo", res = 300)
issue_plot
dev.off()

################################################################################
################################################################################

#Trend of publications by genre

#Import data
issue_gender<-read.table("data/Data_2002_2022/Data_plots/IssueByGender.csv", sep=",", header=TRUE)

#Create data
issue_gender_data <- issue_gender %>%
  gather(key = "Gender", value = "Total", -c(Issue,Period))
issue_gender_data
issue_gender_data$Issue<-factor(issue_gender_data$Issue, levels=issue_gender$Issue)

#Color by sex
colors_issue<-c("#ffbe4f","#0c457d","#e8702a","#d3290f",
                "#52d053","#0a5d00","#765898","#000000")

#Plot
issue_gender_plot<-ggplot(issue_gender_data, aes(x=Issue, y=Total, fill=Gender))+
  geom_bar( position = "dodge", stat = 'identity')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1),
        text=element_text(size=11))+
  ylab("Number of publications")+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=colors_issue)+
  facet_grid(~Period,scales="free_x", space = "free_x")
issue_gender_plot

#Export
png(filename = "graphics/Data_2002_2022/Data_plots/issue_gender_plot.png", 
    width = 1700, height = 850, units = "px",  type = "cairo", res = 300)
issue_gender_plot
dev.off()

################################################################################
################################################################################

#Trend of the number of citations

#Import data
Citations<-read_csv("data/Data_2002_2022/Data_plots/Citations.csv")

#Plot
citations_plot<-ggplot(Citations, aes(x=Issue, y=Citations, group=1))+
  geom_point(color="blue")+geom_line(color="blue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1),
        text=element_text(size=11))+
  ylab("Number of citations")+
  facet_grid(~Period,scales="free_x", space = "free_x")
citations_plot

#Export
png(filename = "graphics/Data_2002_2022/Data_plots/Citations_plot.png", 
    width = 1700, height = 850, units = "px",  type = "cairo", res = 300)
citations_plot
dev.off()

################################################################################
################################################################################

