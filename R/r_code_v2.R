library(ggplot2)
library(lubridate)
library(scales)
library(treemap)
library(stats)
library(tidyr)
library(dplyr)

yearly <- read.csv("~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/work/2004_14.csv", stringsAsFactors = F)
monthly <- read.csv("~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/work/2014_15Q2_v2.csv", stringsAsFactors = F)
monthly$X <- dmy(monthly$X)

p_yearly <- ggplot(yearly, aes(X, EU.28)) + 
    geom_line(size  = 1.5) + 
    theme_minimal(base_size = 10) + 
    labs(x="Year" , y="EU–28 countries [thousands]")
#     geom_point(size = 3) +
#     geom_text(yearly[9:11,], aes(X, EU.28,label=EU.28))
p_yearly
ggsave("fig1.png", path = "~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/figures", width = 5, height = 3)

sum_2015Q2 <- sum(monthly[13:18, 2])
sum_2014 <- sum(monthly[1:12, 2])

p_monthly <- ggplot(monthly, aes(X, EU.28)) + 
    geom_line(size  = 1.5) + 
    theme_minimal(base_size = 10) + 
    labs(x="Month" , y="EU–28 countries [thousands]") +
    annotate("rect", xmin=monthly[13,1], xmax=monthly[18,1], ymin=50, ymax= 90, 
               fill=NA, colour="red") +
    annotate("text", x=monthly[15,1], y=87, label=paste("Total:", sum_2015Q2), size=3)
p_monthly
ggsave("fig2.png", path = "~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/figures", width = 5, height = 3)

# Main destinations
origin <- read.csv("~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/work/origin.csv")

tree <- treemap(origin[7:36,], index = c("origin", "destination"),
        vSize = "percent", vColor = "destination", type = "categorical",
        palette="Paired", title = "", title.legend = "Destination")




# Immigrant/population ratio
ratio <- read.csv("~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/work/migr_ratio_res.csv", stringsAsFactors = F)

ratio <- gather(ratio, "country", "ratio", 2:32)
ratio$ratio <- as.numeric(ratio$ratio)
ratio2014 <- ratio[ratio$year==2014, ]
top5 <- c("Germany","Hungary", "Austria", "Italy", "France")
ratio_top5 <- filter(ratio2014, country %in% top5)

p_ratio2014 <- ggplot(ratio2014, aes(country, ratio)) +
    geom_bar(stat = "identity") + 
#     geom_point(stat = "identity",
#                shape = 95,
#                size = 10) + 
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 35,
                                     hjust = 1,
                                     vjust = 1,
                                     size = 10)) +
    geom_text(aes(label = round(ratio, digits = 0)),
              size = 3,
              vjust = -0.8) +
    geom_hline(aes(yintercept=mean(ratio2014$ratio),
                   colour = "red")) +
    annotate("text", x="Romania", y=10,
             label="EU–28 average", size=4, colour = "red") + 
    labs(x="", y="Immigrants/Total [%]")
p_ratio2014
ggsave("fig5.png", path = "~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/figures", width = 7, height = 4.5)

# Acceptance rate
applicant_pop <- read.csv("~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/work/applicant_per_pop.csv", stringsAsFactors = F)
app_pop <- applicant_pop[2:33,] 
app_app <- app_pop[,1:2]
app_rate <- app_pop[,c(1,3)]
app_app$panel <- "Nr. of applicant [per million pop.]"
colnames(app_app) <- c("country", "value", "panel")
app_rate$panel <- "Acceptance rate [%]"
colnames(app_rate) <- c("country", "value", "panel")
app_pop <- rbind(app_app, app_rate)


p_appl_pop <- ggplot(app_pop, aes(x = country, y = value)) + 
    facet_grid(panel ~ .,scales = "free") +
    geom_bar(data = app_pop,
             stat = "identity") +
    geom_text(data = app_rate,
              aes(country, value, label = value),
              size = 4,
              vjust = -1) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 35,
                                     hjust = 1,
                                     vjust = 1,
                                     size = 10),
          axis.text.y = element_text(size = 10),
          strip.text.y = element_text(size = 10)) +
    labs(x = "",
         y = "")
p_appl_pop
ggsave("fig6.png", path = "~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/figures", width = 7, height = 5)

# Chance for integration
chance <- read.csv("~/Documents/Studies/MSc_Geomatics_TU_Delft/IN4400_Prog-and-DataSci/IN4400_project/work/chance_v2.csv", stringsAsFactors = F, na.strings = c("#VALUE!",0))
chance <- gather(chance, "country", "index", 2:32)
chance2013 <- chance[chance$year==2013,]
chance2013 <- arrange(chance2013,desc(index))

p_chance2013 <- ggplot(chance2013, aes(reorder(country, -index), index)) + 
    geom_point(stat = "identity",
               shape = 95,
               size = 10) +
    scale_y_continuous(limits=c(0.5,1)) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35,
                                     hjust = 1,
                                     vjust = 1,
                                     size = 12)) +
    geom_text(aes(label = round(index, digits = 2)),
              size = 4,
              vjust = -1) +
    labs(x="" , y="Integration index (2013)")
p_chance2013
