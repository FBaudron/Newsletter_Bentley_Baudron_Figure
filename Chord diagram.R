#' ---
#' title: "Figure of 'A growing global wheat crisis: securing current & future supply'"
#' author: "Frédéric Baudron & Alison Bentley"
#' date: "May 13th, 2022"
#' ---

# LOADING REQUIRED PACKAGES-----------------------------------------------------

library(dplyr)
library(migest)
library(circlize)

setwd('C:\\Users\\FBaudron\\Documents\\CIMMYT\\0. Publi\\Bentley et al\\Newsletter\\')

reg <-read.csv("fao countries.csv")
reg <- reg[,c(1,3)]

exp <- reg
imp <- reg

names(exp) <- c("Reporter.Country.Code..FAO.", "Expsubreg")
names(imp) <- c("Partner.Country.Code..FAO.", "Impsubreg")

data <-read.csv("wheat trade matrix.csv")

data <- data[data$Year > 2017,]
data <- data[data$Year < 2020,]
data <- data[ which(data$Element == "Export Quantity"), ]

data <- merge(data, exp, by = "Reporter.Country.Code..FAO.", all.x = TRUE)
data <- merge(data, imp, by = "Partner.Country.Code..FAO.", all.x = TRUE)

data <- na.omit(data)

data$Expsubreg <- ifelse(data$Reporter.Countries == "Russian Federation", "Russia", data$Expsubreg)
data$Expsubreg <- ifelse(data$Reporter.Countries == "United States of America", "USA", data$Expsubreg)
data$Expsubreg <- ifelse(data$Reporter.Countries == "Canada", "Canada", data$Expsubreg)
data$Expsubreg <- ifelse(data$Reporter.Countries == "France", "France", data$Expsubreg)
data$Expsubreg <- ifelse(data$Reporter.Countries == "Ukraine", "Ukraine", data$Expsubreg)
data$Expsubreg <- ifelse(data$Reporter.Countries == "Argentina", "Argentina", data$Expsubreg)
data$Expsubreg <- ifelse(data$Reporter.Countries == "Australia", "Australia", data$Expsubreg)
data$Expsubreg <- ifelse(data$Expsubreg == "Australia and New Zealand", "Oceania", data$Expsubreg)

data$Impsubreg <- ifelse(data$Partner.Countries == "Russian Federation", "Russia", data$Impsubreg)
data$Impsubreg <- ifelse(data$Partner.Countries == "United States of America", "USA", data$Impsubreg)
data$Impsubreg <- ifelse(data$Partner.Countries == "Canada", "Canada", data$Impsubreg)
data$Impsubreg <- ifelse(data$Partner.Countries == "France", "France", data$Impsubreg)
data$Impsubreg <- ifelse(data$Partner.Countries == "Ukraine", "Ukraine", data$Impsubreg)
data$Impsubreg <- ifelse(data$Partner.Countries == "Argentina", "Argentina", data$Impsubreg)
data$Impsubreg <- ifelse(data$Partner.Countries == "Australia", "Australia", data$Impsubreg)
data$Impsubreg <- ifelse(data$Impsubreg == "Australia and New Zealand", "Oceania", data$Impsubreg)

data$Expsubreg <- ifelse(data$Reporter.Countries == "Norway", "Northern Europe", data$Expsubreg)

data$Impsubreg <- ifelse(data$Partner.Countries == "Norway", "Northern Europe", data$Impsubreg)

data <- subset(data, Expsubreg != "Antarctica")
data <- subset(data, Impsubreg != "Antarctica")

subreg <- data[c(14,17,18)]

subreg <- subreg %>%
  group_by(Expsubreg, Impsubreg) %>% 
  summarise_each(funs(sum))

names(subreg) <- c("orig_reg", "dest_reg", "flow")

subreg$orig_reg <- ifelse(subreg$orig_reg == "Eastern Europe", "Eastern Europe (1)", subreg$orig_reg)
subreg$orig_reg <- ifelse(subreg$orig_reg == "Oceania", "Oceania (2)", subreg$orig_reg)
subreg$orig_reg <- ifelse(subreg$orig_reg == "South America", "South America (3)", subreg$orig_reg)
subreg$orig_reg <- ifelse(subreg$orig_reg == "Western Europe", "Western Europe (4)", subreg$orig_reg)

subreg$dest_reg <- ifelse(subreg$dest_reg == "Eastern Europe", "Eastern Europe (1)", subreg$dest_reg)
subreg$dest_reg <- ifelse(subreg$dest_reg == "Oceania", "Oceania (2)", subreg$dest_reg)
subreg$dest_reg <- ifelse(subreg$dest_reg == "South America", "South America (3)", subreg$dest_reg)
subreg$dest_reg <- ifelse(subreg$dest_reg == "Western Europe", "Western Europe (4)", subreg$dest_reg)

labels <- subreg[,c(1)]
labels <- unique(labels)

# write.csv(labels, "labels chord.csv")
df1 <-read.csv("labels chord.csv")

colors <- c(Argentina = "#4C1D4BFF", Australia = "#4C1D4BFF",  Canada = "#4C1D4BFF",
            Caribbean = "#4C1D4BFF", 'Central America' = "#4C1D4BFF", 'Central Asia' = "#4C1D4BFF",
            'Eastern Africa' = "#4C1D4BFF", 'Eastern Asia' = "#4C1D4BFF", 'Eastern Europe (1)' = "#4C1D4BFF",
            France = "#4C1D4BFF", Melanesia = "#4C1D4BFF", 'Middle Africa' = "#4C1D4BFF", 
            'Northern Africa' = "#4C1D4BFF", 'Northern Europe' = "#4C1D4BFF", 'Oceania (2)' = "#4C1D4BFF",
            Russia = "#F69C73FF", 'South-Eastern Asia' = "#4C1D4BFF", 'South America (3)' = "#4C1D4BFF",
            'Southern Africa' = "#4C1D4BFF", 'Southern Asia' = "#4C1D4BFF", 'Southern Europe' = "#4C1D4BFF",
            Ukraine = "#FAEBDDFF", USA = "#4C1D4BFF", 'Western Africa' = "#4C1D4BFF",
            'Western Asia' = "#4C1D4BFF", 'Western Europe (4)' = "#4C1D4BFF")

jpeg("Chord wheat trade.jpeg", units="cm", width=25, height=30, res=1000) 

par(mar = c(0,0,0,0), bg = c("grey10"))

circos.par(track.margin = c(-0.3, 0.3))

chordDiagram(x = subreg, directional = 1, 
             grid.col = colors,
             transparency = 0.25,
             link.lwd = 1,
             link.lty = 1,
             link.border = 1,
             direction.type = c("arrows"),
             annotationTrack = "grid", 
             annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = 1, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    region = df1$region[df1$region == sector.index]
    circos.text(x = mean(xlim), y = 2, labels = region, cex = 1, col = "white", facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }
)

title(main = list("Global wheat trade (2018 & 2019)",
                  cex=2.2,
                  col="white"),
      line = -4)

title(sub = list("(1) excluding Russia & Ukraine, (2) excluding Australia, (3) excluding Argentina, (4) excluding France",
                  cex=1,
                  col="white"),
      line = -2)

dev.off()





