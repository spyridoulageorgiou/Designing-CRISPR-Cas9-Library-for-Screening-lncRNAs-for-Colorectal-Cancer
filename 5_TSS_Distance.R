#Import dataset
library(readxl)
epithelial <- read_excel("C:/Users/Spyridoula/Downloads/Epithelial_lncRNAv38_info_excel (3).xlsx")

#Select only lncRNAs
lncrnas <- as.data.frame(epithelial[epithelial$hg38.wgEncodeGencodeAttrsV38.geneType == "lncRNA",])


#Create tss
lncrnas$tss.start <- 1
lncrnas$tss.end <- 1

plus_strand <- lncrnas[, 3] == "+"
lncrnas[plus_strand,]$tss.start <- lncrnas[plus_strand, 4]
lncrnas[plus_strand,]$tss.end <- lncrnas[plus_strand, 4] + 1

minus_strand <- lncrnas[, 3] == "-"
lncrnas[minus_strand,]$tss.start <- lncrnas[minus_strand, 5] - 1
lncrnas[minus_strand,]$tss.end <- lncrnas[minus_strand, 5] 

lncrnas.bed <- lncrnas[,c(2,17,18,1,3,8)]

write.table(lncrnas.bed,"C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/CRISPick Results/TSS distance/TSS_Distance.txt", quote = FALSE, sep = '\t', row.names =  FALSE, col.names =  FALSE)
#write.table("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/CRISPick Results/TSS distance")


### HISTOGRAM ###

tss_data <- read.table("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/CRISPick Results/TSS distance/TSS_Distance.txt")

# Disable scientific notation for large numbers
options(scipen = 999)

hist_result <- hist(tss_data$V2, 
     main = "TSS Position Histogram",
     xlab = "TSS Position",
     col = "purple",      # Bar color
     border = "black",    # Border color
     breaks = 20,
     freq = TRUE)         # Number of bins




# Frequency counts
counts <- hist_result$counts

# Data frame with bin centers and frequencies
hist_data <- data.frame(
  Bin_Center = hist_result$mids,
  Frequency = counts
)

write.table(hist_data, "histogram_frequencies.txt", sep = "\t", row.names = FALSE)
