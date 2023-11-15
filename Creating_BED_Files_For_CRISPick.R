library(readxl)
setwd("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex")
#Create bed file from gencode
gencode <- read_excel("C:/Users/Spyridoula/Downloads/All_gencodev38_genes (1).xlsx")

#keep protein coding
gencode <- gencode[gencode$hg38.wgEncodeGencodeAttrsV38.geneType == "protein_coding",]

gencode.df <- gencode[,c(2,4,5,1,3,7,11)]
gencode.df <- as.data.frame(gencode.df)
gencode.final <- gencode.df

plus_strand <- gencode.df[, 5] == "+"
gencode.final[plus_strand, 2] <- gencode.df[plus_strand, 2] - 1
gencode.final[plus_strand, 3] <- gencode.df[plus_strand, 2] + 1

minus_strand <- gencode.df[, 5] == "-"
gencode.final[minus_strand, 2] <- gencode.df[minus_strand, 3] - 1
gencode.final[minus_strand, 3] <- gencode.df[minus_strand, 3] + 1

write.table(gencode.df, "gencode.bed",col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)

epithelial <- read.csv("C:/Users/Spyridoula/Downloads/Epithelial_lncRNAv38_info_excel.xlsx - Epithelial_lncRNAv38_info.csv")

#Create bed froom epithelail file
epithelial <- epithelial[,c(2,4,5,1,3,7,11)]

epithelial500 <- epithelial 
epithelial750 <- epithelial 
epithelial1000 <- epithelial 

#500
plus_strand <- epithelial500[, 5] == "+"
epithelial500[plus_strand, 2] <- epithelial[plus_strand, 2] - 500
epithelial500[plus_strand, 3] <- epithelial[plus_strand, 2] + 500

minus_strand <- epithelial500[, 5] == "-"
epithelial500[minus_strand, 2] <- epithelial[minus_strand, 3] - 500
epithelial500[minus_strand, 3] <- epithelial[minus_strand, 3] + 500

#750
plus_strand <- epithelial750[, 5] == "+"
epithelial750[plus_strand, 2] <- epithelial[plus_strand, 2] - 750
epithelial750[plus_strand, 3] <- epithelial[plus_strand, 2] + 750

minus_strand <- epithelial750[, 5] == "-"
epithelial750[minus_strand, 2] <- epithelial[minus_strand, 3] - 750
epithelial750[minus_strand, 3] <- epithelial[minus_strand, 3] + 750

#1000
plus_strand <- epithelial1000[, 5] == "+"
epithelial1000[plus_strand, 2] <- epithelial[plus_strand, 2] - 1000
epithelial1000[plus_strand, 3] <- epithelial[plus_strand, 2] + 1000

minus_strand <- epithelial1000[, 5] == "-"
epithelial1000[minus_strand, 2] <- epithelial[minus_strand, 3] - 1000
epithelial1000[minus_strand, 3] <- epithelial[minus_strand, 3] + 1000

write.table(epithelial500, "500.bed", col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)
write.table(epithelial750, "750.bed", col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)
write.table(epithelial1000, "1000.bed", col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)


bed <- function(x,name){
  x <- x[,c(2,4,5,1,3,6,7)]
  write.table(x, print(name), col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)
}

bed(`plus_minus_1000.(1)`, "1000.bed")
bed(`plus_minus_750.(1)`, "750.bed")
bed(`plus_minus_500.(1)`, "500.bed")
