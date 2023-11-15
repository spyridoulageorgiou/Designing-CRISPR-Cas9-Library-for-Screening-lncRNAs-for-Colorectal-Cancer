library(readxl)

crispick500 <- read_excel("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/CRISPick Results/5 sgRNAs/CRISPick (5 sgRNAs).xlsx")
duplicated <- crispick500[duplicated(crispick500$`sgRNA Sequence`),]

###Convert crispick results to bed


#Keep input, strand and position
duplicated <- duplicated[,c(1,16,18,19)]

#Keep only negative controls in one table
negatives <- as.data.frame(duplicated[duplicated$Input == "(NEG_CONTROL)",])

#Keep all the others 
duplicated.to.continue <- as.data.frame(duplicated[!duplicated$Input == "(NEG_CONTROL)",])

#Create bed from crispick
#for + strand start: cut -17 end: cut +2
#for - strand start: cut -3 end: cut + 16

#Create two new columns equal to value 1
duplicated.to.continue$start <- 1
duplicated.to.continue$end <- 1

#500
plus_strand <- duplicated.to.continue[, 2] == "+"
duplicated.to.continue[plus_strand,]$start <- duplicated.to.continue[plus_strand, 3] - 17 
duplicated.to.continue[plus_strand,]$end <- duplicated.to.continue[plus_strand, 3] + 2

minus_strand <- duplicated.to.continue[, 2] == "-"
duplicated.to.continue[minus_strand,]$start <- duplicated.to.continue[minus_strand, 3] - 3
duplicated.to.continue[minus_strand,]$end <- duplicated.to.continue[minus_strand, 3] + 16

#Convert names to chromosomes
#keep only refseq id
duplicated.to.continue[,1] <- substr(duplicated.to.continue[,1],1,12)

#convert chr
duplicated.to.continue[,1] <- sub("^NC_0*([[:digit:]]+).*$", "chr\\1", duplicated.to.continue[,1])

# added some code so i can get the files to my directory

library(writexl)

output_dir <- "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/Duplicates/"
output_file_duplicated <- paste0(output_dir, "duplicated.xlsx")
output_file_duplicated_to_continue <- paste0(output_dir, "duplicated_to_continue.xlsx")

write_xlsx(duplicated, output_file_duplicated)
write_xlsx(duplicated.to.continue, output_file_duplicated_to_continue)
