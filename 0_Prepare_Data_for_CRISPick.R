# Fix coordinates

library(readxl)
library(writexl)

non_intersected_500 <- read_excel("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/500_non_intersected.xlsx")
epithelial_original <- read_excel("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/Epithelial_lncRNAv38_info_excel.xlsx", col_names = FALSE)

colnames(epithelial_original) <- c("transcript_name", "chromosome", "strand", "start", "end", "name2", "geneId", "geneName", "geneType", "geneStatus", "transcriptId", "transcriptName", "transcriptType", "transcriptStatus")
epithelial_original <- epithelial_original[-1, ] # 1st row removed cause it contains the column names

for (i in 1:nrow(non_intersected_500)) {
  # get the transcript name, start and end from the non_intersected_500
  transcript_name <- non_intersected_500[i, "transcript_name"]
  start500 <- non_intersected_500[i, "start"]
  end500 <- non_intersected_500[i, "end"]
  
  # setting start and end of epithelial as NA
  start_epi <- NA
  end_epi <- NA
  
  for (j in 1:nrow(epithelial_original)) {
    if (transcript_name == epithelial_original[j, "transcript_name"]) {
      start_epi <- as.numeric(epithelial_original[j, "start"])  # convert to numeric, wasn't working before doing that
      end_epi <- as.numeric(epithelial_original[j, "end"])  # convert to numeric
      break  # break once a match is found
    }
  }
  
  # update start and end values in non_intersected_500
  non_intersected_500[i, "start"] <- start_epi
  non_intersected_500[i, "end"] <- end_epi
}

write_xlsx(non_intersected_500, "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/500_non_intersected_original_coordinates.xlsx")





### Get them ready for CRISPick

non_intersected_500_original_coordinates <- read_excel("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/500_non_intersected_original_coordinates.xlsx")

# empty list so I can check if the formatted entries are correct before saving them in an excel
formatted_entries <- list()

for (i in 1:nrow(non_intersected_500_original_coordinates)) {
  # get info for chr and strand
  chr <- non_intersected_500_original_coordinates[i, 1]
  strand <- non_intersected_500_original_coordinates[i, 5]
  if (strand == "+") {
    position <- paste0(chr, ":+:", non_intersected_500_original_coordinates[i, 2]) # start is used as start
  } else if (strand == "-") {
    position <- paste0(chr, ":-:", non_intersected_500_original_coordinates[i, 3]) # end is used as start
  } else {
    next  # i don't think i actually need this
  }
  
  # add the formated entries to the list
  formatted_entries[[i]] <- position
}

# split the list into multiple lists of 500 entries each for CRISPick
num_entries <- length(formatted_entries)
num_lists <- ceiling(num_entries / 500)
split_lists <- split(formatted_entries, rep(1:num_lists, each = 500, length.out = num_entries))

for (i in 1:num_lists) {
  output_file <- paste0("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Final/list_", i, ".xlsx")
  write_xlsx(data.frame(entries = unlist(split_lists[[i]])), output_file)
}
