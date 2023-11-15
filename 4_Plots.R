library(readxl)
library(writexl)

non_intersect_500 <- read_excel("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/clean_500_non_intersect.xlsx", col_names = FALSE)
non_intersect_750 <- read_excel("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/clean_750_non_intersect.xlsx", col_names = FALSE)
non_intersect_1000 <- read_excel("C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/clean_1000_non_intersect.xlsx", col_names = FALSE)

# excels to data frames
non_intersect_500.df <- as.data.frame(non_intersect_500)
non_intersect_750.df <- as.data.frame(non_intersect_750)
non_intersect_1000.df <- as.data.frame(non_intersect_1000)

# unique genes
unique_genes_500 <- unique(non_intersect_500.df[, 6])
unique_genes_750 <- unique(non_intersect_750.df[, 6])
unique_genes_1000 <- unique(non_intersect_1000.df[, 6])


# unique transcripts
unique_transcripts_500 <- unique(non_intersect_500.df[, 4])
unique_transcripts_750 <- unique(non_intersect_750.df[, 4])
unique_transcripts_1000 <- unique(non_intersect_1000.df[, 4])

# if unique_transcripts_1000 is included in unique_transcripts_500 and unique_transcript_750
is_included_500 <- unique_transcripts_1000 %in% unique_transcripts_500
is_included_750 <- unique_transcripts_1000 %in% unique_transcripts_750

# if unique_transcripts_750 is included in unique_transcripts_500
is_included_500_for_750 <- unique_transcripts_750 %in% unique_transcripts_500

# FALSE values in is_included
any_false <- any(!is_included_500_for_750)
print(any_false)


# Unique Transcripts Plot
# vector combining all unique transcripts
all_unique_transcripts <- c(
  unique_transcripts_500,
  unique_transcripts_750,
  unique_transcripts_1000
)

transcript_counts <- table(all_unique_transcripts)



# range of frequencies
min_freq <- min(transcript_counts)
max_freq <- max(transcript_counts)

# where the histogram bins should stop
breaks <- seq(min_freq - 1, max_freq + 1, by = 1)

hist(transcript_counts, breaks = breaks, col = "#E0BBE4", main = "Frequency of Transcripts",
     xlab = "Frequency", ylab = "Count", xlim = c(0, 3), ylim = c(0, 1050), labels = TRUE, axes = FALSE)

axis(side = 1, at = 0:3, labels = 0:3)

axis(side = 2)




file_path <- "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Semester 3/4 weeks Project/alex/Plots/transcripts_frequencies_1_2.xlsx"

# transcripts appearing 1 or 2 times
filtered_counts_1_2 <- transcript_counts[transcript_counts %in% c(1, 2)]

result_df_1_2 <- data.frame(
  Transcript = names(filtered_counts_1_2),
  Frequency = as.numeric(filtered_counts_1_2),
  SourceDataFrame = NA
)

# data frame that each transcript comes from
result_df_1_2$SourceDataFrame[result_df_1_2$Transcript %in% unique_transcripts_500] <- "non_intersect_500"
result_df_1_2$SourceDataFrame[result_df_1_2$Transcript %in% unique_transcripts_750] <- "non_intersect_750"
result_df_1_2$SourceDataFrame[result_df_1_2$Transcript %in% unique_transcripts_1000] <- "non_intersect_1000"

# Save the results to an Excel file
write_xlsx(result_df_1_2, file_path)





unique_genes_500 <- length(unique_genes_500)
unique_transcripts_500 <- length(unique_transcripts_500)
unique_genes_750 <- length(unique_genes_750)
unique_transcripts_750 <- length(unique_transcripts_750)
unique_genes_1000 <- length(unique_genes_1000)
unique_transcripts_1000 <- length(unique_transcripts_1000)

# matrix of data for the barplot
data <- matrix(c(unique_genes_500, unique_transcripts_500,
                 unique_genes_750, unique_transcripts_750,
                 unique_genes_1000, unique_transcripts_1000), nrow = 2, byrow = FALSE)

group_names <- c("500", "750", "1000")
bar_names <- c("Unique Genes", "Unique Transcripts")
colors <- c("#E0BBE4", "#A8C9E6")

# grouped barplot
barplot(data, beside = TRUE, col = colors, main = "Grouped Barplot",
        xlab = "Group", ylab = "Count", ylim = c(0, max(data) * 1.15),
        legend.text = bar_names, args.legend = list(x = "topright", bty = "n"),
        names.arg = group_names)


#### sgRNAs analysis

sgRNAs.data.500 <- as.data.frame(read_excel("C:\\Users\\Spyridoula\\OneDrive\\Desktop\\Universität\\Semester 3\\4 weeks Project\\alex\\CRISPick Results\\sgRNAs.xlsx", 
                      sheet = "500"))

sgRNAs.data.500 <- sgRNAs.data.500[!grepl("NEG_CONTROL", sgRNAs.data.500$Input),]
sgRNAs.data.500 <- sgRNAs.data.500[!grepl("POS_CONTROL", sgRNAs.data.500$Input),]

number_sgRNA500 <- nrow(sgRNAs.data.500)
df.frequency.500 <- as.data.frame(table(sgRNAs.data.500[,1]))

unique.sgrnas.500 <- length(unique(sgRNAs.data.500[,1]))

# range of frequencies
min_freq <- min(df.frequency.500[,2])
max_freq <- max(df.frequency.500[,2])

# where the histogram bins should stop
breaks <- seq(min_freq - 1, max_freq + 1, by = 1)

hist(df.frequency.500[,2], breaks = breaks, col = "purple", xlab = "Number of sgRNAs", ylab = "Counts", ylim = c(0, 1100), xlim = c(0, 5), labels = TRUE)
axis(side = 1, at = 0:5, labels = 0:5)

axis(side = 2)

#750
sgRNAs.data.750 <- as.data.frame(read_excel("C:\\Users\\Spyridoula\\OneDrive\\Desktop\\Universität\\Semester 3\\4 weeks Project\\alex\\CRISPick Results\\sgRNAs.xlsx", 
                                            sheet = "750"))

sgRNAs.data.750 <- sgRNAs.data.750[!grepl("NEG_CONTROL", sgRNAs.data.750$Input),]
sgRNAs.data.750 <- sgRNAs.data.750[!grepl("POS_CONTROL", sgRNAs.data.750$Input),]

number_sgRNA750 <- nrow(sgRNAs.data.750)
df.frequency.750 <- as.data.frame(table(sgRNAs.data.750[,1]))

unique.sgrnas.750 <- length(unique(sgRNAs.data.750[,1]))

# range of frequencies
min_freq <- min(df.frequency.750[,2])
max_freq <- max(df.frequency.750[,2])

# where the histogram bins should stop
breaks <- seq(min_freq - 1, max_freq + 1, by = 1)

hist(df.frequency.750[,2], breaks = breaks, col = "purple", xlab = "Number of sgRNAs", ylab = "Counts", ylim = c(0, 1000), xlim = c(0, 5), labels = TRUE)
axis(side = 1, at = 0:5, labels = 0:5)

axis(side = 2)

#1000
sgRNAs.data.1000 <- as.data.frame(read_excel("C:\\Users\\Spyridoula\\OneDrive\\Desktop\\Universität\\Semester 3\\4 weeks Project\\alex\\CRISPick Results\\sgRNAs.xlsx", 
                                             sheet = "1000"))

sgRNAs.data.1000 <- sgRNAs.data.1000[!grepl("NEG_CONTROL", sgRNAs.data.1000$Input),]
sgRNAs.data.1000 <- sgRNAs.data.1000[!grepl("POS_CONTROL", sgRNAs.data.1000$Input),]

number_sgRNA1000 <- nrow(sgRNAs.data.1000)
df.frequency.1000 <- as.data.frame(table(sgRNAs.data.1000[,1]))

unique.sgrnas.1000 <- length(unique(sgRNAs.data.1000[,1]))

# range of frequencies
min_freq <- min(df.frequency.1000[,2])
max_freq <- max(df.frequency.1000[,2])

# where the histogram bins should stop
breaks <- seq(min_freq - 1, max_freq + 1, by = 1)

hist(df.frequency.1000[,2], breaks = breaks, col = "purple", xlab = "Number of sgRNAs", ylab = "Counts", ylim = c(0, 1000), xlim = c(0, 5), labels = TRUE)
axis(side = 1, at = 0:5, labels = 0:5)

axis(side = 2)


