 sort -k1,1 -k2,2n TSS_Distance.txt > tss_distance_sorted.txt 

#Find intersections
bedtools intersect -a tss_distance_sorted.txt -b gencode.bed -wa -wb > intersected_lncRNAs_full.txt

cut -f4 intersected_lncRNAs_full.txt > lncrnas_overlaping_transcripts.txt
cut -f6 intersected_lncRNAs_full.txt > lncrnas_overlaping_genes.txt
