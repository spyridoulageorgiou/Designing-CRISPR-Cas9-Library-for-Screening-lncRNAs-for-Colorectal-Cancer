#!/bin/bash

#SBATCH --mail-user=spyridoulageorgiou@gmail.com
#SBATCH --job-name="sgrna blat"
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --time=50:00:00
#SBATCH --mem=25G

module load SequenceAnalysis/blat/36;

for i in sequence*
do
SAMPLE=`basename $i | sed s/\_.fasta//`
blat -stepSize=5 -repMatch=2253 -minScore=20 -minIdentity=0 ../hg38.2bit $i $SAMPLE".psl"
done
