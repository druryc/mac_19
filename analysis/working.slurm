#!/bin/bash
#SBATCH --job-name=mcap_rnaseq
#SBATCH --partition=shared
#SBATCH --time=0-24:00:00 ## time format is DD-HH:MM:SS
#SBATCH --cpus-per-task=8
#SBATCH --mem=5000 ## max amount of memory per node you require (in MB)
##SBATCH --core-spec=0 ## Uncomment to allow jobs to request all cores on a node
#SBATCH --error=hello-%A.err ## %A - filled with jobid
#SBATCH --output=hello-%A.out ## %A - filled with jobid
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=druryc@hawaii.edu
#srun -I30 -p sandbox -N 1 -c 1 --mem=20G -t 0-01:00:00 --pty /bin/bash

source ~/.bash_profile
#module load lang/Java
#module load lang/R
#module load lang/Python
module load lang/Anaconda3
module load bio/BLAST+/2.2.31
source activate tools_seq

#mkdir mcap_rna
#mkdir annotations

#from mcap_rna
#PREP DATA
	#gunzip ./seqs/*.fastq.gz
	#rename -v 's/(T\d{1}_\d{4}).+fastq/$1.fastq/' ./seqs/*.fastq
	#ls ./seqs/*.fastq > ./seqs/list.txt; sed -i 's/.fastq//g' ./seqs/list.txt

#PREPARE REFERENCE
	#bwa mem -t 12 ./mcap.fasta mcap_metatranscriptome.fasta > mcap_metatranscriptome.sam
	#samtools fasta -F 4 mcap_metatranscriptome.sam >mcap_transcriptome_aw.fasta
	#salmon index -k 15 -t ~/reference/mcap_transcriptome_aw.fasta -i ~/reference/mcap_index_aw
	#~/cdhit/cd-hit-est -i mcap_transcriptome_aw.fasta -o mcap_transcriptome_aw_compressed.fasta -c 0.95 -G 0 -aL 0.3 -aS 0.3 -T 16 -M 20000 -d 40
	#manually reformatted cluster output in bbedit	

#ANNOTATIONS
	#mkdir GOfromweb
	#cp ~/reference/mcap_transcriptome_aw_compressed.fasta ./
	#wget ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/complete/uniprot_sprot.fasta.gz
	#gunzip uniprot_sprot.fasta.gz
	#makeblastdb -in uniprot_sprot.fasta -dbtype prot
	#pyfasta split -n 25 mcap_transcriptome_aw_compressed.fasta 
	#blastx -query mcap_transcriptome_aw_compressed.00.fasta -db ./uniprot_sprot.fasta -evalue 0.000001 -num_threads 8 -num_alignments 5  -outfmt 6 -out blast_output-00.txt
	#grep -o "sp|[A-Z0-9]*" blast_output-00.txt | sed 's/sp|//' >gene_names-00.txt
	#while read f; do
	#	wget  "https://www.uniprot.org/uniprot/?query=id:"$f"&columns=id,go&format=tab" --output-document=./GOfromweb/"$f".txt;
	#done<gene_names-00.txt
	
	#cat blast_output-* > blast_output.txt
	#cd GOfromweb
	#cat *.txt > master_GO.txt
	#sed '/^Entry/d' master_GO.txt > master_GO_reformat.txt
	#sed '/\t$/d' master_GO_reformat.txt > int.txt
	#awk -F '\t' '{print $1}' int.txt >gene_list.txt
	#sed '/\t$/d' int.txt | tr '\n' 'x' |grep -oE 'GO:.{9}'|tr "x" "\n" |sed 's/\]//g' |perl -p -e 's/;\n/;/'|sed '/^$/d' >GOterms.txt
	#paste gene_list.txt GOterms.txt |awk '{ print length, $0 }' | sort -nr -s |sed -E 's/[0-9]+\s//' >GO_output.txt

#CLIP, REMOVE DUPLICATES, ALIGN, CALL COUNTS, RENAME QUANT FILES BY SAMPLE
#while read f; do 
	#echo $(cat ./seqs/"$f".fastq|wc -l)/4|bc >> ./output/raw_count_n.txt;
	#perl tagseq_clipper.pl ./seqs/"$f".fastq > ./seqs/"$f"_clip.fastq;
	#cutadapt --report=minimal -a "A{8}" -m 20 --cores=12 ./seqs/"$f"_clip.fastq > ./seqs/"$f"_trim.fastq;
	#echo $(cat ./seqs/"$f"_trim.fastq|wc -l)/4|bc >> ./output/trimmed_count_n.txt;
	#rm ./seqs/*clip.fastq;
	#salmon quant -l A -i ~/reference/mcap_index_aw  -r ./seqs/"$f"_trim.fastq -p 8 --validateMappings --gcBias --seqBias --minScoreFraction=0.60 -o ./final_quants/"$f";
#done< ./seqs/list.txt

#find quants -name 'quant.sf' -exec rename -v -- 's|(.*)/(.*)/(.*)$|./final/$2_quant.txt|' {} +

