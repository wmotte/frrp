#!/bin/sh

#$ -q all.q
#$ -cwd
#$ -S /bin/bash
#$ -l h_vmem=6G
#$ -o xmlconversionNEW.out
#$ -e xmlconversionNEW.err
#$ -N PDFtoXMLconversionNEW
#$ -V 

echo "executing grobidConversion script on new pdfs"
echo "creating list of pdfs1"
pdflist1=$(ls /mnt/data/live02/stress/hlamberink/pdf/pdfNEW/pdfs1)

p1=($pdflist1)
echo "number of pdfs in pdfs1: ${#p1[@]}"

echo "preparing input and output files"
infilebase="input=@/mnt/data/live02/stress/hlamberink/pdf/pdfNEW/pdfs1/"
outfilebase="./xmloutNEW/"
outfiletail=".xml"

echo "starting for loop resulting in xml output"
for i in $pdflist1
do
	pmid=$(echo "$i" | tr -d ".pdf" )
	infile=$infilebase$i
	outfile=$outfilebase$pmid$outfiletail

	if [ ! -f $outfile ] 
	then
		curl -v --form $infile http://ml01.ninet.umcutrecht.nl:8070/api/processFulltextDocument -o $outfile
	fi
	
done


echo "creating list of pdfs2"
pdflist2=$(ls /mnt/data/live02/stress/hlamberink/pdf/pdfNEW/pdfs2)

p2=($pdflist2)
echo "number of pdfs in pdfs2: ${#p2[@]}"

echo "preparing input and output files"
infilebase="input=@/mnt/data/live02/stress/hlamberink/pdf/pdfNEW/pdfs2/"
outfilebase="./xmloutNEW/"
outfiletail=".xml"

echo "starting for loop resulting in xml output"
for i in $pdflist2
do
	pmid=$(echo "$i" | tr -d ".pdf" )
	infile=$infilebase$i
	outfile=$outfilebase$pmid$outfiletail

	if [ ! -f $outfile ] 
	then
		curl -v --form $infile http://ml01.ninet.umcutrecht.nl:8070/api/processFulltextDocument -o $outfile
	fi
	
done
