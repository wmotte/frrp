
################################################################################
#
## aim: summarize missing URLs
## date: 2017-11-30
## herm.lamberink@gmail.com
#
################################################################################

setwd( "/mnt/cortexraid/hlamberink" ) 
.libPaths( c(.libPaths(), "/mnt/cortexraid/hlamberink/RLibrary" ) ) 

## load required libraries
library( 'rentrez' )
library( "curl" )
library( 'xml2' ) # used by rvest package
library( 'rvest' ) # web scraping package
library( 'XML' )


################################################################################
# BEGIN FUNCTIONS								#
################################################################################


################################################################################
# EIND FUNCTIONS								#
################################################################################

na1 <- read.csv2( "pdf/all_rctsNA1.csv", stringsAsFactors = FALSE, header = FALSE ); names(na1) <- c("pmid", "url")
na2 <- read.csv2( "pdf/all_rctsNA2.csv", stringsAsFactors = FALSE, header = FALSE ); names(na2) <- c("pmid", "url")
na3 <- read.csv2( "pdf/all_rctsNA3.csv", stringsAsFactors = FALSE, header = FALSE ); names(na3) <- c("pmid", "url")
na4 <- read.csv2( "pdf/all_rctsNA4.csv", stringsAsFactors = FALSE, header = FALSE ); names(na4) <- c("pmid", "url")
na5 <- read.csv2( "pdf/all_rctsNA5.csv", stringsAsFactors = FALSE, header = FALSE ); names(na5) <- c("pmid", "url")
na6 <- read.csv2( "pdf/all_rctsNA6.csv", stringsAsFactors = FALSE, header = FALSE ); names(na6) <- c("pmid", "url")
na7 <- read.csv2( "pdf/all_rctsNA7.csv", stringsAsFactors = FALSE, header = FALSE ); names(na7) <- c("pmid", "url")
na8 <- read.csv2( "pdf/all_rctsNA8.csv", stringsAsFactors = FALSE, header = FALSE ); names(na8) <- c("pmid", "url")
na9 <- read.csv2( "pdf/all_rctsNA9.csv", stringsAsFactors = FALSE, header = FALSE ); names(na9) <- c("pmid", "url")
na10 <- read.csv2( "pdf/all_rctsNA10.csv", stringsAsFactors = FALSE, header = FALSE ); names(na10) <- c("pmid", "url")

na <- rbind(na1,na2,na3,na4,na5,na6,na7,na8,na9,na10)

n <- sum(!complete.cases(na$url))

print( paste( "number of pmids without url:", n))