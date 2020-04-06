################################################################################
# aim: extract data from all WHO trial registries
#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2018-08-07
################################################################################


# load libraries
library('rvest')

#########################
# FUNCTIONS
#########################

#########################
# END FUNCTIONS
#########################

nr <- "NTR5342" # replace by any number
nr <- "CTRI/2010/091/000525"
link <- paste0("http://apps.who.int/trialsearch/Trial2.aspx?TrialID=", nr)
browseURL(link)

### Limitation: "Note: This record shows only 21 elements of the WHO Trial 
### Registration Data Set. To view changes that have been made to the source 
### record, or for additional information about this trial, click on the URL 
### below to go to the source record in the primary register."
### Consequence: need script to extract data from all 20 separately for 
### protocol changes etc.


##### TODO ##### read dataframe with all registry numbers
##### TODO ##### write for loop around following lines

# read html table with info
prereg <- read_html(link) %>%
  html_nodes(xpath = '//table[@id="DataList3"]') %>%
  html_table(fill = T) 
prereg <- prereg[[1]]

# transpose and create proper dataframe with one row for this one study
prereg <- as.data.frame( t( as.matrix( prereg[4:length(prereg[,1]),] ) ),
                     stringsAsFactors = F)
names(prereg) <- prereg[1,]
prereg <- prereg[2,]

