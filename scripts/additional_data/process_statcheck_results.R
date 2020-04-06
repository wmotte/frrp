# aim: read statcheck and georgescu results and store in dataframe

library(plyr)
library(stringr)


############################################
# FUNCTIONS
############################################


###
# take dataframe wit detailed statistical information from StatCheck, and summarize per pmid
##
summarize.statCheck <- function (object) 
{
  x <- object
  pmid <- as.numeric(ddply(x, "pmid", function(x) unique(x$pmid))[,2])
  pValues <- ddply(x, "pmid", function(x) nrow(x))[, 2]
  Errors <- ddply(x, "pmid", function(x) sum(x$Error, na.rm = TRUE))[,2]
  DecisionErrors <- ddply(x, "pmid", function(x) sum(x$DecisionError,na.rm = TRUE))[, 2]
  sigErrors <- ddply(x, "pmid", function(x) sum(x$sigError,na.rm = TRUE))[, 2]
  onetails <- ddply(x, "pmid", function(x) sum(x$OneTail_potential, na.rm = TRUE))[,2]
  res <- data.frame(pmid = pmid, 
                    pValues = pValues, 
                    Errors = Errors, 
                    decisionErrors = DecisionErrors,
                    sigErrors = sigErrors,
                    #onetails = onetails,
                    stringsAsFactors = F)
  return(res)
}


###
# take dataframe wit detailed statistical information from georgescu ORs, and summarize per pmid
##
summarize.ORs <- function (object) 
{
  x <- object
  pmid <- ddply(x, "pmid", function(x) unique(x$pmid))[,2]
  pValues <- ddply(x, "pmid", function(x) nrow(x))[, 2]
  Errors <- ddply(x, "pmid", function(x) sum(x$pError, na.rm = TRUE))[,2]
  decisionErrors <- ddply(x, "pmid", function(x) sum(x$decisionError,na.rm = TRUE))[, 2]
  sigErrors <- ddply(x, "pmid", function(x) sum(x$sigError,na.rm = TRUE))[, 2]
  res <- data.frame(pmid = pmid, 
                    pValues = pValues, 
                    Errors = Errors, 
                    decisionErrors = decisionErrors,
                    sigErrors = sigErrors,
                    stringsAsFactors = F)
  return(res)
}



############################################
# END FUNCTIONS
############################################


# read data results per study and read data elaborate Statcheck results
dd <- read.csv("statCheckElaborate_combined3.csv", stringsAsFactors = F)
dd$pmid <- sub(".xml", "", dd$pmid)


# recompute p-value for the F-test values with 'pval=ns', had not been computed yet
rws <- as.numeric(row.names(dd[is.na(dd$Computed) & dd$Statistic == "F",])) # all have Reported.Comparison == 'ns'
for(i in rws) {
  cc <- df2 <- df1 <- FVals <- dec <- NA
  dec <- 3
  FVals <- as.numeric(dd[i,'Value'])
  df1 <- as.numeric(dd[i,'df1'])
  df2 <- as.numeric(dd[i,'df2'])
  cc <- round(pf(FVals, df1, df2, lower.tail = FALSE), dec)
  
  dd[i,]$Computed <- cc
}

# determine sigError, when reported p <= 0.05, and actual p > 0.05
dd$sigError <- as.logical(NA)
dd[!is.na(dd$Error),]$sigError <- FALSE
dd[!is.na(dd$Reported.P.Value) &
     !is.na(dd$Computed) &
     dd$Reported.Comparison == "=" &
     dd$Reported.P.Value <= 0.05 &
     dd$Computed > 0.05, ]$sigError <- TRUE
dd[!is.na(dd$Reported.P.Value) &
     !is.na(dd$Computed) &
     dd$Reported.Comparison == "<" &
     dd$Computed > dd$Reported.P.Value, ]$sigError <- TRUE
dd[!is.na(dd$Reported.P.Value) &
     !is.na(dd$Computed) &
     dd$Reported.Comparison == ">" &
     dd$Computed <= dd$Reported.P.Value, ]$sigError <- TRUE




# read georgescu_or 
ors <- read.csv("georgescu_or4_adj.csv", stringsAsFactors = F)

### clean ors
ors$pstring <- ors$pval
ors$pval <- str_extract(ors$pstring, "([[:digit:]]*)(\\.)([[:digit:]]+)")
ors$pDec <- sapply(ors$pval, function(x) {
  sp <- strsplit(x, "\\.")[[1]]
  pDec <- nchar(sp[2])
})
ors$pval <- as.numeric(ors$pval)

# remove lines with values containing "pmid "
ors <- as.data.frame(apply(ors, 2, function(x) {
  x[grepl("^pmid ", x)] <- NA
  x
}), stringsAsFactors = F)

# remove lines with only values == 1
#View(ors[!is.na( ors$pError ) & ors$pError == "1",])
ors <- ors[is.na(ors$pError) | ors$pError != "1",]

# as numeric, integer, logical
ors[,c(4:11,13:16)] <- apply(ors[,c(4:11,13:16)], 2, as.numeric)
ors[,c(17:19)] <- apply(ors[,c(17:19)], 2, as.logical)
ors$pmid <- as.integer(ors$pmid)

# remove rows when MinDiffOR > 3 times the reported OR, probably extraction error
#View(ors[!is.na(ors$MinDiffOR) & !is.na(ors$OddsR) & ors$MinDiffOR > 3*ors$OddsR,])
ors <- ors[is.na(ors$MinDiffOR) | is.na(ors$OddsR) | ors$MinDiffOR < 3*ors$OddsR,]

# remove rows when reported pvalue is larger than 1
#View(ors[!is.na(ors$pval) & ors$pval > 1,])
ors <- ors[is.na(ors$pval) | ors$pval <= 1,]

# OBSERVATION: every decisionError == sigError. why?
#table(ors$pError)
#table(ors$decisionError)
#table(ors$sigError)
#View(ors[!is.na(ors$sigError) & ors$sigError == T,])
#View(ors[!is.na(ors$pError) & ors$pval > 0.05 & ors$HighestP <= 0.05, ])
# ANSWER: all decisionErrors the other direction were not identified; sigErrors are correct
ors[!is.na(ors$pError) & ors$pval > 0.05 & ors$HighestP <= 0.05, ]$decisionError <- T


# remove rows containing 99%, 98% ci's, or anything non-95%CI
#View(ors[grepl("%", ors$modORstring) &
#           !grepl("95", ors$modORstring),])
#View(ors[grepl("99%|98%", ors$modORstring),])
#View(ors[grepl("90%", ors$modORstring),])
#View(ors[grepl("99.9%", ors$modORstring),])
#View(ors[grepl("97.5%", ors$modORstring),])
#View(ors[grepl("99%|98%|97.5%|99.9%|90%", ors$modORstring),]) #598 rows
ors <- ors[!grepl("99%|98%|97.5%|99.9%|90%", ors$modORstring),]

# remove p-values 'for non inferiority' or 'for interaction'
#View(ors[grepl("inferior", ors$modORstring),])
#View(ors[grepl("interac", ors$modORstring),])
#View(ors[grepl("interac|inferior", ors$modORstring),])
ors[grepl("interac|inferior", ors$modORstring),
     c("pval", "psign", "ComputedP", "LowestP", "HighestP", "MinDiffP", "pError", "decisionError", "sigError")] <- NA



# correct for potential one-tailed tests statcheck
dd$OneTail_potential <- FALSE

# for every row with Errors > 0, add a number of potential one-tailed tests
for(i in 1:length(dd[,1])){
  
  if(is.na(dd[i,]$Error)) next()
  if(dd[i,]$Error == 0) next()
  

  # recompute risk of potentially misclassifying one-tailed p-value as Error (OneTail == TRUE)
  # The original computation is based on a p-value with two decimals. This leads to a situation 
  # where a p-value below 0.01 is always indicated as OneTail == TRUE, because round(0.008,2) == round(0.006,2)

  if(is.na(dd[i,]$OneTail)) next()
  
  if(dd[i,]$OneTail == T) {
    computed <- dd[i,]$Computed
    comparison <- dd[i,]$Reported.Comparison
    reported <- dd[i,]$Reported.P.Value
    raw <- dd[i,]$Raw
    onetail <- computed/2
    decimals <- dd[i,]$Decimals
    decimals[decimals < 0] <- 0
    OneTail2 <- ifelse( dd[i,]$Error == TRUE & 
                          (grepl("=", comparison) & 
                             ( round(reported, decimals) == floor(onetail*10^decimals)/10^decimals | 
                                 round(reported, decimals) == ceiling(onetail*10^decimals)/10^decimals ) ) | 
                          (grepl("<", comparison) & reported == 0.05 &
                             onetail < reported & computed > reported), 
                        TRUE, 
                        FALSE )
    dd[i,]$OneTail_potential <- OneTail2
    dd[i,]$Error <- F
    dd[i,]$DecisionError <- F
    dd[i,]$sigError <- F
  }
}





# SUMMARIZE PER STUDY ## 
s1 <- summarize.statCheck(dd)
s2 <- summarize.ORs(ors)

# create dichotomous values
s1$sigerror <- s1$decisionerror <- s1$error <- FALSE
s1[s1$Errors > 0,]$error <- TRUE
s1[s1$decisionErrors > 0,]$decisionerror <- TRUE
s1[s1$sigErrors > 0,]$sigerror <- TRUE

s2$sigerror <- s2$decisionerror <- s2$error <- FALSE
s2[s2$Errors > 0,]$error <- TRUE
s2[s2$decisionErrors > 0,]$decisionerror <- TRUE
s2[s2$sigErrors > 0,]$sigerror <- TRUE

# create summary dataframe s1 + s2
s3 <- arrange(rbind(s1,s2),pmid)

# combine output for duplicated studies (e.g. with results from statCheck and Georgescu)
#dups <- s3[duplicated(s3$pmid),]$pmid
#View(dups <- arrange(s3[s3$pmid %in% dups,], pmid))
s4 <- ddply(s3, .(pmid), function(x) {
  data.frame( pmid = unique(x$pmid),
              pValues = sum(x$pValues),
              nErrors = sum(x$Errors),
              nDecisionErrors = sum(x$decisionErrors),
              nSigErrors = sum(x$sigErrors),
              error = sum(x$error),
              decisionerror = sum(x$decisionerror),
              sigerror = sum(x$sigerror),
              stringsAsFactors = F )
})
s4[s4$error > 0,]$error <- T
s4[s4$decisionerror > 0,]$decisionerror <- T
s4[s4$sigerror > 0,]$sigerror <- T

s4$error <- as.logical(s4$error)
s4$decisionerror <- as.logical(s4$decisionerror)
s4$sigerror <- as.logical(s4$sigerror)



# merge with df of all included studies
df <- read.csv("../data/datatotal.csv", stringsAsFactors = F)
dfout <- merge(df[,c("pmid", "doi")],
            s4,
            by = "pmid",
            all.x = TRUE)
dfout$doi <- NULL

# write final output
write.csv(dfout, "statRigor_output.csv", row.names = F)
write.csv(s4, "statRigor_output_small.csv", row.names = F)




####################################################
############# unnecessary code, see conclusion below
####################################################
# read georgescu_ratio
ratios <- read.csv("georgescu_ratio4_adj.csv", stringsAsFactors = F)


### clean ratios
ratios <- as.data.frame(apply(ratios, 2, function(x) {
  x[grepl("^pmid ", x)] <- NA
  x
}), stringsAsFactors = F)
ratios$reported <- as.numeric(ratios$reported)
ratios$computed <- as.numeric(ratios$computed)
ratios$reported.decimals <- as.numeric(ratios$reported.decimals)
ratios$difference <- as.numeric(ratios$difference)
ratios$pmid <- as.integer(ratios$pmid)
#str(ratios)
#summary(ratios)


# remove rows using values smaller than one
#View(ratios[grepl("^0[^(0/)]", ratios$ratio) |
#              grepl("^\\.", ratios$ratio) |
#              grepl("/\\.", ratios$ratio) |
#              grepl("/0[^(/0$)]", ratios$ratio),])
ratios <- ratios[is.na(ratios$ratio) | !(
  grepl("^0[^(0/)]", ratios$ratio) |
    grepl("^\\.", ratios$ratio) |
    grepl("/\\.", ratios$ratio) |
    grepl("/0[^(/0$)]", ratios$ratio) ),]

# replace numbers that are 1000+ but written as 1.000+ (originally 1,000+)
#View(ratios[grepl("[[:digit:]]\\.[[:digit:]]{3}", ratios$ratio),])
ratios$ratio_orig <- ratios$ratio
ratios$ratio <- gsub("([[:digit:]])(\\.)([[:digit:]]{3})", "\\1\\3", ratios$ratio)

# remove rows containing non-integers
#View(ratios[grepl("\\.[[:digit:]]", ratios$ratio),])
ratios <- ratios[!grepl("\\.[[:digit:]]", ratios$ratio),]

# remove rows where reported percentage is larger than 100%, extraction error
#View(ratios[!is.na(ratios$reported) & ratios$reported > 1,])
ratios <- ratios[!is.na(ratios$reported) & ratios$reported <= 1,]

# remove rows where computed percentage is larger than 100% or smaller than 0%, not real proportion
#View(ratios[!is.na(ratios$computed) & ratios$computed > 1,])
ratios <- ratios[is.na(ratios$computed) | ratios$computed <= 1,]
ratios <- ratios[is.na(ratios$computed) |  ratios$computed >= 0,]
#summary(ratios)

# remove rows where ratio contains negative values, or starting with '+'
#View(ratios[grepl("-[[:digit:]]", ratios$ratio),])
ratios <- ratios[is.na(ratios$ratio) | !grepl("-[[:digit:]]", ratios$ratio),]
ratios <- ratios[is.na(ratios$ratio) | !grepl("\\+[[:digit:]]", ratios$ratio),]

# remove rows where ratio contains incomplete numbers
#View(ratios[grepl("[[:digit:]]\\./", ratios$ratio),])
ratios <- ratios[is.na(ratios$ratio) | !grepl("[[:digit:]]\\./", ratios$ratio), ]


# new colums for ratios to recompute percentage
sp <- strsplit(ratios$ratio, "/")
ratios$left <- sapply(sp, function(x) as.numeric(x[1]))
ratios$right <- sapply(sp, function(x) as.numeric(x[2]))
ratios$recomputed <- round( ratios$left / ratios$right, ratios$reported.decimals)

# remove rows where nominator or denominator is not a number
#View(ratios[is.na(as.numeric(ratios$left)) | is.na(as.numeric(ratios$right)),])
ratios <- ratios[!(is.na(ratios$left) | is.na(ratios$right) ),]

# when recomputed proportion != reported proportion, and 
# the original number contained a 1.000+ notation, something is wrong
View(ratios[grepl("[[:digit:]]\\.[[:digit:]]{3}", ratios$ratio_orig) & ratios$reported != ratios$recomputed,])

###############
# CONCLUSION: original algorithm does not work properly when the text contains
# a pattern like 'n/n p%, n/n p%', because it may take the wrong percentage with a ratio.
# FIX THIS IN GeorgescuReporting4.R
# DON'T USE RATIOS (YET, OR AT ALL)
###############




# quit R session
q("no")
