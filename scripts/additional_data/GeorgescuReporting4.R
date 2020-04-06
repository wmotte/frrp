# aim: determine p-value OR/RR/HR and compare to reported p-value
# source Visual Basic script: Georgescu et al. 2018
# date: 2018-11-02
# contact: h.j.lamberink@umcutrecht.nl
##################################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 

options(error = function() traceback(2))

library(stringr)
library(plyr); library(dplyr)


##################################
# FUNCTIONS
##################################

###
# extract ratios and percentages, check whether correct
# source: lines 320 - 510 from Georgescues .frm script translated to R code
##
ratioCheck <- function(intxt) {
  
  #pre-processing steps to reduce textual variability in reportable items
  txt <- gsub( "\\( ", "(", intxt )
  txt <- gsub( " \\)", ")", txt )
  txt <- gsub( "\\(", " (", txt)      #to ensure % and ratio are separated by a space if( otherwise not
  txt <- gsub( "  ", " ", txt)     #eliminate double-spaces
  txt <- gsub( " per cent ", " percent ", txt )
  txt <- gsub( "percent", "%", txt )
  txt <- gsub( " %", "%", txt )
  txt <- gsub("/ ", "/", txt)
  
  #? if( AbStart > 0 ) {
  wrds <- str_split(txt, " ")[[1]]
  UB <- length(wrds)
  LastPeriod <- 0
  #? Erase WordCheck
  #? ReDim WordCheck%(UB)
  WordCheck <- rep(0, UB)
  
  # prepare vehicle
  df_out <- row_out <- NULL
  
  # NOTE loop over all words
  for( t in 1:UB ){ 
    
    # prepare elements
    ratio <- NA
    percent <- NA
    ExamineFlag <- F
    row_out <- NULL 
    
    if( grepl( "\\.$", wrds[t] ) ) LastPeriod <- t
    
    #look for "X/Y (Z%)" pattern
    if( grepl( "/", wrds[t] ) ) {               #potential divisor sign
      if( !grepl( "[\\.,;:]$", wrds[t] ) ) {    #the two values aren#t split by the end of a sentence or phrase
        if( !grepl( "+/-", wrds[t] ) ) {       #make sure the divisor isn#t part of an error estimate
          if( grepl( "%)", wrds[t + 1] ) ) {     #followed by potential percent value reported
            ratio <- wrds[t] 
            percent <- wrds[t + 1]
            ExamineFlag <- T
          }
        }
      }
    }
    
    #look for "Z% (X/Y)" pattern
    if( grepl( "%", wrds[t] ) ) {               #percent indicator
      if( grepl( "/", wrds[t + 1] ) ) {         #next word a potential ratio
        if( !grepl( "[\\.,;:]$", wrds[t] ) ) {  #the two values aren#t split by the end of a sentence or phrase
          if( !grepl( "+/-", wrds[t] )  ) {     #make sure it#s not an error estimate
            ratio <- wrds[t + 1]
            percent <- wrds[t]
            ExamineFlag <- T
          }
        }
      }
    }
    
    if( WordCheck[t] == 1 ) ExamineFlag <- F    #This % or # was part of a valid pattern earlier
    
    #Now check if these are really ratio & percent value being reported
    if( ExamineFlag == T ) {
      Context <- NULL
      if( LastPeriod > 0 ) {
        u <- LastPeriod + 1       #u is indication of the first word of the sentence
      } else {
        u <- 1
      }
      if( grepl( "%", wrds[u] ) & u >= 4 ) {       #the % is the first word of the sentence
        Context <- paste( wrds[u - 4],  wrds[u - 3],  wrds[u - 2],  wrds[u - 1] )
      }
      loopflag <- 0
      while( loopflag == 0 ) {          #get surrounding context
        Context <- paste( Context, wrds[u] )
        if( u < UB ) {
          if( grepl( "\\.$", wrds[u] ) ) { 
            loopflag <- 1
          } else {
            loopflag <- 0
          } 
        } 
        u <- u + 1
        if( u == UB ) {
          loopflag <- 1
        }
      }
      
      #check validity of percent and ratio
      percent <- gsub( "\\.$", "", percent)  
      percent <- gsub( ",", ".", percent)  #Europeans use commas for decimals
      percent <- gsub( "\\(", "", percent)
      percent <- gsub( "\\)", "", percent)
      percent <- gsub( "%", "", percent)
      
      #determine the number of decimals used
      if( grepl( "\\.", percent ) ) {
        sp <- strsplit( percent, "\\.")[[1]]
        dec <- nchar(sp[2]) + 2
      } else {
        dec <- 2
      }
      
      #store as numeric variable
      percent <- suppressWarnings(as.numeric(percent))
      
      ratio <- gsub( "\\.$", "", ratio)
      ratio <- gsub( ",", ".", ratio)  #Europeans use commas for decimals
      ratio <- gsub( "\\(", "", ratio)
      ratio <- gsub( "\\)", "", ratio)
      
      ValidPct <- "N"
      if( !is.na(percent) & percent >= 0 & percent <= 100 ) ValidPct <- "Y"
      
      ValidRatio <- "N"
      if( ValidPct == "Y" ) {
        split_ratio <- strsplit(ratio, "/")[[1]]
        LeftNum <- suppressWarnings(as.numeric(split_ratio[1]))
        RightNum <- suppressWarnings(as.numeric(split_ratio[2]))
        if( !is.na(LeftNum) & !is.na(RightNum) ) ValidRatio <- "Y"
        
        if( length(split_ratio) > 2 ) ValidRatio <- "N"   #multiple slashes
        if( ValidRatio == "Y" ) {        #the #s are ok. One more check for year-based patterns
          if( LeftNum >= 1900 & LeftNum <= 2018 ) {
            O <- as.numeric(str_extract( string = LeftNum, pattern = "[0-9]{2}$" ))
            if( RightNum == O + 1 ) ValidRatio <- "N"             #it#s a year (e.g., 1998/99)
            O <- as.numeric(str_extract( string = LeftNum, pattern = "[0-9]{1}$" ))
            if( RightNum == O + 1 ) ValidRatio <- "N"             #it#s a year (e.g., 1998/9)
            if( LeftNum / RightNum > 0.995 ) ValidRatio <- "N"    #The two are very close together, suggesting year
          }    
        }
      }
      
      diff1 <- NA
      
      if( ValidRatio == "Y" ) {
        if( RightNum != 0 ) {
          WordCheck[t] <- 1   
          WordCheck[t + 1] <- 1   # indicating that the t'th word has been checked together with the element thereafter
          dv <- LeftNum / RightNum      #oddly, using Val() truncates commas in numbers, but string division works
          pct <- percent / 100
          if( pct != 0 ) {
            diff1 <- dv - pct
            #Some errors are based on faulty assumptions about syntax
            #so examine some alternate possibilities to see if they are reasonable
            if( diff1 > 0.1 ) {
              if( grepl( "\\.", RightNum ) ) {           #Sometimes a decimal is supposed to be a comma
                Rightnum2 <- as.numeric(gsub( "\\.", "", RightNum ))
                dv2 <- LeftNum / Rightnum2
                diff2 <- dv2 - pct
                if( diff2 < 0.1 & abs(diff2) < abs(diff1) ) {
                  dv <- dv2
                  diff1 <- diff2
                  ratio <- paste0(LeftNum, "/", Rightnum2)
                }
              }
            }
          } else {
            diff1 <- 0
          }
          
          if( !is.null(diff1) ) {
            if( !is.na(diff1) & diff1 > 0.02 ) {    #sometimes a space is accidentally inserted
              if( grepl( "%", wrds[t] ) &
                  grepl( "[0-9].", str_extract( wrds[t - 1], "(..)$" ) ) &
                  !is.na( suppressWarnings( as.numeric(wrds[t - 1]) ) ) ) {
                percent2 <- as.numeric(paste0( wrds[t - 1], percent ))
                pct2 <- percent2 / 100
                diff2 <- dv - pct2
                if( diff2 < 0.02 & abs(diff2) < abs(diff1) ) {
                  diff1 <- diff2
                  percent <- percent2
                  pct <- pct2
                }
              }
            }
          }
          
          if( !is.null(diff1) ) {
            if( !is.na(diff1) & diff1 > 0.1 ) {
              #in instances where they are reporting something that should add up to 100%,
              #the numerator/denominator are swapped (e.g., in sensitivity/specificity). Try flipping them and
              #see if( they are within 1%. if( so, swap them and mark with "@"
              if( LeftNum > 0 ) {
                dv2 <- RightNum / LeftNum
                diff2 <- dv2 - pct
                if( diff2 < 0.01 & abs(diff2) < abs(diff1) ) {
                  dv <- dv2
                  diff1 <- diff2
                  ratio <- paste0( RightNum, "/", LeftNum, "@" )
                }
              }
            }
          }
          
          #See if rounding up or rounding down gets closer to the reported number (benefit of doubt)
          ReportedNum <- pct
          CalcNum <- dv
          #if( CalcNum != 0 ) {
          #  round_down <- floor( 100 * CalcNum ) / 100
          #  round_up <- ceiling( 100 * CalcNum ) / 100
          #  
          #  diff_down <- round_down - pct
          #  diff_up <- round_up - pct
          #  
          #  diff1 <- pmin(abs(diff_down), abs(diff_up))
          #}
          diff1 <- abs(ReportedNum - CalcNum)
          
          # create log10diff #? why?
          if( diff1 == 0 ) {
            log10diff <- 0
          } else if( CalcNum != 0 & pct != 0 ) {
            log10diff <- (log(CalcNum * 100) / log(10)) - (log(pct * 100) / log(10))
          } else {
            log10diff <- 0
          }
          
          # round based on reported percentage
          #pct <- round( pct, dec )
          CalcNum <- round( CalcNum, dec )
          diff1 <- round( diff1, dec )
          
          OutputFlag <- 1
          LastWord <- TwoLastWords <- NULL
          ProblemFlag <- "WARNING: potential issue comparing ratio's and proportions:"
          
          #screen out words known not to be ratios based on preceding words
          if( t > 1 ) {   #one word
            LastWord <- tolower(wrds[t - 1])
            if( LastWord == "grade" ) OutputFlag <- 0
            if( LastWord == "hpv" ) OutputFlag <- 0
            if( LastWord == "over" ) ProblemFlag <- paste( ProblemFlag, "==" )
            if( LastWord == "almost" ) ProblemFlag <- paste( ProblemFlag, "==" )
          }
          if( t > 2 ) {   #two words
            TwoLastWords <- paste( tolower(wrds[t - 2]), tolower(wrds[t - 1]) )
            if( grepl( "ribotype", TwoLastWords ) ) OutputFlag <- 0
            if( TwoLastWords == "more than" ) ProblemFlag <- paste( ProblemFlag, "==" )
            if( TwoLastWords == "greater than" ) ProblemFlag <- paste( ProblemFlag, "==" )
            if( TwoLastWords == "less than" ) ProblemFlag <- paste( ProblemFlag, "==" )
          }
          
          #flag potential problems based on problematic keywords that are not already screened out
          LCContext <- tolower(Context)
          if( OutputFlag == 1 ) {
            if( grepl( "genotype", LCContext ) )      ProblemFlag <- paste( ProblemFlag, "genotype" )
            if( grepl( "allele", LCContext ) )        ProblemFlag <- paste( ProblemFlag, "allele" )
            if( grepl( "visual acuity", LCContext ) ) ProblemFlag <- paste( ProblemFlag, "vision" )
            if( grepl( "ribotype", LCContext ) )      ProblemFlag <- paste( ProblemFlag, "ribotype" )
            if( grepl( "grade", LCContext ) )         ProblemFlag <- paste( ProblemFlag, "grade" )
            if( grepl( "hpv", LCContext ) )           ProblemFlag <- paste( ProblemFlag, "hpv" )
          }
          
          #screen out known problems by context keywords
          if( grepl( "HES", Context ) | grepl( "ethyl starch", Context ) ) OutputFlag <- 0
          if( grepl( "ompound 48/80", Context ) | grepl( "crude protein content", Context ) ) OutputFlag <- 0
          
          if( OutputFlag == 1 ) {
            if( length(percent) == 0 ) percent <- NA
            if( length(ratio) == 0 ) ratio <- NA
            if( length(diff1) == 0 ) diff1 <- NA
            if( length(log10diff) == 0 ) log10diff <- NA
            if( length(ProblemFlag) == 0 ) ProblemFlag <- NA
            
            if( ProblemFlag == "WARNING: potential issue comparing ratio's and proportions:" ) ProblemFlag <- NA
           # if( !is.na(ProblemFlag) ) message(ProblemFlag)
            
            row_out <- data.frame( ratio = ratio,
                                   percent = percent,
                                   reported = ReportedNum,
                                   computed = CalcNum,
                                   reported.decimals = dec,
                                   difference = diff1,
                                   ProblemFlag = ProblemFlag,
                                   Context = Context) 
            
          }
        }
      } 
    } 
    if( length(row_out) > 0 ) df_out <- rbind(df_out, row_out)
  } # closing for loop line 45
  return( df_out)
}


###
# extract ORs, HRs, RRs etc with confidence intervals, check whether correctly
# reported and whether p-value is correct
# Source: lines 619 - 1042 from Georgescue&Wren .frm script translated to R code
##
ciCheck <- function(intxt) {
  
  txt <- intxt 
  
  ## Steps to reduce textual variability in reported items of interest (ratio-CI pairs in this case)
  if( grepl( x = txt, "\\{")  ) txt <- gsub( x =txt, "\\{", "(")      # standardize parentheticals
  if( grepl( x = txt, "\\}")  ) txt <- gsub( x =txt, "\\}", ")")
  if( grepl( x = txt, "\\[")  ) txt <- gsub( x =txt, "\\[", "(")
  if( grepl( x = txt, "\\]")  ) txt <- gsub( x =txt, "\\]", ")")
  if( grepl( x = txt, "\\( ")  ) txt <- gsub( x =txt, "\\( ", "(")
  if( grepl( x = txt, " \\)")  ) txt <- gsub( x =txt, " \\)", ")")
  if( grepl( x = txt, "\\)\\)" )  ) txt <- gsub( x =txt, "\\)\\)", ") )")
  if( grepl( x = txt, "equals")  ) txt <- gsub( x =txt, "equals", "=")
  if( grepl( x = txt, "less than or equal to")  ) txt <- gsub( x =txt, "less than or equal to", "<=")
  if( grepl( x = txt, " less than ")  ) txt <- gsub( x =txt, " less than ", "<")
  if( grepl( x = txt, " greater than ")  ) txt <- gsub( x =txt, " greater than ", ">")
  if( grepl( x = txt, "< or =")  ) txt <- gsub( x =txt, "< or =", "<=")
  if( grepl( x = txt, "> or =")  ) txt <- gsub( x =txt, "> or =", ">=")
  if( grepl( x = txt, " +/- ")  ) txt <- gsub( x =txt, " +/- ", "+/-")
  if( grepl( x = txt, " <= ")  ) txt <- gsub( x =txt, " <= ", "<=")
  if( grepl( x = txt, " = ")  ) txt <- gsub( x =txt, " = ", "=")
  if( grepl( x = txt, " < ")  ) txt <- gsub( x =txt, " < ", "<")
  if( grepl( x = txt, " > ")  ) txt <- gsub( x =txt, " > ", ">")
  if( grepl( x = txt, ", ,")  ) txt <- gsub( x =txt, ", ,", ",")
  if( grepl( x = txt, " vs. ")  ) txt <- gsub( x =txt, " vs. ", " vs ")
  if( grepl( x = txt, " v. ")  ) txt <- gsub( x =txt, " v. ", " vs ")
  if( grepl( x = txt, "\\(")  ) txt <- gsub( x =txt, "\\(", " (")     # add space on left (so that ratio statements are on the leftmost side of each word)
  if( grepl( x = txt, "  \\(")  ) txt <- gsub( x =txt, "  \\(", " (") # strip double spaces
  if( grepl( x = txt, " =")  ) txt <- gsub( x =txt, " =", "=")
  if( grepl( x = txt, "= ")  ) txt <- gsub( x =txt, "= ", "=")
  if( grepl( x = txt, "relative risk")  ) txt <- gsub( x =txt, "relative risk", "RR")
  if( grepl( x = txt, "risk ratio")  ) txt <- gsub( x =txt, "risk ratio", "RR")
  if( grepl( x = txt, "odds ratio")  ) txt <- gsub( x =txt, "odds ratio", "OR")
  if( grepl( x = txt, "hazard ratio")  ) txt <- gsub( x =txt, "hazard ratio", "HR")
  if( grepl( x = txt, "  ") ) txt <- gsub( x =txt, "  ", "")
  if( grepl( x = txt, "OR \\(OR\\)")  ) txt <- gsub( x =txt, "OR \\(OR\\)", "OR")
  if( grepl( x = txt, "RR \\(RR\\)")  ) txt <- gsub( x =txt, "RR \\(RR\\)", "RR")
  if( grepl( x = txt, "HR \\(HR\\)")  ) txt <- gsub( x =txt, "HR \\(HR\\)", "HR")
  if( grepl( x = txt, "\\(OR \\(OR")  ) txt <- gsub( x =txt, "\\(OR \\(OR", "OR")
  if( grepl( x = txt, "\\(RR \\(RR")  ) txt <- gsub( x =txt, "\\(RR \\(RR", "RR")
  if( grepl( x = txt, "\\(HR \\(HR")  ) txt <- gsub( x =txt, "\\(HR \\(HR", "HR")
  
  UnmodAbstr <- txt
  LastPeriod <- 0
  
  #?  if( AbStart > 0 ) {
  sp <- strsplit(txt, split = " ")[[1]]
  UnmodSP <- strsplit(UnmodAbstr, " ")[[1]]
  UB <- length(sp)
  dfout <- NULL  # added this line to export output nicely
  for( t in 1:UB ) {  
  #for( t in 1:2181 ) {  
    
    if( substr(sp[t], nchar(sp[t]), nchar(sp[t])) == "." ) LastPeriod = t
    
    # look for( (OR=, (RR=, (HR= pattern - these are high-confidence instances
    ParenFlag <- 0
    ORflag <- RRflag <- HRflag <- 0
    LOR <- substr(sp[t], 1, 3) # extract first three characters
    d <- substr(sp[t], 4, 4 ) # extract the fourth character
    if( nchar(sp[t]) == 3 ) {
      if( LOR == "(OR" ) {
        ParenFlag <- ORflag <- 1
        sp[t] = gsub( x =sp[t], "\\(OR", "(RATIO" ) # replace with standard pattern and set ORflag
      }
      if( LOR == "(RR" ) {
        ParenFlag <- RRflag <- 1
        sp[t] <- gsub( x =sp[t], "\\(RR", "(RATIO" ) # replace with standard pattern and set RRflag
      }
      if( LOR == "(HR" ) {
        ParenFlag <- HRflag <- 1
        sp[t] <- gsub( x =sp[t], "\\(HR", "(RATIO" ) # replace with standard pattern and set HRflag
      }
    }
    if( d %in% c(";", ":", ",", "=", " ") ) {
      if( LOR == "(OR" ) {
        ParenFlag <- ORflag <- 1
        sp[t] = gsub( x =sp[t], "\\(OR", "(RATIO" ) # replace with standard pattern and set ORflag
      }
      if( LOR == "(RR" ) {
        ParenFlag <- RRflag <- 1
        sp[t] <- gsub( x =sp[t], "\\(RR", "(RATIO" ) # replace with standard pattern and set RRflag
      }
      if( LOR == "(HR" ) {
        ParenFlag <- HRflag <- 1
        sp[t] <- gsub( x =sp[t], "\\(HR", "(RATIO" ) # replace with standard pattern and set HRflag
      }
    }
    if( sp[t] == "(RATIO" ) sp[t] <- "(RATIO="
    if( d %in% c(";", ":", ",", " ") ) {
      repl <- paste0( "\\(RATIO", d )
      sp[t] <- gsub( x =sp[t], repl, "\\(RATIO=")   # standardize
    }
    if( ParenFlag == 1 ) {     # A potential reporting item is on the left paren, so find the matching right paren
      ORstring <- NULL
      ExamineFlag <- 1  # (first word should have left paren)
      u <- t
      while( u < UB ) {
        ORstring <- paste0(ORstring, sp[u], " ")     # extend to next paren or bracket to get full statement
        if( grepl( x = sp[u], "\\(") ) {        # nested parentheses. for( each "(", demand one more ")"
          ExamineFlag <- ExamineFlag - 1
        }
        if( grepl( x = sp[u], "\\)") |                                  # if right parenthesis is found, or
            substr(sp[u], nchar(sp[u]), nchar(sp[u]) ) == "." ) {         # if it is the end of a sentence
          ExamineFlag <- ExamineFlag + 1
        }
        if( ExamineFlag == 1 ) {               # found the matching right parenthetical, so drop out of loop
          u <- UB
        }
        u <- u + 1
      }
      
      # contains CI (confidence interval) or CL (confidence limit) acronym
      lcor <- tolower(ORstring)
      if( !grepl( x = lcor, "ci") & 
          !grepl( x = lcor, "cl") & 
          !grepl( x = lcor, "confidence interval") & 
          !grepl( x = lcor, "confidence limit") ) {
        ExamineFlag <- 0
      }
    } else {
      ExamineFlag <- 0
    }
    
    # .frm file line 717
    # when pattern was found, start processing. Otherwise do not bother
    if( ExamineFlag == 1 ) {  # closed toward end of function
      Context <- NULL
      if( LastPeriod > 0 ) {
        u <- LastPeriod + 1
      } else {
        u <- 1
      }
      loopflag <- 0
      while( loopflag == 0 ) {  # get surrounding context
        Context <- paste0( Context, UnmodSP[u], " " )
        if( u < UB ) {
          if(substr(UnmodSP[u], nchar(UnmodSP[u]), nchar(UnmodSP[u])) == ".") {
            loopflag <- 1
          } 
        } else {
          loopflag <- 1
        }
        u <- u + 1
      }
      ## parse the odds ratio expression. First, reduce variation.
      ORstring <- gsub("( ){2,}", " ", ORstring) # remove double spaces
      if( substr(ORstring, nchar(ORstring), nchar(ORstring)) == " " ) ORstring <- substr(ORstring, 1, nchar(ORstring) -1)
      if( substr(ORstring, nchar(ORstring), nchar(ORstring)) == "\\." ) ORstring <- substr(ORstring, 1, nchar(ORstring) -1)
      if( substr(ORstring, nchar(ORstring), nchar(ORstring)) == "," ) ORstring <- substr(ORstring, 1, nchar(ORstring) -1)
      if( substr(ORstring, nchar(ORstring), nchar(ORstring)) == ";" ) ORstring <- substr(ORstring, 1, nchar(ORstring) -1)
      if( grepl( x = ORstring, "= ") ) ORstring <- gsub( x =ORstring, "= ", "=")
      if( grepl( x = ORstring, "=OR") ) ORstring <- gsub( x =ORstring, "=OR", "=")
      if( grepl( x = ORstring, "=RR") ) ORstring <- gsub( x =ORstring, "=RR", "=")
      if( grepl( x = ORstring, "=HR") ) ORstring <- gsub( x =ORstring, "=HR", "=")
      d <- substr(ORstring, 8, 8)    # sometimes double punctuation occurs
      if( d %in% "[,:;-]" ) {
        ORstring <- gsub( x =ORstring, paste0("(RATIO=", d), "(RATIO=")
      }
      modORstring = tolower(ORstring)
      modORstring <- gsub(rawToChar(as.raw(c(226,183))), ".", modORstring)  # decimal
      modORstring <- gsub(rawToChar(as.raw(c(194,183))), ".", modORstring)  # decimal
      modORstring <- gsub(rawToChar(as.raw(c(226,94,188))), "-", modORstring)  # dash
      modORstring <- gsub(rawToChar(as.raw(c(194,94,188))), "-", modORstring)  # dash
      modORstring <- gsub(rawToChar(as.raw(c(194,177))), "-", modORstring)  # dash
      modORstring <- gsub(rawToChar(as.raw(c(194,177))), "-", modORstring)  # dash
      modORstring <- gsub(rawToChar(as.raw(c(226, 128, 147))), "-", modORstring) # dash
      modORstring <- gsub(rawToChar(as.raw(c(194, 128, 147))), "-", modORstring) # dash
      modORstring <- gsub(rawToChar(as.raw(c(226, 171, 189))), "=", modORstring) # equals_sign
      modORstring <- gsub(rawToChar(as.raw(c(194, 171, 189))), "=", modORstring) # equals_sign
      modORstring <- gsub(rawToChar(as.raw(c(194, 188))), "=", modORstring) # equals_sign
      modORstring <- gsub(rawToChar(as.raw(c(226, 188))), "=", modORstring) # equals_sign
      modORstring <- gsub(rawToChar(as.raw(c(226, 180, 157, 46))), "=", modORstring) # equals_sign
      modORstring <- gsub(rawToChar(as.raw(c(194, 180, 157, 46))), "=", modORstring) # equals_sign
      #modORstring <- gsub(rawToChar(as.raw(c(210, 402))), "=", modORstring) # equals_sign
      modORstring <- gsub(rawToChar(as.raw(c(226, 172))), "<", modORstring) # smaller_than
      modORstring <- gsub(rawToChar(as.raw(c(194, 172))), "<", modORstring) # smaller_than
      if( grepl( x = modORstring, "~") ) modORstring = gsub( x =modORstring, "~", "-")         # dash (assumed)
      if( grepl( x = modORstring, "&lt;") ) modORstring = gsub( x =modORstring, "&lt;", "<")    # <
      if( grepl( x = modORstring, " to ") ) modORstring = gsub( x =modORstring, " to ", "-")
      if( grepl( x = modORstring, " approximately ") ) modORstring = gsub( x =modORstring, " approximately ", "-")
      if( grepl( x = modORstring, " of ") ) modORstring = gsub( x =modORstring, " of ", "=")
      if( grepl( x = modORstring, " with ") ) modORstring = gsub( x =modORstring, " with ", "|")
      if( grepl( x = modORstring, " for\\( each ") ) modORstring = gsub( x =modORstring, " for\\( each ", " per each ")
      if( grepl( x = modORstring, "p for\\( trend") ) modORstring = gsub( x =modORstring, "p for\\( trend", "p")
      if( grepl( x = modORstring, " for\\( ") ) modORstring = gsub( x =modORstring, " for( ", ", for( ")
      if( grepl( x = modORstring, " in ") ) modORstring = gsub( x =modORstring, " in ", ", in ")
      if( grepl( x = modORstring, "percent") ) modORstring = gsub( x =modORstring, "percent", "%")
      if( grepl( x = modORstring, "per-cent") ) modORstring = gsub( x =modORstring, "per-cent", "%")
      if( grepl( x = modORstring, "per cent") ) modORstring = gsub( x =modORstring, "per cent", "%")
      if( grepl( x = modORstring, "per- cent") ) modORstring = gsub( x =modORstring, "per- cent", "%")
      if( grepl( x = modORstring, "con-fidence") ) modORstring = gsub( x =modORstring, "con-fidence", "confidence")
      if( grepl( x = modORstring, "con fidence") ) modORstring = gsub( x =modORstring, "con fidence", "confidence")
      if( grepl( x = modORstring, "con- fidence") ) modORstring = gsub( x =modORstring, "con- fidence", "confidence")
      if( grepl( x = modORstring, "confi-dence") ) modORstring = gsub( x =modORstring, "confi-dence", "confidence")
      if( grepl( x = modORstring, "confi dence") ) modORstring = gsub( x =modORstring, "confi dence", "confidence")
      if( grepl( x = modORstring, "confi- dence") ) modORstring = gsub( x =modORstring, "confi- dence", "confidence")
      if( grepl( x = modORstring, "in-terval") ) modORstring = gsub( x =modORstring, "in-terval", "interval")
      if( grepl( x = modORstring, "in terval") ) modORstring = gsub( x =modORstring, "in terval", "interval")
      if( grepl( x = modORstring, "in- terval") ) modORstring = gsub( x =modORstring, "in- terval", "interval")
      if( grepl( x = modORstring, "inter-val") ) modORstring = gsub( x =modORstring, "inter-val", "interval")
      if( grepl( x = modORstring, "inter val") ) modORstring = gsub( x =modORstring, "inter val", "interval")
      if( grepl( x = modORstring, "inter- val") ) modORstring = gsub( x =modORstring, "inter- val", "interval")
      if( grepl( x = modORstring, "confidence interval") ) modORstring = gsub( x =modORstring, "confidence interval", "ci")
      if( grepl( x = modORstring, "confidence limit") ) modORstring = gsub( x =modORstring, "confidence limit", "ci")
      if( grepl( x = modORstring, "cl") ) modORstring = gsub( x =modORstring, "cl", "ci")
      if( grepl( x = modORstring, "cis") ) modORstring = gsub( x =modORstring, "cis", "ci")
      if( grepl( x = modORstring, "ci (ci)") ) modORstring = gsub( x =modORstring, "ci (ci)", "ci")
      if( grepl( x = modORstring, "%ci") ) modORstring = gsub( x =modORstring, "%ci", "% ci")
      if( grepl( x = modORstring, " %") ) modORstring = gsub( x =modORstring, " %", "%")
      if( grepl( x = modORstring, " and 95% ci") ) modORstring = gsub( x =modORstring, " and 95% ci", ",95% ci")
      if( grepl( x = modORstring, "ci 95%") ) modORstring = gsub( x =modORstring, "ci 95%", "95% ci")
      if( grepl( x = modORstring, "ci 95") ) modORstring = gsub( x =modORstring, "ci 95", "95% ci")
      if( grepl( x = modORstring, "ci95%") ) modORstring = gsub( x =modORstring, "ci95%", "95% ci")
      if( grepl( x = modORstring, "ci95") ) modORstring = gsub( x =modORstring, "ci95", "95% ci")
      if( grepl( x = modORstring, "95%, ci") ) modORstring = gsub( x =modORstring, "95%, ci", "95% ci")
      if( grepl( x = modORstring, "95% ci - ") ) modORstring = gsub( x =modORstring, "95% ci - ", "95% ci=")
      if( grepl( x = modORstring, "95 ci") ) modORstring = gsub( x =modORstring, "95 ci", "95% ci")
      if( grepl( x = modORstring, "95%-ci") ) modORstring = gsub( x =modORstring, "95%-ci", "95% ci")
      if( grepl( x = modORstring, "95ci") ) modORstring = gsub( x =modORstring, "95ci", "95% ci")
      if( grepl( x = modORstring, "95%ci") ) modORstring = gsub( x =modORstring, "95%ci", "95% ci")
      if( grepl( x = modORstring, "95%; ci") ) modORstring = gsub( x =modORstring, "95%; ci", "95% ci")
      if( grepl( x = modORstring, "95% ci \\(95% ci\\)") ) modORstring = gsub( x =modORstring, "95% ci \\(95% ci\\)", "95% ci")
      if( grepl( x = modORstring, "ci \\(95\\)") ) modORstring = gsub( x =modORstring, "ci \\(95\\)", "95% ci")
      if( grepl( x = modORstring, "ci \\(95%\\)") ) modORstring = gsub( x =modORstring, "ci \\(95%\\)", "95% ci")
      if( grepl( x = modORstring, "ci, 95%") ) modORstring = gsub( x =modORstring, "ci, 95%", "95% ci")
      if( grepl( x = modORstring, "ci:") ) modORstring = gsub( x =modORstring, "ci:", "ci=")
      if( grepl( x = modORstring, "ci :") ) modORstring = gsub( x =modORstring, "ci :", "ci=")
      if( grepl( x = modORstring, "ci\\(") ) modORstring = gsub( x =modORstring, "ci\\(", "ci=")
      if( grepl( x = modORstring, "ci,") ) modORstring = gsub( x =modORstring, "ci,", "ci=")
      if( grepl( x = modORstring, "ci;") ) modORstring = gsub( x =modORstring, "ci;", "ci=")
      if( grepl( x = modORstring, "ci ") ) modORstring = gsub( x =modORstring, "ci ", "ci=")
      if( grepl( x = modORstring, "ci=\\([[:alpha:]]{2}\\)") ) modORstring = gsub( x =modORstring, "ci=\\([[:alpha:]]{2}\\)", "ci=")
      if( grepl( x = modORstring, " \\(95%") ) modORstring = gsub( x =modORstring, " \\(95%", ", 95%")
      if( grepl( x = modORstring, "95% ci") ) modORstring = gsub( x =modORstring, "95% ci", ", 95% ci")
      if( grepl( x = modORstring, " \\(ci") ) modORstring = gsub( x =modORstring, " \\(ci", ", ci")
      if( grepl( x = modORstring, "(p )([[:digit:]])") ) modORstring = gsub( x =modORstring, "(p )([[:digit:]])", "p=\\2")
      if( grepl( x = modORstring, "(p )(\\.[[:digit:]])") ) modORstring = gsub( x =modORstring, "(p )(\\.[[:digit:]])", "p=\\2")
      if( grepl( x = modORstring, "(p)([[:digit:]])") ) modORstring = gsub( x =modORstring, "(p)([[:digit:]])", "p=\\2")
      if( grepl( x = modORstring, "(p)(\\.[[:digit:]])") ) modORstring = gsub( x =modORstring, "(p)(\\.[[:digit:]])", "p=\\2")
      if( grepl( x = modORstring, "\\) p=") ) modORstring = gsub( x =modORstring, "\\) p=", "), p=")
      if( grepl( x = modORstring, "\\)p=") ) modORstring = gsub( x =modORstring, "\\)p=", "), p=")
      if( grepl( x = modORstring, " and p=") ) modORstring = gsub( x =modORstring, " and p=", "p=")
      if( grepl( x = modORstring, "p-value") ) modORstring = gsub( x =modORstring, "p-value", "p")
      if( grepl( x = modORstring, "p value") ) modORstring = gsub( x =modORstring, "p value", "p")
      if( grepl( x = modORstring, "p\\(trend\\)") ) modORstring = gsub( x =modORstring, "p\\(trend\\)", "p")
      if( grepl( x = modORstring, "p for\\( interaction") ) modORstring = gsub( x =modORstring, "p for\\( interaction", "p")
      if( grepl( x = modORstring, " p=") ) modORstring = gsub( x =modORstring, "p=", ", p=")
      if( grepl( x = modORstring, "==") ) modORstring = gsub( x =modORstring, "==", "=")
      if( grepl( x = modORstring, ",,") ) modORstring = gsub( x =modORstring, ",,", ",")
      if( grepl( x = modORstring, ";,") ) modORstring = gsub( x =modORstring, ";,", ",")
      
      # if( multiple OR statments, split the string so the next OR in the main text so it can be found afterwards
      if( grepl( x = modORstring, " and or=") | 
          grepl( x = modORstring, "; or=") | 
          grepl( x = modORstring, " and rr=") |
          grepl( x = modORstring, "; rr=") |
          grepl( x = modORstring, " and hr=") |
          grepl( x = modORstring, "; hr=") ) {
        u <- t
        while( u < UB - 1 ) {
          if( sp[u] == "and" |
              substr(sp[u], nchar(sp[u]), nchar(sp[u])) == ";" ) {
            if( grepl( "^OR=", sp[u+1] ) |
                grepl( "^RR=", sp[u+1] ) |
                grepl( "^HR=", sp[u+1] ) ) {
              sp[u + 1] <- paste0("(", sp[u + 1])  # add paren so next OR can be found
              u <- UB
            }
          }
          u <- u + 1
        }                                                                        # since we've already established it's there,
        if( grepl( x = modORstring, " or=") ) sp2 <- strsplit(modORstring, " or=")[[1]]     # take the next OR statement out of this one
        if( grepl( x = modORstring, " rr=") ) sp2 <- strsplit(modORstring, " rr=")[[1]]     # take the next RR statement out of this one
        if( grepl( x = modORstring, " hr=") ) sp2 <- strsplit(modORstring, " hr=")[[1]]     # take the next HR statement out of this one
        modORstring <- paste0(sp2[1], ")")
      }
      
      u <- nchar(modORstring)
      for( v in 1:u ) {
        d <- substr(modORstring, v, v + 4)
        if( grepl( "0,[[:digit:]]{3}", d ) ) {         # leftmost zero indicates it's probably a decimal
          repl = gsub( x =d, ",", ".")
          modORstring = gsub( x =modORstring, d, repl)
        }
        if( grepl( "[[:digit:]],[[:digit:]]{3}", d ) ) {         # get rid of commas in thousands
          repl = gsub( x =d, ",", "")
          modORstring = gsub( x =modORstring, d, repl)
        }
        if( grepl( "[[:digit:]] ci=", d ) ) {         # no delimiter for( generic CI
          repl = gsub( x =d, " ci=", ", ci=")
          modORstring = gsub( x =modORstring, d, repl)
        }
        if( grepl( "[[:digit:]] 95%", d ) ) {         # no delimiter for( 95% CI
          repl = gsub( x =d, " 95%", ", 95%")
          modORstring = gsub( x =modORstring, d, repl)
        }
        if( grepl( "[[:digit:]],[[:digit:]]{2}[-,; \\)]", d ) ) {         # change European commas to decimals, based on delimiters
          modORstring <- gsub("([[:digit:]])(,)([[:digit:]]{2})([-,; \\)])", "\\1\\.\\3\\4", modORstring) #change numeric "," but not delimiter ","
        }
        d <- substr(modORstring, v, v + 3)
        if( grepl( "[[:digit:]],[[:digit:]][-,; \\)]", d ) ) {         # change European commas to decimals, based on delimiters
          modORstring <- gsub("([[:digit:]])(,)([[:digit:]])([-,; \\)])", "\\1\\.\\3\\4", modORstring) #change numeric "," but not delimiter ","
        }
        if( grepl( "[[:digit:]] p=", d ) ) {         # no delimiter
          repl = gsub( x =d, " p=", ", p=")
          modORstring = gsub( x =modORstring, d, repl)
        }
        if( grepl( "[[:digit:]] p<", d ) ) {         # no delimiter
          repl = gsub( x =d, " p<", ", p<")
          modORstring = gsub( x =modORstring, d, repl)
        }
      }
      
      modORstring <- gsub( x =modORstring, " ", "")
      modORstring <- paste0( modORstring, "|" )
      if( grepl( x = modORstring, ",") ) modORstring <- gsub( x =modORstring, ",", "|")
      if( grepl( x = modORstring, ":") ) modORstring <- gsub( x =modORstring, ":", "|")
      if( grepl( x = modORstring, ";") ) modORstring <- gsub( x =modORstring, ";", "|")
      while( grepl( x = modORstring, "\\|\\|") ) {
        modORstring = gsub( x =modORstring, "\\|\\|", "|")
      }
      if( grepl( x = modORstring, "\\(") ) modORstring <- gsub( x =modORstring, "\\(", "")
      if( grepl( x = modORstring, "\\)") ) modORstring <- gsub( x =modORstring, "\\)", "")
      if( grepl( x = modORstring, "95%ci=") ) modORstring <- gsub( x =modORstring, "95%ci=", "")
      if( grepl( x = modORstring, "ci=") ) modORstring <- gsub( x =modORstring, "ci=", "")
      if( grepl( x = modORstring, "\\.\\|" ) ) modORstring <- gsub( x =modORstring, "\\.\\|", "\\|")
      
      OddsR <- CIlower <- CIupper <- ComputedP <- NULL
      logoddsR <- logoddsL <- logoddsU <- Middle <- diff1 <- 0
      
      sp2 <- strsplit(modORstring, "\\|")[[1]]
      UB2 <- length(sp2)
      if( UB2 > 0 ) {
        for( v in 1:UB2 ) {
          if( grepl("^for", sp2[v]) ) { # these frequently intervene between OR CI pairs
            modORstring <- gsub( x =modORstring, paste0( "\\|", sp2[v] ), "")
            sp2 <- strsplit(modORstring, "\\|")[[1]]
            UB2 <- length(sp2)
          }
        }
        if( grepl( x = sp2[1], "per") &
            !grepl( x = sp2[1], "upper") ) {         # frequently the OR will be expressed in terms of per X
          NewWord <- strsplit(sp2[1], "per")[[1]][1]
          modORstring <- gsub( x =modORstring, sp2[1], NewWord)
          sp2[1] <- NewWord
        }
        if( grepl("^ratio=", sp2[1]) ) {
          OddsR <- gsub( x =sp2[1], "ratio=", "") # First array element is reportable item (renamed 'ratio')
          OddsR <- str_extract(OddsR, "[[:digit:]]*\\.[[:digit:]]+")
          if( !is.na(OddsR)) {
            modORstring <- gsub( x =modORstring, sp2[1], paste0("ratio=", OddsR) )
            sp2[1] <- paste0("ratio=", OddsR)
          }
          if( is.na(OddsR) & UB2 > 3 ) {
            a <- 2
            while( a <= UB2 ) {
              OddsR <- str_extract_all(sp2[a], "[[:digit:]]*\\.[[:digit:]]+")[[1]]
              if(length(OddsR) == 1) { ## found odds ratio; now modify modORstring and sp2
                firsthalf <- paste0( "ratio=", OddsR, collapse = "" )
                secondhalf <- paste0( sp2[-(1:a)], collapse = "|" )
                modORstring <- paste0( firsthalf, "|", secondhalf)
                sp2 <- strsplit(modORstring, "\\|")[[1]]
                a <- UB2  
              }
              if(length(OddsR) > 1) {
                OddsR <- NA
                a <- UB2
              } 
              a <- a + 1
            }
          }
        }
      }
      if( UB2 == 2 ) {                      # second might be CI
        if( grepl( x = sp2[2], "-")  ) {  # the lower and upper are divided by a "-" sign
          sp3 <- strsplit(sp2[2], "-")[[1]]
          CIlower <- sp3[1]
          CIupper <- sp3[2]
        }
      }
      if( UB2 >= 3 ) {   # the lower and upper are divided by some other sign (e.g., comma or sem)
        if( !is.na( suppressWarnings( as.numeric( sp2[2] ) ) ) &
            !is.na( suppressWarnings( as.numeric( sp2[3] ) ) ) ) {   # if( the 2 values are numbers, ) {likely the delimiter was not a "-"
          CIlower <- sp2[2]
          CIupper <- sp2[3]
        } else {
          if( grepl( x = sp2[2], "-") ) {  # if( "-" is in the 2nd field, ) {the final value is likely a p-value
            sp3 <- strsplit(sp2[2], "-")[[1]]
            CIlower <- sp3[1]
            CIupper <- sp3[2]
            if( !grepl( "[[:digit:]]*\\.[[:digit:]]+", CIlower ) ) CIlower <- NA
            if( !grepl( "[[:digit:]]*\\.[[:digit:]]+", CIupper ) ) CIupper <- NA
            if( is.na(suppressWarnings(as.numeric(CIlower) ) ) ) CIlower <- str_extract( CIlower, "[[:digit:]]*\\.[[:digit:]]+")
            if( is.na(suppressWarnings(as.numeric(CIupper) ) ) ) CIupper <- str_extract( CIupper, "[[:digit:]]*\\.[[:digit:]]+")
            if( is.na( CIupper ) ) CIupper <- NULL
            if( is.na( CIlower ) ) CIlower <- NULL
          }
        }
        if( length( CIupper ) == 0 & length( CIlower ) == 0 ) {
          a <- 3
          while( a <= UB2 ) {
            cis <- str_extract_all(sp2[a], "[[:digit:]]*\\.[[:digit:]]+")[[1]]
            if(length(cis) == 2) { ## found confidence interval; now modify modORstring and sp2
              first <- sp2[1]
              second <- sp2[a]
              third <-  paste0( sp2[-(1:a)], collapse = "|" )
              modORstring <- paste0( first, "|", second, "|", third)
              sp2 <- strsplit(modORstring, "\\|")[[1]]
              CIlower <- cis[1]
              CIupper <- cis[2]
              a <- UB2  
            }
            if(length(OddsR) > 1) {
              OddsR <- NA
              a <- UB2
            } 
            a <- a + 1
          }
        }
      }
      ReportedNum <- OddsR #this step is added here, becasue only like this trailing zero's are kept
      repCIlow <- CIlower #idem
      repCIupp <- CIupper #idem
      OddsR <- suppressWarnings( as.numeric(OddsR) )
      CIlower <- suppressWarnings( as.numeric(CIlower) )
      CIupper <- suppressWarnings( as.numeric(CIupper) )
      if( length(CIlower) > 0 & length(CIupper) > 0 & length(OddsR) > 0 ) {
        if( !is.na(CIlower) & !is.na(CIupper) & !is.na(OddsR) ) {
          if( CIlower > CIupper ) {
            OddsR <- CIlower <- CIupper <- NA  # benefit of doubt: wrongly extracted
          } else if( OddsR > CIupper ) {
            OddsR <- CIlower <- CIupper <- NA  # benefit of doubt: wrongly extracted
          } else if( OddsR < CIlower ) {
            OddsR <- CIlower <- CIupper <- NA  # benefit of doubt: wrongly extracted
          } 
        }
      }
      pval <- NULL
      for( v in 2:UB2 ) {
        #        # NOTE: need to refine p-value extraction
        if( grepl( x = sp2[v], "p<") |
            grepl( x = sp2[v], "p=") |
            grepl( x = sp2[v], "p>") ) {
          if( is.null(pval) ) pval <- sp2[v]       # Take the 1st p-value only in case there are multiple
          # sometimes the p-value comes second, so if( no CI limits have been found yet ) {check that possibility
          if( is.null(CIlower) & is.null(CIupper) & v == 2 & UB2 >= 3 ) {
            if( grepl( x = sp2[3], "-") > 0 ) {  # if( "-" is in the next field, ) {split
              sp3 = Split(sp2[3], "-")
              CIlower = sp3[1]
              CIupper = sp3[2]
            } else {
              if( UB2 >= 4 ) {              # } else {, if( a 3rd field is present, ) {use lower=2nd, upper=3rd
                CIlower = sp2[3]
                CIupper = sp2[4]
              }
            }
          }
        }
      }
      if( !is.null(pval) ) {        # strip extra chars off p-val (if( any)
        if( !grepl("^p=", pval) &
            !grepl("^p<", pval) &
            !grepl("^p>", pval ) ) { # if pval does not start with p=, p<, or p>, then remove characters before
          if( grepl( x = pval, "p=") ) {
            pp <- strsplit(pval, "p=" )[[1]]
            if(length(pp) == 2) pval <- paste0("p=", pp[2])
          } 
          if( grepl( x = pval, "p>") ) {
            pp <- strsplit(pval, "p>")[[1]]
            if(length(pp) == 2) pval <- paste0("p>", pp[2])
          } 
          if( grepl( x = pval, "p<") ) { 
            pp <- strsplit(pval, "p<")[[1]]
            if(length(pp) == 2) pval <- paste0("p<", pp[2])
          } 
        }
        pval <- gsub( x =pval, " ", "")
        if( grepl( x = pval, "p=10\\(-") ) pval <- gsub( x =pval, "p=10\\(-", "p=1x10(-")
        pval <- gsub( x =pval, "\\(", "")
        pval <- gsub( x =pval, "\\)", "")
        pval <- gsub( x =pval, "/", "")
        pval <- gsub( x =pval, "`", "")
        pval <- gsub( x =pval, "\\*", "")
        if( grepl( x = pval, "\\*10") ) pval = gsub( x =pval, "\\*10", "x10")
        if( grepl( x = pval, "x10") ) pval = gsub( x =pval, "x10", "E")# convert exponent statements (e.g., 1.2x10(-4) to 1.2E-4)
        if( grepl( x = pval, "e-") ) pval = gsub( x =pval, "e-", "E-") # uppercase will save it from the trimming routine below
        if( !grepl( x = pval, "\\.") & grepl( x = pval, ",") ) pval = gsub( x =pval, ",", ".")  # if( there is no decimal but a comma, ) {it may be European convention
        
        if( grepl( x = pval, "p=\\.") ) pval = gsub( x =pval, "p=.", "p=0.")
        if( grepl( x = pval, "p<\\.") ) pval = gsub( x =pval, "p<.", "p<0.")
        if( grepl( x = pval, "p<=\\.") ) pval = gsub( x =pval, "p<=.", "p<=0.")
        if( grepl( x = pval, "p>\\.") ) pval = gsub( x =pval, "p>.", "p>0.")
        lenp = nchar(pval)
        psplit <- strsplit(pval, "|")[[1]]
        for( v in 5:lenp ) {
          if( !grepl("[[:digit:]]", psplit[v] ) ) {
            pval <- paste0( psplit[1:(v-1)], collapse = "") 
            break()
          }
        }
      }
      if( !is.numeric(OddsR) ) OddsR <- NA   # check to make sure the value has been isolated without extraneous characters
      if( !is.numeric(CIlower) ) CIlower <- NA
      if( !is.numeric(CIupper) ) {
        if( !grepl( x = CIupper, "%") & 
            !grepl( x = CIupper, "&") ) {       # % and & give VAL error
          CIupper <- as.numeric(str_extract(CIupper, "[[:digit:]]\\.[[:digit:]]+")[[1]])
          if(is.na(CIupper)) CIupper <- NA
        } else {
          CIupper <- NA
        }
      }
      ErrFlag  <- NULL
      if( length(OddsR) == 0 ) OddsR <- NA
      if( length(CIlower) == 0 ) CIlower <- NA
      if( length(CIupper) == 0 ) CIupper <- NA
      if( !is.na(OddsR) & OddsR <= 0 ) OddsR  <- NA
      if( !is.na(CIlower) & CIlower < 0 ) CIlower  <- NA    # Lower CI might be zero
      if( !is.na(CIupper) & CIupper <= 0 ) CIupper  <- NA
      if( !is.na(CIlower) & CIlower < 0 ) ErrFlag <- paste0( ErrFlag, "Neg CI lower")
      if( !is.na(CIupper) & CIupper < 0 ) ErrFlag <- paste0( ErrFlag, "Neg CI upper")
      ProcessedFlag = 0
      if( !is.na( OddsR ) & 
          !is.na( CIlower ) & 
          !is.na( CIupper ) ) {       # flag potential errors
        if( CIupper < CIlower ) ErrFlag <- paste0( ErrFlag, "L>U " )
        if( CIlower == 0 ) ErrFlag <- paste0( ErrFlag, "CI(L)=0 " )
        
        logoddsR <- log(OddsR)
        if( CIlower > 0 ) {
          logoddsL <- log(CIlower)
        } else {
          logoddsL <- 0 
        }
        logoddsU <- log(CIupper)
        Middle <- (logoddsU + logoddsL) / 2
        # round based on sig figs and check and see if( it's better to round up or down
        # ReportedNum <- OddsR # moved up ~100 lines to keep trailing 0
        if( Middle < 15 ) {           # huge numbers will cause overflow
          CalcNum <- exp(Middle)
          if( CalcNum > 0 ) CalcNum <- Choose_round_up_or_down(ReportedNum, CalcNum)
          OddsRdiff <- OddsR
          diff1 <- OddsRdiff - CalcNum
          maxinterval <- Calculate_maximum_interval(repCIlow, repCIupp, ReportedNum)
          LowestOR <- maxinterval[1]
          HighestOR <- maxinterval[2]
          MinDiffOR <- maxinterval[3]
          if( diff1 == 0 & MinDiffOR > 0 ) {
            MinDiffOR <- 0
          }
          
          # new lines to recompute pvalue, if pvalue is present in ORstring
          if( length(pval) > 0 ) {
            if( !is.na(pval) ) {
              if(pval == "p<0.00") pval <- "p<0.005"
              if(pval == "p<0.000") pval <- "p<0.0005"
              if(pval == "p<0.0000") pval <- "p<0.00005"
              if(pval == "p<0.00000") pval <- "p<0.000005"
              if(pval == "p<0.000000") pval <- "p<0.0000005"
              p <- recompute_p(pval, repCIupp, repCIlow)
              pnum <- p[ 'pnum' ]
              psign <- p[ 'psign' ]
              ComputedP <- p['ComputedP']
              LowestP <- p['LowestP']
              HighestP <- p['HighestP']
              MinDiffP <- p['MinDiffP']
              pError <- p['pError']
              decisionError <- p['decisionError']
              sigError <- p['sigError']
            } else {
              pval <- pnum <- psign <- ComputedP <- LowestP <- HighestP <- MinDiffP <- pError <- decisionError <- sigError <- NA
            }
          } else {
            pval <- pnum <- psign <- ComputedP <- LowestP <- HighestP  <- MinDiffP <- pError <- decisionError <- sigError <- NA
          }
          
          ProcessedFlag = 1
        } else {
          ErrFlag <- paste0( ErrFlag, "too big " )
        }
      } else {
        CalcNum <- ComputedP <- pnum <- psign <- LowestP <- HighestP  <- MinDiffP <- pError <- decisionError <- sigError <- LowestOR <- HighestOR <- MinDiffOR <- NA
      }
      
      if( ORflag == 1 ) {                        # Restore former statement for reporting purposes
        ORstring <- gsub( x =ORstring, "\\(RATIO=", "(OR=")
        modORstring <- gsub( x =modORstring, "ratio=", "or=")
      }
      if( RRflag == 1 ) {
        ORstring <- gsub( x =ORstring, "\\(RATIO=", "(RR=")
        modORstring <- gsub( x =modORstring, "ratio=", "rr=")
      }
      if( HRflag == 1 ) {
        ORstring <- gsub( x =ORstring, "\\(RATIO=", "(HR=")
        modORstring <- gsub( x =modORstring, "ratio=", "hr=")
      }
      #    if( ProcessedFlag == 1 ) {     # Was able to extract all parameters
      #      print( c( "ORstring", ORstring, "modORstring", modORstring, "pval", pval, "CIlower", CIlower, "OddsR", OddsR, "CIupper", CIupper, "LowestOR", LowestOR, "CalcNum", CalcNum, "HighestOR", HighestOR, "MinDiffOR", MinDiffOR, "Errflag", ErrFlag, "Context", Context) )
      #    } else {                           # did not get all parameters - output it to figure out why
      #      print( c( ORstring, modORstring, pval, "", "", "", "", "", "", "", ErrFlag, Context ) )
      #    }
      if( is.null(ErrFlag) ) ErrFlag <- NA
      if( is.null(modORstring) ) modORstring <- NA
      if( is.null(pval) ) pval <- NA
      if( is.null(psign) ) psign <- NA
      if( is.null(ComputedP) ) ComputedP <- NA
      if( is.null(MinDiffP) ) MinDiffP <- NA
      if( is.null(pError) ) pError <- NA
      if( is.null(decisionError) ) decisionError <- NA
      if( is.null(sigError) ) sigError <- NA
      if( is.null(OddsR) ) OddsR <- NA
      if( is.null(CIlower) ) CIlower <- NA
      if( is.null(CIupper) ) CIupper <- NA
      if( is.null(CalcNum) ) CalcNum <- NA
      if( is.null(LowestOR) ) LowestOR <- NA
      if( is.null(HighestOR) ) HighestOR <- NA
      if( is.null(MinDiffOR) ) MinDiffOR <- NA
      
      df1 <- data.frame( ORstring = ORstring,
                         modORstring = modORstring,
                         OddsR = OddsR,
                         CIlower = CIlower,
                         CIupper = CIupper,
                         ComputedOR = CalcNum,
                         LowestOR = LowestOR,
                         HighestOR = HighestOR,
                         MinDiffOR = MinDiffOR,
                         pval = pval,
                         psign = psign,
                         ComputedP = ComputedP,
                         LowestP = LowestP,
                         HighestP = HighestP,
                         MinDiffP = MinDiffP,
                         pError = pError,
                         decisionError = decisionError,
                         sigError = sigError,
                         ErrFlag = ErrFlag,
                         Context = Context,
                         stringsAsFactors = F)
      dfout <- rbind(dfout, df1)
      ExamineFlag <- 0
    }
  } 
  return(dfout)
}


###
# determine how many significant figures (including trailing zeros) are mentioned in the p-value
## 
# CAVEAT: when ReportedNum is numeric and ends with a trailing zero (e.g. 3.430 or 0.030),
# the conversion to character will loose that zero (rendering "3.43" and "0.03"). Therefore,
# it is crucial that a character string, not a numeric string is entered to extract the correct
# number of decimals and significant figures.
###
Calc_Sig_Figs <- function(ReportedNum){
  
  if( grepl( "\\.", ReportedNum ) ) {  
    sp <- strsplit(as.character(ReportedNum), "\\.")[[1]]
    if( sp[1] == "0" | sp[1] == "" ) {     #if less than one, find out where the #s begin (e.g., 0.0054 has two sig figs, 0.00540 has three)
      nreduce <- sp[2]
      for( t in 1:nchar(sp[2]) ) {
        if( grepl("^0", nreduce) ) {
          nreduce <- sub( "^0", "", nreduce)
          
        } else {
          sigfigs <- nchar(nreduce)
        }
      }
    } else {                   #is > 1 so add both sides
      sigfigs <- nchar(sp[1]) + nchar(sp[2])
    }
  } else {
    sigfigs <- nchar(ReportedNum)
  }
  
  return(sigfigs)
} 


###
# determine how many digits are mentioned in the reported number
## 
Calc_Decimals <- function(ReportedNum) {
  
  if( grepl( "\\.", ReportedNum ) ) {  
    sp <- strsplit(as.character(ReportedNum), "\\.")[[1]]
    digits <- nchar(sp[2])
  } else {
    digits <- nchar(ReportedNum)
  }
  
  return(digits)
} 

###
# 'Based on the # of decimals and sig figs to the right of the decimal in the reported OR, try rounding up and down
#and go with whichever is closer
##  
Choose_round_up_or_down <- function(ReportedNum, CalcNum) {
  
  decimals <- Calc_Decimals(ReportedNum)
  sigfigs <- Calc_Sig_Figs(ReportedNum)       #get sig figs for the reported OR/RR
  
  #if(sigfigs > decimals) fctr <- (10 ^(sigfigs - decimals))  # in case of numbers above 1
  #if(sigfigs <= decimals) fctr <- (10 ^(decimals))          # in case of numbers below 0
  fctr <- 10^(decimals)
  prepared <- CalcNum * fctr
  RoundedUp <- ceiling( prepared ) / fctr
  RoundedDown <- floor( prepared ) / fctr
  
  if( abs(RoundedUp - as.numeric(ReportedNum)) > abs(RoundedDown - as.numeric(ReportedNum)) ){
    CalcNum <- RoundedDown
  } else {
    CalcNum <- RoundedUp
  }
  return(CalcNum)
}


###
# Assuming that the actual interval range might be slightly different due to rounding 
# (e.g., was really 1.15 but was reported as 1.2),
# shift both CI to the lowest possible and the highest possible, then recalulate ORs
## 
Calculate_maximum_interval <- function(repCIlow, repCIupp, ReportedNum) {
  
  if( repCIlow != "" &
      repCIupp != "" &
      ReportedNum != "" &
      !is.na( repCIlow ) &
      !is.na( repCIupp ) &
      !is.na( ReportedNum ) ) {
    
    #The # of decimals SHOULD be the same for both intervals, but just in case
    if( grepl( "\\.", repCIlow ) ) {
      sp <- strsplit(repCIlow, "\\.")[[1]]
      ND1 <- nchar(sp[2])
    } else {
      ND1 <- 0
    }
    if( grepl( "\\.", repCIupp ) ) {
      sp <- strsplit(repCIupp, "\\.")[[1]]
      ND2 = nchar(sp[2])
    } else {
      ND2 = 0
    }
    if( ND1 >= ND2 ) { 
      MaxND <- ND1
    } else {
      MaxND <- ND2
    }
    # add ND0, for ReportedNum
    if( grepl( "\\.", ReportedNum ) ) {
      sp <- strsplit(ReportedNum, "\\.")[[1]]
      ND0 = nchar(sp[2])
    } else {
      ND0 = 0
    }
    if(ND0 < MaxND) MaxND <- ND0
    #calculate lowest possible OR (due to rounding)
    RoundedDownL <- as.numeric(repCIlow) - 0.5 * 10 ^ (ND1 * -1)
    RoundedDownU <- as.numeric(repCIupp) - 0.5 * 10 ^ (ND2 * -1)
    if( RoundedDownL > 0 ) { 
      LOR <- (log(RoundedDownL) + log(RoundedDownU) ) / 2
      LOR <- exp(LOR)
      LOR <- floor(LOR * 10^(MaxND)) / (10^(MaxND) ) # round down to give max benefit of doubt
      LowestOR <- as.character(LOR)
    } else {
      LowestOR <- "0"
      LOR <- NA
    }
    
    #calculate highest possible OR (due to rounding)
    RoundedUpL <- as.numeric(repCIlow) + 0.5 * 10 ^ (ND1 * -1)
    RoundedUpU <- as.numeric(repCIupp) + 0.5 * 10 ^ (ND2 * -1)
    if( RoundedUpL > 0 ) { 
      UOR = (log(RoundedUpL) + log(RoundedUpU) ) / 2
      UOR = exp(UOR)
      UOR <- ceiling(UOR * 10^(MaxND)) / (10^(MaxND) ) #round up to give max benefit of doubt
      HighestOR <- as.character(UOR)
    } else {
      HighestOR = "0"
      UOR <- NA
    }
    
    ReportedOR <- as.numeric(ReportedNum)
    if( !is.na(LOR) & !is.na(UOR) ) {
      
      if( ReportedOR >= LOR & ReportedOR <= UOR ) { 
        MinDiffOR <- 0
      } else {
        if( abs(ReportedOR - LOR) <= abs(ReportedOR - UOR) ) { 
          MinDiffOR <- abs(ReportedOR - LOR)
        } else {
          MinDiffOR <- abs(ReportedOR - UOR)
        }
      }
    } else {
      MinDiffOR <- NA
    }
  } else {
    LowestOR <- HighestOR <- MinDiffOR <- NA
  }

  
  return(list=c(LowestOR, HighestOR, MinDiffOR))
}



###
# recompute p value based on gaussian distribution; determine whether it is
# similar to reported p-value
##
recompute_p <- function(pval, repCIupp, repCIlow) {
  
  #The # of decimals SHOULD be the same for both intervals, but just in case
  if( grepl( "\\.", repCIlow ) ) {
    sp <- strsplit(repCIlow, "\\.")[[1]]
    ND1 <- nchar(sp[2])
  } else {
    ND1 <- 0
  }
  if( grepl( "\\.", repCIupp ) ) {
    sp <- strsplit(repCIupp, "\\.")[[1]]
    ND2 = nchar(sp[2])
  } else {
    ND2 = 0
  }
  if( ND1 >= ND2 ) { 
    MaxND <- ND1
  } else {
    MaxND <- ND2
  }
  
  # round logoddsU down and up to allow for rounding errors in computing p-value
  RoundedDownL <- as.numeric(repCIlow) - 0.5 * 10 ^ (ND1 * -1)
  RoundedDownU <- as.numeric(repCIupp) - 0.5 * 10 ^ (ND2 * -1)
  RoundedUpL <- as.numeric(repCIlow) + 0.5 * 10 ^ (ND1 * -1)
  RoundedUpU <- as.numeric(repCIupp) + 0.5 * 10 ^ (ND2 * -1)
  
  # compute log Odds for reported, rounded down and rounded up
  logoddsU <- log(as.numeric(repCIupp))
  logoddsL <- log(as.numeric(repCIlow))
  
  logoddsU_RD <- log(as.numeric(RoundedDownU))
  logoddsL_RD <- log(as.numeric(RoundedDownL))
  
  logoddsU_RU <- log(as.numeric(RoundedUpU))
  logoddsL_RU <- log(as.numeric(RoundedUpL))
  
  # compute p-values for reported, rounded down and rounded up
  ComputedP <- 2 * pnorm(
    - (1.96*abs(logoddsU + logoddsL)) / 
      (logoddsU - logoddsL)
  )
  ComputedP_small <- 2 * pnorm(             # lower limit rounded up, upper limit rounded down
    - (1.96*abs(logoddsU_RD + logoddsL_RU)) / 
      (logoddsU_RD - logoddsL_RU)
  )
  ComputedP_wide <- 2 * pnorm(             # lower limit rounded down, upper limit rounded up
    - (1.96*abs(logoddsU_RU + logoddsL_RD)) / 
      (logoddsU_RU - logoddsL_RD)
  )
  pnum <- str_extract(pval, "[[:digit:]]\\.[[:digit:]]+")
  pDec <- Calc_Decimals(pnum)
  pnum <- as.numeric(pnum)
  psign <- str_extract(pval, "[=<>]")
  
  if( !is.na( pnum ) ) {
    # determine if there is a difference between reported and computed p, depending on comparison
    if( psign == "=" ) {
      MinDiffP <- abs(pnum - ComputedP)
      MinDiffP_small <- abs(pnum - ComputedP_small)
      MinDiffP_wide <- abs(pnum - ComputedP_wide)
    }
    if( psign == "<" ) {
      if( ComputedP < pnum ) {
        MinDiffP <- 0
      }
      if( ComputedP >= pnum ) {
        MinDiffP <- 1
      }
      if( ComputedP_small < pnum ) {
        MinDiffP_small <- 0
      }
      if( ComputedP_small >= pnum ) {
        MinDiffP_small <- 1
      }
      if( ComputedP_wide < pnum ) {
        MinDiffP_wide <- 0
      }
      if( ComputedP_wide >= pnum ) {
        MinDiffP_wide <- 1
      }
    }
    if( psign == ">" ) {
      if( ComputedP > pnum ) {
        MinDiffP <- 0
      }
      if( ComputedP <= pnum ) {
        MinDiffP <- NA
      }
      if( ComputedP_small > pnum ) {
        MinDiffP_small <- 0
      }
      if( ComputedP_small <= pnum ) {
        MinDiffP_small <- NA
      }
      if( ComputedP_wide > pnum ) {
        MinDiffP_wide <- 0
      }
      if( ComputedP_wide <= pnum ) {
        MinDiffP_wide <- NA
      }
    }
    
    if( !is.na(pDec) ) {
      # round computedP and mindiffP to correct length
      ComputedP <- formatC( round(ComputedP, pDec), pDec, format = "f")
      ComputedP_small <- formatC( round(ComputedP_small, pDec), pDec, format = "f")
      ComputedP_wide <- formatC( round(ComputedP_wide, pDec), pDec, format = "f")
      MinDiffP <- formatC( round(MinDiffP, pDec), pDec, format = "f")
      MinDiffP_small <- formatC( round(MinDiffP_small, pDec), pDec, format = "f")
      MinDiffP_wide <- formatC( round(MinDiffP_wide, pDec), pDec, format = "f")
    }
    
    # determine the range of possible p-values due to rounding errors in the CI
    LowestP <- pmin(ComputedP_small, ComputedP_wide)
    HighestP <- pmax(ComputedP_small, ComputedP_wide)
    
    # determine whether p-value was reported correctly with the version (normal, small, or wide)
    # which is closest to reported p-value. If reported P is within the range of computed, all is fine.
    if( pnum > LowestP & pnum < HighestP ) {
      MinDiffP <- 0
      pError <- F
      decisionError <- F
      sigError <- F
    } else {
      
      if( as.numeric(MinDiffP) <= as.numeric(MinDiffP_small) &
          as.numeric(MinDiffP) <= as.numeric(MinDiffP_wide) ) {
        pError <- ErrorTest(as.numeric(ComputedP), pnum, psign, pDec)
        decisionError <- DecisionErrorTest(as.numeric(ComputedP), pnum, psign, pDec)
        sigError <- sigErrorTest(as.numeric(ComputedP), pnum, psign, pDec)
      }
      if( as.numeric(MinDiffP_small) < as.numeric(MinDiffP) &
          as.numeric(MinDiffP_small) < as.numeric(MinDiffP_wide) ) {
        pError <- ErrorTest(as.numeric(ComputedP_small), pnum, psign, pDec)
        decisionError <- DecisionErrorTest(ComputedP = as.numeric(ComputedP_small), pnum, pDec, psign)
        sigError <- sigErrorTest(ComputedP = as.numeric(ComputedP_small), pnum, pDec, psign)
        MinDiffP <- MinDiffP_small
      }
      if( as.numeric(MinDiffP_wide) < as.numeric(MinDiffP) &
          as.numeric(MinDiffP_wide) < as.numeric(MinDiffP_small) ) {
        pError <- ErrorTest(as.numeric(ComputedP_wide), pnum, psign, pDec)
        decisionError <- DecisionErrorTest(as.numeric(ComputedP_wide), pnum, psign, pDec)
        sigError <- sigErrorTest(as.numeric(ComputedP_wide), pnum, psign, pDec)
        MinDiffP <- MinDiffP_wide
      }
    }
  } else {
    MinDiffP <- LowestP <- HighestP <- pError <- decisionError <- sigError <- NA
  }
  
  
  return(list=c(pnum = pnum,
                psign = psign,
                ComputedP = ComputedP,
                LowestP = LowestP,
                HighestP = HighestP,
                MinDiffP = MinDiffP,
                pError = pError,
                decisionError = decisionError,
                sigError = sigError))
}


###
# determine if recomputed P is different from reported P
##
ErrorTest <- function(ComputedP, pnum, psign, pDec, alpha = 0.05) {
  computed <- as.numeric(ComputedP)
  comparison <- as.vector(psign)
  reported <- as.numeric(pnum)
  pDec <- as.numeric(pDec)
  #  testcomp <- as.vector(x$Test.Comparison)   # remove all references to comparison, only work with computed
  reported[comparison == "ns"] <- alpha
  comparison[comparison == "ns"] <- ">"
  Match <- paste(computed, comparison, reported) 
  InExTests <- grepl("<|>", Match)
  if (any(InExTests)) {
    InExTests[InExTests] <- sapply(Match[InExTests], 
                                   function(m) !eval(parse(text = m)))
  }
  small <- comparison == "<" 
  great <- comparison == ">" 
  equal <- comparison == "="
  if (any(small)) {
    InExTests[small] <- 
      !(round(computed[small], pDec[small]) <= 
      round(reported[small], pDec[small]))
  }
  if (any(great)) {
    InExTests[great] <- 
      !(round(computed[great], pDec[great]) >= 
      round(reported[great], pDec[great]))
  }
  if (any(equal)) {
    equal[equal] <- !(round(computed[equal], pDec[equal]) == 
                        round(reported[equal], pDec[equal]))
  }
  Error <- !(InExTests == FALSE & equal == FALSE)
  return(Error)
}


###
# determine if recomputed P is different from reported P, so that it passes the
# 0.05 threshold
##
DecisionErrorTest <- function(ComputedP, pnum, pDec, psign, alpha = 0.05, pEqualAlphaSig = TRUE) {
  computed <- ComputedP
  comparison <- psign
  reported <- pnum
  #  testcomp <- as.vector(psign)
  reported[comparison == "ns"] <- alpha
  comparison[comparison == "ns"] <- ">"
  small <- comparison == "<"
  great <- comparison == ">"
  equal <- comparison == "="
  
  AllTests <- grepl("=|<|>", comparison)
  if (any(AllTests)) {
    if (pEqualAlphaSig) {
      AllTests[equal] <- (reported[equal] <= alpha & computed[equal] > alpha) | 
        (reported[equal] > alpha & computed[equal] <= alpha)
      
      AllTests[small] <- reported[small] <= 
        alpha & computed[small] >= alpha
      
      AllTests[great] <- reported[great] >= 
        alpha & computed[great] <= alpha
    }
    else {
      AllTests[equal] <- (reported[equal] < alpha & computed[equal] >= alpha) | 
        (reported[equal] >= alpha & computed[equal] < alpha)
      
      AllTests[small] <- reported[small] <= 
        alpha & computed[small] >= alpha
      
      AllTests[great] <- reported[great] >= 
        alpha & computed[great] < alpha
    }
  }
  AllTests <- as.logical(AllTests)
  return(AllTests)
}

###
# determine if recomputed P is different from reported P, so that it passes the
# 0.05 threshold and was reported as a significant value
##
sigErrorTest <- function(ComputedP, pnum, pDec, psign, alpha = 0.05, pEqualAlphaSig = TRUE) {
  computed <- ComputedP
  comparison <- psign
  reported <- pnum
  #  testcomp <- as.vector(psign)
  reported[comparison == "ns"] <- alpha
  comparison[comparison == "ns"] <- ">"
  small <- comparison == "<"
  great <- comparison == ">"
  equal <- comparison == "="
  
  AllTests <- grepl("=|<|>", comparison)
  if (any(AllTests)) {
    if (pEqualAlphaSig) {
      AllTests[equal] <- (reported[equal] <= alpha & computed[equal] > alpha) 
      
      AllTests[small] <- reported[small] <= 
        alpha & computed[small] >= alpha
      
      AllTests[great] <- FALSE # reported is greater than alpha, so did not report significant result
    }
    else {
      AllTests <- NA # if no psign is given, unknown
    }
  }
  AllTests <- as.logical(AllTests)
  return(AllTests)
}


##################################
# END FUNCTIONS
##################################


' for testing separate docs
intxt <- readLines("U:/data/grobid/xmlout/15309242.xml")
intxt <- paste( intxt, collapse = " " )
intxt <- gsub( "<ref[^>]+>", " ", intxt )
intxt <- gsub( "</ref>", " ", intxt )
intxt <- gsub( "<[^>]+>", " ;", intxt )
intxt <- gsub( "\t", " ", intxt)
txt2 <- readLines("U:/data/pdf/text/15309242.txt")
txt2 <- paste( txt2, collapse = " " )
'


filez1 <- list.files( "/mnt/data/live02/stress/hlamberink/grobid/xmlout" )
filez2 <- list.files( "/mnt/data/live02/stress/hlamberink/grobid/xmloutNEW" )
filez3 <- list.files( "/mnt/data/live02/stress/hlamberink/scopus/xmlGrobid" )

print(length(filez1))
print(length(filez2))
print(length(filez3))

# prepare vehicles
ratio_checked <- OR_checked <- NULL

# loop over all files
for( j in 1:3 ) {
#for( j in 1:1 ) {
  print(paste("starting round j =", j))
  
  if( j == 1 ) {
    filez <- filez1
    pathbase <- "/mnt/data/live02/stress/hlamberink/grobid/xmlout/"
    pathbase2 <- "/mnt/data/live02/stress/hlamberink/pdf/text/"
  }
  if( j == 2 ) {
    filez <- filez2
    pathbase <- "/mnt/data/live02/stress/hlamberink/grobid/xmloutNEW/"
    pathbase2 <- "/mnt/data/live02/stress/hlamberink/pdf/text/"
  }
  if( j == 3 ) {
    filez <- filez3
    pathbase <- "/mnt/data/live02/stress/hlamberink/scopus/xmlGrobid/"
    pathbase2 <- "/mnt/data/live02/stress/hlamberink/scopus/text/"
  }
  
  
  for( i in 1:length(filez)) {
  #for( i in 1:100 ) {
  
    
    # update progressbar
    #setTxtProgressBar( progbar, i )
    if( i %in% seq(1, 200000, 100) ) print( paste( "progress round", j, ":", round(100* i/length(filez), 1 ), "% at", Sys.time() ) )
    
    pmid <- gsub( ".xml", "", filez[i])
    
    # read Grobid xml text, collapse to one character string, and remove all xml tags
    intxt <- suppressWarnings( readLines( paste0( pathbase, filez[i] ) ) )
    intxt <- paste( intxt, collapse = " " )
    intxt <- gsub( "<ref[^>]+>", " ", intxt )
    intxt <- gsub( "</ref>", " ", intxt )
    intxt <- gsub( "<[^>]+>", " ;", intxt )
    intxt <- gsub( "\t", " ", intxt)
    
    txt2 <- suppressWarnings( readLines( paste0( pathbase2, pmid, ".txt" ) ) )
    txt2 <- paste( txt2, collapse = " " )

    # sometimes, the xml file or text file is corrupt with little characters; in this
    # case, use only the alternative file, otherwise use both
    use <- "both"
    if( nchar(intxt) > 10*nchar(txt2) ) use <- "intxt"
    if( nchar(txt2) > 10*nchar(intxt) ) use <- "txt2"
    
    errs_ratio_xml <- errs_ratio_txt <- errs_ors_xml <- errs_ors_txt <- 0
    
    #CHECK CORRECTNESS OF RATIOS WITH PERCENTAGES
    if( use == "both" ) {
      ratios <- tryCatch( ratioCheck(intxt), 
                          error = function(e) {
                            print(paste("pmid", pmid, e))
                            errs_ratio_xml <- errs_ratio_xml + 1
                          },
                          warning = function(w) { 
                            print(paste("pmid", pmid, w)) 
                          }
      )
      ratios2 <- tryCatch(ratioCheck(txt2), 
                          error = function(e) {
                            print(paste("pmid", pmid, e)) 
                            errs_ratio_txt <- errs_ratio_txt + 1
                          },
                          warning = function(w) { 
                            print(paste("pmid", pmid, w)) 
                          }
      )
      ratios <- rbind(ratios,ratios2)
    } else if( use == "intxt" ) {
      ratios <- tryCatch(ratioCheck(intxt), 
                         error = function(e) {
                           print(paste("pmid", pmid, e)) 
                           errs_ratio_xml <- errs_ratio_xml + 1
                         },
                         warning = function(w) { 
                           print(paste("pmid", pmid, w))
                          }
      )
    } else if( use == "txt2") {
      ratios <- tryCatch(ratioCheck(txt2), 
                         error = function(e) {
                           print(paste("pmid", pmid, e)) 
                           errs_ratio_txt <- errs_ratio_txt + 1
                         },
                         warning = function(w) { 
                           print(paste("pmid", pmid, w)) 
                        }
      )
    }
    if( !is.data.frame(ratios) ) {
      ratios <- NULL
    } else {
      ratios$pmid <- pmid
      ratios$id <- paste0(ratios$ratio, "|", ratios$percent)
      ratios <- ratios[!duplicated(ratios$id),]
      ratios$id <- NULL
    }
    
    
    #CHECK CORRECTNESS OF ORs, RRs, and HRs, and remove duplicate rows
    if( use == "both" ) {
      ORs <- tryCatch(ciCheck(intxt), 
                      error = function(e) {
                        print(paste("pmid", pmid, e)) 
                        errs_ors_xml <- errs_ors_xml + 1
                      },
                      warning = function(w) { 
                        print(paste("pmid", pmid, w)) 
                      }
      )
      ORs2 <- tryCatch(ciCheck(txt2), 
                       error = function(e) {
                         print(paste("pmid", pmid, e)) 
                         errs_ors_txt <- errs_ors_txt + 1
                       },
                       warning = function(w) { 
                         print(paste("pmid", pmid, w)) 
                        }
      )
      ORs <- rbind(ORs, ORs2)
      rm(ORs2)
    } else if( use == "intxt" ) {
      ORs <- tryCatch(ciCheck(intxt), 
                      error = function(e) {
                        print(paste("pmid", pmid, e))
                        errs_ors_xml <- errs_ors_xml + 1
                      },
                      warning = function(w) { 
                        print(paste("pmid", pmid, w)) 
                        }
      )
    } else if( use == "txt2") {
      ORs <- tryCatch(ciCheck(txt2), 
                      error = function(e) {
                        print(paste("pmid", pmid, e))
                        errs_ors_txt <- errs_ors_txt + 1
                      },
                      warning = function(w) { 
                        print(paste("pmid", pmid, w)) 
                        }
      )
    }
    if( !is.data.frame(ORs) ) {
      ORs <- NULL
    } else {
      ORs$pmid <- pmid
      ORs$id <- paste0(ORs$OddsR, "|", ORs$CIlower, "|", ORs$CIupper, "|", ORs$pval)
      ORs <- ORs[!duplicated(ORs$id),]
      ORs$id <- NULL
    }
    
    # bind
    ratio_checked <- rbind(ratio_checked, ratios)
    OR_checked <- rbind(OR_checked, ORs)
    
    if( i == length(filez) ) {
      write.csv(ratio_checked, paste0("georgescu_ratio4_partial_", j, ".csv"), row.names = F )
      write.csv(OR_checked, paste0("georgescu_or4_partial_", j, ".csv"), row.names = F )
    }
      
  }
}

# reorder columns
ratio_checked <- dplyr::select(ratio_checked, pmid, everything())
OR_checked <- dplyr::select(OR_checked, pmid, everything())

# write files
write.csv( ratio_checked, "georgescu_ratio4.csv", row.names = F )
write.csv( OR_checked, "georgescu_or4.csv", row.names = F )


print(length(unique(OR_checked$pmid)))
summ <-  ddply(OR_checked, .(pmid), summarize,
        nErrors = sum(pError == TRUE) > 0,
        nDecisionErrors = sum(decisionError == TRUE) > 0)
print( sum( summ$nErrors, na.rm = T ) )
print( sum( summ$nDecisionErrors, na.rm = T ) )

print( paste( "number of errors in for loop (ratio, xml)", errs_ratio_xml ) )
print( paste( "number of errors in for loop (ratio, txt)", errs_ratio_txt ) )
print( paste( "number of errors in for loop (ORs, xml)", errs_ors_xml ) )
print( paste( "number of errors in for loop (ORs, txt)", errs_ors_txt ) )

# quit R session
q("no")