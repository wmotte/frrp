#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2018-03-05
#
# aim:  determine statistical reporting errors
#################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 

library( 'statcheck' )
library( 'plyr' ); library( 'dplyr' )
library( 'psych' )

#######################################
# FUNCTIONS
#######################################

###
# the function 'statcheck' from package statcheck, with the progress bar removed
# in addition, in line 525 a new if( is.na) statement is added
##
statme <- function (x, stat = c("t", "F", "cor", "chisq", "Z"), OneTailedTests = FALSE, 
          alpha = 0.05, pEqualAlphaSig = TRUE, pZeroError = TRUE, OneTailedTxt = FALSE, 
          AllPValues = FALSE) 
{
  Res <- data.frame(Source = NULL, Statistic = NULL, df1 = NULL, 
                    df2 = NULL, Test.Comparison = NULL, Value = NULL, Reported.Comparison = NULL, 
                    Reported.P.Value = NULL, Computed = NULL, Error = NULL, 
                    DecisionError = NULL, CopyPaste = NULL, Location = NULL, 
                    stringsAsFactors = FALSE, dec = NULL, testdec = NULL, 
                    OneTail = NULL, OneTailedInTxt = NULL, APAfactor = NULL)
  class(Res) <- c("statcheck", "data.frame")
  OneTailedInTxt <- NULL
  pRes <- data.frame(Source = NULL, Statistic = NULL, Reported.Comparison = NULL, 
                     Reported.P.Value = NULL, Raw = NULL, stringsAsFactors = FALSE)
  if (length(x) == 0) 
    return(Res)
  if (is.null(names(x))) 
    names(x) <- 1:length(x)
#  message("Extracting statistics...")   ### disabled
  for (i in 1:length(x)) {
    txt <- x[i]
    pLoc <- gregexpr("([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*)", 
                     txt, ignore.case = TRUE)[[1]]
    if (pLoc[1] != -1) {
      pRaw <- substring(txt, pLoc, pLoc + attr(pLoc, "match.length") - 
                          1)
      nums <- gregexpr("(\\d*\\.?\\d+\\s?e?-?\\d*)|ns", 
                       pRaw, ignore.case = TRUE)
      suppressWarnings(pValsChar <- substring(pRaw, sapply(nums, 
                                                           "[", 1), sapply(nums, function(x) x[1] + attr(x, 
                                                                                                         "match.length")[1] - 1)))
      suppressWarnings(pVals <- as.numeric(pValsChar))
      eqLoc <- gregexpr("p\\s?.?", pRaw)
      pEq <- substring(pRaw, sapply(eqLoc, function(x) x[1] + 
                                      attr(x, "match.length")[1] - 1), sapply(eqLoc, 
                                                                              function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                1))
      pEq[grepl("ns", pRaw, ignore.case = TRUE)] <- "ns"
      pvalues <- data.frame(Source = names(x)[i], Statistic = "p", 
                            Reported.Comparison = pEq, Reported.P.Value = pVals, 
                            Raw = pRaw, stringsAsFactors = FALSE)
      pvalues <- pvalues[pvalues$Reported.P.Value <= 1 | 
                           is.na(pvalues$Reported.P.Value), ]
      pRes <- rbind(pRes, pvalues)
      rm(pvalues)
    }
    onesided <- gregexpr("one.?sided|one.?tailed|directional", 
                         txt, ignore.case = TRUE)[[1]]
    if (onesided[1] != -1) {
      onesided <- 1
    }
    else {
      onesided <- 0
    }
    OneTailedInTxt <- as.logical(onesided)
    if ("t" %in% stat) {
      tLoc <- gregexpr("t\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))", 
                       txt, ignore.case = TRUE)[[1]]
      if (tLoc[1] != -1) {
        tRaw <- substring(txt, tLoc, tLoc + attr(tLoc, 
                                                 "match.length") - 1)
        tRaw <- gsub("(?<=\\d),(?=\\d+)", "", tRaw, perl = TRUE)
        tRaw <- gsub("(?<=\\=)\\s+(?=.*\\,)", "", tRaw, 
                     perl = TRUE)
        tRaw <- gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", 
                     " -", tRaw, perl = TRUE)
        tRaw <- gsub("(?<=\\=)(?=(\\.|\\d))", " ", tRaw, 
                     perl = TRUE)
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns", 
                         tRaw, ignore.case = TRUE)
        df <- as.numeric(substring(tRaw, sapply(nums, 
                                                "[", 1), sapply(nums, function(x) x[1] + attr(x, 
                                                                                              "match.length")[1] - 1)))
        suppressWarnings(tValsChar <- substring(tRaw, 
                                                sapply(nums, "[", 2), sapply(nums, function(x) x[2] + 
                                                                               attr(x, "match.length")[2] - 1)))
        suppressWarnings(tVals <- as.numeric(tValsChar))
        testdec <- attr(regexpr("\\.\\d+", tValsChar), 
                        "match.length") - 1
        testdec[testdec < 0] <- 0
        testEqLoc <- gregexpr("\\)\\s?[<>=]", tRaw)
        testEq <- substring(tRaw, sapply(testEqLoc, function(x) x[1] + 
                                           attr(x, "match.length")[1] - 1), sapply(testEqLoc, 
                                                                                   function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                     1))
        suppressWarnings(pValsChar <- substring(tRaw, 
                                                sapply(nums, "[", 3), sapply(nums, function(x) x[3] + 
                                                                               attr(x, "match.length")[3] - 1)))
        suppressWarnings(pVals <- as.numeric(pValsChar))
        eqLoc <- gregexpr("p\\s?[<>=]", tRaw, ignore.case = TRUE)
        pEq <- substring(tRaw, sapply(eqLoc, function(x) x[1] + 
                                        attr(x, "match.length")[1] - 1), sapply(eqLoc, 
                                                                                function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                  1))
        pEq[grepl("ns", tRaw, ignore.case = TRUE)] <- "ns"
        dec <- attr(regexpr("\\.\\d+", pValsChar), "match.length") - 
          1
        dec[dec < 0] <- 0
        dec[pEq == "ns"] <- 3
        Computed <- round(pt(-1 * abs(tVals), df) * 2, dec)         ###added
        tRes <- data.frame(Source = names(x)[i], Statistic = "t", 
                           df1 = NA, df2 = df, Test.Comparison = testEq, 
                           Value = tVals, Reported.Comparison = pEq, Reported.P.Value = pVals, 
                           Computed = Computed, Location = tLoc,
                           Raw = tRaw, stringsAsFactors = FALSE, dec = dec, 
                           testdec = testdec, OneTailedInTxt = OneTailedInTxt)
        Res <- rbind(Res, tRes)
        rm(tRes)
      }
    }
    if ("F" %in% stat) {
      FLoc <- gregexpr("F\\s?\\(\\s?\\d*\\.?(I|l|\\d+)\\s?,\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))", 
                       txt, ignore.case = TRUE)[[1]]
      if (FLoc[1] != -1) {
        FRaw <- substring(txt, FLoc, FLoc + attr(FLoc, 
                                                 "match.length") - 1)
        FRaw <- gsub("l|I", 1, FRaw)
        nums <- gregexpr("(\\d*\\.?\\d+\\s?e?-?\\d*)|ns", 
                         FRaw, ignore.case = TRUE)
        df1 <- as.numeric(substring(FRaw, sapply(nums, 
                                                 "[", 1), sapply(nums, function(x) x[1] + attr(x, 
                                                                                               "match.length")[1] - 1)))
        df2 <- as.numeric(substring(FRaw, sapply(nums, 
                                                 "[", 2), sapply(nums, function(x) x[2] + attr(x, 
                                                                                               "match.length")[2] - 1)))
        Fsplit <- strsplit(FRaw, "\\)", perl = TRUE)
        FValsRaw <- lapply(Fsplit, function(x) x[2])
        FandDF <- lapply(Fsplit, function(x) x[1])
        FValsRaw <- gsub("(?<=\\d),(?=\\d+)", "", FValsRaw, 
                         perl = TRUE)
        FRaw <- paste(FandDF, ")", FValsRaw, sep = "")
        numsF <- gregexpr("(\\d*\\.?\\d+)|ns", FValsRaw)
        suppressWarnings(FValsChar <- substring(FValsRaw, 
                                                sapply(numsF, "[", 1), sapply(numsF, function(x) x[1] + 
                                                                                attr(x, "match.length")[1] - 1)))
        suppressWarnings(FVals <- as.numeric(FValsChar))
        testdec <- attr(regexpr("\\.\\d+", FValsChar), 
                        "match.length") - 1
        testdec[testdec < 0] <- 0
        testEqLoc <- gregexpr("\\)\\s?[<>=]", FRaw)
        testEq <- substring(FRaw, sapply(testEqLoc, function(x) x[1] + 
                                           attr(x, "match.length")[1] - 1), sapply(testEqLoc, 
                                                                                   function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                     1))
        suppressWarnings(pValsChar <- substring(FValsRaw, 
                                                sapply(numsF, "[", 2), sapply(numsF, function(x) x[2] + 
                                                                                attr(x, "match.length")[2] - 1)))
        suppressWarnings(pVals <- as.numeric(pValsChar))
        eqLoc <- gregexpr("p\\s?[<>=]", FRaw, ignore.case = TRUE)
        pEq <- substring(FRaw, sapply(eqLoc, function(x) x[1] + 
                                        attr(x, "match.length")[1] - 1), sapply(eqLoc, 
                                                                                function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                  1))
        pEq[grepl("ns", FRaw, ignore.case = TRUE)] <- "ns"
        dec <- attr(regexpr("\\.\\d+", pValsChar), "match.length") - 
          1
        dec[dec < 0] <- NA
        if(is.na(pVals)) dec <- 3
        Computed <- round(pf(FVals, df1, df2, lower.tail = FALSE), dec)
        FRes <- data.frame(Source = names(x)[i], Statistic = "F", 
                           df1 = df1, df2 = df2, Test.Comparison = testEq, 
                           Value = FVals, Reported.Comparison = pEq, Reported.P.Value = pVals, 
                           Computed = Computed, 
                           Location = FLoc, Raw = FRaw, stringsAsFactors = FALSE, 
                           dec = dec, testdec = testdec, OneTailedInTxt = OneTailedInTxt)
        Res <- rbind(Res, FRes)
        rm(FRes)
      }
    }
    if (any(c("r", "cor", "correlations") %in% stat)) {
      rLoc <- gregexpr("r\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))", 
                       txt, ignore.case = TRUE)[[1]]
      if (rLoc[1] != -1) {
        rRaw <- substring(txt, rLoc, rLoc + attr(rLoc, 
                                                 "match.length") - 1)
        rRaw <- gsub("(?<=\\=)\\s+(?=.*\\,)", "", rRaw, 
                     perl = TRUE)
        rRaw <- gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", 
                     " -", rRaw, perl = TRUE)
        rRaw <- gsub("(?<=\\=)(?=(\\.|\\d))", " ", rRaw, 
                     perl = TRUE)
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns", 
                         rRaw, ignore.case = TRUE)
        df <- as.numeric(substring(rRaw, sapply(nums, 
                                                "[", 1), sapply(nums, function(x) x[1] + attr(x, 
                                                                                              "match.length")[1] - 1)))
        suppressWarnings(rValsChar <- substring(rRaw, 
                                                sapply(nums, "[", 2), sapply(nums, function(x) x[2] + 
                                                                               attr(x, "match.length")[2] - 1)))
        suppressWarnings(rVals <- as.numeric(rValsChar))
        testdec <- attr(regexpr("\\.\\d+", rValsChar), 
                        "match.length") - 1
        testdec[testdec < 0] <- 0
        testEqLoc <- gregexpr("\\)\\s?[<>=]", rRaw)
        testEq <- substring(rRaw, sapply(testEqLoc, function(x) x[1] + 
                                           attr(x, "match.length")[1] - 1), sapply(testEqLoc, 
                                                                                   function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                     1))
        suppressWarnings(pValsChar <- substring(rRaw, 
                                                sapply(nums, "[", 3), sapply(nums, function(x) x[3] + 
                                                                               attr(x, "match.length")[3] - 1)))
        suppressWarnings(pVals <- as.numeric(pValsChar))
        eqLoc <- gregexpr("p\\s?[<>=]", rRaw, ignore.case = TRUE)
        pEq <- substring(rRaw, sapply(eqLoc, function(x) x[1] + 
                                        attr(x, "match.length")[1] - 1), sapply(eqLoc, 
                                                                                function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                  1))
        pEq[grepl("ns", rRaw, ignore.case = TRUE)] <- "ns"
        dec <- attr(regexpr("\\.\\d+", pValsChar), "match.length") - 
          1
        dec[dec < 0] <- 0
        dec[pEq == "ns"] <- 3
        pComputed <- round(pmin(pt(-1 * abs(r2t(rVals, df)), df) * 2, 1), dec)     ### added
        pComputed[is.nan(pComputed)] <- NA
        rRes <- data.frame(Source = names(x)[i], Statistic = "r", 
                           df1 = NA, df2 = df, Test.Comparison = testEq, 
                           Value = rVals, Reported.Comparison = pEq, Reported.P.Value = pVals, 
                           Computed = pComputed, Location = rLoc, Raw = rRaw, 
                           stringsAsFactors = FALSE, dec = dec, testdec = testdec, 
                           OneTailedInTxt = OneTailedInTxt)
        Res <- rbind(Res, rRes)
        rm(rRes)
      }
    }
    if ("Z" %in% stat) {
      zLoc <- gregexpr("[^a-z]z\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))", 
                       txt, ignore.case = TRUE)[[1]]
      if (zLoc[1] != -1) {
        zRaw <- substring(txt, zLoc, zLoc + attr(zLoc, 
                                                 "match.length") - 1)
        zRaw <- gsub(".?(z|Z)", "Z", zRaw, perl = TRUE)
        zRaw <- gsub("(?<=\\d),(?=\\d+\\.)", "", zRaw, 
                     perl = TRUE)
        zRaw <- gsub("(?<=\\=)\\s+(?=.*\\,)", "", zRaw, 
                     perl = TRUE)
        zRaw <- gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", 
                     " -", zRaw, perl = TRUE)
        zRaw <- gsub("(?<=\\=)(?=(\\.|\\d))", " ", zRaw, 
                     perl = TRUE)
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns", 
                         zRaw, ignore.case = TRUE)
        suppressWarnings(zValsChar <- substring(zRaw, 
                                                sapply(nums, "[", 1), sapply(nums, function(x) x[1] + 
                                                                               attr(x, "match.length")[1] - 1)))
        suppressWarnings(zVals <- as.numeric(zValsChar))
        testdec <- attr(regexpr("\\.\\d+", zValsChar), 
                        "match.length") - 1
        testdec[testdec < 0] <- 0
        testEqLoc <- gregexpr("(z|Z|z'|Z')\\s?[<>=]", 
                              zRaw)
        testEq <- substring(zRaw, sapply(testEqLoc, function(x) x[1] + 
                                           attr(x, "match.length")[1] - 1), sapply(testEqLoc, 
                                                                                   function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                     1))
        suppressWarnings(pValsChar <- substring(zRaw, 
                                                sapply(nums, "[", 2), sapply(nums, function(x) x[2] + 
                                                                               attr(x, "match.length")[2] - 1)))
        suppressWarnings(pVals <- as.numeric(pValsChar))
        eqLoc <- gregexpr("p\\s?[<>=]", zRaw, ignore.case = TRUE)
        pEq <- substring(zRaw, sapply(eqLoc, function(x) x[1] + 
                                        attr(x, "match.length")[1] - 1), sapply(eqLoc, 
                                                                                function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                  1))
        pEq[grepl("ns", zRaw, ignore.case = TRUE)] <- "ns"
        dec <- attr(regexpr("\\.\\d+", pValsChar), "match.length") - 
          1
        dec[dec < 0] <- 0
        dec[pEq == "ns"] <- 3
        Computed <- round( pnorm(abs(zVals), lower.tail = FALSE) * 2, dec )          ### added
        zRes <- data.frame(Source = names(x)[i], Statistic = "Z", 
                           df1 = NA, df2 = NA, Test.Comparison = testEq, 
                           Value = zVals, Reported.Comparison = pEq, Reported.P.Value = pVals, 
                           Computed = Computed, Location = zLoc, Raw = zRaw, stringsAsFactors = FALSE, 
                           dec = dec, testdec = testdec, OneTailedInTxt = OneTailedInTxt)
        Res <- rbind(Res, zRes)
        rm(zRes)
      }
    }
    if ("chisq" %in% stat) {
      chi2Loc <- gregexpr("((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trF ]\\s?)|([^trF]2\\s?))2?\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)\\s?[<>=]\\s?\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))", 
                          txt, ignore.case = TRUE)[[1]]
      if (chi2Loc[1] != -1) {
        chi2Raw <- substring(txt, chi2Loc, chi2Loc + 
                               attr(chi2Loc, "match.length") - 1)
        substr(chi2Raw, 1, 1)[grepl("\\d", substr(chi2Raw, 
                                                  1, 1))] <- " "
        chi2Raw_inclN <- chi2Raw
        chi2Raw <- gsub("N\\s?=\\s?\\d*\\,?\\d*\\,?\\d*", 
                        "", chi2Raw, ignore.case = TRUE)
        chi2Raw <- gsub("(?<=\\d),(?=\\d+\\.)", "", chi2Raw, 
                        perl = TRUE)
        chi2Raw <- gsub("\\((?=2\\s?\\()", "", chi2Raw, 
                        perl = TRUE)
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns", 
                         sub("^.*?\\(", "", chi2Raw), ignore.case = TRUE)
        df <- as.numeric(substring(sub("^.*?\\(", "", 
                                       chi2Raw), sapply(nums, "[", 1), sapply(nums, 
                                                                              function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                1)))
        suppressWarnings(chi2ValsChar <- substring(sub("^.*?\\(", 
                                                       "", chi2Raw), sapply(nums, "[", 2), sapply(nums, 
                                                                                                  function(x) x[2] + attr(x, "match.length")[2] - 
                                                                                                    1)))
        suppressWarnings(chi2Vals <- as.numeric(chi2ValsChar))
        testdec <- attr(regexpr("\\.\\d+", chi2ValsChar), 
                        "match.length") - 1
        testdec[testdec < 0] <- 0
        testEqLoc <- gregexpr("\\)\\s?[<>=]", chi2Raw)
        testEq <- substring(chi2Raw, sapply(testEqLoc, 
                                            function(x) x[1] + attr(x, "match.length")[1] - 
                                              1), sapply(testEqLoc, function(x) x[1] + 
                                                           attr(x, "match.length")[1] - 1))
        suppressWarnings(pValsChar <- substring(sub("^.*?\\(", 
                                                    "", chi2Raw), sapply(nums, "[", 3), sapply(nums, 
                                                                                               function(x) x[3] + attr(x, "match.length")[3] - 
                                                                                                 1)))
        suppressWarnings(pVals <- as.numeric(pValsChar))
        eqLoc <- gregexpr("p\\s?[<>=]", chi2Raw, ignore.case = TRUE)
        pEq <- substring(chi2Raw, sapply(eqLoc, function(x) x[1] + 
                                           attr(x, "match.length")[1] - 1), sapply(eqLoc, 
                                                                                   function(x) x[1] + attr(x, "match.length")[1] - 
                                                                                     1))
        pEq[grepl("ns", chi2Raw, ignore.case = TRUE)] <- "ns"
        dec <- attr(regexpr("\\.\\d+", pValsChar), "match.length") - 
          1
        dec[dec < 0] <- 0
        dec[pEq == "ns"] <- 3
        Computed <- round(pchisq(chi2Vals, df, lower.tail = FALSE), dec)           ### added
        chi2Res <- data.frame(Source = names(x)[i], Statistic = "Chi2", 
                              df1 = df, df2 = NA, Test.Comparison = testEq, 
                              Value = chi2Vals, Reported.Comparison = pEq, 
                              Reported.P.Value = pVals, 
                              Computed = Computed, 
                              Location = chi2Loc, 
                              Raw = chi2Raw_inclN, stringsAsFactors = FALSE, 
                              dec = dec, testdec = testdec, OneTailedInTxt = OneTailedInTxt)
        Res <- rbind(Res, chi2Res)
        rm(chi2Res)
      }
    }
  }
  Source <- NULL
  Res <- ddply(Res, .(Source), function(x) x[order(x$Location), 
                                             ])
  if (nrow(Res) > 0) {
    Res <- Res[Res$Reported.P.Value <= 1 | is.na(Res$Reported.P.Value), 
               ]
  }
  ErrorTest <- function(x, ...) {
    computed <- as.vector(x$Computed)
    comparison <- as.vector(x$Reported.Comparison)
    reported <- as.vector(x$Reported.P.Value)
    testcomp <- as.vector(x$Test.Comparison)
    reported[comparison == "ns"] <- alpha
    comparison[comparison == "ns"] <- ">"
    Match <- paste(computed, comparison, reported)
    InExTests <- grepl("<|>", Match)
    if (any(InExTests)) {
      InExTests[InExTests] <- sapply(Match[InExTests], 
                                     function(m) !eval(parse(text = m)))
    }
    smallsmall <- testcomp == "<" & comparison == "<"
    smallgreat <- testcomp == "<" & comparison == ">"
    greatsmall <- testcomp == ">" & comparison == "<"
    greatgreat <- testcomp == ">" & comparison == ">"
    if (any(smallsmall)) {
      InExTests[smallsmall] <- round(computed[smallsmall], 
                                     x$dec[smallsmall]) <= round(reported[smallsmall], 
                                                                 x$dec[smallsmall])
    }
    if (any(greatgreat)) {
      InExTests[greatgreat] <- round(computed[greatgreat], 
                                     x$dec[greatgreat]) >= round(reported[greatgreat], 
                                                                 x$dec[greatgreat])
    }
    InExTests[smallgreat] <- FALSE
    InExTests[greatsmall] <- FALSE
    ExTests <- comparison == "="
    if (any(ExTests)) {
      ExTests[ExTests] <- !(round(computed[ExTests], x$dec[ExTests]) == 
                              round(reported[ExTests], x$dec[ExTests]))
    }
    smallequal <- x$Test.Comparison == "<" & comparison == 
      "="
    greatequal <- x$Test.Comparison == ">" & comparison == 
      "="
    if (any(smallequal)) {
      ExTests[smallequal] <- round(computed[smallequal], 
                                   x$dec[smallequal]) <= round(reported[smallequal], 
                                                               x$dec[smallequal])
    }
    if (any(greatequal)) {
      ExTests[greatequal] <- round(computed[greatequal], 
                                   x$dec[greatequal]) >= round(reported[greatequal], 
                                                               x$dec[greatequal])
    }
    Error <- !(InExTests == FALSE & ExTests == FALSE)
    return(Error)
  }
  DecisionErrorTest <- function(x, ...) {
    computed <- x$Computed
    comparison <- x$Reported.Comparison
    reported <- x$Reported.P.Value
    testcomp <- as.vector(x$Test.Comparison)
    reported[comparison == "ns"] <- alpha
    comparison[comparison == "ns"] <- ">"
    equalequal <- testcomp == "=" & comparison == "="
    equalsmall <- testcomp == "=" & comparison == "<"
    equalgreat <- testcomp == "=" & comparison == ">"
    smallequal <- testcomp == "<" & comparison == "="
    smallsmall <- testcomp == "<" & comparison == "<"
    smallgreat <- testcomp == "<" & comparison == ">"
    greatequal <- testcomp == ">" & comparison == "="
    greatsmall <- testcomp == ">" & comparison == "<"
    greatgreat <- testcomp == ">" & comparison == ">"
    AllTests <- grepl("=|<|>", comparison)
    if (any(AllTests)) {
      if (pEqualAlphaSig == TRUE) {
        AllTests[equalequal] <- (reported[equalequal] <= 
                                   alpha & computed[equalequal] > alpha) | (reported[equalequal] > 
                                                                              alpha & computed[equalequal] <= alpha)
        AllTests[equalsmall] <- reported[equalsmall] <= 
          alpha & computed[equalsmall] > alpha
        AllTests[equalgreat] <- reported[equalgreat] >= 
          alpha & computed[equalgreat] <= alpha
        AllTests[smallequal] <- reported[smallequal] <= 
          alpha & computed[smallequal] >= alpha
        AllTests[smallsmall] <- reported[smallsmall] <= 
          alpha & computed[smallsmall] >= alpha
        AllTests[greatequal] <- reported[greatequal] > 
          alpha & computed[greatequal] <= alpha
        AllTests[greatgreat] <- reported[greatgreat] >= 
          alpha & computed[greatgreat] <= alpha
      }
      else {
        AllTests[equalequal] <- (reported[equalequal] < 
                                   alpha & computed[equalequal] >= alpha) | (reported[equalequal] >= 
                                                                               alpha & computed[equalequal] < alpha)
        AllTests[equalsmall] <- reported[equalsmall] < 
          alpha & computed[equalsmall] >= alpha
        AllTests[equalgreat] <- reported[equalgreat] >= 
          alpha & computed[equalgreat] < alpha
        AllTests[smallequal] <- reported[smallequal] < 
          alpha & computed[smallequal] >= alpha
        AllTests[smallsmall] <- reported[smallsmall] <= 
          alpha & computed[smallsmall] >= alpha
        AllTests[greatequal] <- reported[greatequal] >= 
          alpha & computed[greatequal] < alpha
        AllTests[greatgreat] <- reported[greatgreat] >= 
          alpha & computed[greatgreat] < alpha
      }
      AllTests[smallgreat] <- FALSE
      AllTests[greatsmall] <- FALSE
    }
    AllTests <- as.logical(AllTests)
    return(AllTests)
  }
  if (nrow(Res) > 0) {
    if (OneTailedTests == TRUE) {
      Res$Computed <- Res$Computed/2
    }
    Res$Error <- ErrorTest(Res)
    Res$DecisionError <- DecisionErrorTest(Res)
    DecisionErrorAlphas <- logical()
    alphas <- c(0.01, 0.1)
    for (a in alphas) {
      alpha <- a
      DecisionErrorAlphas <- c(DecisionErrorAlphas, DecisionErrorTest(Res))
    }
    if (any(DecisionErrorAlphas[!is.na(DecisionErrorAlphas) & 
                                !is.nan(DecisionErrorAlphas)])) {
#      message("\n Check the significance level. \n \n Some of the p value incongruencies are decision errors if the significance level is .1 or .01 instead of the conventional .05. It is recommended to check the actual significance level in the paper or text. Check if the reported p values are a decision error at a different significance level by running statcheck again with 'alpha' set to .1 and/or .01. \n ")    ### disabled
    }
    if (OneTailedTests == FALSE) {
      computed <- Res$Computed
      comparison <- Res$Reported.Comparison
      reported <- Res$Reported.P.Value
      raw <- Res$Raw
      onetail <- computed/2
      OneTail <- ifelse(Res$Error == TRUE & 
                          (grepl("=", comparison) & round(reported, 2) == round(onetail, 2)) | 
                          (grepl("<", comparison) & reported == 0.05 & onetail < reported & computed > reported), 
                        TRUE, 
                        FALSE)
      Res$OneTail <- OneTail
      if (any(OneTail[!is.na(OneTail)] == TRUE & OneTailedTxt[!is.na(OneTailedTxt)] == 
              FALSE)) {
#        message("\n Check for one tailed tests. \n \n Some of the p value incongruencies might in fact be one tailed tests. It is recommended to check this in the actual paper or text. Check if the p values would also be incongruent if the test is indeed one sided by running statcheck again with 'OneTailedTests' set to TRUE. To see which Sources probably contain a one tailed test, try unique(x$Source[x$OneTail]) (where x is the statcheck output). \n ")  ### disabled
      }
    }
    if (OneTailedTxt == TRUE) {
      Res1tailed <- Res
      Res1tailed$Computed <- Res1tailed$Computed/2
      Res1tailed$Error <- ErrorTest(Res1tailed)
      Res1tailed$DecisionError <- DecisionErrorTest(Res1tailed)
      Res$Error[!((Res$Statistic == "F" | Res$Statistic == 
                     "Chi2") & Res$df1 > 1) & Res$OneTailedInTxt == 
                  TRUE & Res1tailed$Error == FALSE] <- FALSE
      Res$DecisionError[!((Res$Statistic == "F" | Res$Statistic == 
                             "Chi2") & Res$df1 > 1) & Res$OneTailedInTxt == 
                          TRUE & Res1tailed$DecisionError == FALSE] <- FALSE
    }
    correct_round <- numeric()
    lower <- Res$Value - (0.5/10^Res$testdec)
    upper <- Res$Value + (0.5/10^Res$testdec)
    for (i in seq_len(nrow(Res))) {
      if (Res[i, ]$Statistic == "F") {
        upP <- pf(lower[i], Res[i, ]$df1, Res[i, ]$df2, 
                  lower.tail = FALSE)
        lowP <- pf(upper[i], Res[i, ]$df1, Res[i, ]$df2, 
                   lower.tail = FALSE)
      }
      else if (Res[i, ]$Statistic == "t") {
        if ( is.na( lower[i] ) ) {
          lowP <- NA
          upP <- NA
        } else if ( lower[i] < 0) {
          lowP <- pt(lower[i], Res[i, ]$df2) * 2
          upP <- pt(upper[i], Res[i, ]$df2) * 2
        } else {
          upP <- pt(-1 * lower[i], Res[i, ]$df2) * 2
          lowP <- pt(-1 * upper[i], Res[i, ]$df2) * 2
        }
      }
      else if (Res[i, ]$Statistic == "Chi2") {
        upP <- pchisq(lower[i], Res[i, ]$df1, lower.tail = FALSE)
        lowP <- pchisq(upper[i], Res[i, ]$df1, lower.tail = FALSE)
      }
      else if (Res[i, ]$Statistic == "r") {
        if (lower[i] < 0) {
          lowP <- pmin(pt(r2t(lower[i], Res[i, ]$df2), 
                          Res[i, ]$df2) * 2, 1)
          upP <- pmin(pt(r2t(upper[i], Res[i, ]$df2), 
                         Res[i, ]$df2) * 2, 1)
        }
        else {
          upP <- pmin(pt(-1 * r2t(lower[i], Res[i, ]$df2), 
                         Res[i, ]$df2) * 2, 1)
          lowP <- pmin(pt(-1 * r2t(upper[i], Res[i, ]$df2), 
                          Res[i, ]$df2) * 2, 1)
        }
      }
      else if (Res[i, ]$Statistic == "Z" | Res[i, ]$Statistic == 
               "z") {
        if (lower[i] < 0) {
          lowP <- pnorm(abs(lower[i]), lower.tail = FALSE) * 
            2
          upP <- pnorm(abs(upper[i]), lower.tail = FALSE) * 
            2
        }
        else {
          upP <- pnorm(lower[i], lower.tail = FALSE) * 
            2
          lowP <- pnorm(upper[i], lower.tail = FALSE) * 
            2
        }
      }
      if (OneTailedTests == TRUE) {
        upP <- upP/2
        lowP <- lowP/2
      }
      if (Res[i, "Reported.Comparison"] == "=") {
        correct_round[i] <- ifelse(Res[i, ]$Error == 
                                     TRUE & Res$Reported.P.Value[i] >= round(lowP, 
                                                                             Res$dec[i]) & Res$Reported.P.Value[i] <= round(upP, 
                                                                                                                            Res$dec[i]), TRUE, FALSE)
      }
      if (Res[i, "Reported.Comparison"] == "<") {
        correct_round[i] <- ifelse(Res[i, ]$Error == 
                                     TRUE & Res$Reported.P.Value[i] > lowP, TRUE, 
                                   FALSE)
      }
      if (Res[i, "Reported.Comparison"] == ">") {
        correct_round[i] <- ifelse(Res[i, ]$Error == 
                                     TRUE & Res$Reported.P.Value[i] < upP, TRUE, 
                                   FALSE)
      }
    }
    CorrectRound <- as.logical(correct_round)
    if (pZeroError == TRUE) {
      ImpossibleP <- (Res$Reported.P.Value <= 0)
    }
    else {
      ImpossibleP <- (Res$Reported.P.Value < 0)
    }
    Res$Error[ImpossibleP] <- TRUE
    NoErrorDecisionError <- Res$Error == FALSE & Res$DecisionError == 
      TRUE
    Res$DecisionError[NoErrorDecisionError] <- FALSE
    pRes_selection <- pRes[pRes$Source %in% Res$Source, ]
    Res_selection <- Res[Res$Source %in% pRes_selection$Source, 
                         ]
    APA <- by(Res_selection, Res_selection$Source, nrow)/by(pRes_selection, 
                                                            pRes_selection$Source, nrow)
    Res$APAfactor <- round(as.numeric(apply(Res, 1, function(x) APA[which(names(APA) == 
                                                                            x["Source"])])), 2)
    Res$Error[CorrectRound] <- FALSE
    Res$DecisionError[CorrectRound] <- FALSE
    Res <- data.frame(Source = Res$Source, Statistic = Res$Statistic, 
                      df1 = Res$df1, df2 = Res$df2, Test.Comparison = Res$Test.Comparison, 
                      Value = Res$Value, Reported.Comparison = Res$Reported.Comparison, 
                      Reported.P.Value = Res$Reported.P.Value, Computed = Res$Computed, 
                      Decimals = Res$dec,                                                  ### added to output
                      Raw = Res$Raw, Error = Res$Error, DecisionError = Res$DecisionError, 
                      OneTail = Res$OneTail, OneTailedInTxt = Res$OneTailedInTxt, 
                      APAfactor = Res$APAfactor)
    class(Res) <- c("statcheck", "data.frame")
  }
  if (AllPValues == FALSE) {
    if (nrow(Res) > 0) {
      return(Res)
    }
    else {
#      Res <- cat("statcheck did not find any results\n")   ### disabled
      NULL
    }
  }
  else {
    return(pRes)
  }
}

#######################################
# END FUNCTIONS
#######################################

# create list of files
filelist1 <- list.files( "/mnt/data/live02/stress/hlamberink/grobid/xmlout" )
filelist2 <- list.files( "/mnt/data/live02/stress/hlamberink/grobid/xmloutNEW" )
filelist3 <- list.files( "/mnt/data/live02/stress/hlamberink/scopus/xmlGrobid" )

print( length( filelist1 ) ) 
print( length( filelist2 ) )
print( length( filelist3 ) )
print( sum( length( filelist1 ), length( filelist2 ), length( filelist3 ) ) )

# output container
out1 <- NULL
summ1 <- NULL
out2 <- NULL
summ2 <- NULL


# define start and stop for loop
#if( file.exists( "statCheckResults.csv" ) ){ 
#  summ <- read.csv( "statCheckResults.csv", stringsAsFactors = FALSE )
#  start <- length( summ[,1] ) + 1
#  
#  } else {
#  start <- 1
#}
#print(start)
start <- 1
stop <- length(filelist1)


# load previously gathered data
#if( file.exists( "statCheckElaborate.csv" ) )
#{
#  out1 <- read.csv( "statCheckElaborate.csv", stringsAsFactors = FALSE )
#}
  
  
# set progress bar
progbar <- txtProgressBar( min = 0, max = stop, style = 3 )

# loop over text files from 'pdf'
for( y in start:stop ) 
{
  # update progressbar
  setTxtProgressBar( progbar, y )

  # create filename
  x <- filelist1[ y ]
  pmid <- gsub(".xml", "", x)
  
  filepathA <- paste0( "/mnt/data/live02/stress/hlamberink/grobid/xmlout/", x )
  filepathB <- paste0( "/mnt/data/live02/stress/hlamberink/grobid/xmloutNEW/", x )
  filepath2 <- paste0( "/mnt/data/live02/stress/hlamberink/pdf/text/", pmid, ".txt")
  
  if( file.exists(filepathA) ) {
    txt <- readLines( filepathA )
  } else if( file.exists(filepathB) ) { 
    txt <- readLines( filepathB ) 
  }
  
  txt <- paste( txt, collapse = " " )
  txt <- gsub( "<[^>]+>", ";", txt )
  txt <- gsub( "\t", " ", txt )
  
  txt2 <- readLines( filepath2 )
  txt2 <- paste( txt2, collapse = " " )
  
  checked <- statme( txt )
  checked2 <- statme( txt2 )
  checked <- rbind(checked,checked2)
  rm(checked2)
  checked <- checked[!duplicated(checked$Raw),]
  
  
  checked <- statme( txt )
  checked <- checked[!duplicated(checked$Raw),]
  
  if( !is.null( checked ) ) { 
      summaryz <- summary(checked)
      summaryz <- summaryz[ summaryz$Source == "Total", ]
      summaryz$pmid <- pmid
      summaryz$Source <- NULL
      summaryz$source <- "pdftext"
    } else {
      summaryz <- data.frame( pValues = NA,
                              Errors = NA,
                              DecisionErrors = NA,
                              pmid = gsub(".txt", "", x),
                              source = "pdftext")
    }
  
  if( !is.null( checked ) ) checked <- cbind( pmid, checked )
  
  out1 <- rbind(out1, checked)
  summ1 <- rbind(summ1, summaryz)
  
  
#  write.csv( summ1, "statCheckResults.csv", row.names = FALSE )
#  if( !is.null( out1 ) )  write.csv( out1, "statCheckElaborate.csv", row.names = FALSE )
}



######### SCOPUS == filelist3
###################

# define start and stop for loop
#if( file.exists( "statCheckResults2.csv" ) ){ 
#  summ <- read.csv( "statCheckResults2.csv", stringsAsFactors = FALSE )
#  start <- length( summ[,1] ) + 1
#  
#} else {
#  start <- 1
#}
#print(start)
start <- 1
stop <- length(filelist3)


# load previously gathered data
#if( file.exists( "statCheckElaborate2.csv" ) )
#{
#  out2 <- read.csv( "statCheckElaborate2.csv", stringsAsFactors = FALSE )
#}


# set progress bar
progbar <- txtProgressBar( min = 0, max = stop, style = 3 )

# loop over text files from 'scopus'
for( y in start:stop )
{
  # update progressbar
  setTxtProgressBar( progbar, y )
  
  # create filename
  x <- filelist3[ y ]
  pmid <- gsub(".xml", "", x)
  filepath <- paste0( "/mnt/data/live02/stress/hlamberink/scopus/xmlGrobid/", x )
  filepath2 <- paste0( "/mnt/data/live02/stress/hlamberink/scopus/text/", pmid, ".txt" )
  
  txt <- readLines( filepath )
  txt <- paste( txt, collapse = " " )
  txt <- gsub( "<[^>]+>", ";", txt )
  txt <- gsub( "\t", " ", txt )
  
  txt2 <- readLines( filepath2 )
  txt2 <- paste( txt2, collapse = " " )
  
  checked <- statme( txt )
  checked2 <- statme( txt2 )
  checked <- rbind(checked,checked2)
  rm(checked2)
  checked <- checked[!duplicated(checked$Raw),]
  
  
  if( !is.null( checked ) ) { 
    summaryz <- summary(checked)
    summaryz <- summaryz[ summaryz$Source == "Total", ]
    summaryz$pmid <- pmid
    summaryz$Source <- NULL
    summaryz$source <- "scopus"
  } else {
    summaryz <- data.frame( pValues = NA,
                            Errors = NA,
                            DecisionErrors = NA,
                            pmid = gsub(".txt", "", x),
                            source = "scopus")
  }
  
  if( !is.null( checked ) ) checked <- cbind( pmid, checked )
  
  out2 <- rbind(out2, checked)
  summ2 <- rbind(summ2, summaryz)
  
#  write.csv( summ2, "statCheckResults2.csv", row.names = FALSE )
#  if( !is.null( out2 ) )  write.csv( out2, "statCheckElaborate2.csv", row.names = FALSE )
}



# bind out1 & out2
out <- rbind( out1, out2 )
summ <- rbind( summ1, summ2 )

# summarize
dfsummary <- data.frame( nWithStats = length( unique( out$pmid ) ),
                         nWithError = sum( !is.na(summ$Errors) & summ$Errors > 0 ),
                         nWithDecisionE = sum( !is.na(summ$DecisionErrors) & summ$DecisionErrors > 0 ),
                         nTotalXMLfiles = length( filelist1 ) + length( filelist2 ) + length( filelist3 ),
                         proportionWithStats = length( unique( out$pmid ) ) / ( length( filelist1 ) + length( filelist2) + length( filelist3 ) ) )
print( dfsummary )

# add comparison between reported and computed p
out$pDifference <- abs( out$Reported.P.Value - out$Computed )

# write files
write.csv( dfsummary, "summaryStatcheck3.csv", row.names = FALSE )
write.csv( out, "statCheckElaborate_combined3.csv", row.names = FALSE )
write.csv( summ, "statCheckResults_combined3.csv", row.names = FALSE )


# quit R session
q( save = "no" )