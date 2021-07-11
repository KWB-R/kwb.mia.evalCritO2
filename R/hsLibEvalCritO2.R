# hsPlotAllToPdf ---------------------------------------------------------------

#' Plot All To PDF File
#' 
#' Print result of evaluation in forms of barplots into pdf file
#' 
#' @param strPdf path to PDF file to which to plot
#' @param \dots further arguments passed to \code{\link{hsPlotAll}}
#' @export
hsPlotAllToPdf <- function(strPdf, ...) 
{
  kwb.utils::hsPrepPdf(strPdf) # Open pdf device
  hsPlotAll(...)   
  grDevices::dev.off() # Close pdf device
}

# hsPlotAll --------------------------------------------------------------------

#' Plot All
#' 
#' Plot result of evaluation in forms of barplots, with default settings
#' 
#' @param strDb path to Microsoft Access Database file
#' @param strTable name of database table containing the data to plot
#' @param myScaled logical. Default: \code{TRUE}
#' @param myReverse logical. Default: \code{TRUE} 
#' @param myBeside if \code{TRUE} (the default), bars are plotted side by side
#' @param myCexNames character expansion factor for names
#' @param mySub subtitle. Default: ""
#' @param dbg if \code{TRUE}, debug messages are shown
#' @param myWidth bar width. Default: 1
#' @param myValLabs logical. Default: \code{FALSE}
#' @param yearsInSub logical. Default: \code{FALSE}
#' @param lng language code, one of "en" (English, the default) or "de" (German)
#' @param \dots further arguments passed to \code{\link{hsPlotCritEvents}}
#' @export
hsPlotAll <- function(
  strDb, 
  strTable, 
  myScaled = TRUE, 
  myReverse = TRUE, 
  myBeside = TRUE, 
  myCexNames = 1, 
  mySub = "",
  dbg = FALSE, 
  myWidth = 1, 
  myValLabs = FALSE,
  yearsInSub = FALSE,
  lng = "de",
  ...
) 
{
  frm <- kwb.db::hsGetTable(mdb = strDb, tbl = strTable)

  # create "lookup table" mp for names of monitoring points
  mp <- NULL # c("7.2" = "TEG", "8.55" = "CAP", "12.79" = "BEL", "17.54" = "MUE")
  
  # Create "Year x km"-matrices containing number of events/calendar days
  matLE <- hsGroupByYearAndKm(frm, "LamEvents",  vecMp = mp)
  mat2E <- hsGroupByYearAndKm(frm, "2mgEvents",  vecMp = mp)
  matLD <- hsGroupByYearAndKm(frm, "LamKalTage", vecMp = mp)
  mat2D <- hsGroupByYearAndKm(frm, "2mgKalTage", vecMp = mp)
  
  # Create list of matrices
  lstMat <- list(matLE, mat2E, matLD, mat2D)      

  if (dbg) print(lstMat)
  
  # Set graphic parameters and reset on exit
  # mfrow: Prepare grid of 2 x 2 plots
  # mar: original margins plus 1 more on top
  # xpd: to allow legend to be outside of the plot region
#  opar <- par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(8,4,5,2), xpd = TRUE)
  
  opar <- graphics::par(mfrow=c(2,2), oma=c(2,0,2,0))
  on.exit(graphics::par(opar))
  
  # Plot event numbers and numbers of calendar days  
  vecTitles <- c(
    hsTranslate("Events with suboptimal conditions", lng),
    hsTranslate("Events with critical conditions", lng),
    hsTranslate("Calendar days with suboptimal conditions", lng),
    hsTranslate("Calendar days with critical conditions", lng))

  vecYLab <- character()
  vecYLab[1:2] <- hsTranslate("Number of events", lng)
  vecYLab[3:4] <- hsTranslate("Number of calendar days", lng)

  for (i in 1:4) {
    # Lammersen events
    strXLab <- "" # "Monitoring point (Spree km)" 
    hsPlotCritEvents(
      lstMat[[i]], 
      strTitle    = vecTitles[i], 
      mySub       = "", 
      boolXScaled = myScaled, 
      boolDescKm  = myReverse, 
      boolDescYr  = FALSE, 
      myBeside    = myBeside, 
      myWidth     = myWidth, 
      myCexNames  = myCexNames,
      myAxis      = FALSE, 
      myValLabs   = myValLabs,
      dbg         = dbg, 
      xlab        = "Spreekilometer", 
      ylab        = vecYLab[i],
      yearsInSub  = yearsInSub,
      ...
    )
  }
  graphics::title(mySub, outer=TRUE)
  kmtext <- list("Schleuse M\u00FChlendamm" = 17.5, 
                 "Monbijoubr\u00FCcke" = 16.3,
                 "Abzweig BSSK" = 14.6,
                 "Bellevue" = 12.8,
                 "Gotzkowskybr\u00FCcke" = 10.3,
                 "Caprivibr\u00FCcke" =  8.5,
                 "Tegeler Weg" =  7.2,
                 "Schleuse Charlottenburg" = 6.3)

  nowtext <- sprintf("Erstellt: %s", format(Sys.time(), "%d.%m.%Y %H:%M"))
  legtext <- paste(sub("\\.", ",", sprintf("km %0.1f", as.double(unlist(kmtext)))), 
                   names(kmtext), sep = ": ", collapse = "; ")
  
  if (any(frm$Spree_km != 0)) 
    graphics::mtext(side=1, line = 0, legtext, outer=TRUE, cex = 0.7)
  
  graphics::mtext(side=1, line = 1, nowtext, outer=TRUE, cex = 0.7)
}

# hsGroupByYearAndKm -----------------------------------------------------------

#' Group By Year And Km
#' 
#' Groups input data.frame by its columns "Jahr" and "Spree_km".
#' 
#' @param frmData data.frame with columns "Jahr" and "Spree_km"
#' @param strValField Name of value field in input data.frame of which for each Jahr/Spree_km 
#'   group the sum will be calculated.
#' @param boolDescKm if TRUE, columns in result matrix will be ordered according to decreasing
#'   "Spree_km" values 
#' @param boolDescYr if TRUE, columns in result matrix will be ordered according to decreasing
#'   "Jahr" values
#' @param vecMp Vector containing pairs of km value and monitoring point name
#' 
#' @return Returns the matrix calculated by hsGroupBy2Fields()
#' @export
hsGroupByYearAndKm <- function(
  frmData, 
  strValField, 
  boolDescKm=FALSE, 
  boolDescYr=FALSE,
  vecMp=NULL
) 
{
  mat <- kwb.base::hsGroupBy2Fields(frmData, strValField, "Jahr", "Spree_km", 
    boolDescYr, boolDescKm)

  # Append name of monitoring point to km value
  if (! is.null(vecMp)) {
    cn <- colnames(mat)
    for (i in 1:length(cn)) {
      #@2011-12-19: use %in% instead of hsContains!
      #if (hsContains(names(vecMp), cn[i])) {
      if (cn[i] %in% names(vecMp)) {
        mp <- vecMp[[cn[i]]]
      }
      else {
        mp <- "" # "???"
      }
      colnames(mat)[i] <- paste(colnames(mat)[i], mp, sep=";")
    }
  }
  mat
}

# hsReformatName ---------------------------------------------------------------

#' Reformat Name
#' 
#' Label of the form "[<station>: ]km ##.#" is created from \emph{myName} where
#'   the part in brackets (station name) is optional.
#'   The part following a semicolon in \emph{myName} is treated as the station 
#'   name. 
#'   
#' @keywords internal
hsReformatName <- function(myName) 
{
  kmName <- strsplit(myName, ";")
  myName <- ""
  if (length(kmName[[1]]) > 1) {
    myName <- sprintf("%s: ", kmName[[1]][2])
  }
  #sprintf("%skm %4.1f", myName, as.double(kmName[[1]][1]))
  
  #@2013-01-25: only the number!
  sub("\\.", ",", sprintf("%4.1f", as.double(kmName[[1]][1])))
}

# hsReorderMatrix --------------------------------------------------------------

#' Reorder Matrix Columns
#' 
#' @keywords internal
hsReorderMatrix <- function
(
  matData, 
  boolXScaled, 
  boolDescKm, 
  boolDescYr,
  vecKm, 
  vecYr, 
  dbg = FALSE
) 
{
  # Reorder matrix columns always according to increasing values!?
  if (dbg) {
    cat("In hsReorderMatrix:\n")
    cat(sprintf("boolXScaled = %s\n", boolXScaled))
    cat(sprintf("boolDescKm  = %s\n", boolDescKm))
    cat(sprintf("boolDescYr  = %s\n", boolDescYr))
    cat(sprintf("vecKm: %s\n", paste(vecKm, collapse=",")))
    cat(sprintf("vecYr: %s\n", paste(vecYr, collapse=",")))
    cat(sprintf("class of matData: %s\n", class(matData)))
  }

  # If we do not use x positions, we have to arrange the columns according
  # to the km values (ascending or descending); otherwise we always order
  # by ascending km values and the reversion will be done in hsBarplot
  if (!boolXScaled) {
    if (boolDescKm) {
      matData <- matData[ , order(- vecKm), drop = FALSE]
    }
    else {
      matData <- matData[ , order(  vecKm), drop = FALSE]
    }
  } 
  else {
      matData <- matData[ , order(  vecKm), drop = FALSE]
  }

  # Order matrix rows by year if there is more than one row
  # If x axis is reverse, sorting direction is also reversed
  if (!is.null(nrow(matData))) { 
    if ((boolDescKm && !boolDescYr) || (!boolDescKm && boolDescYr)) {
      # Sort descending    
      matData <- matData[order(- vecYr), , drop = FALSE]  
    }
    else {
      # Sort ascending
      matData <- matData[order(  vecYr), , drop = FALSE]  
    }
  }

  if (dbg) {
    cat("matData ordered:\n")
    print(matData)
    cat(sprintf("class of matData ordered: %s\n", class(matData)))
  }  

  # Return the matrix
  matData
}

# hsNextHighest ----------------------------------------------------------------

#' Next Highest Number for Nice Labelling
#' 
#' @keywords internal
hsNextHighest <- function(x) 
{
  if (x == 0) {
    return(1)
  }
  divisor <- 10^(as.integer(log(x, base = 10))) / 2
  cat(sprintf("divisor: %f\n", divisor))  
  if (x %% divisor == 0) {
    x
  }
  else {
    divisor * (as.integer(x / divisor) + 1)
  }
}

# hsGetYmax --------------------------------------------------------------------

#' Get Ymax
#' 
#' Next highest multiple of appropriate powers of ten, but at least 5
#' 
#' @keywords internal
hsGetYmax <- function(
  matData, 
  myBeside, 
  myExpand = 1.1, 
  dbg = FALSE
) 
{
  if (myBeside) {
    myYmax <- max(matData)
  }
  else {
    myYmax <- max(colSums(matData))
  }

  # Next highest multiple of appropriate powers of ten, but at least 5
  myYmax <- max(hsNextHighest(myExpand * myYmax), 5)
  
  if (dbg) {
    cat(sprintf("myYmax = %f\n", myYmax))
  }
  
  # Return the maximum y value
  myYmax  
}

# hsGrayCodes ------------------------------------------------------------------

#' Gray Codes
#' 
#' Sequence of numbers between 0 and 1 to be used as gray intensities
#' 
#' @keywords internal
hsGrayCodes <- function(n, dbg = FALSE) 
{
  # If there is only one gray code requested, return medium gray
  if(n == 1) {
    vecGray <- 0.5
  }
  else {
    # otherwise return a sequence of gray codes equally distributed in (0,1)
    vecGray <- seq(0, 1, length.out = n)
  }
  
  if (dbg) cat(sprintf("gray codes: %s\n", paste(vecGray, collapse=",")))

  # Return the gray code(s)
  vecGray  
}

# hsFullSubTitle ---------------------------------------------------------------

#' Full Subtitle
#' 
#' Given a vector of year numbers a title "of year <year>" 
#'   or "of years <startYear> to <endYear> is returned
#' 
#' @keywords internal
hsFullSubTitle <- function(mySub, vecYr) 
{
  if (length(vecYr) == 1) {
    strPeriod <- paste("of year", vecYr[1])
  }
  else {
    strPeriod <- paste("of years", min(vecYr), "to", max(vecYr))
  }

  # Return the sub-title
  paste(mySub, strPeriod)  
}

# hsAddLegend ------------------------------------------------------------------

#' Add Legend to Plot
#' 
#' @keywords internal
hsAddLegend <- function(matData, vecGray, boolDescKm, boolDescYr) 
{
  myNames <- rownames(matData)
  myCols <- grDevices::gray(vecGray)
  if ((boolDescKm && !boolDescYr) || (!boolDescKm && boolDescYr)) {
    myNames <- rev(myNames)
    myCols <- rev(myCols)
  }
  cat("myNames: ")
  print(myNames)
  cat("myCols: ")
  print(myCols)
  
  # xpd = TRUE, all plotting is clipped to the figure region
  graphics::legend("top", legend = myNames, fill = myCols, horiz = TRUE, 
    box.lty = 0, inset = -0.2, xpd = TRUE)
}

# hsPlotCritEvents -------------------------------------------------------------

#' Plot Result of Evaluation as Barplots
#' 
#' @keywords internal
hsPlotCritEvents <- function(
  matData, 
  strTitle    = "Events", 
  mySub       = "", 
  boolXScaled = FALSE, 
  boolDescKm  = TRUE, 
  boolDescYr  = FALSE, 
  myBeside    = TRUE, 
  myWidth     = 1, 
  myYmax      = -1, 
  myCexNames  = -1, 
  myAxis      = TRUE, 
  myValLabs   = FALSE, 
  dbg         = FALSE, 
  yearsInSub  = FALSE,
  xlab = "",
  ...
) 
{
  # Vector of distinct km values; Additional information ";<MP>" removed
  vecKm <- as.numeric(sub(";.*", "", colnames(matData)))
  
  # Vector of distinct years
  vecYr <- as.numeric(rownames(matData))
  
  # if there is only one Spree-km, call the standard barplot function
  if (length(vecKm) == 1) {
    vals <- matData[, 1]
    bp <- graphics::barplot(vals, names.arg = vecYr, las=2, ...)
    graphics::text(bp, vals, labels = vals, pos = 1)
  }
  else {
    
    # Prepare the bar labels. In matData, the columns are named in the form of
    # "<km-value>;<MP>" where <km-value> is the km-value and <MP> is the acronym 
    # of the monitoring point (if any). Here we create labels of the form: 
    # "[<MP>:] km <km-value>", where the "<MP>:"-part is optional.
    myNames <- colnames(matData)
    for (i in (1:length(myNames))) {
      myNames[i] <- hsReformatName(myNames[i])
    }
        
    # Reorder matrix columns (km) and rows (years) by their (numerically 
    # interpreted) names
    matData <- hsReorderMatrix(matData, boolXScaled, boolDescKm, boolDescYr,
                               vecKm, vecYr, dbg = TRUE)
    
    # If no maximum y value is given, find an appropriate value
    if (myYmax == -1) {
      myYmax <- hsGetYmax(matData, myBeside, myExpand = ifelse(myBeside, 1.1, 1),
                          dbg = dbg)
    }  
    
    # Gray codes
    vecGray <- hsGrayCodes(length(vecYr), dbg)
    
    # If bars are to be arranged according to descending km values we have 
    # to reverse the names, too! 
    # @2011-11-22: But only if the x axis is not scaled!
    if (boolDescKm && ! boolXScaled) 
      myNames <- rev(myNames)
    
    if (length(myNames) == 1 && sub(",", ".", myNames)[1] == " 0.0") {
      myNames <- ""
      xlab <- ""
    }
    
    # If position of bars is to be scaled according to the km-value set the
    # positions to the km-values here
    myPosition <- NULL
    if (boolXScaled) 
      myPosition <- vecKm
    
    # Set default scaling factor for bar labels
    if (myCexNames == -1)
      myCexNames <- ifelse(boolXScaled, ifelse(myBeside, 0.85, 0.9), 1)        
    
    # Call my personal barplot function that allows to locate the bars at 
    # proper x positions
    if (TRUE) {
      kwb.barplot::hsBarplot(
        myHeight   = matData,
        myWidth    = myWidth,
        myPosition = myPosition,
        myYlim     = c(0,myYmax),
        myReverse  = boolDescKm,
        myBeside   = myBeside,
        dbg    = dbg,
        myAxis     = myAxis,
        myValLabs  = myValLabs,
        col        = grDevices::gray(vecGray),
        names.arg  = myNames,
        cex.names  = myCexNames,
        las        = 2,
        xlab = xlab,
        #   axis.lty   = 1, 
        #   main       = strTitle,
        ...
      )
    }
    else {
      hsBarplot2(
        myHeight   = matData,
        myPos      = myPosition,
        myWidth    = myWidth,
        myBeside   = myBeside,
        dbg        = dbg,
        dx         = myWidth,
        names.arg  = myNames,
        cex.names  = myCexNames,
        las        = 2,
        ylim       = c(0, myYmax),      
        col        = grDevices::gray(vecGray),
        ...
      )  
    }    
  }
  
  # Put main title and sub-title
  graphics::mtext(strTitle, 3, line = 2)
  
  #@2012-10-24;HS
  if (mySub != "") {
    if (yearsInSub) {
      mySub <- hsFullSubTitle(mySub, vecYr)
    }
    graphics::mtext(mySub, 3, line = 2, cex = 0.8)    
  }
  
  # Add a legend to the plot
  # inset: e.g. -0.1 -> 10% of plot height above the plot

#  if (! is.null(rownames(matData))) {  
#  if (nrow(matData) > 1) {
  if (length(unique(vecYr)) > 1 && length(vecKm) > 1) {
    cat("adding legend... ")  
    hsAddLegend(matData, vecGray, boolDescKm, boolDescYr)
    cat("ok.\n")    
  }
#  }
#  }

  # Add a grid, but without allowing to leave the plot area
  graphics::par(xpd = FALSE)
  graphics::grid(nx = NA, ny = NULL)

  # Add a horizontal line at x = 0
  graphics::abline(h = 0)
  graphics::par(xpd = TRUE)  
}

# hsTranslate ------------------------------------------------------------------

#' Translate
#' 
#' translates \emph{text.en} into target language \emph{lng}.
#' 
#' @param text.en english text (character vector of length 1 expected)
#' @param lng target language: en = English, de = German
#' @export
hsTranslate <- function(text.en, lng)
{
  map <- list()
  map[["de"]] <- list(
    "suboptimal conditions" = "suboptimale Bedingungen",
    "critical conditions" = "kritische Bedingungen",
    "Number of events" = "Anzahl Ereignisse",
    "Number of calendar days" = "Anzahl Kalendertage",
    "Events with suboptimal conditions" = "Ereignisse mit suboptimalen Bedingungen", 
    "Events with critical conditions" = "Ereignisse mit kritischen Bedingungen", 
    "Calendar days with suboptimal conditions" = "Kalendertage mit suboptimalen Bedingungen",
    "Calendar days with critical conditions" = "Kalendertage mit kritischen Bedingungen",
    "based on simulations", "basierend auf Gewaesserguetesimulation")

  ### return text itself if target language is English
  if (lng == "en") {
    text.en
  }
  else {
    map[[lng]][[text.en]]
  }
}

# hsPlotO2Eval -----------------------------------------------------------------

#' Plot O2 Evaluation
#' 
#' @param dat data frame with columns \emph{Jahr}, \emph{LamEvents}, \emph{2mgEvents},
#'   \emph{LamKalTage}, \emph{2mgKalTage}
#' @param main \code{main} plot title
#' @param lng language code, one of "en" (English, the default) or "de" (German)
#' @export
hsPlotO2Eval <- function(dat, main = "Title?", lng = "en") 
{
  # save default, for resetting
  def.par <- graphics::par(no.readonly = TRUE) 
  
  # Prepare data for plots
  heights <- list(
    matrix(c(dat[, "LamEvents"], dat[, "2mgEvents"]), nrow = 2, byrow=TRUE),
    matrix(c(dat[, "LamKalTage"], dat[, "2mgKalTage"]), nrow = 2, byrow=TRUE))
  
  # Prepare layout of plot with upper plot area for title and legend only
  nf <- graphics::layout(matrix(1:3, ncol = 1, byrow = TRUE), heights=c(1, 4, 4))
  
  # 1st plot: Title and legend
  graphics::par(mar = c(0, 0, 4, 0))
  graphics::plot.new()
  graphics::mtext(main, side = 3, line = 2)
  graphics::legend("top", horiz = TRUE,
         legend = c(hsTranslate("suboptimal conditions", lng),
                    hsTranslate("critical conditions", lng)), 
         fill = grDevices::gray.colors(2))  
  graphics::par(mar = c(3.1, 4.1, 3.1, 2.1))
  
  # 2nd and 3rd plot: number of events/calendar days
  for (i in 1:2) {
    ylab <- ifelse(i == 1, 
                   hsTranslate("Number of events", lng),
                   hsTranslate("Number of calendar days", lng))
    graphics::barplot(heights[[i]], 
            beside = TRUE, 
            ylab = ylab,
            main = ylab,
            names.arg = dat$Jahr)
    graphics::grid(nx = NA, ny = NULL)
    graphics::abline(0, 0)    
  }
  
  # reset graphical parameters to default
  graphics::par(def.par)
}

# hsPlotO2EvalPdf --------------------------------------------------------------

#' Plot Result of O2 Evaluation to PDF
#' 
#' @param dat data frame with columns \emph{Jahr}, \emph{LamEvents}, \emph{2mgEvents},
#'   \emph{LamKalTage}, \emph{2mgKalTage}
#' @param main \code{main} plot title
#' @param pdffile path to PDF file to which to plot
#' @export
hsPlotO2EvalPdf <- function(dat, main = "Title?", pdffile = NULL) 
{
  if (is.null(pdffile)) {
    pdffile <- kwb.utils::hsPrepPdf(boolLandscape=FALSE)
  } else {
    kwb.utils::hsPrepPdf(pdffile, boolLandscape=FALSE)
  }
  hsPlotO2Eval(dat, main)
  grDevices::dev.off()
  kwb.utils::hsShowPdf(pdffile)
}
