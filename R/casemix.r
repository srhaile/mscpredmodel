#' @title Summary of case-mix for key moderators
#' 
#' @importFrom stats aggregate quantile as.formula
#' @import ggplot2
#' 
#' @description Examines case-mix according to key moderators, by study.
#' 
#' @param bss output from \code{\link{get_bs_samples}}
#' @param mods Which variables should be examined, of the \code{mods} in \code{bss}? (default NULL, means all)
#' @param max.levels If a numeric variable has fewer than \code{max.levels} distinct values, than it will be displayed as a categorical / factor variable. If a categorical variable 
#' @param plot if TRUE (the default) then summary plots are produced. If not, tables of summary statistics are returned.
#' 
#' @examples 
#' dat <- msc_sample_data()
#' dat$agegrp <- cut(dat$age, c(0, 20, 40, 60, 80, 100))
#' dat$agegrp2 <- cut(dat$age, seq(0, 100, 5))
#' dat$agegrp3 <- as.numeric(dat$agegrp2)
#' bssamp <- get_bs_samples(dat, id, study, outcome, n.samples = 10, 
#'                   scores = c("a", "b", "c", "d", "e", "f"), 
#'                   moderators = c("age", "agegrp", "agegrp2", "agegrp3", "female", "sex", "x1"))
#' casemix(bssamp)
#' casemix(bssamp, plot = FALSE)
#' casemix(bssamp, mods = c("agegrp2", "agegrp3"), max.levels = 10)   
#' 
#' @export             
casemix <- function(bss, mods = NULL, max.levels = 20, plot = TRUE){
  if(is.null(mods)){
    mods <- bss$mods
  }
  
  check.mods <- mods %in% bss$mods
  if(any(!check.mods)){
    warning("Following mods were not in bss: ", 
            paste(mods[!check.mods], collapse = ", "), "\n", 
            "Keeping only: ", paste(mods[check.mods], collapse = ", "))
    mods <- mods[check.mods]
    if(length(mods) == 0) mods <- bss$mods
  }
  
  orig.mods <- lapply(bss$orig.sample, `[`, c("cohort", mods))
  datmods <- do.call("rbind", orig.mods)
  if(length(mods) > 1){
    k <- apply(datmods[, mods], 2, function(x) length(table(x)))
  } else {
    k <- length(table(datmods[, mods]))
  }
  
  modclass <- sapply(datmods[, mods], class)
  num.fct <- mods[(k <= max.levels) & (modclass %in% c("integer", "numeric"))]
  if(length(num.fct) > 0)     message("Variables(s) ", paste(num.fct, collapse = ", 
          "), " are numeric but have few unique values. These will be converted to factors.")
  for(ii in num.fct){
    datmods[, ii] <- factor(datmods[, ii])
  }
  modclass <- sapply(datmods[, mods], class)
  choosefn <- ifelse(modclass %in% c("integer", "numeric"), "num", "fct")
  check.levels <- (k > max.levels) & (modclass == "factor")
  if(any(check.levels)){
    message("Variable(s) ", mods[check.levels], " are factors but have many levels. Should these be recoded?")
  }
  if(is.numeric(datmods$cohort)) datmods$cohort <- factor(datmods$cohort, unique(datmods$cohort))
  
  ## get numeric summaries
  numsumm <- function(x){
    out <- quantile(x, p = 1:3/4)
    names(out) <- c("q1", "median", "q3")
    out
  }
  fctsumm <- function(x){
    prop.table(table(x))
  }
  
  ## plot numeric summaries using ggplot
  numplot <- function(y = "age"){
    sym.y <- sym(y)
    ggplot(aes(.data$cohort, !!sym.y), data = datmods) + 
      geom_boxplot() + ggtitle(y)
  }
  
  fctplot <- function(y = "female"){
    sym.y <- sym(y)
    ggplot(aes(.data$cohort, fill = !!sym.y), data = datmods) +
      geom_bar(position = "fill") + 
      scale_fill_viridis_d() + 
      ylab("proportion of subjects") + ggtitle(y)
  }
  
  
  fms <- paste(mods, "~ cohort")
  aggmods <- plotmods <- vector("list", length(mods))
  names(aggmods) <- names(plotmods) <- mods
  
  for(i in 1:length(mods)){
    if(!plot){
      this.fm <- paste(mods[i], "~ cohort")
      this.fun <- switch(choosefn[i],
                         num = numsumm,
                         fct = fctsumm)
      aggmods[[i]] <- aggregate(as.formula(this.fm), data = datmods, this.fun) 
    }
    if(plot){
      this.plot <- switch(choosefn[i],
                          num = numplot,
                          fct = fctplot)
      plotmods[[i]] <- this.plot(mods[i]) 
    }
  }
  
  if(plot){
    out <- plotmods
  } else {
    out <- aggmods
  }
  out
}

