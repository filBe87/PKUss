### =========================================================================
### cor.plot
### =========================================================================
#' Make plot to investigate correlation structure
#'
#' @param data data.frame with predictor variables to be considered
#' @return A pretty overview plot
#' @author Taken from ecospat package & modified by Philipp Brun
#' @export
#' @examples
#'
#' # cor.plot of some data
#'
#'cor.plot(obs.sel[,18:24])
#'
cor.plot=function(data){

  # thin out data if more than 1000 obervations are used
  if(nrow(data)>1000){
    data=data[sample(1:nrow(data),1000),]
  }

  # Define upper triangle function
  panel.smooth=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                       cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
  {
    points(x, y, pch = 16, col = "#00000020")
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth, ...)
  }

  # Define histogram function
  ## put histograms on the diagonal
  panel.hist <- function(x, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="#bdbdbd",border="#636363",...)
  }

  # Define correlation value text function
  panel.cor=function(x,y,...){
    cr=round(cor(x,y,use="na.or.complete"),digits=2)
    colo=ifelse(abs(cr)<0.7,"#31a354",ifelse(abs(cr)>0.85,"#de2d26","#ec7014"))
    xmid=mean(par("usr")[1:2])
    ymid=mean(par("usr")[3:4])
    cx=2.5*abs(cr)*5/par("mfrow")[1]+.5
    text(xmid,ymid,cr,col=colo,cex=cx,font=2)
  }

  pairs(data, lower.panel = panel.smooth, diag.panel = panel.hist,
        upper.panel = panel.cor)
}

