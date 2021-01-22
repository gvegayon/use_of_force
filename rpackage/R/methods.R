#' @export
#' @param odds Logical scalar. When TRUE it prints the odds ratios.
#' @param labels Named vector. Changes the labels of the model.
print.clogit_perm <- function(x, odds = TRUE, labels = NULL, out = "ascii", ...) {

  cis <- t(confint(x))

  space_fmt <- if (out == "ascii") {
    c("** ", "*  ", "   ", "< 0.01", "  %.2f", "\n")
  } else {
    c(
      "**\\hphantom{*}",
      "*\\hphantom{**}",
      "\\hphantom{***}",
      "$< 0.01$",
      "$\\hphantom{< }%.2f$",
      "\\\\\n"
    )
  }

  dat <- cbind(Coef = coef(x), cis, pval = x$pvals)
  if (odds)
    dat[,1:3] <- exp(dat[,1:3])

  if (!is.null(labels))
    rownames(dat) <- labels[rownames(dat)]

  # Checking the max rowname
  maxtit <- max(nchar(rownames(dat)))
  main_txt <- if (out == "ascii") {
    paste(sprintf("%%%ds", maxtit), " %9.2f%s [%5.2f, %5.2f] %s")
  } else  {
    paste(sprintf("%%%ds", maxtit), "& $%9.2f^{%s}$ & $[%5.2f, %5.2f]$ & %s")
  }

  dat <- sprintf(
    main_txt,
    rownames(dat),
    dat[,"Coef"],
    ifelse(dat[,4] <= .01, "***",
           ifelse(dat[,4] <= .05, space_fmt[1],
                  ifelse(dat[,4] <= .1, space_fmt[2], space_fmt[3]))),
    dat[,2],
    dat[,3],
    ifelse(dat[,4] <= .01, space_fmt[4], sprintf(space_fmt[5],dat[,4]))
  )

  # Dealing with others
  gof.names   <- c("N events", "N perm", "N", "AIC", "BIC")
  gof         <- c(x$fit$nevent, nrow(x$coefs), x$fit$n, stats::AIC(x$fit), stats::BIC(x$fit))
  cat("\nCONDITIONAL LOGIT (WITH PERMUTATION)\n")
  cat(sprintf("%10s: %d", gof.names[1:3], gof[1:3]), sep = "\n")
  cat(sprintf("%10s: %.2f", gof.names[4:5], gof[4:5]), sep = "\n")
  cat("MODEL PARAMETERS:\n")
  cat(paste(dat, collapse = space_fmt[6]))
  cat("\n")
  invisible(x)

}
