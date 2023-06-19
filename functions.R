# mean absolute error
mae =  function(yhold,
                yfore,
                text = NA,
                round.digit = 4) {
  efore = yhold - yfore 
  mae =  mean(abs(efore))
  if (is.na(text)) {
    return(paste("MAE is", round(mae, round.digit), sep = " "))
  } else {
    return(paste(text, round(mae, round.digit), sep = " "))
  }
}

# mean absolute percentage error
mape = function(yhold,
                 yfore,
                 text = NA,
                 round.digit = 4) {
  efore = yhold - yfore 
  mape =  mean(abs(efore / yhold))
  if (is.na(text)) {
    return(paste("MAPE is", round(mape * 100, round.digit), sep = " "))
  } else {
    return(paste(text, round(mape * 100, round.digit), sep = " "))
  }
}

# Model Selection Criteria - DIC, WAIC, PSBF and PIT
model.selection.criteria <- function(inla.result, plot.PIT = FALSE,
                                     n.train) {
  dic <- inla.result$dic$dic
  waic <- inla.result$waic$waic
  cpo <- inla.result$cpo$cpo
  #psbf <- sum(log(cpo[1:n.train]))
  PIT <- inla.result$cpo$pit
  msc <- cbind(DIC=dic, WAIC = waic)
  #PsBF = psbf
  if(isTRUE(plot.PIT)){
    pit.hist <- hist(PIT, plot = F)
    return(list(msc = msc, hist=plot(pit.hist, main ="")))
  } else{
    return(msc = msc)
  }
  
}
