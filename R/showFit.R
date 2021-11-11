# showFit takes the output from an ARIMA model and displays it in the form of the users choice

library(PolynomF)
library(stringr)
library(ts343)

showFit = function(model, form = "list"){
  
  S <- model$arma[5]
  d <- model$arma[6]
  D <- model$arma[7]
  
  delt = round(getdelta(model),3) #Getting the constant
  
  fit = polyChar(model) #runs the model through PolyChar
  
  # Polynomial multiplication for combined
  czt = fit[[1]]*fit[[2]]*fit[[3]]*fit[[4]]
  cat = fit[[5]]*fit[[6]]
  
  # Reading polynomial multiplication as strings
  sczt = as.character(fit[[1]]*fit[[2]]*fit[[3]]*fit[[4]])
  scat = as.character(fit[[5]]*fit[[6]])
  
  # Replaces all the X's with B's
  fczt = str_replace_all(sczt,"x", "B")
  fcat = str_replace_all(scat,"x", "B")
  
  #Option 1: Full List - Returns Polychar model
  if(form == "fullList"){
    return(print(fit, variable = "B"))
  }
  
  #Option 2: List - Returns list of polynomials that are present in the model
  if(form == "list"){
    
    newlist = list()
    
    if(delt != 0){
      for(i in 1:7){
        if(as.character(fit[[i]]) != "1"){
          newlist = append(newlist, fit[i])
        }
      }
    }else{
      for(i in 1:6){
        if(as.character(fit[[i]]) != "1"){
          newlist = append(newlist, fit[i])
        }
      }
    }
    
    return(print(newlist, variable = "B"))
  }
  
  # Option 3: factor - returns a string of characteristic polynomials from an ARIMA model that are in factored form
  if(form == "factor"){
    
    ar = paste0("(",fit[[1]],")")
    sar = paste0("(",fit[[2]],")")
    ma = paste0("(",fit[[5]],")")
    sma = paste0("(",fit[[6]],")")
    
    if(d < 2){
      diff = paste0("(", fit[[3]],")")
    } else{
      diff = paste0("(1 - B)^",d," ")
    }
    
    if(D < 2){
      sdiff = paste0("(", fit[[4]],")")
    }else{
      sdiff = paste0("(1 - B^",S,")^",D," ")
    }
    
    if(delt != 0){
      s = paste0(ar,sar,diff,sdiff,"Z_t =",delt,"+",ma,sma,"a_t")
    }else{
      s = paste0(ar,sar,diff,sdiff,"Z_t =",ma,sma,"a_t")
    }
    
    new = str_remove_all(s, "\\(1\\)" )
    new2 = str_replace_all(new,"x", "B")
    
    return(print(new2))
  }
  
  # Option 4: Combines all the polynomials on each side of the fitted model equation
  if(form == "combine"){
    
    if(delt != 0){
      return(print(paste0("(", fczt,") Z_t =",delt,"+(", fcat,")a_t")))
    }else{
      return(print(paste0("(", fczt,") Z_t = (", fcat,")a_t")))
    }
    
  }
  
  # Option 5: Moves the auto-regressive polynomial to the right side of the equals sign, in the form Z_t = ...
  if(form == "rhs"){
    
    rhsz = -1*czt + 1
    
    srhsz = as.character(rhsz)
    frhsz = str_replace_all(srhsz,"x", "B")
    
    if(delt != 0){
      return(print(paste0(" Z_t =",delt,"+(",frhsz,")Z_t + (", fcat,")a_t")))
    }else{
      return(print(paste0(" Z_t = (",frhsz,")Z_t + (", fcat,")a_t")))
    }
    
  }
  
}