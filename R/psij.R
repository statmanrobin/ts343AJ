library(pracma)

#Function that finds Psi_j weights for a given function.
psij = function(model, inf = "ma", deg = 6, digits = 4){
  
  fit = polyChar(model)
  
  #Multiplying each characteristic polynomials to make 1 Z_t and 1 a_t polynomials
  c_zt = fit[[1]]*fit[[2]]*fit[[3]]*fit[[4]]
  c_at = fit[[5]]*fit[[6]]
  
  #Making the Z_t and a_t polynomials a function
  p1= c_at
  p2= c_zt
  f1=as.function(p1)
  f2=as.function(p2)
  
  
  if(inf == "ma"){
    
    f=function(x) {f1(x)/f2(x)}
    out = rev(round(taylor(f, 0, n = deg),digits))
    out = out[-1]
    return(out)
    
  }
  
  if(inf == "ar"){
    
    f=function(x) {f2(x)/f1(x)}
    out = rev(round(taylor(f, 0, n = deg),digits))
    out = out[-1]
    return(out)
    
  }
  
  
}