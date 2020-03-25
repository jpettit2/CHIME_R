library(shiny)
library(dplyr)
library(zoo)
library(plotly)
library(DT)




sir <- function(S, I, R, beta, gamma_p, N,day=i){
  
  Sn = (-beta * S * I) + S
  In = (beta * S * I - gamma_p * I) + I
  Rn = gamma_p * I + R
  
  if (Sn < 0){ Sn = 0 }   
  if(In < 0){In = 0}
  if (Rn < 0){ Rn = 0}

  scale = N/(Sn +In + Rn)
  #print(scale)
  return(data.frame(Day = day, Sn =Sn * scale, In= In * scale, Rn =Rn * scale))
}