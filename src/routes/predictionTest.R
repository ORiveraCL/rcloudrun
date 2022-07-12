library(tidyverse)
library(lubridate)

#* @post /super_calculo
function(req, res){
  numero <- req$body
  number <- c(numero$number)
  superMegaSuma(number) -> body
  body
}

superMegaSuma <- function(body){

  suma <- body + 10
}