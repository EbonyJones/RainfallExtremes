#Congruence function
congruence <- function(x,y){
  test.stat = sum(x*y, na.rm = T)/sqrt(sum(x^2, na.rm = T)*sum(y^2, na.rm = T))
  return(test.stat)
}
