##### Lab 3 - Advanced R programming #####

euclidean <- function(a,b){

gcd <- min(a,b)

if(gcd != 0){

while (a%%gcd !=0 | b%%gcd !=0){
  gcd <- gcd-1
  
}
} else gcd <- max(a,b)

return(gcd)

}






