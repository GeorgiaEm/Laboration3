##### Lab 3 - Advanced R programming #####
#Function 1
euclidean <- function(a,b){

gcd <- min(a,b)

if(gcd != 0){

while (a%%gcd !=0 | b%%gcd !=0){
  gcd <- gcd-1
  
}
} else gcd <- max(a,b)

return(gcd)

}

#Function 2
dijkstra<-function(data,init_node){
  
  n <- length(unique(wiki_graph[,1]))
  
  
  dist <- matrix(999, n, 1)
  
  colnames(dist) <- init_node
  rownames(dist) <- unique(wiki_graph[,1])
  
  
  dist[colnames(dist)==rownames(dist),] <- 0
  
  
  
  
  solved<-rep(NA,n)
  j <-1
  solved[j] <- init_node
  
  
  while(NA%in%solved){
    v1 <- wiki_graph[,1][wiki_graph[,1]%in%solved]
    v2 <- wiki_graph[,2][wiki_graph[,1]%in%solved]
    w <- wiki_graph[,3][wiki_graph[,1]%in%solved]
    
    res <- cbind(v1,v2,w)
    res <- res[!v2%in%solved,]
    
    sum <- rep(0,length(res[,1]))
    for (i in 1:length(sum)){
      sum[i] <- dist[rownames(dist)==res[i,1],1]
    }
    
    summa<-sum+res[,3]
    
    
    
    v1 <- res[,1]
    v2 <- res[,2]
    w <- summa
    
    
    
    dist[rownames(dist)==v2[which.min(w)],] <- w[which.min(w)]
    j <- j+1
    solved[j] <- v2[which.min(w)]
  }
  return(dist)
  
}



