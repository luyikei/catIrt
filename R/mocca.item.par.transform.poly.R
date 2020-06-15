mocca.item.par.transform.poly <- function(item_par){
  for(i in 1:nrow(item_par)){
    if(item_par[i,1]==0){
      item_par[i,] <- item_par[i,]
    } else {
      item_par[i,2] <- -1*item_par[i,2]
      item_par[i,3] <- -1*item_par[i,3]
    }
  }
  return(item_par)
}