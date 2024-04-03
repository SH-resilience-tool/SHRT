dijkstra_gen <- function(g1, dist_mat2,D2, updateProgress){
  require(igraph)
  library(parallel)
  library(doParallel)
  time1<-Sys.time()
  
  results2<-list(0)
  results2_1<-list(0)
  results2_2<-list(0)
  results2_3<-list(0)
  results2_4<-list(0)
  
  pred2<-list(0)
  g_es1<-g1
  
  cl <- makeCluster(4)
  registerDoParallel(cl)
  values_of_n <-1:vcount(g_es1)
  values_of_n1<-floor(vcount(g_es1)/4)
  values_of_n<-1:values_of_n1

  # results2 <- foreach(n = values_of_n) %dopar% {
  #   source("shortest_main.R")
  #   cycle_main(n, g_es1)
  # }
  # source("C:/Users/Cate/Desktop/toy model febbraio/shortest_main.R")
  progress <- 1 / 4
  print(progress)
  updateProgress(progress * 100)
#   
  results2_1 <- foreach(n = values_of_n) %dopar% {
    source("shortest_main.R")
    cycle_main(n, g_es1)
  }

  progress <- 2 / 4
  print(progress)
  updateProgress(progress * 100)

  values_of_n<-values_of_n+values_of_n1
  results2_2 <- foreach(n = values_of_n) %dopar% {
    source("shortest_main.R")
    cycle_main(n, g_es1)
  }
  progress <- 3 / 4
  updateProgress(progress * 100)

  values_of_n<-values_of_n+values_of_n1
  results2_3 <- foreach(n = values_of_n) %dopar% {
    source("shortest_main.R")
    cycle_main(n, g_es1)
  }

  progress <- 1
  print(progress)
  updateProgress(progress * 100)

  values_of_n<-(values_of_n1*3+1):vcount(g_es1)
  results2_4 <- foreach(n = values_of_n) %dopar% {
    source("shortest_main.R")
    cycle_main(n, g_es1)
  }

  results2<-c(results2_1,results2_2, results2_3, results2_4)

  stopCluster(cl)

#   time2<-Sys.time()
#   print(time2-time1) 
# 
#   print("ok")
  nn=1
  for (nn in 1:length(results2)) {
    D2[[nn]] <- results2[[nn]][[2]]
    dist_mat2[,nn]<-results2[[nn]][[1]][,nn]
    pred2[[nn]] <- results2[[nn]][[3]][[nn]]
    # dist_mat2[,nn]<-results2[[nn]][,nn]
  }
return(list(dist_mat2,D2, pred2))
#   
}
