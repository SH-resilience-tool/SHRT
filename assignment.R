
assignment_gen<-function(matrice_od,g1,results3){
  g_es1<-g1
  D2<-results3[[2]]
  pred2<-results3[[3]]
  diag(matrice_od)<-0
  flussi_tot<-cbind(as_edgelist(g_es1,names=FALSE),0,0)
  
  for (w in 1:ncol(matrice_od)){ ####per tutte le destinazioni
    print(w)
    n_hy<-ncol(pred2[[w]])/2
    flussi_tot[,4]<-0
    domanda_o<-0
    domanda_o<-cbind(1:vcount(g_es1),round(matrice_od[,w],0)) #si considera un'origine alla volta, verso tutte le destinazioni
    flussi<-0
    flussi2<-0
    flussi2<-cbind(D2[[w]][,c(1,2)],matrix(0,nrow=vcount(g_es1), ncol=n_hy),pred2[[w]],D2[[w]][,4])
    flussi<-flussi2[order(flussi2[,2], decreasing=TRUE),]
    
    for (c in 1:n_hy){
      for (b in 1:nrow(flussi)){
        flussi[b,2+c]<-flussi[b,2+c]+(domanda_o[which(domanda_o[,1]==flussi[b,1]),2]*flussi[b,2+2*n_hy+c])
        flussi[which(flussi[,1]==flussi[b,2+n_hy+c]),(2+1):(2+n_hy)]<-flussi[which(flussi[,1]==flussi[b,2+n_hy+c]),(2+1):(2+n_hy)]+flussi[b,(2+c)]*flussi[which(flussi[,1]==flussi[b,2+n_hy+c]),(2+2*n_hy+1):(2+2*n_hy+n_hy)]
      }
    }
    
    for (c in 1:n_hy){
      h=0
      for (h in 1:nrow(flussi)){
        ef<-flussi[h,2+n_hy+c]
        ei<-flussi[h,1]
        flussi_tot[which((flussi_tot[,1]==ei)&(flussi_tot[,2]==ef)),4]<-flussi[h,2+c]
      }
      flussi_tot[,3]<-flussi_tot[,3]+flussi_tot[,4]
      flussi_tot[,4]<-0
    }
    # progress <- w / (ncol(matrice_od))
    # updateProgress(progress * 100)
  }
  
  # g_flows<-g_es1
  # E(g_flows)$flows<-flussi_tot[,3]  #assegnare flussi al grafico
  # g_flows2<-g_flows
  # 
  # # ecount(g_flows)
  # edge_attr(g_flows)<-list()
  # E(g_flows)$flows<-flussi_tot[,3]  #assegnare flussi al grafico
  # # ecount(g_flows2)
  # g_flows<-simplify(g_flows, edge.attr.comb="sum")
  # # ecount(g_flows)
  return(flussi_tot)
  
  }
  
