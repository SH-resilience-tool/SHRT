cycle_main<-function(n, g_es1) {
  require(igraph)
  PEN<-5
  pred2<-list(0)
  D2<-list(0)
  dist_mat<-matrix(0,nrow=vcount(g_es1), ncol=vcount(g_es1))
  dist_mat2<-matrix(0,nrow=vcount(g_es1), ncol=vcount(g_es1))
  v_tot<-vcount(g_es1)  #numero di nodi nel grafico
  v_names<-attr(V(g_es1),"names")  #nomi dei nodi del grafico , oppure si può usare V(g)$name[210]
  results2<-0
  
  print(n)
  start<-n   #la posizione del nodo analizzato--> destinazione
  distances = rep(Inf, vcount(g_es1)) 
  distances[start] = 0
  visited = rep(0, vcount(g_es1)) # 0 se costo iniziale (non visitato), 2 se nodo nel serbatoio, 1 se itinerario definitivo
  visited[start]=2
  predecessor = rep(start,vcount(g_es1)) 
  predecessor [start]= 0
  tank=rep(0,vcount(g_es1)) 
  tank[1]=start
  node_name=V(g_es1)
  FI=rep(0, vcount(g_es1))
  diversion=rep(0, vcount(g_es1))
  mode_predecessor<-rep(0, vcount(g_es1))
  pred<- as.list(rep(0, v_tot))  #empty list for predecessors
  div_prob<- as.list(rep(0, v_tot)) 
  D<-as.matrix(cbind( 1:vcount(g_es1),distances,visited,predecessor,tank, FI, diversion, node_name, mode_predecessor)) 
  D1=D  
  D1
  
  ###############ALGORITMO###########################
  while (D1[1,5]!=0){
    cj=0
    ci=0
    i=0
    j=0
    fstar3=0
    cij=0
    line_j<-0
    line_p<-0
    
    fstar<-as.numeric(as.character(neighbors(g_es1, v_names[D1[1,5]], mode = "in")))   #in: dijkstra all'indietro
    
    if (length(fstar)==0){
      D1[,5]<-c(D1[-1,5],0)
      next}
    cij1=incident(g_es1, v_names[D1[1,5]], mode = c("in"))$weights  #costi archi stella uscente dal primo nodo del serbatoio
    ci=D1[which(D1[,1]==as.character(D1[1,5])),2] #costo definitivo per arrivare al nodo i (colonna costo)
    for (j in 1:length(fstar)){
      if (D1[which(D1[,1]==fstar[j]),3]!=1){
        shortest=cbind(matrix(data = Inf, nrow = 3, ncol = 2), 0)
        line_freq<-0
        freq_tot<-Inf
        i=1+i
        fstar3[i]<-fstar[j]
        cij[i]<-cij1[j]
        cj[i]=D1[which(D1[,1]==fstar3[i]),2]   #Costo provvisorio nodo j (colonna costo)
        ##########PRIMO COSTO SENZA FREQUENZA#################
        shortest[1,1]<-cj[i]
        shortest[1,2]<-D1[which(D1[,1]==fstar3[i]),4]  #rimane il predecessor che c'è
        shortest[2,1]<-ci+cij[i] 
        shortest[2,2]<-D1[1,5]   #in questo caso il costo cambia e il pred è il primo nodo del serbatotoio 
        shortest[3,2]<-0    
        
        ##########SECONDO COSTO CON FREQUENZA############  
        if (incident(g_es1, v_names[D1[1,5]], mode = c("in"))$freq[j]!=0){
          line_freq[i]<-incident(g_es1, v_names[D1[1,5]], mode = c("in"))$freq[j] #fi
          freq_tot[i]<-D1[which(D1[,1]==fstar3[i]),6]+line_freq[i]              #FI
          
          ###SE LINE 1 DIVERSA DA LINE 2, FACCIO COSI, SENNO LA MATRICE RIMANE COME PRIMA
          line_j<-incident(g_es1, v_names[D1[1,5]], mode = c("in"))$mode1[j]
          # line_i<-incident(g_es1, v_names[D1[1,5]], mode = c("out"))$mode
          line_p<-D1[which(D1[,1]==as.character(D1[1,5])),9]   #linea del predecessor
          
          # if (length(which(line_i==line_j))==0){  #se linee diverse bisogna cambiare
          if (line_j!=line_p){
            shortest[2,1]<-ci+cij[i]+(60/(2*line_freq[i]))+PEN
            shortest[2,2]<-D1[1,5]   #1 predecessor
          }
          shortest[2,3]<-line_freq[i]  #1 predecessor
          shortest[3,1]<-cj[i]+(ci+cij[i]-cj[i])*(line_freq[i]/freq_tot[i]) 
          shortest[which(is.na(shortest))]<-Inf
          shortest[3,2]<-D1[1,5]   #metto solo l'ultimo nodo, che è da aggiungere a quello precedente
          shortest[3,3]<-freq_tot[i]
        }
        if (min(shortest[,1])==shortest[3,1]){
          pred[[which(D1[,1]==fstar3[i])]]<-append(pred[[which(D1[,1]==fstar3[i])]], shortest[3,2])
          div_prob[[which(D1[,1]==fstar3[i])]]<-append(div_prob[[which(D1[,1]==fstar3[i])]], line_freq[i])
          D1[which(D1[,1]==fstar3[i]),7] =(line_freq[i]/freq_tot[i]) #diversion probability 2nd path
        } else {pred[[which(D1[,1]==fstar3[i])]]<-shortest[which.min(shortest[,1]),2]
        div_prob[[which(D1[,1]==fstar3[i])]]<-line_freq[i]
        D1[which(D1[,1]==fstar3[i]),7] =1}
        
        # cj[i]<-min(shortest[,1])
        D1[which(D1[,1]==fstar3[i]),2] = min(shortest[,1])  #cj[i]   #aggiornare costo 
        D1[which(D1[,1]==fstar3[i]),4] = shortest[which.min(shortest[,1]),2]  #aggiornare predecessor
        D1[which(D1[,1]==fstar3[i]),6] = shortest[which.min(shortest[,1]),3]  #aggiornare frequenza
        D1[which(D1[,1]==fstar3[i]),9]=line_j
      }#aggiornare matrice
      D1[which(D1[,1]==fstar3[i]),3] =2 #visited, nel serbatoio
      D1[which(D1[,1]==as.character(D1[1,5])),3] =1
    }
    #aggiornare serbatorio per il nodo (aggiungere stella uscente) e ordinare serbatoio in base al costo
    tank<-c(D1[-1,5],fstar3) #tolgo il primo nodo
    tank2<-cbind(tank[(!duplicated(tank))&(tank!=0)],0)
    if (length(tank2)>1){
      for (s in 1:length(tank2[,1])){  
        tank2[s,2]<-D1[which(D1[,1]==tank2[s,1]),2]
      }
      tank_ordered<-tank2[order(tank2[,2],decreasing=FALSE),1]
      D1[,5]<-c(tank_ordered,rep(0,vcount(g_es1)-length(tank_ordered)))  }
    if (length(tank2)<2){ D1[,5]<-rep(0,vcount(g_es1))  }
    
    # dist_mat[start,]<-t(D1[,2])
    # dist_mat2[,start]<-D1[,2]
  }
  
  pred2[[n]]<-cbind(do.call(rbind, pred), do.call(rbind, div_prob)/rowSums(do.call(rbind, div_prob)))
  if (ncol(pred2[[n]])>2){
    n_hy<-ncol(pred2[[n]])/2
    pred2[[n]][is.na(pred2[[n]][,n_hy+1])==TRUE,n_hy+1] <-1
    pred2[[n]][which(is.na(pred2[[n]][,(n_hy+2)])==TRUE),(n_hy+2):(n_hy+n_hy)]<-0
    
  }
  D2[[n]]<-D1
  dist_mat2[,n]<-D2[[n]][,2]
  return(list(dist_mat2,D1, pred2))
   # return(dist_mat2)
}
