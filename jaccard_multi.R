#Jaccard
#Reads all pathway files in a directory,
#Calculates Jaccard index between every pair. 

#Read the folder.

file_list<-list.files()
Pathways<-list()

for(i in file_list){
  x<-as.matrix(read.table(i, header=TRUE))
  assign(paste(colnames(x)), i) ##nombre de columna = nombre variable
  y<-(read.table(i, header=TRUE))
  Pathways<-c(Pathways, y)
  rm(x)
  rm(i)
}

#Ahora hagamos una lista de todos esos archivos con sus nombres 
lista_pw<-names(Pathways)
#Calcula el indice de Jaccard para dos conjuntos
jaccard<-function(a,b)
{
  x<-intersect(a,b)
  y<-union(a,b)
  nx<-length(x)
  ny<-length(y)
  J<-as.numeric(nx/ny)
  print(J)
}

###Daniel Terán  Sergio Alcalá: Credit for loops
jaccard_1_elemento_lista<-function(x, Pathways)

{
  df<-matrix(names(Pathways)) 
  js<-NULL 
  
  for(i in Pathways) 
  {
    q<-jaccard(x,i) 
    js<-c(js, q) 
  }
  
  js<-(as.matrix(js))
  
  resultado<-cbind(df, js) 
  
  print(resultado)
  
}

wk<-function(x)
 
{
  piwis<-Pathways
  df<-matrix(names(piwis))
  js<-NULL 
  
  for(i in piwis) 
  {
    q<-jaccard(x,i) 
    js<-c(js, q) 
  }
  js<-(as.matrix(js))
  resultado<-cbind(df, js) 
  print(resultado)
}


jck<-function(PW){
  #PW es la lista de genes 
  lapply(PW, wk)
}

jck(Pathways)
