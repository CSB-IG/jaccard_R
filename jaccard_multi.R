#Cálculo del índice de Jaccard para un conjunto de Pathways
#Este programa lee todos los archivos de una carpeta (que son pathways),
#y saca el índice de Jaccard de todos contra todos. 

#Primero leemos los archivos de la carpeta.

#Como está el programa hay que correrlo en una carpeta que NADA MÁS tenga los 
#archivos de interés. Seguro se puede generalizar pero no es de interés ahora
##los archivos deben ser listas, de igual longitud. 
###para Pathways del BROAD, usar el "Cortador" para quitarles los comentarios
file_list<-list.files()
Pathways<-list()
#Este for loop va a leer todos los archivos de la lista de archivos. 
for(i in file_list){
  x<-as.matrix(read.table(i, header=TRUE))
  assign(paste(colnames(x)), x) ##nombre de columna = nombre variable
  y<-(read.table(i, header=TRUE))
  Pathways<-c(Pathways, y)
  rm(x)
  rm(i)
}

#Ahora hagamos una lista de todos esos archivos con sus nombres 
lista_pw<-names(Pathways)
#lista_pw<-lista_pw[!lista_pw == "file_list"]
#lista_pw<-lista_pw[!lista_pw == "Pathways"]
#Debe haber una forma más elegante de leer todos los objetos MENOS 
#"file_list" pero no se me ocurre ahora. Así funciona.

#Sacar una lista de todos los pares de pathways
combi<-combn(Pathways, 2)

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
#probemos Jaccard 

#Prueba<-jaccard(combi[1,9], combi[2,9])
gatos<-c("Garfield", "Heathcliff", "López", "Catdog", "Félix")
perros<-c("Pluto", "Underdog", "Catdog")
rueba<-jaccard(perros, gatos)
#rueba debe ser 0.1428

#Funcion para calcular Jaccard de 1 PW vs todos los demás

#funcion para calcular Jaccard de un elemento en una lista con todos los 
#elementos de la lista
jaccard_1_elemento_lista<-function(x, Pathways)
  #x es un elemento de la lista (1 pathway)
  #Pathways es la lista 
{
  df<-matrix(names(Pathways)) #genera una matriz con los nombres de PW
  js<-NULL #va a contener los valores de Jaccard
  
  for(i in Pathways) #for loop para todos los elementos en la lista
  {
    q<-jaccard(x,i) #calcula el índice de jaccard para x y un PW i
    js<-c(js, q) # agrega el indice de Jaccard x-i a la lista 
  }
  #print("interludio musical: ¡gatitos y unicornios!")
  js<-(as.matrix(js)) #convierte la lista de js en matriz
  #print(js)
  #print(df)
  resultado<-cbind(df, js) #junta los resultados con el PW en una matriz
  #print("Otro interludio musical. Ahora con más gatitos!")
  print(resultado)
  
}

#probemos
#jaccard_1_elemento_lista(Pathways$BIOCARTA_WNT_PATHWAY, Pathways)

##Jaccard para la lista de PWs

#Usamos dos funciones: Wacko y Jacko

#Wacko es igual a jaccard_1_elemento_lista pero toma los PW del ambiente

wacko<-function(x)
  #x es un elemento de la lista (1 pathway)
  #piwis es la lista de Pathways definida en el ambiente
{
  piwis<-Pathways
  df<-matrix(names(piwis)) #genera una matriz con los nombres de PW
  js<-NULL #va a contener los valores de Jaccard
  
  for(i in piwis) #for loop para todos los elementos en la lista
  {
    q<-jaccard(x,i) #calcula el índice de jaccard para x y un PW i
    js<-c(js, q) # agrega el indice de Jaccard x-i a la lista 
  }
  js<-(as.matrix(js)) #convierte la lista de js en matriz
  #print(js)
  #print(df)
  resultado<-cbind(df, js) #junta los resultados con el PW en una matriz
  print(resultado)
}

#Jacko
jacko<-function(PW){
  #PW es la lista de genes 
  lapply(PW, wacko)
}
#probemos
jacko(Pathways)