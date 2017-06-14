library("XML")

library("methods")

setwd("C:/Users/dyska/Downloads")

dane <- xmlTreeParse("m.xml")

dtd <- xmlRoot(dane)


getPriority <- function(wezel) {
  
  #xmlElementsByTagName(el, name, recursive = FALSE)
  porownanie = xmlElementsByTagName(wezel, 'porownania')
  
  #podzbior porownan double, wybiera element porownania z porownanie
  listaPorownan <- as.double(xmlToList(porownanie$porownania))
  
  macierz <- matrix(data=1,nrow=3,ncol=3)
  
  #seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
  #length.out = NULL, along.with = NULL, ...)
  i <- 1
  for (row in seq(1,2)) {
    for (element in seq(row+1,3)) {
     # a(ij)= 1/a(ji)
      
      macierz[row,element] <- listaPorownan[i]
      macierz[element,row] <- 1 / listaPorownan[i]
      
      i <- i + 1
    }
  }
  # wektor wlasny dla najwiekszej wartosci wlasnej
  # vectors[,1]  --> wszystkie wiersze w pierwszej kolumnie

  #print(eigen(macierz))

  #eigen zwraca values and vectors z macierzy
  wektorWlasny <- Re(eigen(macierz)$vectors[,1])
  
  # normalizacja wektora wlasnego
  wektorWlasny <- wektorWlasny / sum(wektorWlasny)
  
  listaWezlow = xmlElementsByTagName(wezel, 'kryteria')
  print(listaWezlow)
  # zeby przejsc na sam dol kryterium
  if (length(listaWezlow) != 0) {
    
    vec <- vector(mode = "list", length = length(listaWezlow))
    
    # rekurencja
    i <- 1
    for(dziecko in listaWezlow) {
      vec[[i]] <- getPriority(dziecko)
      i <- i + 1
    }
    
    # rep replicates the values in x, here 0
    priorityVector <- rep(0, length = length(vec[[1]]))

    #wynik czyli priority vector
    i <- 1
    for(k in vec) {
      
      priorityVector <- priorityVector + k * wektorWlasny[i]
      i <- i + 1
    }

    return(priorityVector / sum(priorityVector))
    
  } else { 
    
      return (wektorWlasny)
  }
}
getPriority(dtd) 

