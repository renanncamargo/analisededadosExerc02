########################################
# Teste 2         
# Nome(s): 
########################################

## 1 - Agrupamento

groupsum <- function(df, colgroup, colsum){
  valorAgrup<-c(NULL)
  regraAgrupamento<- unique(df[[colgroup]])
  for(i in regraAgrupamento){
   valor <- (sum(df[[colsum]][(df[[colgroup]] == i)])) 
   valorAgrup <- c(valorAgrup, valor)
  }
  dfFinal <- data.frame(regraAgrupamento, valorAgrup)
  names(dfFinal)[1:2] <- c(colgroup, colsum)
  return(dfFinal)
}

groupsum (chuvas, "cidade", "chuva")

##### Exemplos no PDF:
##### dia <- c(01, 01, 02, 02, 03, 03, 04, 04, 05, 05)
##### cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')
##### chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)
##### chuvas <- data.frame(cidade, dia, chuva)
##### groupsum(chuvas, "cidade", "chuva")
##### customer_churn = read.table("customer_churn.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)
##### groupsum(customer_churn, "Contract", "MonthlyCharges")

## 2 - Binario para Decimal

binToDec <- function(...){
  tamanhos <- NULL
  lista <- list(...)
  tamLista <- length(lista)
  resultado <- NULL
  x <- 0
  for (vec in list(...)) {
    tamanhos <- c(tamanhos, length(vec))
  }
  for(i in 1:tamLista){
    k <- tamanhos[[i]]
    for(j in (1:tamanhos[[i]])){
      x <- x + ((lista[[i]][[j]])*(2^(k-1)))
      k <- k-1
    }
    resultado <- c(resultado, x)
    x <- 0
  }
  return(resultado)
}

##### Exemplos no PDF:
##### binToDec(c(1, 0))
##### binToDec(c(0, 0, 1), c(1, 1))
##### binToDec(rep(1, 3), rep(0, 2), rep(c(1,0), 2))

## 3 - Ocorrencia de Palavras

wordCount <- function(countText, text){
  tamanho <- 0
  total <- 0
  words <- strsplit(gsub("!", "", gsub("[.]", "", gsub(",", "", text))), " ")
  
  for (vec in words) {
    tamanho <- length(vec)
  }
  
  for(i in 1:tamanho){
    if(toupper(countText) == toupper(words[[1]][[i]]))
      total <- total + 1
  }
  return(total)
}


##### Exemplos no PDF:
##### text <- "O rAto roeu a roupa do Rei de Roma! RainhA raivosa rasgou o resto."
##### wordCount("rato", text)
##### wordCount("roma", text)
##### text <- "A vaca malHada foi molhADA por outra VACA, MOLhada e MALhaDa."
##### wordCount("outra", text)
##### wordCount("vaca", text)
##### wordCount("molhada", text)
##### text <- "Se a liga me ligasse, eu tambem ligava a liga. Mas a liga nao me liga, eu tambem nao ligo a liga."
##### wordCount("liga", text)
##### wordCount("ligasse", text)

## 4 - Ordenacao de Panquecas

giro <- function(vector, i){
  tamanho <- length(vector)
  aux <- 0
  aux <- vector[tamanho]
  vector[tamanho] <- vector[i]
  vector[i] <- aux
  return(vector)
} 

ordenar <- function(vector){
  #auxVector <- NULL
  tamanho <- length(vector)
      for (i in 1:tamanho) {
        maximo <- max(vector[i:tamanho])
        if (vector[i] == maximo){
          for(j in i:tamanho){
            teste <- 1
            vector <- giro(vector, i)
            print(vector)
            vector <- vector[tamanho:j]
            teste <- teste +1
          }
        }
    }
}

##### Exemplos no PDF:
##### panquecas <- c(3,4,1,2)
##### giro(panquecas, 2)
##### panqueca <- ordenar(panquecas)
##### pilha <- c(2, 10, 4, 8, 6, 9, 1, 5, 7, 3)
##### pilha <- ordenar(pilha)
