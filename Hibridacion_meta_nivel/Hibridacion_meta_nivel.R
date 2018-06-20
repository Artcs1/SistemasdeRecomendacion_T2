HMetaN.SimilUsers <- function(dataset) {
  library(proxy)
  dataset = as.matrix(dataset)
  sim = simil(dataset)
  sim <- as.matrix(sim)
  sim[is.na(sim) == T] = 0
  return (sim)
}

HMetaN.SimilItems <- function(dataset) {
  library(proxy)
  dataset = as.matrix(dataset)
  sim = simil(t(dataset))
  sim <- as.matrix(sim)
  sim[is.na(sim) == T] = 0
  return (sim)
}

HMetaN.model <- function(dataset,testset) {
  users <- dataset[,1] #ID dos usuÃ¡rios
  movies <- dataset[,2] #ID dos filmes
  ratings <- dataset[,3] # Ratings
  
  nusers <- max(c(dataset[,1],testset[,2])) # nÃºmero de usuarios
  nmovies <- max(c(dataset[,2],testset[,3])) # nÃºmero de filmes
  scores <- matrix(rep(0,nusers*nmovies),nusers,nmovies) # interacciÃ³n usuario filme 
  
  for(i in 1:length(users))
    scores[users[i],movies[i]] = ratings[i] #construÃ§ao da matriz de interaÃ§Ã£o usuario filme
  svd <- svd(scores)
  P = svd$u
  Q = svd$v
  
  model = list()
  model$similUsers = HMetaN.SimilUsers(P)
  model$similItems = HMetaN.SimilItems(t(Q))
  model$score = scores
  return (model)
}

HMetaN.predictUsers <- function(model, user, movie, k) {
  score = as.matrix(model$score)
  sim = as.matrix(model$similUsers)
  
  similar.user <- order(- sim[user,])# Ordenando as similiaridades de um filme en forma decrescente
  rated.user <- which(score[,movie] > 0)    # Escolher os items que o usuario avalio
  most.similar.rated <- intersect(similar.user, rated.user)[1 : min(k, length(rated.user))] #interseï¿½ï¿½o

  #Calculo de la prediï¿½ï¿½o
  if (is.na(most.similar.rated[1])) {#Se la interseï¿½ï¿½o e vacia retorna a media
    return (mean(score[user,score[user,]>0]))
  }
  
  sumSim <- 0
  sumWeight <- 0
  #Calculo de prediï¿½ï¿½o con os k vizinhos mais proximos
  for (v in most.similar.rated) {
    score.mean = mean(score[user,score[v,]>0])
    sumWeight <- sumWeight + sim[user, v] * (score[v, movie]-score.mean)
    sumSim <- sumSim + sim[user, v]
  }
  
  if(sumSim==0) {
    return (mean(score[user,score[user,]>0]))
  }
  
  return(sumWeight / sumSim)
}

HMetaN.predictItems <- function(model,user,movie, k) {
  score = as.matrix(model$score)
  sim = as.matrix(model$similItems)
  similar.movie <- order(- sim[,movie])# Ordenando as similiaridades de um filme en forma decrescente
  rated.movie <- which(score[user,] > 0)    # Escolher os items que o usuario avalio
  most.similar.rated <- intersect(similar.movie, rated.movie)[1 : min(k, length(rated.movie))] #interseï¿½ï¿½o
  
  #Calculo de la prediï¿½ï¿½o
  if (is.na(most.similar.rated[1])) {#Se la interseï¿½ï¿½o e vacia retorna a media
    return (mean(score[user,score[user,]>0]))
  }
  
  sumSim <- 0
  sumWeight <- 0
  #Calculo de prediï¿½ï¿½o con os k vizinhos mais proximos
  for (j in most.similar.rated) {
    sumWeight <- sumWeight + sim[movie, j] * score[user, j]
    sumSim <- sumSim + sim[movie, j]
  }
  
  if(sumSim==0) {
    return (mean(score[user,score[user,]>0]))
  }
  return(sumWeight / sumSim)
}

HMetaN.predict <- function(model, user, movie, k) {
  predictItems = HMetaN.predictItems(model,user,movie,k) 
  predictMovies = HMetaN.predictUsers(model,user,movie,k)
  predict = 0.532*predictItems + 0.468*predictMovies
  if(is.na(predict)){
    if(is.na(predictMovies) && is.na(predictItems)==FALSE) {
      predict =predictItems
    } else if(is.na(predictMovies)==FALSE && is.na(predictItems)){
      predict = predictMovies
    } else {
      predict = mean(model$score)
    }
  }
  if(predict<0){
    predict = 0.1
  } else if(predict>5) {
    predict = 5
  }
  return(predict)
}

HMetaN.test <- function(model,testset,vizinhos,name) {
  testset = as.matrix(testset[,2:3])
  testUser = testset[,1] #Usuarios
  testMovie = testset[,2] #Filmes
  
  tam = length(testUser)
  ids = (1:(tam))
  ids = ids-1
  ratings = rep(0,tam) #vetor para os ratings
  
  for ( i in 1:tam) {
    ratings[i] = HMetaN.predict(model,testUser[i],testMovie[i],vizinhos)  #predição
  }
  
  my.dataset <- data.frame(id = ids, rating = ratings) #Criação de um dataframe
  write.csv(my.dataset,name,row.names=F)  #Exportacion
}