MP.model <- function(dataset, testset, genreset) {
  dataset <- as.matrix(dataset)
  testset <- as.matrix(testset)
  genreset <- as.matrix(genreset)
  testset <- testset[, 2:3]

  users <- dataset[, 1] #ID dos usuï¿½rios
  movies <- dataset[, 2] #ID dos filmes
  ratings <- dataset[, 3] # Ratings
  
  nusers  <-  max(c(dataset[, 1], testset[, 1])) # nï¿½mero de usuarios
  nmovies <- max(c(dataset[, 2], testset[, 2], as.numeric(genreset[, 1]))) # nï¿½mero de filmes
  scores <- matrix(rep(0,nusers*nmovies), nusers, nmovies) # interacciï¿½n usuario filme
  
  for (i in 1 : length(users)) {
    scores[users[i], movies[i]] <- ratings[i] #construï¿½ao da matriz de interaï¿½ï¿½o usuario filme
  }
  
  listgener = list("Animation"=1,"Adventure"=2,"Comedy"=3,"Action"=4,"Drama"=5,"Thriller"=6,"Crime"=7,"Romance"=8,"Children's"=9,"Documentary"=10,"Sci-Fi"=11,"Horror"=12,"Western"=13,"Mystery"=14,"Film-Noir"=15,"War"=16,"Musical"=17,"Fantasy"=18)
  genres = matrix(rep(0,nmovies*18),nmovies,length(listgener)) # generos
  
  #construção da matriz género vs filme
  for(i in 1:nmovies) {
    gener = strsplit(genreset[i,3],split = "|" , fixed = TRUE )[[1]]
    for(j in 1:length(gener)) {
      value = as.numeric(listgener[gener[j]])
      genres[i,value]=1;
    }
  }
  model <- list()
  model$genres <- genres
  model$score <- scores
  return (model)
}

MP.predict <- function(model, user , movie) {
  genres <- as.matrix(model$genres)
  score <- as.matrix(model$score[user,])
  P.y = rep(0,5)
  for(i in 1:length(score)) {
    P.y[score[i]] = P.y[score[i]] + 1
  }

  P.xy = rep(1,5)
  for(i in 1:5) { # evaluacion
    if(P.y[i]>0) {
      for(j in 1:ncol(genres)) {#numero de generos
        a = 0
        for(k in 1:nrow(genres)) {#numero de movies
          if(genres[movie,j]==genres[k,j] && score[k]==i) {
            a = a + 1
          }
        }
        if(a!=0) {
          P.xy[i] = P.xy[i] * (a / P.y[i])
        } else {
          P.xy[i] = 0
          break
        }
      }
    }
  }
  if(sum(P.y)>0){
    P.y = P.y / sum(P.y)
  }
  
  P.yx = P.xy * P.y
  return(order(-P.yx)[1])
}

MP.test <- function(model,testset,name) {
  testset = as.matrix(testset[,2:3])
  testUser = testset[,1] #Usuarios
  testMovie = testset[,2] #Filmes
  
  tam = length(testUser)
  ids = (1:(tam))
  ids = ids-1
  ratings = rep(0,tam) #vetor para os ratings
  
  for ( i in 1:tam) {
    ratings[i] = MP.predict(model,testUser[i],testMovie[i])  #predição
  }
  
  my.dataset <- data.frame(id = ids, rating = ratings) #Criação de um dataframe
  write.csv(my.dataset,name,row.names=F)  #Exportacion
}