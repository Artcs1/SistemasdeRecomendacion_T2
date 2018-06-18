FBC.sio.model <- function(dataset, testset, metaset) {
    library(proxy)
    dataset <- as.matrix(dataset)
    testset <- as.matrix(testset)
    metaset <- as.matrix(metaset)
    testset <- testset[, 2:3]

    users <- dataset[, 1] #ID dos usuï¿½rios
    movies <- dataset[, 2] #ID dos filmes
    ratings <- dataset[, 3] # Ratings

    nusers  <-  max(c(dataset[, 1], testset[, 1], as.numeric(metaset[, 1]))) # nï¿½mero de usuarios
    nmovies <- max(c(dataset[, 2], testset[, 2])) # nï¿½mero de filmes
    scores <- matrix(rep(0,nusers*nmovies), nusers, nmovies) # interacciï¿½n usuario filme

    for (i in 1 : length(users)){
      scores[users[i], movies[i]] <- ratings[i] #construï¿½ao da matriz de interaï¿½ï¿½o usuario filme
    }


    gender <- list("M" = 1, "F" = 2)
    genders <- matrix(rep(0,2*nusers), 2, nusers) # gender

    #construo da matriz gender vs iten's
    for (i in 1 : nrow(metaset)) {
    	value = gender[metaset[i, 2	]]
        genders[as.numeric(value), i] = 1
    }

    sim.gender <- simil(t(genders), method <- "Jaccard")
    sim.gender <- as.matrix(sim.gender)
    sim.gender[is.na(sim.gender) == T] = 0

    sim.age <- matrix(rep(0,nusers*nusers), nusers, nusers) # age
    agemin = min(as.numeric(metaset[,3]))
    agemax = max(as.numeric(metaset[,3]))
    for (i in 1:nrow(metaset)) {
      for(j in i:nrow(metaset)) {
        sim.age[i,j] = 1- abs(as.numeric(metaset[i,3])-as.numeric(metaset[j,3]))/(agemax-agemin)
        sim.age[j,i] = sim.age[i,j]
      }
    }
    noccupation <- as.numeric(max(metaset[, 4]))
    occupations <- matrix(rep(0,noccupation*nusers), noccupation, nusers) # occupation

    #construï¿½ï¿½o da matriz occupation vs iten's
    for (i in 1 : nrow(metaset)) {
        occupations[as.numeric(metaset[i, 4]), i] = 1
    }

    sim.occupation <- simil(t(occupations), method <- "Jaccard")
    sim.occupation <- as.matrix(sim.occupation)
    sim.occupation[is.na(sim.occupation) == T] = 0


    sim <- (sim.gender+sim.age+ sim.occupation)/3
    model <- list()
    model$sim <- sim
    model$score <- scores
    return (model)
}

FBC.sio.predict <- function(model, user , movie, K) {
  sim <- as.matrix(model$sim)
  score <- as.matrix(model$score)

  similar.user <- order(- sim[,user])# Ordenando as similiaridades de um filme en forma decrescente
  rated.user <- which(score[,movie] > 0)    # Escolher os items que o usuario avalio
  most.similar.rated <- intersect(similar.user, rated.user)[1 : min(K, length(rated.user))] #interseï¿½ï¿½o


  #Calculo de la prediï¿½ï¿½o
  if (is.na(most.similar.rated[1])) {#Se la interseï¿½ï¿½o e vacia retorna a media
      return (mean(score))
  }
   
  sumSim <- 0
  sumWeight <- 0
  #Calculo de prediï¿½ï¿½o con os k vizinhos mais proximos
  for (j in most.similar.rated) {
    score.mean = mean(score[user,score[j,]>0])
    sumSim <- sumSim + sim[user, j]
    sumWeight <- sumWeight + sim[user, j] * (score[j, movie]-score.mean)
  }

  if(sumSim==0)
    return (mean(score))
  
  return(sumWeight / sumSim)
}

FBC.sio.test <- function(model,testset,vizinhos,name) {
  testset = as.matrix(testset[,2:3])
  testUser = testset[,1] #Usuarios
  testMovie = testset[,2] #Filmes
  
  tam = length(testUser)
  ids = (1:(tam))
  ids = ids-1
  ratings = rep(0,tam) #vetor para os ratings
  
  for ( i in 1:tam) {
    ratings[i] = FBC.sio.predict(model,testUser[i],testMovie[i],vizinhos)  #predição
  }
  
  my.dataset <- data.frame(id = ids, rating = ratings) #Criação de um dataframe
  write.csv(my.dataset,name,row.names=F)  #Exportacion
}