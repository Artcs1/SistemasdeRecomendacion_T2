  FBC.genres.sim <- function(dataset,testset,genreset) {
  dataset = as.matrix(dataset)
  testset = as.matrix(testset)
  genreset = as.matrix(genreset)
  testset = testset[,2:3]
  nmovies = max(c(dataset[,2],testset[,2])) # n�mero de filmes
  
  A = list("Animation"=1,"Adventure"=2,"Comedy"=3,"Action"=4,"Drama"=5,"Thriller"=6,"Crime"=7,"Romance"=8,"Children's"=9,"Documentary"=10,"Sci-Fi"=11,"Horror"=12,"Western"=13,"Mystery"=14,"Film-Noir"=15,"War"=16,"Musical"=17,"Fantasy"=18)
  genres = matrix(rep(0,18*nmovies),18,nmovies) # generos

  #constru��o da matriz g�nero vs filme
  for(i in 1:nmovies) {
    g = strsplit(genreset[i,3],split = "|" , fixed = TRUE )[[1]]
    for(j in 1:length(g)) {
      value = as.numeric(A[g[j]])
      genres[value,i]=1;
    }
  }
  
  #Calculo das similiraridades entre os filmes respeito a seus generos con o m�todo Jaccard
  library(proxy)
  sim = simil(t(genres),  method="Jaccard")
  sim = as.matrix(sim)
  sim[is.na(sim)==T]  = 0
  
  
  return (sim)
}

computeTFIDF	<-	function(row) { # TF - IDF 	
  df	=	sum(row[1:3564]	>	0)	
  w	=	rep(0,	length(row))	
  w[row	>	0]	=	(1	+	log2(row[row	>	0]))	*	log2(3564/df)	
  return(w)	
}

FBC.metadados.sim <- function(dataset,testset,reviewset) {
  dataset = as.matrix(dataset)
  testset = as.matrix(testset)
  reviewset = as.matrix(reviewset)
  testset = testset[,2:3]

  nmovies = max(c(dataset[,2],testset[,2])) # número de filmes
  
  library(tm)
  library(SnowballC)
  
  reviews = c()
  for(i in 1:nmovies) {
    reviews = c(reviews , paste(reviewset[which(as.numeric(reviewset[,1]) == i),2],collapse = " ")) # Concatenando todos os textos
  }
  
  reviewList = as.list(reviews) # Transformando o vetor para uma lista
  nDocs = length(reviewList)
  reviewList = VectorSource(reviewList)
  corpus = Corpus(reviewList)
  corpus = tm_map(corpus,	removePunctuation)	# Tokenização
  corpus = tm_map(corpus, content_transformer(tolower))# Normalização de termos
  corpus = tm_map(corpus,	stripWhitespace) # Normalização de termos
  
  tdm = 	TermDocumentMatrix(corpus,	control=list(stopwords=TRUE,stemWords=TRUE,wordLengths=c(1,15)))
  #Remoção de stopwords
  #Radicalização
  
  
  m = as.matrix(tdm) # matrix do vocabulario vs termos
  n = t(apply(m,	1,	FUN=computeTFIDF)) # fazer TF - IDF
  n = scale(n,	center=FALSE,	scale=sqrt(colSums(n^2))) # normalização
  sim = t(n) %*% n # Calculo das similaridades
  
  return (sim)
}

HM.model <- function(dataset,testset,genreset,reviewset) {
  users = dataset[,1] #ID dos usuários
  movies = dataset[,2] #ID dos filmes
  ratings = dataset[,3] # Ratings
  
  nusers = max(c(dataset[,1],testset[,2])) # número de usuarios
  nmovies = max(c(dataset[,2],testset[,3])) # número de filmes
  scores = matrix(rep(0,nusers*nmovies),nusers,nmovies) # interacción usuario filme 
  
  for(i in 1:length(users))
    scores[users[i],movies[i]] = ratings[i] #construçao da matriz de interação usuario filme
  
  sim.genres = FBC.genres.sim(dataset,testset,genreset)
  sim.metadatos = FBC.metadados.sim(dataset,testset,reviewset)
  
  sim = 0.385*sim.genres+0.615*sim.metadatos
  
  model = list(score = scores,sim = sim)
  return (model)
}

HM.predict <- function(model, user , movie, K) {
  simil = as.matrix(model$sim)
  score = as.matrix(model$score)
  
  similar.movies	=	order(-simil[movie,])# Ordenando as similiaridades de um filme en forma decrescente
  rated.movies	=	which(score[user,]	>	0)	# Escolher os items que o usuario avalio
  most.similar.rated	=	intersect(similar.movies,	rated.movies)[1:min(K,length(rated.movies))] #interseção
  
  
  #Calculo de la predição
  sumSim	=	0
  sumWeight	=	0
  if(is.na(most.similar.rated[1])) {#Se la interseção e vacia retorna a media
    return (3.603814)
  }
  
  #Calculo de predição con os k vizinhos mais proximos
  for(j	in	most.similar.rated)	{
    sumSim	=	sumSim	+	simil[movie,	j]
    sumWeight	=	sumWeight	+	simil[movie,	j]	*	score[user,	j]
  }
  
  sumSim=sumSim+1e-12 # Em caso sumSim seja 0
  
  return(sumWeight/sumSim)
}

HM.test <- function(model,testset,vizinhos,name) {
  testset = as.matrix(testset[,2:3])
  testUser = testset[,1] #Usuarios
  testMovie = testset[,2] #Filmes
  
  tam = length(testUser)
  ids = (1:(tam))
  ids = ids-1
  ratings = rep(0,tam) #vetor para os ratings
  
  for ( i in 1:tam) {
    ratings[i] = HM.predict(model,testUser[i],testMovie[i],vizinhos)  #predi��o
  }
  
  my.dataset <- data.frame(id = ids, rating = ratings) #Cria��o de um dataframe
  write.csv(my.dataset,name,row.names=F)  #Exportacion
}

HM.pretest <- function(model,testset)
{
  testset = as.matrix(testset[,2:4])
  testUser = testset[,1] #Usuarios
  testMovie = testset[,2] #Filmes
  
  tam = length(testUser)
  ids = (1:(tam))
  ids = ids-1
  ratings = rep(0,tam) #vetor para os ratings
  error = c()
  
  for(i in 1:30) # testando os k vizinhos
  {
    for(j in 1:tam)
    {
      ratings[j] = HM.predict(model,testUser[j],testMovie[j],i)  #predi��o
    }
    r = ratings;
    print(paste("HM_",i,sep=""))
    e = RMSE(r,testset[,3]) #calculo de Error
    print(e)
    error = c(error,e) # lista de errors
  }
  
  return (error/sum(error)) # normaliza��o
}
