FBC.genres.model <- function(dataset,testset,genreset)
{
    dataset = as.matrix(dataset)
    testset = as.matrix(testset)
    genreset = as.matrix(genreset)
    testset = testset[,2:3]
    
    users = dataset[,1] #ID dos usuários
    movies = dataset[,2] #ID dos filmes
    ratings = dataset[,3] # Ratings
    
    nusers = max(c(dataset[,1],testset[,1])) # número de usuarios
    nmovies = max(c(dataset[,2],testset[,2])) # número de filmes
    scores = matrix(rep(0,nusers*nmovies),nusers,nmovies) # interacción usuario filme 
    
    for(i in 1:length(users))
        scores[users[i],movies[i]] = ratings[i] #construçao da matriz de interação usuario filme
    
    A = list("Animation"=1,"Adventure"=2,"Comedy"=3,"Action"=4,"Drama"=5,"Thriller"=6,"Crime"=7,"Romance"=8,"Children's"=9,"Documentary"=10,"Sci-Fi"=11,"Horror"=12,"Western"=13,"Mystery"=14,"Film-Noir"=15,"War"=16,"Musical"=17,"Fantasy"=18)
    genres = matrix(rep(0,18*nmovies),18,nmovies) # generos
    
    
    #construção da matriz género vs filme
    for(i in 1:nmovies)
    {
        g = strsplit(genreset[i],split = "|" , fixed = TRUE )[[1]] 
        for(j in 1:length(g))
        {
            value = as.numeric(A[g[j]])
            genres[value,i]=1;
        }
    }
    
    #Calculo das similiraridades entre os filmes respeito a seus generos con o método Jaccard
    library(proxy)	
    sim	=	simil(t(genres),	method="Jaccard")	
    sim	=	as.matrix(sim)	
    sim[is.na(sim)==T]	=	0	
    
    
    model = list(sim = sim,score = scores)
    return (model)
}

FBC.genres.predict <- function(model, user , movie, K)
{
    simil = as.matrix(model$sim)
    score = as.matrix(model$score)
    
    similar.movies	=	order(-simil[movie,])# Ordenando as similiaridades de um filme en forma decrescente
    rated.movies	=	which(score[user,]	>	0)	# Escolher os items que o usuario avalio
    most.similar.rated	=	intersect(similar.movies,	rated.movies)[1:min(K,length(rated.movies))] #interseção	
    
    
    #Calculo de la predição
    sumSim	=	0	
    sumWeight	=	0
    if(is.na(most.similar.rated[1]))#Se la interseção e vacia retorna a media
    {
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

computeTFIDF	<-	function(row)# TF - IDF 
{	
    df	=	sum(row[1:3564]	>	0)	
    w	=	rep(0,	length(row))	
    w[row	>	0]	=	(1	+	log2(row[row	>	0]))	*	log2(3564/df)	
    return(w)	
}

Repart <- function(dataset)
{
    dataset = as.matrix(dataset);
    data = c();
    for( i in 1:3564) # Iterando sobre todos os items
    {
        ids = which(as.numeric(reviewset[,1]) == i) # Numero de reviews da pelicula
        if(length(ids)!=0)
            data = rbind(data, dataset[sample(ids,1),]) # Sample de un dato
    }
    return (data)
}

FBC.metadados.model <- function(dataset,testset,reviewset)
{
    dataset = as.matrix(dataset)
    testset = as.matrix(testset)
    reviewset = as.matrix(reviewset)
    testset = testset[,2:3]
    
    users = dataset[,1] #ID dos usuários
    movies = dataset[,2] #ID dos filmes
    ratings = dataset[,3] # Ratings
    
    nusers = max(c(dataset[,1],testset[,1])) # número de usuarios
    nmovies = max(c(dataset[,2],testset[,2])) # número de filmes
    scores = matrix(rep(0,nusers*nmovies),nusers,nmovies) # interacción usuario filme 
    
    for(i in 1:length(users))
        scores[users[i],movies[i]] = ratings[i] #construçao da matriz de interação usuario filme
    
    library(tm)	
    library(SnowballC)
    
    reviews = c()
    for(i in 1:nmovies)
    {
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
    
    model = list(score = scores,sim = sim)
    return (model)
}

FBC.metadados.predict <- function(model, user , movie, K)
{
    simil = as.matrix(model$sim)
    score = as.matrix(model$score)
    
    similar.movies	=	order(-simil[movie,])# Ordenando as similiaridades de um filme en forma decrescente
    rated.movies	=	which(score[user,]	>	0)	# Escolher os items que o usuario avalio
    most.similar.rated	=	intersect(similar.movies,	rated.movies)[1:min(K,length(rated.movies))] #interseção	
    
    
    #Calculo de la predição
    sumSim	=	0	
    sumWeight	=	0
    if(is.na(most.similar.rated[1]))#Se la interseção e vacia retorna a media
    {
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

Hp.FBC.KKNS.predict <- function(modelgenres,modelmetadados,modelsio,user,movie,K)
{
    p1 = FBC.genres.predict(modelgenres,user,movie,K)
    p2 = FBC.metadados.predict(modelmetadados,user,movie,K)
    p3 = FBC.sio.predict(modelsio,user,movie,K)
    return ((p1 + p2 +p3)/3)
}

Hp.FBC.KKNS.test <- function(modelgenres,modelmetadados,modelsio,testset,vizinhos,name)
{
    testset = as.matrix(testset[,2:3])
    testUser = testset[,1] #Usuarios
    testMovie = testset[,2] #Filmes
    
    tam = length(testUser)
    ids = (1:(tam))
    ids = ids-1
    ratings = rep(0,tam) #vetor para os ratings
    
    for ( i in 1:tam)
    {
        ratings[i] = Hp.FBC.KKNS.predict(modelgenres, modelmetadados , modelsio, testUser[i],testMovie[i],vizinhos)  #predição
    }
    
    my.dataset <- data.frame(id = ids, rating = ratings) #Criação de um dataframe
    write.csv(my.dataset,name,row.names=F)  #Exportacion
}

Hp.FBC.KKNS.pretest <- function(modelgenres, modelmetadados , modelsio,testset) #Devolvera uma lista con los errores cuadraticos de las prediçoes com 1..K viznhos
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
            ratings[j] =Hp.FBC.KKNS.predict(modelgenres, modelmetadados , modelsio, testUser[i],testMovie[i],vizinhos)  #predição
        }
        r = ratings;
        print(paste("FBC_KNN_",i,sep=""))
        e = RMSE(r,testset[,3]) #calculo de Error
        print(e)
        error = c(error,e) # lista de errors
    }
    
    return (error/sum(error)) # normalização
}