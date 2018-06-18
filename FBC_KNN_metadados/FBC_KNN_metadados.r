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

FBC.metadados.test <- function(model,testset,vizinhos,name)
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
        ratings[i] = FBC.metadados.predict(model = model, user = testUser[i],movie = testMovie[i],K = vizinhos)  #predição
    }
    
    my.dataset <- data.frame(id = ids, rating = ratings) #Criação de um dataframe
    write.csv(my.dataset,name,row.names=F)  #Exportacion
}

FBC.metadados.pretest <- function(model,testset) #Devolvera uma lista con los errores cuadraticos de las prediçoes com 1..K viznhos
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
            ratings[j] = FBC.metadados.predict(model = model, user = testUser[j],movie = testMovie[j],K = i)  #predição
        }
        r = ratings;
        print(paste("FBC_KNN_",i,sep=""))
        e = RMSE(r,testset[,3]) #calculo de Error
        print(e)
        error = c(error,e) # lista de errors
    }
    
    return (error/sum(error)) # normalização
}
