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

FBC.genres.test <- function(model,testset,vizinhos,name)
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
        ratings[i] = FBC.genres.predict(model = model, user = testUser[i],movie = testMovie[i],K = vizinhos)  #predição
    }
    
    my.dataset <- data.frame(id = ids, rating = ratings) #Criação de um dataframe
    write.csv(my.dataset,name,row.names=F)  #Exportacion
}

FBC.genres.pretest <- function(model,testset)
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
            ratings[j] = FBC.genres.predict(model = model, user = testUser[j],movie = testMovie[j],K = i)  #predição
        }
        r = ratings;
        print(paste("FBC_KNN_",i,sep=""))
        e = RMSE(r,testset[,3]) #calculo de Error
        print(e)
        error = c(error,e) # lista de errors
    }
    
    return (error/sum(error)) # normalização
}