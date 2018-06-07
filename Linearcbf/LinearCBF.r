LinearCBF.model <- function(dataset,testset,genreset,lr=0.05,reg = 0.002,miter = 10)
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
    
    genres = t(genres); #tranposto da matriz
    
    ngenres = ncol(genres);
    genres = as.matrix(cbind(genres,1)) #adicionando uma columna
    profiles = matrix(rnorm(nusers*(ngenres+1),mean = 0 ,sd = 0.1),nusers,ngenres +1)#criação dos perfiles
    
    error = c()
    for(l in 1:miter) #numero de iterações
    {
        squared_error = 0
       for(j in 1:length(users)) # Por cada interação de tudos os usuarios
       {
           u = users[j]
           i = movies[j]
           r_ui = ratings[j]
           e_ui = profiles[u,]%*%genres[i,]-r_ui #erro da nota prevista com a nota real 
           squared_error = squared_error + e_ui^2  # sumando errores cuadraticos
           for(k in 1:ngenres)
           {
               profiles[u,k] = profiles[u,k] - lr * (e_ui*genres[i,k] + reg * profiles[u,k]) #Actualização de os profiles
           }
           k = ngenres + 1
           profiles[u,k] = profiles[u,k] - lr*(e_ui * genres[i,k])
       }
        squared_error = sqrt(squared_error/length(users))
        error = c(error,squared_error)
    }
    
    model = list(profiles = profiles ,genres = genres,error = error)
    return (model)
}

LinearCBF.predict <- function(model,user,movie)
{
    profiles = as.matrix(model$profiles)
    genres = as.matrix(model$genres)
    return (profiles[user,]	%*%	genres[movie,]) #predição
}

LinearCBF.test <- function(model,testset,name)
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
        ratings[i] = LinearCBF.predict(model = model, user = testUser[i],movie = testMovie[i])  #predição
    }
    
    my.dataset <- data.frame(id = ids, rating = ratings) #Criação de um dataframe
    write.csv(my.dataset,name,row.names=F)  #Exportacion
}


LinearCBF.pretest <- function(dataset,testset,genreset) #Devolvera uma lista con los errores cuadraticos de las prediçoes com 1..K viznhos
{
    dataset = as.matrix(dataset)
    testset = as.matrix(testset[,2:4])
    testUser = testset[,1] #Usuarios
    testMovie = testset[,2] #Filmes
    
    tam = length(testUser)
    ids = (1:(tam))
    ids = ids-1
    ratings = rep(0,tam) #vetor para os ratings
    error = c()
    
    regularization = seq(0.0,0.1,0.01)
    learningrate = seq(0.01,0.1,0.01)
    maxiter = seq(10,30,5)
    
    tam = length(testUser)
    ids = (1:(tam))
    ids = ids-1
    ratings = rep(0,tam) #vetor para os ratings
    error = c()
    nombres = c()
    
    for(i in learningrate)
    {
        for(j in regularization)
        {
            for(k in maxiter)
            {
                print(i)
                print(j)
                print(k)
                modelo = LinearCBF.model(dataset,testset,genreset,i,j,k)
                for(l in 1:tam)
                {
                    ratings[l] = LinearCBF.predict(model = modelo, user = testUser[l],movie = testMovie[l])  #predição
                }
                r = ratings;
                cad = paste("LinearFBC","lr",i,"rt",j,"miter",k,sep="_");
                print(cad)
                e = RMSE(r,testset[,3]) #calculo de Error
                print(e)
                nombres = c(nombres,cad)
                error = c(error,e) # lista de errors
            }  
        }
    }
        
    names(error) = nombres;
    return (error/sum(error)) # normalização
}