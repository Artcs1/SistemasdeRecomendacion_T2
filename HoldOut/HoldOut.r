RMSE <- function(traindata,testdata)
{
    return ((sqrt(sum((traindata-testdata)^2)))/length(testdata)) # Error 
}

Holdout <- function(dataset,train.size = 0.7)
{
    dataset = as.matrix(dataset)
    ids = sample(0:nrow(dataset),size = floor(nrow(dataset)*train.size)) # Sample do conjunto de treinamento
    
    traindata = dataset[ids,] #Novo conjunto de treinamento
    testdata = dataset[-ids,] #Novo conjunto de test
    
    
    testdata = cbind(rep(1,nrow(testdata)),testdata)
    
    return (list(treinamento = traindata, test = testdata))
}
