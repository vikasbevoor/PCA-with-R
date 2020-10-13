# Loading Wine data
wine <- read.csv("C:/Users/Admin/Downloads/wine (2).csv")
View(wine)

#Data exploration
summary(wine)
str(wine)

# Considering only numerical values for applying PCA
data <- wine[,-1]
attach(data)

# Correlation 
cor(data)

# PCA model with cor = TRUE for getting PCA scores
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)


summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

# graph showing importance of principal components 
plot(pcaObj)
biplot(pcaObj)

# Graph of variance changes with no of principle components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")

# Top 3 PCA Scores 
pcaObj$scores[,1:3]

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
wine <-cbind(wine,pcaObj$scores[,1:3])
View(wine)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-wine[,15:17]
View(clus_data)

# Normalizing the data 
norm_clus<-scale(clus_data) 
dist1<-dist(norm_clus,method = "euclidean") 

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") 

plot(fit1) # Displaying Dendrogram
plot(fit1, hang=-1)

# Cutting the dendrogram for 4 clusters
groups<-cutree(fit1,4) 
rect.hclust(fit1, k=4, border="red")

# cluster numbering 
membership_1<-as.matrix(groups) 

View(membership_1)

# binding column wise with orginal data
final1<-cbind(membership_1,wine) 
View(final1)

# Grouped data
aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)


# K-means clustering
View(clus_data)

# Normalizing the data
normalized_data <- scale(clus_data)
View(normalized_data)

#elbow curve or scree-plot to decide the k value
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 
for (i in 1:9) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
title(sub = "K-Means Clustering Scree-Plot")

# K means clustering model
fit <- kmeans(normalized_data, 4) 
str(fit)

# append cluster membership
final<- data.frame(wine, fit$cluster) 
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
View(data)
# Grouped data
aggregate(data[,1:13], by=list(fit$cluster), FUN=mean)
