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

# Normalizing the data 
norm_clus<-scale(data) 
dist1<-dist(norm_clus,method = "euclidean") 

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") 

plot(fit1) # Displaying Dendrogram
plot(fit1, hang=-1)

# Cutting the dendrogram for 3 clusters
groups<-cutree(fit1,3) 
rect.hclust(fit1, k=3, border="red")

# cluster numbering 
membership_1<-as.matrix(groups) 

View(membership_1)

# binding column wise with orginal data
final1<-cbind(membership_1,wine) 
View(final1)

# Grouped data
aggregate(final1[,-c(1:2)],by=list(membership_1),FUN=mean)




# K-means clustering
View(norm_clus)
normalized_data <- norm_clus

#elbow curve or scree-plot to decide the k value
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 
for (i in 1:9) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
title(sub = "K-Means Clustering Scree-Plot")

# K means clustering model
fit <- kmeans(normalized_data, 3) 
str(fit)

# append cluster membership
final<- data.frame(wine, fit$cluster) 
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)

# Grouped data
aggregate(data[,1:13], by=list(fit$cluster), FUN=mean)
