# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wineo <- subset(wine, select = -Type)
wineo_scale <- scale(wineo)
summary(wineo)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wineo_scale)

# Exercise 2:
#   * How many clusters does this method suggest?

# By the bend in the graph from 2-4, 3 would be a likely choice.

#   * Why does this method work? What's the intuition behind it?

# The bend of the curve observed in the graph indicates a point in time where additional clustering 
# would only give marginal improvement of accuracy and precision and continuation along the curve 
# increases the risk of over-fitting.

##CROWDSOURCED
# The SSE (Sum of Squared Errors of Prediction) is defined as the sum of the squared distance 
# between each member of a cluster and its cluster centroid.
# A higher SSE indicates lower accuracy and lower precision, while a lower SSE indicates higher
# accuracy and higher precision. 

#   * Look at the code for wssplot() and figure out how it works

## wssplot() = custom-built function that accepts a scaled dataset with dummy variable "data"
# "nc" = # of total potential clusters, hard-coded
# set.seed = # at which to set R's random number generator (for control)
## function actions
# 1) Count number of rows in dataset "data", and then subtract 1 from that number
# 2) Multiply result by sum of variance across columns
# 3) For each column, 2 through 15, set.seed at 1234, and calculate within-class sum of squares scatter matrix
# 4) Plot data on a line chart, connected by distinct points

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wineo_scale, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

# This method also suggests 3 clusters.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

k = 3
fit.km <- kmeans(wineo_scale, centers = k, iter.max = 1000)
str(fit.km)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

# Yes, because the majority of the wines are clustered within the 3 clusters specified, 
# with only outliers at 2.

table(fit.km$cluster, wine$Type)

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library

library(cluster)
clusplot(wineo_scale, fit.km$cluster, color = TRUE)

# * Would you consider this a good clustering?
# Yes, because the majority of the wines are clustered within the 3 clusters specified