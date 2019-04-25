### loading data
data("iris")
?iris
head(iris)
str(iris)

### Data Summarization
# The dataset 'iris' is from R. It is called Fisher's or Anderson's iris data set.
# 'iris' is a data frame with 150 cases (rows) and 5 variables (columns) named: 
# Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.

### Plotting by Species
library(ggplot2)
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species))+
  geom_point()

### Plot summary
# We can see there are 3 different species in dataset 'iris' and for different species
# they have different petal.width and petal.length.
# for Setosa, the petal width is no more than .75 and petal.length less than 2.
# for Versicolor, the petal width is between 1 and 1.75 and petal.length between 3 and 5.
# for Virginica, the petal width is larger than 1.5 and petal.length larger than 4.5.
# there are not overlaps for setosa and other two species, and some minor overlaps between
# versicolor and virginica


set.seed(222)
irisCluster = kmeans(iris[,3:4], 3)

table(irisCluster$cluster, iris$Species)

# By comparing the result from the self-discovered k-means result with
# the true species type result. We see the k-means results are very close
# to the ture type results. Although there are 6 misrepresented points,
# the result is satisfying.