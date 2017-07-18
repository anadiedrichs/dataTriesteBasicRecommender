library(readr)
YOUR_FULL_PATH_to_RATTING <- "~/phd-repos/ictp-datascience/movie-dataset-repo-pair-project/dataTriesteBasicRecommender/ml-latest-small/ratings.csv"
YOUR_FULL_PATH_to_Movies <- "~/phd-repos/ictp-datascience/movie-dataset-repo-pair-project/dataTriesteBasicRecommender/ml-latest-small/movies.csv"

#' load dataset
ratings <- read_csv(YOUR_FULL_PATH_to_RATTING)
movies <- read_csv(YOUR_FULL_PATH_to_Movies)

#' see structure of movies
#'
str(movies)
str(ratings)

#' we want to see some data
#' 
head(movies,n=10)
head(movies,n=10)

#' merging the datasets by movieId
#' 
dataset <- merge(movies,ratings,by="movieId")

#' sorting to see the most watched movies
library(plyr)
counting <- count(dataset,vars="title")
head(counting)
#' plotting most seen movies
ggplot(data=counting, aes(freq)) + geom_bar()
#' MOST rated movies

head(counting[order(counting$freq,decreasing=TRUE),])

#' order to see the first most seen or rated movies
#' 
head(sort(dataset,decreasing = TRUE))

#'ratings histogram
library(ggplot2)
ggplot(dataset, aes(rating)) + geom_histogram()

#' install data.table
#' 
install.packages("data.table")
library(data.table)

#getting all the genres of the movies
library(data.table)

movgen <- as.data.frame(movies$genres,stringsAsFactors = FALSE)
unlist(strsplit(movgen$`movies$genres`, "[|]"))
movgen_list <- unique(unlist(strsplit(movgen$`movies$genres`, "[|]")))

#cleaning the genres to get an matrix 
movgen2 <- as.data.frame(tstrsplit(movgen[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(movgen2) <- c(1:7)

#' matrix of genres by movie
mm <- matrix(0,ncol = length(movgen_list),nrow = nrow(movies))
colnames(mm) <- movgen_list
#
for(i in 1:nrow(mm))
{
  mm[i,which(colnames(mm) %in% movgen2[i,])] <- 1
}

movgen_matrix2 <- mm

#' matrix of rating users
#' 0 para null values, 1 for ratings over 3, -1 for ratings less or equal than 3
#'  
binary_ratings <- ratings
binary_ratings[which(binary_ratings$rating <=3),"rating"] <- -1
binary_ratings[is.na(binary_ratings$rating),"rating"] <- 0
binary_ratings[which(binary_ratings$rating > 3),"rating"] <- 1

#' is there any null value yet?
binary_ratings[!complete.cases(binary_ratings),]

# professor's code
binary_ratings2 <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binary_ratings2)){
  binary_ratings2[which(is.na(binary_ratings2[,i]) == TRUE),i] <- 0
}

binary_ratings2 = binary_ratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

dim(binary_ratings2)