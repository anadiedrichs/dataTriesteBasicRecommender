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
# install.packages("data.table")
library(data.table)

#getting all the genres of the movies
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

#' dummy change of name...
movgen_matrix2 <- mm

#' convert to int, checking if that works
movgen_matrix2 <- as.data.frame(movgen_matrix2, stringsAsFactors=FALSE)
for (c in 1:ncol(movgen_matrix2)) {
  movgen_matrix2[,c] <- as.integer(movgen_matrix2[,c])
} #convert from characters to integers
#' other way to do it is:
#' 
sapply(movgen_matrix2,as.integer)

#' matrix of rating users
#' 0 para null values, 1 for ratings over 3, -1 for ratings less or equal than 3
#'  
binary_ratings <- as.data.frame(ratings, stringsAsFactors = FALSE)
binary_ratings[which(binary_ratings$rating <=3),"rating"] <- as.integer(-1)
binary_ratings[is.na(binary_ratings$rating),"rating"] <- as.integer(0)
binary_ratings[which(binary_ratings$rating > 3),"rating"] <- as.integer(1)

#' is there any null value yet?
binary_ratings[!complete.cases(binary_ratings),]

#' professor's code
#' por qué queda una matrix de 9066 x 671??
#' 9066 revisiones o reviews
#' Rows are movieIds, cols are userIds
#' profesor's code:
binary_ratings2 <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binary_ratings2)){
  binary_ratings2[which(is.na(binary_ratings2[,i]) == TRUE),i] <- as.integer(0)
}

binary_ratings2 = binary_ratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

dim(binary_ratings2)

#' You might notice that the movies dataset has 9125 movies, but the ratings dataset only has 9066
#' movies. To deal with this, remove the movies that have never been rated from the genres matrix.
#' Use the code below:
unique_movieIds <- unique(movies$movieId) #9125
unique_ratings <- unique(ratings$movieId) #9066
movies2 <- movies[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from movgen_matrix2
movgen_matrix3 <- movgen_matrix2[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movgen_matrix3) <- NULL
#

#' checking dimensions
dim(movgen_matrix3)
dim(binary_ratings2)

#' Now we can calculate the dot product of the genre matrix (movgen_matrix3) and the ratings
#' matrix (binary_ratings2) and obtain the user profiles. Ensure you convert to binary scale

user_profile <- matrix(0,nrow = ncol(movgen_matrix3), ncol=ncol(binary_ratings2))

for(c in 1:ncol(binary_ratings2))
{
  for(i in 1:ncol(movgen_matrix3))
  {
    user_profile[i,c]<- sum(movgen_matrix3[,i]*binary_ratings2[,c])   
  }
}

# turn user profile matrix in binary values
user_profile[which(user_profile<=0),] <- 0
user_profile[which(user_profile>0),] <- 1

#' Trying metrics

