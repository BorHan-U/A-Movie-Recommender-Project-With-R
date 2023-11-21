
#-----------------------------------------------------------------------+
# Title: Movie Recommendation system	 Course: CSCI - 4340	| Sec: 01	|
#-----------------------------------------------------------------------+	  
#		GROUP MEMBERS
#-----------------------------------------------------------------------+
# 	   Name: 					            Matric No:	      |
#-----------------------------------------------------------------------+
# Md Borhan Uddin 					          1822665	      	|
# Saif al Faied						            1828615	        |
# Mahin Islam						              1723486	      	|
# IMRAN						                    1828271	      	|
#-----------------------------------------------------------------------+


#Required libraries
library(recommenderlab)
library(ggplot2)                       
library(data.table)
library(reshape2)
library(dplyr)

#Load Datasets and datasets summary
movie_data_set <- read.csv("/Users/borhan/Desktop/Machine Learnings/Project/Code/movies.csv",stringsAsFactors=FALSE)
rating_data_set <- read.csv("//Users/borhan/Desktop/Machine Learnings/Project/Code/ratings.csv")
str(movie_data_set)
summary(movie_data_set)
head(movie_data_set)
summary(rating_data_set) 
head(rating_data_set)

#pre-processing
genre <- as.data.frame(movie_data_set$genres, stringsAsFactors=FALSE)
library(data.table)
genre2 <- as.data.frame(tstrsplit(genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) #DataFlair
colnames(genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_matrix1 <- matrix(0,10330,18)
genre_matrix1[1,] <- list_genre
colnames(genre_matrix1) <- list_genre

for (index in 1:nrow(genre2)) {
  for (col in 1:ncol(genre2)) {
    gen_col = which(genre_matrix1[1,] == genre2[index,col]) #Author DataFlair
    genre_matrix1[index+1,gen_col] <- 1
  }
}
genre_matrix2 <- as.data.frame(genre_matrix1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_matrix2)) {
  genre_matrix2[,col] <- as.integer(genre_matrix2[,col]) #convert from characters to integers
} 
str(genre_matrix2)

Search_Matrix <- cbind(movie_data_set[,1:2], genre_matrix2[])
head(Search_Matrix)   

ratingMatrix <- dcast(rating_data_set, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1])

#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

recommendation_model$IBCF_realRatingMatrix$parameters

similarity_matrix <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_matrix)

image(as.matrix(similarity_matrix), main = "User's Similarities")

movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(movie_similarity)

image(as.matrix(movie_similarity), main = "Movies similarity")


rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) # extracting unique ratings

Table_for_ratings <- table(rating_values) # creating a count of movie ratings
Table_for_ratings

#visualization
# count views for each movie
Movie_views <- colCounts(ratingMatrix)  
# create dataframe of views
Table_views <- data.frame(movie = names(Movie_views),
                          views = Movie_views) 
# sort by number of views
Table_views <- Table_views[order(Table_views$views,
                                 decreasing = TRUE), ] 
Table_views$title <- NA
for (index in 1:10325){
  Table_views[index,3] <- as.character(subset(movie_data_set,
                                              movie_data_set$movieId == Table_views[index,1])$title)
}
Table_views[1:5,]


# using ggplot, top films are visualized by users
ggplot(Table_views[1:5, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'brown') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggtitle("Top films viewed by users")


image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of 25 X 25")


movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings

minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of top movie users by movies")

#Average rating Decomposition by users
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("brown"), col=I("black")) +
  ggtitle("Decomposition of per users rating by average") my possible telawah section is 3
sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

#Building the Recommendation System using R
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters
recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)


#extracting model info
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap following fist row and coloumn")

#coloumn count
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("red"), col=I("blue"))+ ggtitle("Column count segmentation")

top_recommendations <- 10 # the number of items to recommend to each user

#predicted recommendation by model
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

First_user <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_First_user <- predicted_recommendations@itemLabels[First_user]
movies_user2 <- movies_First_user
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data_set,
                                             movie_data_set$movieId == movies_First_user[index])$title)
}
movies_user2

# matrix with the recommendations for each user
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) 
#dim(recc_matrix)
recommendation_matrix[,1:4]


##Decomposing of items by number in IBCF
number_of_items <- factor(table(recommendation_matrix))
chart_title <- "Decomposing of items number in IBCF"
qplot(number_of_items, fill=I("green"), col=I("red")) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)
for(i in 1:4) {
  table_top[i,1] <- as.character(subset(movie_data_set,
                                        movie_data_set$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie Title", "No. of Items")
head(table_top)


