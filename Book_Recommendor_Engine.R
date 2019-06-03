
setwd("/Users/keerthigopalakrishnan/Downloads/DM2/")

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)



books <- fread('books.csv')
ratings <- fread('ratings.csv')
book_tags <- fread('book_tags.csv')
tags <- fread('tags.csv')



head(books)

##### Check for duplicate ratings #####

ratings[, N := .N, .(user_id, book_id)]
nrow(ratings[N > 1])


# Remving duplicate ratings

ratings <- ratings[N == 1]



# Subsetting to 30% of the data

set.seed(11)
user_fraction <- 0.2
users <- unique(ratings$user_id)
set.seed(11)
sample_users <- sample(users, round(user_fraction * length(users)))
nrow(ratings)


ratings <- ratings[user_id %in% sample_users]
nrow(ratings)

#Exploratory Data Analysis

# No of ratings


ratings %>% 
  ggplot(aes(x = rating, fill = as.factor(rating))) +
  geom_bar() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"))


# No of ratings per user


ratings %>% 
  group_by(user_id) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_bar(fill = "orange", color = "grey20") + coord_cartesian(c(3, 50)) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))


# No of ratings per book

ratings %>% 
  group_by(book_id) %>% 
  summarize(number_of_ratings_per_book = n()) %>% 
  ggplot(aes(number_of_ratings_per_book)) + 
  geom_bar(fill = "violet", color = "grey20", width = 1) + coord_cartesian(c(0,40))+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))




# Top 10 rated books


library(data.table)
library(DT)

books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


library(stringr)
genres <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics", "Contemporary", "Cookbooks", "Crime", "Ebooks", "Fantasy", "Fiction", "Gay and Lesbian", "Graphic Novels", "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", "Memoir", "Music", "Mystery", "Nonfiction", "Paranormal", "Philosophy", "Poetry", "Psychology", "Religion", "Romance", "Science", "Science Fiction", "Self Help", "Suspense", "Spirituality", "Sports", "Thriller", "Travel", "Young Adult"))

exclude_genres <- c("fiction", "nonfiction", "ebooks", "contemporary")
genres <- setdiff(genres, exclude_genres)

available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]

df <- book_tags %>% 
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n / sumN) %>%
  arrange(-percentage) %>%
  left_join(tags, by = "tag_id")






df %>% 
  ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = 'Set3') + labs(y = 'Percentage', x = 'Genre')  +theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))


#Correlation plot

df <- books %>% 
  select(one_of(c("books_count","ratings_count", "average_rating"))) %>% 
  as.matrix()


df <- books %>% 
  select(one_of(c("books_count","original_publication_year","ratings_count", "work_ratings_count", "work_text_reviews_count", "average_rating"))) %>% 
  as.matrix()


library(GGally)
ggcorr(df, use = "pairwise.complete.obs", label=TRUE)


# Restructuring the data for collaborative filtering

ratings_new <- merge(ratings, books, by = "book_id")
ratings_new <- ratings_new[,1:4]

dimensions <- list(user_id = sort(unique(ratings_new$user_id)), book_id = sort(unique(ratings_new$book_id)))
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)
ratingmat_new <- spread(select(ratings_new, book_id, user_id, rating), book_id, rating) %>% select(-user_id)

ratingmat <- as.matrix(ratingmat)
ratingmat_new <- as.matrix(ratingmat_new)
dimnames(ratingmat) <- dimensions
dimnames(ratingmat_new) <- dimensions
ratingmat[1:5, 1:5]

ratingmat_new[1:5, 1:5]



# SVD

ratingmat0 <- ratingmat_new
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
rm(ratingmat0)
gc()

library("recommenderlab")

real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

real_rating_original <- real_ratings

model_svd_cosine <- Recommender(real_ratings, method = "SVD", param = list(method = "cosine", nn = 4))

# Making predictions

current_user <- "21713"

prediction <- predict(model_svd_cosine, real_ratings[current_user, ], type = "ratings")


as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))




eval_scheme <- evaluationScheme(real_ratings[1:500,], method = "bootstrap", k = 100, given = -1, goodRating = 5)
alg <- list("SVD" = list(name = "SVD",  param = list(nn = 4)))
results <- evaluate(eval_scheme , alg, type = "ratings")



df <- lapply(results, function(x) slot(x, "results"))
results_df <- df %>% 
  lapply(function(x) unlist(lapply(x, function(x) unlist(x@cm[ ,"RMSE"])))) %>% 
  as.data.frame() %>% 
  gather(key = "Algorithm", value = "RMSE")

results_df %>% 
  ggplot(aes(Algorithm, RMSE, fill = Algorithm)) +
  geom_bar(stat = "summary") + geom_errorbar(stat = "summary", width = 0.3, size = 0.8) +
  coord_cartesian(ylim = c(0.6, 1.3)) + guides(fill = FALSE)


### UBCF Prediction

current_user <- "21713"

model.ubcf <- Recommender(real_ratings, method = "UBCF", param = list(method = "cosine", nn = 4))

prediction <- predict(model.ubcf, real_ratings[current_user, ], type = "ratings")

as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))  

### popular Prediction

current_user <- "21713"

model.popular <- Recommender(real_ratings, method = "POPULAR", param = NULL)

prediction <- predict(model.popular, real_ratings[current_user, ], type = "ratings")

as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))  

### IBCF

current_user <- "21713"

e <- evaluationScheme(real_ratings[1:500], method="bootstrap", train = 0.8, given=-1, goodRating=5)
IBCF_N_C <- Recommender(getData(e), "IBCF")
prediction <- predict(IBCF_N_C, real_ratings[current_user, ], type = "ratings")

as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


### model comparison

scheme <- evaluationScheme(real_ratings[1:1000], method = "bootstrap", k = 10, given = -1, goodRating = 4)

algorithms <- list("random" = list(name = "RANDOM", param = NULL),
                   "popular" = list(name = "POPULAR"),
                   "UBCF" = list(name = "UBCF"),
                   "SVD" = list(name = "SVD"),
                   "IBCF" = list(name = "IBCF")
)

results <- evaluate(scheme, algorithms, type = "ratings", progress = FALSE)

tmp <- lapply(results, function(x) slot(x, "results"))
res <- tmp %>% 
  lapply(function(x) unlist(lapply(x, function(x) unlist(x@cm[ ,"RMSE"])))) %>% 
  as.data.frame() %>% 
  gather(key = "Algorithm", value = "RMSE")

res %>% 
  mutate(Algorithm=factor(Algorithm, levels = c("random", "popular", "UBCF", "SVD","IBCF"))) %>%
  ggplot(aes(Algorithm, RMSE)) + geom_bar(stat = "summary" , fill = c("palevioletred3","paleturquoise3","lightgoldenrod1","burlywood","darkolivegreen3")) + 
  geom_errorbar(stat = "summary", width = 0.3, size = 0.8) + coord_cartesian(ylim = c(0.6, 1.3)) + 
  guides(fill = FALSE) + ggtitle("Model Comparison")

