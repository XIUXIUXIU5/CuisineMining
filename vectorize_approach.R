library("rjson")
json_file <- "train.json"
json_data <- fromJSON(file=json_file)

unique_ingre <- unique(unlist(sapply(json_data, function (x) {x$ingredients})))

#sapply(unique_ingre, function (x) { x %in% unlist(single$ingredients)})

make_ingre_vector <- function (json_data, unique_ingre) {
  r <- c()
  for (i in 1:length(json_data)) {
    r[[i]] <- unique_ingre %in% json_data[[i]]$ingredients
  }
  return (r)
}
receipe.cuisine <- unlist(sapply(json_data, function (x) {x$cuisine}))
receipe.ingredients <- make_ingre_vector(json_data,unique_ingre)

#receipe.ingredients <-t(receipe.ingredients)
#colnames(receipe.ingredients) <- unique_ingre
receipe.df <- data.frame(receipe.cuisine,receipe.ingredients)

library(party)





