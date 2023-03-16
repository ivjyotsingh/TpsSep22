box::use(
  utils[read.csv]
)

box::use(
  here
)


#' @export
fetch_data <- function(){
  read.csv(here::here("Data","train.csv"))
}

#' @export
fetch_test <- function(){
  read.csv(here::here("Data","test.csv"))
}
