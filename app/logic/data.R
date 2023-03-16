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

