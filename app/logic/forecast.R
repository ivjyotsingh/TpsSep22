


#Creating a dataframe to handle the one time covid shock as a holiday
covid <- data_frame(
  holiday = "Covid",
  ds = as.Date(c("2020-02-01")),
  lower_window = 0,
  upper_window = 182
)

#Initiating vectors for nested for loops
countries <- c("Belgium","France","Germany","Italy","Poland","Spain")
stores <- c("KaggleMart","KaggleRama")
products <- c("Kaggle Advanced Techniques","Kaggle Getting Started",
              "Kaggle Recipe Book","Kaggle for Kids: One Smart Goose")