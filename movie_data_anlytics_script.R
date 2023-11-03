#### Load necessary libraries
#### libraries = packages 
#### package contains functions 
#### functions = pre-written code

install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
library(dplyr)
library(ggplot2)
library(caret)

#####Read the dataset
data <- read.csv("/Users/Sourabh/Downloads/moviedata.csv")

##dataframe, vectors


# Descriptive Analysis

# Summary statistics of numerical variables
summary_data <- summary(data[, c("gross_collections", "Year", "inflation_adjusted_value")])
print("Summary Statistics:")
print(summary_data)

# Frequency tables for categorical variables
cat_vars <- c("Language", "Genre", "director", "lead_actor")
cat_freq <- table(data[, "Language"])
print(cat_freq)

cat_freq <- table(data[, "lead_actor"])
print(cat_freq)



par(mfrow=c(1,1)) # Create a 1x3 layout for plots
hist(data$gross_collections, main="Gross Collections", xlab="Value", col="lightblue")

hist(data$Year, main="Year", xlab="Value", col="lightblue")
hist(data$inflation_adjusted_value, main="Inflation Adjusted Value", xlab="Value", col="lightblue")


par(mfrow=c(1,2)) # Create a 1x2 layout for plots
plot(data$Year, data$gross_collections, main="Year vs. Gross Collections", xlab="Year", ylab="Gross Collections", pch=19, col="blue")
plot(data$Year, data$inflation_adjusted_value, main="Year vs. Inflation Adjusted Value", xlab="Year", ylab="Inflation Adjusted Value", pch=19, col="green")

# Bar charts for the number of movies by actor, genre, director, and language
par(mfrow=c(2,2)) # Create a 2x2 layout for plots

data <- data %>%
  arrange(desc(inflation_adjusted_value))  

# Number of movies by actor
actor_counts <- data %>% group_by(lead_actor) %>% summarise(count = n()) 
ggplot(actor_counts, aes(x = lead_actor, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Movies by Actor", x = "Actor", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of movies by genre
genre_counts <- data %>% group_by(Genre) %>% summarise(count = n())
ggplot(genre_counts, aes(x = Genre, y = count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Number of Movies by Genre", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of movies by director
director_counts <- data %>% group_by(director) %>% summarise(count = n())
ggplot(director_counts, aes(x = director, y = count)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Number of Movies by Director", x = "Director", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of movies by language
language_counts <- data %>% group_by(Language) %>% summarise(count = n())
ggplot(language_counts, aes(x = Language, y = count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Number of Movies by Language", x = "Language", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization of Inflation-Adjusted Collections of Rajinikanth's Movies Over Time
rajinikanth_data <- data %>%
  filter(lead_actor == "Rajinikanth")

# Create a scatter plot with grid lines for each year
plot(rajinikanth_data$Year, rajinikanth_data$inflation_adjusted_value, 
     pch = 19, col = "blue", 
     xlab = "Year", ylab = "Inflation-Adjusted Collection", 
     main = "Inflation-Adjusted Collections of Rajinikanth's Movies Over Time",
     xaxt = "n")


# Add grid lines for each year
axis(1, at = unique(rajinikanth_data$Year), labels = unique(rajinikanth_data$Year))
grid()


# Predictive Analysis

# Linear Regression: Predicting gross collections based on year
model_lm <- lm(gross_collections ~ Year, data = data)
summary(model_lm)
print("Linear Regression - Predicting Gross Collections based on Year:")


# Linear Regression: Predicting gross collections based on language, director, and lead actor
model_lm_multi <- lm(gross_collections ~ Language + director + lead_actor, data = data)
summary(model_lm_multi)
print("Linear Regression - Predicting Gross Collections based on Language, Director, and Lead Actor:")
x
# New movie information for prediction
new_movie <- data.frame(
  Language = "Tamil",
  director = "Lokesh Kanagaraj",
  lead_actor = "Vijay"
)

# Predicting gross collections for the new movie
predicted_collection <- predict(model_lm_multi, newdata = new_movie)
print(paste("Predicted Gross Collections for the new movie:", round(predicted_collection, 2)))



