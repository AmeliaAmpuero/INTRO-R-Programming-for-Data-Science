library(httr)
library(rvest)

####### TASK 1 ####### 
get_wiki_covid19_page <- function() {
  #Wiki page base URL
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  
  #URL parameter:
  query <- list(title="Template:COVID-19_testing_by_country")
  
  #Send HTTP GET request to the Wiki page URL
  response <- GET(url = wiki_base_url, query = query)
  
  #Return the response
  return(response)
}
response <- get_wiki_covid19_page()
response

####### TASK 2 #######
table_url <- "https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country"

root_node <-read_html(table_url)

table_node <- html_table(root_node)

df <- as.data.frame(table_node[[2]])
df

####### TASK 3 #######
colnames(df) <- c("country", "date", "tested","units","confirmed","confirmed.tested.ratio","tested.population.ratio","confirmed.population.ratio","ref")

df$country <- as.factor(df$country)
df$date <- as.factor(df$date)
df$tested <- as.numeric(gsub(",","",df$tested))
df$confirmed <- as.numeric(gsub(",","",df$confirmed))
df$'confirmed.tested.ratio' <- as.numeric(gsub(",","",df$`confirmed.tested.ratio`))
df$'tested.population.ratio' <- as.numeric(gsub(",","",df$`tested.population.ratio`))
df$'confirmed.population.ratio' <- as.numeric(gsub(",","",df$`confirmed.population.ratio`))

df["ref"] <- NULL
df["units"] <- NULL

preprocess_covid_data_frame <- function(df) {
  shape <- dim(df)
  # Remove the last row
  df<- df[1:172, ]
  return(df)
}
data_frame<-preprocess_covid_data_frame(df)
data_frame
write.csv(data_frame, "C:/Users/Amelia/Desktop/IBM CERTIFICATE/5 Introduction to R programming/Introduction to R Programming for Data Science/Task3_data_frame.csv", row.names = FALSE)

####### TASK 4 #######
subset_df <- df[5:10, c("country", "confirmed")]
subset_df

####### TASK 5 #######
sum_confirmed <- sum(data_frame$confirmed)
sum_tested <- sum(data_frame$tested)
positive_ratio <- sum_confirmed/sum_tested
positive_ratio

####### TASK 6 #######
print(data_frame$country)
class(data_frame$country)
data_frame$country <- as.character(data_frame$country)
sorted_A.Z <- sort(data_frame$country)
sorted_Z.A <- sort(data_frame$country,decreasing = TRUE)
sorted_Z.A

####### TASK 7 #######
countries_United<-regexpr("United.+",data_frame$country)
regmatches(data_frame$country,countries_United)

####### TASK 8 #######
subset_OMAN <- df["123", c("country", "confirmed","confirmed.population.ratio")]
subset_OMAN

subset_PERU<<- df["129", c("country", "confirmed","confirmed.population.ratio")]
subset_PERU

####### TASK 9 #######
if (subset_OMAN$confirmed.population.ratio>subset_PERU$confirmed.population.ratio) {
  print("Oman has higher COVID-19 infection risk")
} else {
  print("Peru has higher COVID-19 infection risk")
}

####### TASK 10 #######
subset_threshold <- data_frame[,c("country","confirmed.population.ratio")]
subset_threshold

for (i in 1:nrow(subset_threshold)) {
  # Check if the ratio is less than 1 percent
  if (subset_threshold$confirmed.population.ratio[i] < 0.01) {
    # Print the name of the country
    print(subset_threshold$country[i])
  }
}










