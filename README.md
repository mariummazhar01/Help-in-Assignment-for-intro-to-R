# Help-in-Assignment-for-intro-to-R
# If you are working on your local Jupyter notebook, please uncomment the below code and install the packages
#install.packages("httr")
#install.packages("rvest")

install.packages("httr")
install.packages("rvest")
install.packages("tidyverse")
#require("httr")
#require("rvest")

library(httr)
library(rvest)
library(tidyverse)
get_wiki_covid19_page <- function() {
    
  # Our target COVID-19 wiki page URL is: https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country  
  # Which has two parts: 
    # 1) base URL `https://en.wikipedia.org/w/index.php  
    # 2) URL parameter: `title=Template:COVID-19_testing_by_country`, seperated by question mark ?
    
  # Wiki page base
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  # You will need to create a List which has an element called `title` to specify which page you want to get from Wiki
  # in our case, it will be `Template:COVID-19_testing_by_country`
  query_params <- list(title = "Template:COVID-19_testing_by_country")
 
  # - Use the `GET` function in httr library with a `url` argument and a `query` arugment to get a HTTP response
  response <- GET(wiki_base_url, query = query_params)
    
  # Use the `return` function to return the response
  return(response)
}
# Call the get_wiki_covid19_page function and print the response
get_wiki_covid19_page()
# Get the root html node from the http response in task 1 
root_node <-read_html(get_wiki_covid19_page())
root_node
# Get the table node from the root html node
table_node <- html_nodes(root_node, "table")
table_node 
# Read the table node and convert it into a data frame, and print the data frame for review
covid_df <- as.data.frame(html_table(table_node[2]))
covid_df  
# Print the summary of the data frame
cov_19 <- summary(covid_df)
cov_19
preprocess_covid_data_frame <- function(data_frame) {
    
    shape <- dim(data_frame)

    # Remove the World row
    data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
    # Remove the last row
    data_frame <- data_frame[1:172, ]
    
    # We dont need the Units and Ref columns, so can be removed
    data_frame["Ref."] <- NULL
    data_frame["Units[b]"] <- NULL
    
    # Renaming the columns
    names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
    
    # Convert column data types
    data_frame$country <- as.factor(data_frame$country)
    data_frame$date <- as.factor(data_frame$date)
    data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
    data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
    data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
    data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
    data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
    
    return(data_frame)
}
# call `preprocess_covid_data_frame` function and assign it to a new data frame
new_data_frame<-preprocess_covid_data_frame(covid_df)
new_data_frame 
