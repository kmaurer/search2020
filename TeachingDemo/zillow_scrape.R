### Code for web-scraping the Zillow data for kNN Teaching Demo

# Load Packages
library(tidyverse)
# devtools::install_github('fascinatingfingers/ZillowR')
library(ZillowR)
library(XML)
library(xml2)
library(rvest)

#---------------------
###
source("MyZillowSecretKey.R") # You would need to set up your own account and get an ID
set_zillow_web_service_id(my_zillow_id)

### Store the information about my house
my_house <- GetSearchResults(address = '5839 K Bell', citystatezip = 'Oxford, OH')
my_house
# Convert from xml to a nested list of home attributes
my_house_list <- xmlToList(my_house$response)
names(my_house_list) <- c("house_id","urls","location","zestimate","misc")
my_house_list$zestimate
my_house_list$zestimate$amount$text

## Use GetComps() function from ZillowR package to find 25 similar houses to mine
ncomp = 25
my_comps <- GetComps(zpid = 92370503, count = ncomp, rentzestimate = TRUE)
my_comp_list <- xmlToList(my_comps$response)
str(my_comp_list[[2]][1,3])

# look at list properties
my_comp_list[[2]]["address",3][[1]]$street
my_comp_list[[2]]["zpid",3][[1]]

my_comp_list[[2]]["links",4][[1]]$homedetails
my_comp_list[[2]]["zestimate",3]
# Doesn't contain very much info, BUT does contain url links for all comps

# Pull one comparable house
my_comp_list[[2]]["links",4][[1]]$homedetails

# pick a comparable from set of ncomp
comp_num <- 4

# build function that can take any zillow details url and return set of features
get_house_df <- function(home_url){
  comp_html <- read_html(home_url)
  
  #convert html to set of strings
  comp_scrape <- comp_html %>%
    html_nodes(".fact-label , .fact-value") %>%
    html_text()
  # leverage colon structure for identifying variable names, the item that follows is the value
  var_idx <- c(str_which(comp_scrape, ":"))
  
  comp_df <- data.frame(key=comp_scrape[var_idx],
                        value=comp_scrape[var_idx+1]) %>%
    filter(!duplicated(key)) %>%
    mutate(url=home_url)
  return(comp_df)
}
comp_num=5
get_house_df(my_comp_list[[2]]["links",comp_num][[1]]$homedetails)

### Find all sales of single family homes in Oxford in last 2 years
# save urls from the 10 pages of sales links on main Oxford Zillow Page
sales_pages <- read.csv("~/Github/search2020/TeachingDemo/OxfordSales.csv", header=F,stringsAsFactors = FALSE)
# loop over pages saving the details url links for each home sold 
all_sold_addresses <- NULL
for(page in 1:nrow(sales_pages)){
  # read html into R
  all_sales <- read_html(sales_pages$V1[page])
  # Parse out the photo cards from each house
  sales_txt <- all_sales %>%
    html_nodes(".photo-cards_short") %>%
    html_text()
  # substring to retrieve their addresses
  sales_mess <- str_split(sales_txt, pattern='streetAddress\\\":\\\"')
  sold_addresses <- sapply(2:length(sales_mess[[1]]), function(x){
    str_split(sales_mess[[1]][x],"\\\",")[[1]][1]
  })
  all_sold_addresses <- c(all_sold_addresses,sold_addresses)
  Sys.sleep(runif(1,3,7))
}
all_sold_addresses


# Combine into tidy df with one row per house, start to gather info on homes
# all_sold <- data.frame(street = all_sold_addresses, url="NA", zpid = "NA",
#                       lat=NA, long=NA, zestimate=NA, stringsAsFactors = FALSE)
# all_sold_full_lists <- list(NULL)
# remove i=36, 122, 141, 151, 169, 
for(i in 170:nrow(all_sold)){
  # look up sold house via address
  sold_house <- GetSearchResults(address = all_sold_addresses[i], citystatezip = 'Oxford, OH')
  # Convert from xml to a nested list of home attributes
  if(!is.null(sold_house$response)){
    sold_house_list <- xmlToList(sold_house$response)
    # get url for details
    names(sold_house_list) <- c("house_id","urls","location","zestimate","misc")
    all_sold$url[i] <- sold_house_list$urls$homedetails
    all_sold$zpid[i] <- sold_house_list$house_id
    all_sold$lat[i] <- as.numeric(sold_house_list$location$latitude)
    all_sold$long[i] <- as.numeric(sold_house_list$location$longitude)
    all_sold$zestimate[i] <- as.numeric(sold_house_list$zestimate$amount$text)
    all_sold_full_lists[[i]] <- sold_house_list
    Sys.sleep(runif(1,3,7))
    print(i)
  }
}

save(all_sold, all_sold_full_lists, file="sold_homes.Rdata")

get_house_df(all_sold$url[1])

