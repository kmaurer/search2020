### Code for web-scraping the Zillow data for kNN Teaching Demo

# Load Packages
library(tidyverse)
# devtools::install_github('fascinatingfingers/ZillowR')
library(ZillowR)
library(XML)
library(xml2)
library(rvest)

#---------------------
# Initial Goal is to evaluate the structure of both the Zillow API queries through ZillowR
# and the direct html scraping through rvest

### Here I use my Zillow API key that I source from a file not on GitHut
source("~/Github/search2020/TeachingDemo/MyZillowSecretKey.R") # You would need to set up your own account and get an ID
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
# If I didn't want the whole town, and only the neighbors Zillow defines, then I could stop here

### Now want to build functions to target url of a specific house, then pull and parse its attributes
# Pull one comparable house using its url
my_comp_list[[2]]["links",4][[1]]$homedetails

# pick a comparable from set of ncomp
comp_num <- 4

### build function that can take any zillow house url and return set of features

get_house_df <- function(home_url){
  comp_html <- read_html(home_url)
  #convert html to set of strings
  comp_scrape <- comp_html %>%
    html_nodes(".fact-label , .fact-value") %>%
    html_text()
  # leverage colon structure for identifying variable names, the item that follows is the value
  var_idx <- c(str_which(comp_scrape, ":"))
  # put the variable names and values into a tall dataframe that we can convert to wide later
  # we don't move to tall here because every house has a different number of values, as entered by the realtor
  comp_df <- data.frame(key=comp_scrape[var_idx],
                        value=comp_scrape[var_idx+1]) %>%
    filter(!duplicated(key)) 
  return(comp_df)
}
comp_num=5
comp_url <- my_comp_list[[2]]["links",comp_num][[1]]$homedetails
get_house_df(comp_url)


#--------------------------------------------------------------------------
### Find all sales of single family homes in Oxford in last 2 years

# save urls from the 10 pages of sales links on main Oxford Zillow Page
# (and yes, this was a pain in the neck to manually grab URLs, but it was better to be done and wonky, than not done and pretty)
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
  # Sleep randomly between scrapes so as not to upset any auto-moderators
  Sys.sleep(runif(1,3,7))
}
all_sold_addresses


### Combine into tidy df with one row per house, start to gather info on homes
### Loop over all addresses and use ZillowR to get API info about locations, urls, Zillow estimated prices
#!# WARNING: Needs to be improved here to allow gentle fail built in for failed API queries 
# manually removed broken i=36, 122, 141, 151, 169, 176, 203, 215, 280, 281, 323, 370, 382, 392, 393 

# Initialize to capture simplified set of info in data frame and 
# a list of all API info that could be accessed again without API queries
all_sold <- data.frame(street = all_sold_addresses, url="NA", zpid = "NA",
                       lat=NA, long=NA, zestimate=NA, stringsAsFactors = FALSE)
all_sold_full_lists <- list(NULL)
# Note: this may need to be temporary/unsaved objects to be in compliance with API Terms of Service (unsure of legal-ese)

for(i in 1:nrow(all_sold)){
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
all_sold

### Now use our URL scraping function to point to each of the urls in the set of Oxford houses
details_tall_all <- NULL
for(i in 1:nrow(all_sold)){
  house_detail <- get_house_df(all_sold$url[i]) 
  tmp <- house_detail%>%
    mutate(key = str_trim(str_remove_all(as.character(key), "[:#]")),
           url = all_sold$url[i])
  details_tall_all <- rbind(details_tall_all,tmp)
  Sys.sleep(runif(1,4,7))
  print(i)
}

# create simple set of column
details_tall <- details_tall_all %>%
  filter(key %in% c("Baths", "Beds", "Floor size","Parking", 
                    "Last sale price/sqft","Last sold","Lot")) 

# Clean the variables, go from tall to wide, merge with API details, parse strings and subset to bound lot and price
oxford_real_estate <- details_tall %>% 
  unique() %>%
  spread(key, value) %>%
  right_join(all_sold, by=c(url="url")) %>%
  mutate(lot_units = word(Lot, -1),
         lot_size = ifelse(lot_units=="acres",
                           as.numeric(str_remove(word(Lot,1),","))*43560, 
                           as.numeric(str_remove(word(Lot,1),","))),
         sqft = as.numeric(str_remove(word(`Floor size`,1),",")),
         sale_price =  as.numeric(gsub('\\$|,', '',word(`Last sold`,-1))),
         sale_month = word(`Last sold`,1,2)) %>%
  select(street:sale_month, -lot_units)%>%
  na.omit() %>%
  filter(lot_size < 1000000,
         sale_price < 400000)

# save(all_sold, all_sold_full_lists,details_tall,details_tall_all, file="sold_homes.Rdata")



