#install.packages("readxl")
#install.packages("xlsx")
#install.packages("skimr")
library("xlsx")
library("readxl")
library("plyr")
# Importing libraries
library(data.table)           
library(readxl)               
library(tidyverse)
library(lubridate)
library(skimr)                
library(knitr)                
library(treemap)

#read excel datafame
retail.a <- read_excel("online_retail_09.10.xlsx")
retail.b <- read_excel("online_retail_10.11.xlsx")
retail.df <- rbind(retail.a,retail.b)

# Examining data
dim(retail.df)
summary(retail.df)
class(retail.df)
names(retail.df)
head(retail.df)
tail(retail.df)
names(retail.df)
t(t(names(retail.df)))
class(retail.df)
levels(retail.df)
# First glance at the data

retail.df %>%  skim()

# if the Invoice starts with letter 'C', it indicates a cancellation 

retail.df %>% 
  filter(grepl("C", retail.df$Invoice)) %>% 
  summarise(Total = n())

dim(retail.df)
#summary(retail.df) -  Invoice - Length:1067371

# Cancellations are not needed for the analysis so they can be removed
retail.df  <- retail.df %>% 
  filter(!grepl("C", retail.df$Invoice)) 

dim(retail.df)
#summary(retail.df) - Invoice - Length:1047877 

# NEGATIVE QUANTITIES
# filtering by non positive Quantity, Description shows manually entered adjustments codes. 
retail.df %>% 
  filter(Quantity <= 0) %>% 
  group_by(Description, Price) %>% 
  summarise(count =n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

dim(retail.df) # 1047877       8
# remove all rows with non-positive _Quantity_. 
retail.df  <- retail.df %>%
  filter(Quantity > 0)

dim(retail.df) # 1044420, 8 



# NON-PRODUCT STOCKCODES 
# There are a handful of non-product related codes
stc <- c('AMAZONFEE', 'BANK CHARGES', 'C2', 'DCGSSBOY', 'DCGSSGIRL',
         'DOT', 'gift_0001_', 'PADS', 'POST')

# Summary
retail.df %>%  
  filter(grepl(paste(stc, collapse="|"), StockCode))  %>% 
  group_by(StockCode, Description) %>% 
  summarise(count =n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

# These can all be removed. 
retail.df <- filter(retail.df, !grepl(paste(stc, collapse="|"), StockCode))


#DESCRIPTION
# Additional adjustment codes to remove
descr <- c( "check", "check?", "?", "??", "damaged", "found", 
            "adjustment", "Amazon", "AMAZON", "amazon adjust", 
            "Amazon Adjustment", "amazon sales", "Found", "FOUND",
            "found box", "Found by jackie ", "Found in w/hse", "dotcom",
            "dotcom adjust", "allocate stock for dotcom orders ta", "FBA",
            "Dotcomgiftshop Gift Voucher £100.00", "on cargo order",
            "wrongly sold (22719) barcode", "wrongly marked 23343",
            "dotcomstock", "rcvd be air temp fix for dotcom sit", "Manual",
            "John Lewis", "had been put aside", "for online retail orders",  
            "taig adjust", "amazon", "incorrectly credited C550456 see 47",
            "returned", "wrongly coded 20713", "came coded as 20713", 
            "add stock to allocate online orders", "Adjust bad debt",
            "alan hodge cant mamage this section", "website fixed",
            "did  a credit  and did not tick ret", "michel oops",
            "incorrectly credited C550456 see 47", "mailout", "test",
            "Sale error",  "Lighthouse Trading zero invc incorr", "SAMPLES",
            "Marked as 23343", "wrongly coded 23343","Adjustment", 
            "rcvd be air temp fix for dotcom sit", "Had been put aside."
)


# Filtering out the unwanted entries.
retail.df <- retail.df %>% 
  filter(!Description %in% descr)

dim(retail.df) # 1039532       8

# there are also some 600 NAs in _Description_. 
sum(is.na(retail.df$Description))

dim(retail.df) # 1039532       8



# CUSTOMER ID
# There is still a significant number of NAs in _CustomerID_. 


#remove rows with "NA "
retail.df <-retail.df[complete.cases(retail.df),]

dim(retail.df) # 802747      8


#retail.df$Price <- retail.df$Price >= 1

#dim(retail.df) # 802747      8



# there are almost 5 times as many Orders as there are Customers so I'm using `InvoiceNo` for orders 
sapply(retail.df[,c('Invoice','Customer ID')], function(x) length(unique(x)))


# a couple of housekeeping tasks to sort out

retail.df <- retail.df %>%
  # Setting 'Description' and 'Country' as factors
  mutate(Description = as.factor(Description)) %>%
  mutate(Country = as.factor(Country)) %>% 
  # Changing 'InvoiceNo' type to numeric
  mutate(Invoice = as.numeric(Invoice)) %>% 
  # Extracting 'Date' and 'Time' from 'InvoiceDate'
  mutate(InvoiceDate = as.Date(InvoiceDate)) %>% 
  

glimpse(retail.df)

# EXPLORATORY DATA ANALYSIS

# What items do people buy more often?
retail.df %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(Description, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 10 Best Sellers", title = "Most Ordered Products") +
  coord_flip() +
  theme_grey(base_size = 12)

# Top 10 most sold products represent around 3% of total items sold by the company
retail.df %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  mutate(pct=(count/sum(count))*100) %>% 
  arrange(desc(pct)) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)


# What day of the week do people buy more often?
retail.df %>% 
  ggplot(aes(wday(InvoiceDate, 
                  week_start = getOption("lubridate.week.start", 1)))) + 
  geom_histogram(stat = "count" , fill = "forest green", colour = "dark green") +
  labs(x = "Day of Week", y = "") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),
                     labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_grey(base_size = 14)

# How many items does each customer buy?
retail.df %>% 
  group_by(Invoice) %>% 
  summarise(n = mean(Quantity)) %>%
  ggplot(aes(x=n)) +
  geom_histogram(bins = 100000, fill = "purple", colour = "black") + 
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = "Average Number of Items per Purchase", y = "") +
  theme_grey(base_size = 14)

# What is the average value per order?
retail.df %>% 
  mutate(Value = Price * Quantity) %>% 
  group_by(Invoice) %>% 
  summarise(n = mean(Value)) %>%
  ggplot(aes(x=n)) +
  geom_histogram(bins = 200000, fill="firebrick3", colour = "sandybrown") + 
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = "Average Value per Purchase", y = "") + 
  theme_grey(base_size = 14)

# Which countries do they sell their goods to?
treemap(retail.df,
        index      = c("Country"),
        vSize      = "Quantity",
        title      = "",
        palette    = "Set2",
        border.col = "grey40")

library(data.table)
library(tidyverse)            
library(knitr)
library(recommenderlab)

# Removing duplicates 
retail.df <- retail.df %>%  # create unique identifier
  mutate(InNo_Desc = paste(Invoice, Description, sep = ' ')) # filter out duplicates 
retail.df <- retail.df[!duplicated(retail.df$InNo_Desc), ] %>% 
  select(-InNo_Desc) # drop unique identifier

dim(retail.df)   #765673      8

# Create the rating matrix 
ratings_matrix <- retail.df %>%# Select only needed variables
  select(Invoice, Description) %>% # Add a column of 1s
  mutate(value = 1) %>%# Spread into user-item format
  spread(Description, value, fill = 0) %>%
  select(-Invoice) %>%# Convert to matrix
  as.matrix() %>%# Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")

# Create evaluation scheme
?evaluationScheme
scheme <- ratings_matrix %>% 
  evaluationScheme(method = "cross",
                   k      = 2, 
                   train  = 0.7,
                   given  = -1)

# Set up List of Algorithms
algorithms <- list(
  "association rules" = list(name  = "AR", param = list(supp = 0.01, conf = 0.01)),
  "random items"      = list(name  = "RANDOM",  param = NULL),
  "popular items"     = list(name  = "POPULAR", param = NULL),
  "item-based CF"     = list(name  = "IBCF", param = list(k = 2)),
  "user-based CF"     = list(name  = "UBCF", param = list(method = "Cosine", nn = 500))
)

# Estimate the Models
results <- recommenderlab::evaluate(scheme, 
                                    algorithms, 
                                    type  = "topNList", 
                                    n     = c(1, 3, 5, 10, 15, 20)
)

# Results for each single model can be easily retrieved and inspected. 
results$'popular' %>% 
  getConfusionMatrix() 

# Sort out results

tmp <- results$`user-based CF` %>% 
  getConfusionMatrix()  %>%  
  as.list() # Pull into a list all confusion matrix information for one model

as.data.frame( Reduce("+",tmp) / length(tmp)) %>% # average value of 5 cross-validation rounds
  mutate(n = c(1, 3, 5, 10, 15, 20)) %>% # Add a column for number of recommendations calculated
  select('n', 'precision', 'recall', 'TPR', 'FPR') # Select only columns needed and sorting out order


# I put the previous steps into a formula. 
avg_conf_matr <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 3, 5, 10, 15, 20)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}



# use  `map()` to get all results in a tidy format, ready for charting.

results_tbl <- results %>%
  map(avg_conf_matr) %>% # iterate function across all models
  enframe() %>% # Turning into an unnested tibble
  unnest() # Unnesting to have all variables on same level

# ROC curve
results_tbl %>%
  ggplot(aes(FPR, TPR, colour = fct_reorder2(as.factor(name), FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves",
       colour = "Model") +
  theme_grey(base_size = 14)

# Precision-Recall curve
results_tbl %>%
  ggplot(aes(recall, precision, 
             colour = fct_reorder2(as.factor(name),  precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves",
       colour = "Model") +
  theme_grey(base_size = 14)

## Predictions for a new user

# create a made-up order with a string containing 6 products selected at random.
customer_order1 <- c("GREEN REGENCY TEACUP AND SAUCER",
                    "SET OF 3 BUTTERFLY COOKIE CUTTERS",
                    "JAM MAKING SET WITH JARS",
                    "SET OF TEA COFFEE SUGAR TINS PANTRY",
                    "SET OF 4 PANTRY JELLY MOULDS")

# create a made-up order with a string containing 6 products selected at random.
customer_order2 <- c("HEAR MEASURING SPOONS LARGE",
                    "BLUE PADDED SOFT MOBILE",
                    "FELTCRAFT DOLL MARIA",
                    "SAVE THE PLANET MUG",
                    "RETRO COFFEE MUGS ASSORTED")

# put string in a format that recommenderlab accepts.
new_order_rat_matrx <- retail.df %>% 
  select(Description) %>% # Select item descriptions from retail dataset
  unique() %>% 
  mutate(value = as.numeric(Description %in% customer_order1)) %>% # Add a 'value' column
  spread(key = Description, value = value) %>% # Spread into sparse matrix format
  as.matrix() %>% # Change to a matrix
  as("binaryRatingMatrix") # Convert to recommenderlab class 'binaryRatingsMatrix'


# create a `Recommender`
?Recommender
recomm <- Recommender(getData(scheme, 'train'), 
                      method = "IBCF",   
                      param = list(k = 2))


# pass the `Recommender` and the made-up order to the `predict` function to create 
# a top 10 recommendation list for the new customer.
pred <- predict(recomm, 
                newdata = new_order_rat_matrx, 
                n       = 10)


# inspect pediction as a list
as(pred, 'list')

# put string in a format that recommenderlab accepts.
new_order_rat_matrx <- retail.df %>% 
  select(Description) %>% # Select item descriptions from retail dataset
  unique() %>% 
  mutate(value = as.numeric(Description %in% customer_order2)) %>% # Add a 'value' column
  spread(key = Description, value = value) %>% # Spread into sparse matrix format
  as.matrix() %>% # Change to a matrix
  as("binaryRatingMatrix") # Convert to recommenderlab class 'binaryRatingsMatrix'


# create a `Recommender`
?Recommender
recomm <- Recommender(getData(scheme, 'train'), 
                      method = "IBCF",   
                      param = list(k = 2))


# pass the `Recommender` and the made-up order to the `predict` function to create 
# a top 10 recommendation list for the new customer.
pred <- predict(recomm, 
                newdata = new_order_rat_matrx, 
                n       = 10)


# inspect pediction as a list
as(pred, 'list')







