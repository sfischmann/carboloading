##look at the data
dim(dh_causal_lookup)
head(dh_causal_lookup)
glimpse(dh_causal_lookup)
colnames(dh_causal_lookup)

## look at data product_lookup
head(dh_product_lookup)
glimpse(dh_product_lookup)
colnames(dh_product_lookup)

##look at data transactions
head(dh_transactions)
glimpse(dh_transactions)
colnames(dh_transactions)

## glimpse data 
head(dh_store_lookup)
glimpse(dh_store_lookup)

##missing values
colSums(is.na(dh_product_lookup))

##Look for missing values
colSums(is.na(dh_causal_lookup))

## missing values
colSums(is.na(dh_transactions))

## missing values
colSums(is.na(dh_store_lookup))

## length should be 5 
Dh_store <- dh_store_lookup %>% 
  filter(nchar(as.character(store_zip_code))==5)

setequal(store_1, store_lookup)

##correct data type for product lookup
dh_product_lookup <- dh_product_lookup %>% mutate(
  upc = as.numeric(upc),
  commodity = as.factor(commodity),
  brand = as.factor(brand)
)

##correct data type for causal lookup
causal_lookup <- dh_causal_lookup > 
  mutate(
    upc = as.numeric("upc"),
    feature_desc = as.factor(feature_desc),
    display_desc = as.factor(display_desc),
    geography = as.factor(geography)
  )

##correct data type for transactions
dh_transactions <- dh_transactions %>% mutate(
  upc = as.numeric(upc),
  geography = as.factor(geography),
  coupon = as.factor(coupon)
  
)

## convert time to 24 hours
dh_transactions$time_of_transaction <- sub("([[:digit:]]{2,2})$", ":\\1", dh_transactions$time_of_transaction) 

## sales greater than
transactions <- dh_transactions %>%
  filter(dh_transactions$dollar_sales >0)

## create joins/merging 
table_1 <- merge(dh_causal_lookup, dh_product_lookup, by = "upc")
table_2 <- merge(table_1, dh_transactions,by = c("upc", "store", "week", "geography") 
table_3 <- merge(dh_transactions, dh_product_lookup, by='upc')
final_db <- merge(table_2, store, by = "store")

##final_db


##retun top values 

top_n(table_2,5,"product")
top_n(table_2,5,"brand")
top_n(table_1,5,"commodity")

##
g1  <-table_3 %>% group_by(geography, commodity) %>% summarise(sum_units = sum(units)) %>% 
  ggplot(aes(x= reorder(commodity, -sum_units), y= sum_units, fill= geography))+
  geom_col(position ="dodge")+
  coord_flip()+
  xlab("Commodity")+
  ylab("Units Sold")

#commodity and units sold  
g2 <-table_3 %>% group_by(geography, commodity) %>% summarise(sum_sales = sum(dollar_sales)) %>% 
  ggplot(aes(x= reorder(commodity, -sum_sales), y= sum_sales, fill= geography))+
  geom_col(position ="dodge" )+
  coord_flip()+
  xlab("Commodity")+
  ylab("Sales in $")

grid.arrange(g1, g2)

## bar chart  
table_3 %>% group_by(commodity,display_desc, feature_desc) %>% summarise(sales_total = sum(dollar_sales)) %>% 
  plot_ly( x = ~display_desc, y = ~sales_total, type = "bar", text = text, 
           name = 'Display Description') %>% 
  add_trace( x = ~feature_desc, y = ~sales_total, type = "bar", text = text, 
             name = 'Feature Description') %>%
  layout(title = "Sales by Feature Display and Feature Description")

table_3 %>% group_by(brand) %>% summarise(sales_total = sum(dollar_sales), unit_Sales = sum(units)) %>% 
  plot_ly( x = ~brand, y = ~sales_total, type = "bar", text = text, 
           name = 'Total Sales') %>% 
  add_trace( x = ~brand, y = ~unit_Sales, type = "bar", text = text, 
             name = 'Unit Sales') %>%
  layout(title = "Sales and Units Sold by Brand")

table_3 %>% group_by(coupon) %>% summarise(sales_1 = sum(dollar_sales), sales_2 = sum(units)) %>%
  plot_ly(x = ~coupon, y = ~sales_1, type = "bar", name = "Total_Sales") %>%
  add_trace(x = ~coupon, y = ~sales_2, type = "bar", name = "Total Units") %>%
  layout(title = "Coupons ")

table_3 %>% filter(coupon == 1) %>% group_by(commodity, brand) %>% summarise(sum_sales = sum(dollar_sales), unit_sales = sum(units)) %>%
  plot_ly(x = ~commodity, y = ~sum_sales, type = "bar", name = "Total Sales" ) %>%
  add_trace(x = ~commodity, y = ~unit_sales, type = "bar", name = "Unit Sales" )

g1 <- table_3 %>% group_by( commodity) %>% summarise(unit_sales = sum(units), sales = sum(dollar_sales)) %>%
  ggplot(aes( x=1, y=unit_sales, fill=commodity))+
  geom_col(position="dodge")+
  ggtitle("Units over two years")


g2 <- table_3 %>% group_by( commodity) %>% summarise(unit_sales = sum(units), sales = sum(dollar_sales)) %>%
  ggplot(aes( x=2, y=sales, fill=commodity))+
  geom_col(position="dodge")+
  ggtitle("Revenue over two years")

grid.arrange(g1, g2)


##sales trend of pasta
table_3 %>%
  subset(table_3$commodity == "pasta") %>%
  group_by(week) %>%
  mutate(weeklysales = sum(dollar_sales)) %>%
  ggplot(aes(x = week, y = weeklysales, color = commodity)) +
  geom_line() +
  xlab("Week Number") +
  ggtitle("Weekly sales trend of Pasta ")


## Sales Trend of Pancake Mix 
table_3 %>%
  subset(table_3$commodity == "pancake mixes") %>%
  group_by(week) %>%
  mutate(weeklysales = sum(dollar_sales)) %>%
  ggplot(aes(x = week, y = weeklysales, color = commodity)) +
  geom_line() +
  xlab("Week Number") +
  ggtitle("Weekly sales trend of Pancake Mixes ")

table_3 %>%
  subset(table_3$commodity == "pasta sauce") %>%
  group_by(week) %>%
  mutate(weeklysales = sum(dollar_sales)) %>%
  ggplot(aes(x = week, y = weeklysales, color = commodity)) +
  geom_line() +
  xlab("Week Number") +
  ggtitle("Weekly sales trend of Pasta Sauce ")

## sales by commodities and geographical location
table_3 %>%
  group_by(commodity)  %>%
  ggplot(aes(x = factor(geography), fill = factor(geography))) +
  geom_bar(color = "black") +
  facet_grid(~commodity) +
  xlab("Geographical region") +
  scale_fill_discrete(name = "Region")

##Unique households by geographical region
table_3 %>%
  group_by(commodity,geography)  %>%
  summarise(Num_Of_Transactions = n(), Uniquehouholds = length(unique(household)))
