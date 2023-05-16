

# •A description of the sources of your data.
# https://www.kaggle.com/datasets/vijayuv/onlineretail

# •Explore your data.
# •Loading dataset 
setwd("/Users/nirajkc/Desktop/module 7")
retail <- read.csv("OnlineRetail.csv")

# • Loading require library.
library(ggplot2)  # popular data visualization package
library(dplyr)  #for data manipulation functions- mutate(),filter(),group_by(),summarize(), arrange()

# •printing top five rows of dataset: 
head(retail)

# •checking dimension  dataset
dim(retail)

# •structure of dataset
str(retail)

#Total unique Product & Unique Customers
length(unique(retail$Description))
length(unique(retail$CustomerID))

# •Summary of quantity and UnitPrice 
summary(retail$Quantity)
summary(retail$UnitPrice)

# •For character variables:
# •Frequency of unique countries in descending order:
my_table <- table(retail$Country)
my_sorted_table <- sort(my_table, decreasing = TRUE)
my_sorted_table

# •Frequency of unique Description in descending order:
my_table1 <- table(retail$Description)
my_sorted_table1 <- sort(my_table1, decreasing = TRUE)
my_sorted_table1
length(unique(retail$Description))

# Q. Dose the data set need some cleaning or customization in order to fit with your model’s requirements?
# cleaning or customization 
# •Checking Null values
colSums(is.na(retail))

# Removing null values
retail <- na.omit(retail)
colSums(is.na(retail))

# Removing UnitPrice & Quantity value <= 0 
retail <- retail %>% filter(UnitPrice > 0, Quantity >0)
min(retail$Quantity)
min(retail$UnitPrice)

# checking unique rows of dataset
dim(unique(retail))[1]

# separate date, month, year, daysOfWeek column from InvoiceDate 
retail$InvoiceDate <- as.POSIXct(retail$InvoiceDate, format="%m/%d/%Y %H:%M") #converting string InvoiceDate to date and time format
library(lubridate)#for manipulating dates & times 
retail$month <- month(retail$InvoiceDate)
retail$year <- year(retail$InvoiceDate)
retail$dayOfWeek <- wday(retail$InvoiceDate, label=TRUE)

# Change into factors  for newly created column using as.factor() function:
retail$month <- as.factor(retail$month)
retail$dayOfWeek <- as.factor(retail$dayOfWeek)
retail$year <- as.factor(retail$year)
retail$Country <- as.factor(retail$Country)
str(retail)

#Creating new column Total Sales by multiplying Quantity*UnitPrice
retail = mutate(retail, TotalSales = Quantity*UnitPrice)

# creating Diff column (by subtracting latest date minus InvoiceDate column)
max_date <- max(retail$InvoiceDate) # max date
retail$Diff =  difftime(max_date, retail$InvoiceDate, units = "days")
retail$Diff <- floor(retail$Diff)

# RFM (Recency, Frequency, Monetary) analysis:
RFM <- summarise(group_by(retail,CustomerID),Frequency = n(), Monetary = sum(TotalSales), Recency = min(Diff))
RFM$Recency <- as.numeric(RFM$Recency)
summary(RFM)

# Now, visualizing the results
# Transactions By Year Analysis
year_counts <- table(retail$year)
barplot(year_counts, 
        main = "Yearly transactions", 
        xlab = "Year", 
        ylab = "Total number", 
        col = "blue")
text(x = barplot(year_counts),y = year_counts,labels = year_counts,pos = 1, col = "blue")
title("Total Yearly Transactions")

# Revenue By day of the week Analysis:
sales_by_day <- retail %>% group_by(dayOfWeek) %>% summarise(TotalSales = sum(TotalSales))
# Plot bar chart
barplot(sales_by_day$TotalSales, names.arg = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
        xlab = "Day of Week", ylab = "Total Sales", main = "Total Sales by Day of Week")



sales_by_mnth <- retail %>% group_by(month) %>% summarise(TotalSales = sum(TotalSales))
# Plot bar chart
barplot(sales_by_mnth$TotalSales, names.arg = c("Jan", "Feb", "March", "April", "May", "June","July", "Aug", "Sept", "Oct", "Nov", "Dec"), 
        xlab = "Months", ylab = "Total Sales", main = "Total Sales by Month")


# Revenue country wise:
options(scipen = 999) #removes scientific notation
sales_by_country <- retail %>% 
  group_by(Country) %>% 
  summarise(TotalSales = sum(TotalSales)) %>% 
  arrange(desc(TotalSales)) %>% 
  head(5)
# Plot bar chart
bp <- barplot(sales_by_country$TotalSales, 
        names.arg = sales_by_country$Country, 
        xlab = "Countries", 
        ylab = "Total Sales", 
        main = "Top 5 Countries by Sales Revenue")
# Add labels on top of each bar
text(x = bp, y = sales_by_country$TotalSales, labels = sales_by_country$TotalSales, pos = 1, col="blue")



# IV. Model’s Baseline
#K-Means Clustering
# Scaling the data
RFM <- data.frame(RFM) 
row.names(RFM) <- RFM$CustomerID
RFM <- RFM[,-1]
RFM_scaled <- scale(RFM) 
head(RFM_scaled) 
# Determining Optimal Cluster:
# 1.	Elbow Method:
library(factoextra) # needed for fviz_nbclust() to estimate the optimal number of clusters
set.seed(123)
fviz_nbclust(RFM_scaled, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)

# 2.	Average Silhoute Method:
set.seed(123)
fviz_nbclust(RFM_scaled, kmeans, method = "silhouette") 

#  visualize kmeans clusters using both k=3 and k=4: 
set.seed(123)
k3 <- kmeans(RFM_scaled, centers = 3, nstart = 25)
k4 <- kmeans(RFM_scaled, centers = 4, nstart = 25)

# k=3
fviz_cluster(k3, geom = "point", data = RFM_scaled, pointsize = 0.2) + ggtitle("k = 3")
# k=4
fviz_cluster(k4, geom = "point", data = RFM_scaled, pointsize = 0.2) + ggtitle("k = 4")

# summary statistics of each cluster for each of the variables
res <- cbind(RFM, ClusterId = k3$cluster)
res <- as.data.frame(res)
head(res)

library(gridExtra)
a <- ggplot(res, aes(x = ClusterId, y = Frequency, group = ClusterId )) + geom_boxplot(show.legend = FALSE) 
b <- ggplot(res, aes(x = ClusterId, y = Monetary, group = ClusterId)) + geom_boxplot(show.legend = FALSE) 
c <- ggplot(res, aes(x = ClusterId, y = Recency, group = ClusterId)) + geom_boxplot(show.legend = FALSE) 
grid.arrange(a,b,c, ncol = 3)





