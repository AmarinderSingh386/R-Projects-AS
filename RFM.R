library(dplyr)
library(ggplot2)
library(readxl)

getwd()
setwd("C:/Users/samar/Downloads")

df<- read_excel("Amarinder RFM.xlsx", col_names = TRUE)

#Fix date Column
df$OrderDate<- as.Date(df$OrderDate)
str(df)

library(writexl)

write_xlsx(df, "MidtermExam.xlsx")
View(df)

#RFM Package
install.packages("rfm")
library(rfm)

View(rfm_data_orders )
View(rfm_data_customer)

rfm_result <- rfm_table_customer(rfm_data_customer, customer_id, number_of_orders,
                                 recency_days, revenue, analysis_date =as.Date('2007-01-01'))

print(rfm_result)

#"MIDTERM" DF results 

rfm_result2 <- rfm_table_order(data=df, customer_id=CustomerID, order_date=OrderDate
, revenue=SalesAmount, analysis_date =as.Date('2014-01-28'))

rfm_result2

#SEGMENT LABELS
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

#UPPER & LOWER BOUNDS FOR RECENCY/FREQUENCY & MONETARY FOR ABOVE SEGMENTS
recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segment <- rfm_segment(rfm_result2,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)

View(segment)

segment %>% count(segment) %>% arrange(desc(n)) %>% rename(Segment = segment, Count = n)

#K MEAN ANALYSIS

df_km<-data.frame(CustID=segment$customer_id, Recency=segment$recency_score, Freq=segment$recency_days, Monetary=segment$amount)

std_seg_data<-scale(df_km[, c("Recency", "Freq", "Monetary")])

set.seed(42)
km<-kmeans(std_seg_data, 4, iter.max = 100)

#Cluster membership
km$cluster
#Centroids
km$centers
#Cluster sum squares
km$withinss
#Cluster size
km$size

#install.packages("factoextra")
library(factoextra)
fviz_cluster(km, data=std_seg_data, palette="rgb", ggtheme = theme_classic())

#Elbow method
fviz_nbclust(std_seg_data, kmeans, method="wss", k.max=20)
#The optimal number of clusters using elbow methord is 6 as we dont see any
#drastic decrease post 6. 

