#Use Library
library(dplyr)
library(arules)
# Use movies dataset
movieDS <- read.csv("movie_subset.csv", header = TRUE)
glimpse(movieDS)
#1 Calculate the number of distinct users and movies
n_distinct(movieDS$movieId)#4598
n_distinct(movieDS$userId)#100
#2 Split the dataset according to the movie title and user ID.
data_list <- split(movieDS$title, movieDS$userId)
#3 Transform the split-ted list into a transactional dataset.
Movie_trx <- as(data_list, "transactions")
summary(Movie_trx)
#4 Plot the absolute item frequency plot of the 10 most popular movies.
itemFrequencyPlot(Movie_trx, 
                  topN = 10)
#5 Using the apriori() function, find out the 5 most popular itemsets for a minimum support of 0.3 and minimum size of 2.
support_all1 <-  apriori(Movie_trx,
                        parameter = list(target="frequent itemsets",
                                         supp = 0.3, minlen=2)
)
inspect(head(sort(support_all1, by="support"), 5))
#6 Using the apriori function, find out the 5 least popular itemsets for a minimum support of 0.3 and minimum size of 2.
support_all2 <-  apriori(Movie_trx,
                          parameter = list(target="frequent itemsets",
                                           supp = 0.3, minlen=2)
)
inspect(tail(sort(support_all2, by="support"), 5))
#7 Summarize the set of extracted rules - with minimum support of 30% and minimum confidence of 90%.
rules_online <- apriori(Movie_trx,
                        parameter = list(supp = 0.3, conf = 0.9, minlen = 2))
#8 From the set of extracted rules from previous question, what is the consequent of the rule with the highest lift? Type the code and the answer in comments
# Inspect the first 5 rules with highest lift
inspect(head(sort(rules_online, by="lift"), 5))
#Answer:Lord of the ring:The two tower is the consequent with highest life 2.6
####What influenced Pulp Fiction?
#9 Extract rules with the item Pulp Fiction as a consequent (rhs). (suppport 0.3 and confidence 0.5) Inspect the first five rules.
rules_PF_rhs <-
  apriori(Movie_trx,
          parameter = list(supp=0.3, conf=0.5, minlen=2),
          appearance = list(rhs = "Pulp Fiction"),
          control = list(verbose=F))
inspect(head(rules_PF_rhs,5))
#10 Find the 10 rules with the highest lift for which the item Pulp Fiction is a consequent.
inspect(head(sort(rules_PF_rhs, by="lift"), 10))
#11Which items are included in the rule with the highest confidence measure and with the item Pulp Fiction as a consequent? Type the code and comment the answer. Hint: similar to previous one but with confidence
rules_PF_rhs1 <-
  apriori(Movie_trx,
          parameter = list(supp=0.3, conf=0.7, minlen=2),
          appearance = list(rhs = "Pulp Fiction"),
          control = list(verbose=F))
inspect(head(rules_PF_rhs1,5))
#Answer:Item included in the rule with higest confidence measure with pulp fiction as a consequent is "Seven" (.81 confidence)
####What did Pulp Fiction influence?
#12 Extract rules with the item Pulp Fiction as a antecedent item (left hand side). Inspect the first five rules with the item Pulp Fiction as a antecedent item and with minimum confidence 50%, support 0.3 and minimum length is 2
rules_PF_lhs <-
  apriori(Movie_trx,
          parameter = list(supp=0.3, conf=0.5, minlen=2),
          appearance = list(lhs = "Pulp Fiction"),
          control = list (verbose=F))
inspect(head(rules_PF_lhs,5))

