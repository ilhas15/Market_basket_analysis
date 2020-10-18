#Get package
library(arules)
library(arulesViz)
library(datasets)

data("Groceries")

#top 20 item frequency plot
itemFrequencyPlot(Groceries, 
                  topN=20, 
                  type= "absolute", 
                  main= "Top-20 quantity Item",
                  xlab= "items", 
                  ylab = "frequency")

rules_soda= apriori(Groceries, parameter = list(supp= 0.001, conf=0.8))

options(digits = 2)
inspect(rules_soda[1:5])

summary(rules_soda)
rules_soda=sort(rules_soda, decreasing = TRUE, by="confidence")

rules_soda= apriori(Groceries, parameter = list(supp= 0.001, conf= 0.8, maxlen = 3))

subset.matrix= is.subset(rules_soda,rules_soda)
subset.matrix[lower.tri(subset.matrix, diag = T)]= NA
redundant=colSums(subset.matrix, na.rm = T) >= 1
rules_soda.pruned=rules_soda[!redundant]
rules_soda=rules_soda.pruned

rules_soda= apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="soda"),
               control = list(verbose=F))
rules_soda= sort(rules_soda, decreasing=TRUE,by="confidence")
inspect(rules_soda[1:5])

rules_soda= apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="soda"),
               control = list(verbose=F))
rules_soda= sort(rules_soda, decreasing=TRUE,by="confidence")
inspect(rules_soda)

plot(rules_soda, method = "graph",
     main= "Produk yang akan dibeli bersamaan dengan Whole Milk", 
     shading = NA,
     engine = 'interactive')


