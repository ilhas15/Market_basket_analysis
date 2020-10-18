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

rules= apriori(Groceries, parameter = list(supp= 0.001, conf=0.8))

options(digits = 2)
inspect(rules[1:5])

summary(rules)
rules=sort(rules, decreasing = TRUE, by="confidence")

rules= apriori(Groceries, parameter = list(supp= 0.001, conf= 0.8, maxlen = 3))

subset.matrix= is.subset(rules,rules)
subset.matrix[lower.tri(subset.matrix, diag = T)]= NA
redundant=colSums(subset.matrix, na.rm = T) >= 1
rules.pruned=rules[!redundant]
rules=rules.pruned

rules= apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules= sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

rules= apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules= sort(rules, decreasing=TRUE,by="confidence")
inspect(rules)

plot(rules, method = "graph", main= "Produk yang akan dibeli bersamaan dengan Whole Milk", shading = NA,engine = 'interactive')


  