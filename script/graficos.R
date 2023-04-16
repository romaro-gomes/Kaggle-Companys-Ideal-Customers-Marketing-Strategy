library(here)
library(tidyverse)
library(cluster)
library(factoextra)
library("ggsci")
library('RVAideMemoire')
library(flextable)
library(rstatix)


modelo = readRDS(here('objects/clara_cluster.R'))

dataset=readRDS(here('objects/final_dataset.R'))
mnt= c("MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds")
colnames(dataset)

byf.shapiro(Income ~cluster, data=dataset) #Os dados não são normais


wilcox.test(Income ~ cluster, data = dataset) #A renda entre os dois gupos não é normal


#-----------------------------------------------------------------------------


dataset |>
  group_by(cluster) |>
  get_summary_stats(Income, type = "median_iqr") |>
  flextable() |>
  set_header_labels(cluster = "Grupo", variable = "Variavél", n = "Quantidade",
                    median = "Mediana", iqr = "IIQ")


dataset |>
  ggplot() +
  geom_boxplot(aes(x = as.factor(cluster), y = Income, fill = as.factor(cluster))) +
  labs(x = "Groups",
       y = "",
       title = "Income by Group",
       subtitle = "All custumers"
       ) + 
  scale_fill_jco()+
  theme(legend.position = "none")


dataset |>
  ggplot(aes(x = as.factor(cluster), y = Income, fill = as.factor(cluster))) +
  geom_violin() +
  labs(x = "Group",
       y = "Annual Income",
       title = "Income by Group",
       subtitle= "Between 10%-90% of customers"
       ) + 
  scale_fill_jco()+
  theme(legend.position = "none") +
  scale_y_continuous(limits = quantile(dataset$Income,c(0.1, 0.9))) 

dataset |> 
  ggplot(aes(x=Education,y=Income)) +
  geom_col(aes(fill=as.factor(cluster)),position = 'dodge') +
  scale_fill_jco() +
  labs(x = "Education",
       y = "Income by year",
       title= "Income by Group",
       fill="Group")
  
#----------------------------------------------------------------------------------------------
byf.shapiro(MntWines ~cluster, data=dataset) #Os dados não são normais

wilcox.test(MntWines ~ cluster, data = dataset) #A renda entre os dois gupos não é normal

dunn_test(MntWines ~ cluster, data = dataset, p.adjust.method = "bonferroni")

dataset |>
  group_by(cluster) |>
  get_summary_stats(MntWines, type = "median_iqr") |>
  flextable() |>
  set_header_labels(cluster = "Grupo", variable = "Variavél", n = "Quantidade",
                    median = "Mediana", iqr = "IIQ")

#--------------------------------------------------------------------------------------------
byf.shapiro(MntFruits ~cluster, data=dataset) #Os dados não são normais

wilcox.test(MntFruits~ cluster, data = dataset) #A renda entre os dois gupos não é normal


dataset |>
  group_by(cluster) |>
  get_summary_stats(MntFruits, type = "median_iqr") |>
  flextable() |>
  set_header_labels(cluster = "Grupo", variable = "Variavél", n = "Quantidade",
                    median = "Mediana", iqr = "IIQ")

#-----------------------------------------------------------------------------------
byf.shapiro(MntGoldProds ~cluster, data=dataset) #Os dados não são normais

wilcox.test(MntGoldProds~ cluster, data = dataset) #A renda entre os dois gupos não é normal


dataset |>
  group_by(cluster) |>
  get_summary_stats(MntGoldProds, type = "median_iqr") |>
  flextable() |>
  set_header_labels(cluster = "Grupo", variable = "Variavél", n = "Quantidade",
                    median = "Mediana", iqr = "IIQ")
#---------------------------------------------------------------------------------------------
tabela= table(dataset$Response, dataset$cluster)
proportions(tabela, 1) |> class()

tbl <- xtabs(~ Response + cluster, data = dataset)
summary(tbl)
proportions(tbl, "cluster")

chisq.test(dataset$Response, dataset$cluster, correct=FALSE)
