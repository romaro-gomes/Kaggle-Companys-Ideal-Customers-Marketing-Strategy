
library(here)
library(tidyverse)
library(cluster)
library(factoextra)
library("ggsci")

data_raw=read_tsv(here('data/marketing_campaign.csv'))

data=na.omit(data_raw)

dim(data)

change_to_factors=c("Complain","AcceptedCmp1", "AcceptedCmp2", 'AcceptedCmp3', 'AcceptedCmp4',
                    'AcceptedCmp5','Education','Marital_Status',"Year_Birth","Response")

# data[change_to_factors] |> head()
data_to_change = data
data_to_change[change_to_factors] =lapply(data_to_change[change_to_factors],as.factor)
data_to_change[change_to_factors] = lapply(data_to_change[change_to_factors],unclass) #transforma os fatores em numeros

#head(data)

rm(change_to_factors)


data_selected_columns= data_to_change |> dplyr::select(!c('ID','Dt_Customer','Z_CostContact',"Z_Revenue"))
rm(data_to_change)

#data_selected_columns |> head()

data_selected_columns_scaled = apply(as.matrix(data_selected_columns),2,as.numeric) |> scale()

rm(data_selected_columns)

fviz_nbclust(data_selected_columns_scaled, clara, method = "silhouette")+
  theme_classic()

#saveRDS(data_selected_columns_scaled,file = here('objects/dataset_for_training.R'))

clara_cluster_for_data<- clara(data_selected_columns_scaled, 2, samples = 50, pamLike = TRUE)


#saveRDS(clara_cluster_for_data,file =here("objects/clara_cluster.R"))

customers_types= fviz_cluster(clara_cluster_for_data,
             palette = 'jco',
             ellipse.type = "t", 
             geom = "point", pointsize = 1,
             ggtheme = theme_classic(),
             main="Customers Types"
)

customers_types
#ggsave(here('graphic/customers_types.png'),customers_types)

classification_clients=(cbind(data, cluster = clara_cluster_for_data$cluster))
classification_clients
#saveRDS(data,file=here('objects/clean_dataset.R'))
#saveRDS(classification_clients,file=here('objects/final_dataset.R'))
 
marital_plot = classification_clients |> 
  group_by(cluster,Marital_Status) |> 
  summarise(n=n()) |>  
  ggplot(aes(x=Marital_Status, y=n,fill=Marital_Status, label= n)) +
  geom_col()+ 
  labs(title= "Differences between Clients Marital Status",caption="Source: Kaggle") +
  ylab("") +
  xlab("") +
  facet_grid(. ~ cluster,scales = 'free') +
  geom_text(vjust = -0.5, size=3) +
  theme(
    text = element_text(size = 8)
  )

marital_plot

#ggsave(here('graphic/marital_plot.png'),plot=marital_plot,device = 'png')

  
