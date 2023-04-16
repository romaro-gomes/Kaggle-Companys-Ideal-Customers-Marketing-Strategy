
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
data[change_to_factors] =lapply(data[change_to_factors],unclass)
rm(change_to_factors)

data_selected_columns= data |> dplyr::select(!c('ID','Dt_Customer','Z_CostContact',"Z_Revenue"))


#data_selected_columns |> head()

data_selected_columns_scaled = apply(as.matrix(data_selected_columns),2,as.numeric) |> scale()
rm(data_selected_columns)

fviz_nbclust(data_selected_columns_scaled, clara, method = "silhouette")+
  theme_classic()


clara_cluster_for_data<- clara(data_selected_columns_scaled, 2, samples = 50, pamLike = TRUE)


fviz_cluster(clara_cluster_for_data,
             palette = 'jco',
             ellipse.type = "t", 
             geom = "point", pointsize = 1,
             ggtheme = theme_classic(),
             main="Customers Types"
)

classification_clients=(cbind(data, cluster = clara_cluster_for_data$cluster))
 
marital_plot = classification_clients |> 
  group_by(cluster,Marital_Status) |> 
  summarise(n=n()) |>  
  ggplot(aes(x=Marital_Status, y=n,fill=Marital_Status, label= n)) +
  geom_col()+ 
  facet_grid(. ~ cluster,scales = 'free') +
  geom_text(vjust = -0.5, size=3)


marital_plot 
  
