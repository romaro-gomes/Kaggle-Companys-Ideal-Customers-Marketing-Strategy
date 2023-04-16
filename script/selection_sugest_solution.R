j=0
data_numeric= c()
data_categorical = c()
for(i in lapply(data, class)){
  j = j + 1
  print(i)
  if(i == "numeric" ){
    data_numeric=append(data_numeric,colnames(data[j]))
  } else {
    data_categorical=append(data_categorical,colnames(data[j]))
  }
}
