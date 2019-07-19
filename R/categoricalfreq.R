
categoricaldistribution <- function(data, target_var)
{
  d0 <- data[data[target_var] == 0, ]
  d1 <- data[data[target_var] == 1, ]
  categorical_columns = c()

  for (i in 1:ncol(data))
  {
    if (is.factor(data[,i]))
    {
      values = c()
      values = append(values, c(names(data[i])))
      categorical_columns = append(categorical_columns, values, after = length(categorical_columns))
    }
  }

  for (i in 1:length(categorical_columns))
  {
    j = data.frame()
    k = data.frame()
    l = data.frame()
    j = as.data.frame(table(d0[categorical_columns[i]]))
    k = as.data.frame(table(d1[categorical_columns[i]]))
    m = k[,2]
    l = cbind(j,m)
    colnames(l) <- c('category','distribution of non events', 'distribution of events')
    write.xlsx(l, file="categorical_variables.xlsx", sheetName=categorical_columns[i],append=TRUE, row.names=FALSE)
  }

}

