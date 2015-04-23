knapsack <- function(value, weight, limit){
  benefit.to.cost <- value / weight #Create ratio
  df = data.frame(value, weight, benefit.to.cost) # turn it into a DF
  df <- df[with(df, order(-benefit.to.cost)), ] # Sort by benefit.to.cost
  rownames(df) <- NULL # Reset the row names for easier indexing
  df$total.weight <- ifelse(cumsum(df$weight) <= limit, cumsum(df$weight), 0) # Add first items that fit
  # I need to add a break here if nothing fits in the bag on the first pass
  for(i in 2:nrow(df)){ #Start in row 2 because some values have been added above
    df$total.weight[i] <- ifelse(df$weight[i] + df$total.weight[i-1] <= limit, # If adding won't go over limit
                                 df$weight[i] + df$total.weight[i-1], df$total.weight[i-1]) # If it will, keep Weight the same
  }
  df$add <- 0
  df$add[1] <- ifelse(df$total.weight[1] > 0, 1, 0)
  for(i in 2:nrow(df)){ #Start in row 2 
    df$add[i] <- ifelse(df$total.weight[i] > df$total.weight[i-1], 1, 0) # 1 if it has been added
  }
  return(df)
}
