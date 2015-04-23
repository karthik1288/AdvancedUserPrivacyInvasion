setwd("C:\\Users\\Karthik\\Desktop\\infosec")

source ("Knapsack_Func.R")
data.file <- file.path('1.csv')
powertable <- read.table(data.file, header = TRUE, sep = ',',fill = TRUE)

# Create a numeric vector containing just real power data
Global_active_power <- (as.numeric(as.character(powertable$realpkwh)))
summary(Global_active_power)
l <- length (Global_active_power)

# Create data to test the algo
weight <- c(0.03834, 0.0300, 0.0250, 0.0071, 0.0567, 0.0031, 0.0010, 0.0167, 0.0079)
value <- c(1,1,1,1,1,1,1,1,1)

i = 1
print(knapsack(value,weight,Global_active_power[i]))
header<- c(A,B,C,D,E,F,G,H,I)
write(header, file = "app_table.txt",
      ncolumns = 9,sep = " ",append= TRUE)

while (i<l)
{
  # Write the algo as a function
  mat <- knapsack(value,weight,Global_active_power[i])
  print(mat)
  print(mat$add)
  write(mat$add, file = "app_table.txt",
        ncolumns = 9,
        append = TRUE, sep = " ")
  value <- value + (mat$add/10)
  i = i+1
  print (i)
}

