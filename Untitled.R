

centers <- read.csv('test2_copy.csv')

centers

library(DT)
mtcars2 = mtcars[, c(1:5, 9)]
mtcars2$am = factor(mtcars$am, c(0, 1), c('automatic', 'manual'))
# search for Ma or Me
datatable(
  mtcars2, colnames = c('model' = 1),
  filter = list(position = 'top', clear = FALSE),
  options = list(
    search = list(regex = TRUE, caseInsensitive = TRUE, search = 'M[ae]'),
    pageLength = 5
  )
)

centers[2,14]
typeof(centers)
typeof(mtcars)

mtcars[2,3]

centersFrame <- as.data.frame(centers, header=TRUE)

typeof(centersFrame)

centersFrame[3,14]
