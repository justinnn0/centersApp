

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

