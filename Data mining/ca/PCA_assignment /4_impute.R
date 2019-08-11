data <- read.csv('College/PCA/3b_cast.csv')
library(mice)

impute <- mice(data)
imputed_data <- complete(impute)

write.csv(imputed_data,'College/PCA/4_imputed_mice.csv')
