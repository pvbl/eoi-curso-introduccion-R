# save as bos_rf_score.R

bos_rf <- readRDS("bos_rf.rds")
library(randomForest)

#* @param df data frame of variables
#* @post /score
function(crim,zn,indus)
{
  df <- data.frame(
    crim = crim,
    zn = zn,
    indus = indus
  )
  predict(bos_rf, df)
}