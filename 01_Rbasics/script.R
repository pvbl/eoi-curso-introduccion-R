get_col_means_script<-function(df){
  colMeans(df[, lapply(df, is.numeric) == TRUE])
}


filter_df_script<- function(df){
  subset(df,lang_known>mean(df$lang_known))
}
