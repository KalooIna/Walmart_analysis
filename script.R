library( readr )
library( plotly )
library( viridis )

getwd()
setwd( "C:/Users/KalooIna/Documents/DSTI/Statistical Analysis for Machine Learning/Walmart" )
df <- read_csv( "lab_data.csv" )

dim( df )
colSums( is.na( df ))

for ( i in names( df )) {
  cat( i , "=" , class( df[[ i ]]), "\n" )
}

df$Date <- as.Date( df$Date , format = "%d-%m-%Y" )
df$Store <- as.factor( df$Store )

mean( df$Weekly_Sales )
median( df$Weekly_Sales )
range( df$Weekly_Sales )

# analyzing all sales by store
store_b_weeklysales <- aggregate( df$Weekly_Sales ~ df$Store , data = df , sum )
names( store_b_weeklysales ) <- c( "Store" , "Weekly_Sales" )
store_b_weeklysales <- store_b_weeklysales[ order( store_b_weeklysales$Weekly_Sales , decreasing = TRUE ) , ]
store_b_weeklysales

plot_ly(
  data = store_b_weeklysales ,
  x = ~ Store ,
  y = ~ Weekly_Sales , 
  type = "bar"
)

# analyzing weekly sales of store 33
store33_weeklysales <- df[ df$Store == 33 , ]
store33_weeklysales <- aggregate( store33_weeklysales$Weekly_Sales ~ store33_weeklysales$Date , data = store33_weeklysales , sum )
names( store33_weeklysales ) <- c( "Date" , "Weekly_Sales" )
store33_weeklysales

plot_ly(
  data = store33_weeklysales ,
  x = ~ Date ,
  y = ~ Weekly_Sales ,
  type = "scatter" ,
  mode = "lines"
)

# sales per CPI
df$sales_p_CPI <- df$Weekly_Sales / df$CPI
max_index <- which.max( df$sales_p_CPI )
df[ max_index , c( "Store" , "Date" )]

# heatmaps
## heatmap of correlation
df_num <- df %>% select( where( is.numeric ))
corrmat <- cor( df_num , use = "complete.obs" )
corr_fig <- plot_ly(
  z = corrmat,
  x = colnames( corrmat ),
  y = rownames( corrmat ),
  type = "heatmap",
  colors = viridis( 100 ) ,
  zmin = -1 ,
  zmax = 1 ,
  text = round( corrmat , 2 ) ,
  texttemplate = "%{text}" ,
  textfont = list( color = "white" )
)
corr_fig <- corr_fig %>% layout(
  title = list( text = "Correlation matrix heatmap" ),
  xaxis = list( title = "", tickangle = -45 ),
  yaxis = list( title = "" )
)
corr_fig
## heatmap of covariance
covmat <- cov( df_num , use = "complete.obs" )
cov_fig <- plot_ly(
  z = covmat,
  x = colnames( covmat ),
  y = rownames( covmat ),
  type = "heatmap",
  colors = viridis( 100 ) ,
  zmin = -1 ,
  zmax = 1 ,
  text = round( covmat , 2 ) ,
  texttemplate = "%{text}" ,
  textfont = list( color = "white" )
)
cov_fig <- cov_fig %>% layout(
  title = list( text = "Covariance matrix heatmap" ),
  xaxis = list( title = "" , tickangle = -45 ),
  yaxis = list( title = "" )
)
cov_fig









