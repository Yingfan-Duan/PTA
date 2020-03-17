sales_train_dat %>%
  dplyr::group_by(shop_id, item_id, date_block_num) %>%
  dplyr::summarise(
    last_sales1 = sum(item_cnt_day, na.rm = TRUE),
    last_price1 = max(item_price, na.rm = TRUE)
  ) %>% head()





df_regression <- sales_train_dat %>% group_by(date_block_num, shop_id, item_id) %>% summarise(item_sold = sum(item_cnt_day)) %>% ungroup()

# add constant 30
df_regression$item_sold_transformed <- df_regression$item_sold+30

# Box Cox transformation
lambda <- BoxCox.lambda(df_regression$item_sold_transformed)
df_regression$item_sold_transformed <- BoxCox(df_regression$item_sold_transformed, lambda)

# add needed variables and convert them into appropriate mode
df_regression <- mutate(df_regression, month = as.factor(date_block_num %% 12 +1))


#### Fit the model
model_linear <- lm(data = df_regression, 
                   formula = item_sold_transformed ~ shop_id + item_id + month + date_block_num)

summary(model_linear)

fcst_linear <- predict(model_linear, sales_test[,c("shop_id", "item_id")])

plot(x = 1:500 ,y = df_regression$item_sold_transformed[1:500], type = "l")
lines(model_linear$fitted.values[1:500], col = "red")

