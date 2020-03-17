# set working directory
setwd("F://PTA//Beginning_Project//code")

# load data
library(data.table)
library(xgboost)
library(caret)
library(foreach)
library(doParallel)
library(dplyr)

train <- fread("../data/sales_train.csv", data.table = TRUE)
test <- fread("../data/test.csv", data.table = TRUE)
submit <- fread("../data/sample_submission.csv", data.table = TRUE)

items <- fread("../data/items.csv", data.table = TRUE)
item_cat <- fread("../data/item_categories.csv", data.table = TRUE)
shops <- fread("../data/shops.csv", data.table = TRUE)


# preprocessing
train[, date:= as.Date(date, "%d.%m.%Y")]
train[, shop_id:= shop_id %>% as.character()]
train[, item_id:= item_id %>% as.character()]
train[, revenue:= ifelse((item_cnt_day < 0)|(item_price < 0), 0, item_price*item_cnt_day)]

test[, ID:= ID %>% as.character()]
test[, shop_id:= shop_id %>% as.character()]
test[, item_id:= item_id %>% as.character()]

items[, item_id:= item_id %>% as.character()]
items[, item_category_id:= item_category_id %>% as.character()]

item_cat[, item_category_id:= item_category_id %>% as.character()]

shops[, shop_id:= shop_id %>% as.character()]

# subset:  extract item_id / shop_id including in test set from train set
sub_train <- test %>%
  dplyr::mutate(tmp_id = 1) %>%
  dplyr::left_join(data.frame(tmp_id = 1,
                              date_block_num = seq(0, 34, by = 1)), by = "tmp_id") %>%
  dplyr::left_join(train, by = c("shop_id", "item_id", "date_block_num")) %>%
  dplyr::arrange(shop_id, item_id, date) %>%
  dplyr::left_join(shops, by = "shop_id") %>%
  dplyr::left_join(items, by = "item_id")

# Feature engineering
create_features <- function(df, target_month){
  
  # replace negative value and NA with 0
  df <- df %>%
    dplyr::mutate(
      item_cnt_day = ifelse(item_cnt_day < 0, 0, item_cnt_day),
      item_price = ifelse(is.na(item_price), 0, item_price))
  
  # 1. stats of item_price summarized by shop_id / item_id 
  data1 <- df %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(date_block_num < target_month) %>%
    dplyr::arrange(shop_id, item_id, date) %>%
    dplyr::group_by(shop_id, item_id) %>%
    dplyr::summarise(
      price_min = min(item_price, na.rm = TRUE),
      price_mean = mean(item_price, na.rm = TRUE),
      price_median = median(item_price, na.rm = TRUE),
      price_max = max(item_price, na.rm = TRUE),
      price_sd = sd(item_price, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # 2. stats of item_price summarized by item_id 
  data2 <- df %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(date_block_num < target_month) %>%
    dplyr::group_by(item_id) %>%
    dplyr::summarise(
      price_min_all = min(item_price, na.rm = TRUE),
      price_mean_all = mean(item_price, na.rm = TRUE),
      price_median_all = median(item_price, na.rm = TRUE),
      price_max_all = max(item_price, na.rm = TRUE),
      price_sd_all = sd(item_price, na.rm = TRUE),
      first_month = min(date_block_num, na.rm = TRUE)  # why pick first month 
    ) %>%
    dplyr::ungroup()
  
  # 3. num of sales and item price of 1~6 months before 
  data3 <- df %>%
    dplyr::group_by(shop_id, item_id, date_block_num) %>%  # why 1-6 months, only have 5?
    dplyr::summarise(
      last_sales1 = sum(item_cnt_day, na.rm = TRUE),
      last_price1 = max(item_price, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(shop_id, item_id, date_block_num) %>%
    dplyr::mutate(
      last_sales2 = lag(last_sales1, 1),
      last_sales3 = lag(last_sales1, 2),
      last_sales4 = lag(last_sales1, 3),
      last_sales5 = lag(last_sales1, 4),
      last_price2 = lag(last_price1, 1),
      last_price3 = lag(last_price1, 2),
      last_price4 = lag(last_price1, 3),
      last_price5 = lag(last_price1, 4),
      last_sales1_3_mean = (last_sales1 + last_sales2 + last_sales3)/3  # why compute mean?
    ) %>%
    dplyr::filter(date_block_num == target_month - 1)
  
  # 4. stats of total num sales summarized by shop_id / item_id, count num of month with no sales
  data4 <- df %>%
    dplyr::filter(date_block_num < target_month) %>%
    dplyr::mutate(
      is_no_sales = ifelse(is.na(item_cnt_day), 1 ,0),
      item_cnt_day = ifelse(is.na(item_cnt_day), 0 ,item_cnt_day)
    ) %>%
    dplyr::group_by(shop_id, item_id, date_block_num) %>%
    dplyr::summarise(
      total_num_sales = sum(item_cnt_day, na.rm = TRUE),
      is_no_sales = max(is_no_sales, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(shop_id, item_id) %>%
    dplyr::summarise(
      total_num_sales_min = min(total_num_sales, na.rm = TRUE),
      total_num_sales_mean = mean(total_num_sales, na.rm = TRUE),
      total_num_sales_median = median(total_num_sales, na.rm = TRUE),
      total_num_sales_max = max(total_num_sales, na.rm = TRUE),
      total_num_sales_sd = sd(total_num_sales, na.rm = TRUE),
      no_sales = sum(is_no_sales, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # 5. stats of revenue summarized by shop_id
  data5 <- df %>%
    dplyr::filter(date_block_num < target_month) %>%
    dplyr::mutate(
      revenue = ifelse(is.na(revenue), 0 ,revenue)
    ) %>%
    dplyr::group_by(shop_id, date_block_num) %>%
    dplyr::summarise(
      total_sales = sum(revenue, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(shop_id) %>%
    dplyr::summarise(
      total_sales_min = min(total_sales, na.rm = TRUE),
      total_sales_mean = mean(total_sales, na.rm = TRUE),
      total_sales_median = median(total_sales, na.rm = TRUE),
      total_sales_max = max(total_sales, na.rm = TRUE),
      total_sales_sd = sd(total_sales, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # 6. num of items at each shop
  data6 <- df %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(date_block_num < target_month) %>%
    dplyr::group_by(shop_id) %>%
    dplyr::summarise(
      num_item = n_distinct(item_id)
    ) %>%
    dplyr::ungroup()
  
  # 7. max number of sales with same item categories in past period 
  data7 <- df %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::group_by(shop_id, item_id, date_block_num) %>%
    dplyr::summarise(
      total_num_sales = sum(item_cnt_day, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(shop_id, item_id, date_block_num) %>%
    dplyr::distinct(shop_id, item_id, .keep_all = TRUE) %>%
    dplyr::filter(date_block_num < target_month) %>%
    dplyr::left_join(items, by = "item_id") %>%
    dplyr::group_by(shop_id, item_category_id) %>%
    dplyr::summarise(
      past_total_num_sales_max = max(total_num_sales, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # 8. target 
  target <- df %>%
    dplyr::filter(date_block_num == target_month) %>%
    dplyr::group_by(shop_id, item_id) %>%
    dplyr::summarise(
      total_num_sales = sum(item_cnt_day, na.rm = TRUE),
      price = mean(item_price, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      total_num_sales = ifelse(is.na(total_num_sales), 0, total_num_sales),
      total_num_sales_limited = ifelse(total_num_sales > 20, 20, total_num_sales)  # why choose 20
    )
  
  # data merge
  data <- target %>%
    dplyr::left_join(data1, by = c("shop_id", "item_id")) %>%
    dplyr::left_join(data2, by = c("item_id")) %>%
    dplyr::left_join(data3, by = c("shop_id", "item_id")) %>%
    dplyr::left_join(data4, by = c("shop_id", "item_id")) %>%
    dplyr::left_join(data5, by = c("shop_id")) %>%
    dplyr::left_join(data6, by = c("shop_id")) %>%
    dplyr::left_join(items, by = "item_id") %>%
    dplyr::left_join(data7, by = c("shop_id", "item_category_id")) %>%
    dplyr::left_join(item_cat, by = "item_category_id") %>%
    dplyr::left_join(shops, by = "shop_id") %>%
    dplyr::mutate(
      # replace na
      last_price1 = ifelse(last_price1 != 0, last_price1,
                           ifelse(is.na(price_median), price_median_all, price_median)),
      last_price2 = ifelse(last_price2 != 0, last_price2,
                           ifelse(is.na(price_median), price_median_all, price_median)),
      last_price3 = ifelse(last_price3 != 0, last_price3,
                           ifelse(is.na(price_median), price_median_all, price_median)),
      last_price4 = ifelse(last_price4 != 0, last_price4,
                           ifelse(is.na(price_median), price_median_all, price_median)),
      last_price5 = ifelse(last_price5 != 0, last_price5,
                           ifelse(is.na(price_median), price_median_all, price_median)),
      price_min = ifelse(is.na(price_min), price_min_all, price_min),
      price_mean = ifelse(is.na(price_mean), price_mean_all, price_mean),
      price_median = ifelse(is.na(price_median), price_median_all, price_median),
      price_max = ifelse(is.na(price_max), price_max_all, price_max),
      price_sd = ifelse(is.na(price_sd), price_sd_all, price_sd),
      
      # trend of item price
      diff_price1 = last_price1 - price_median,
      diff_price2 = last_price1 - price_median_all,
      
      # total sales period
      duration = ifelse(is.na(first_month), 0, target_month - first_month),
      
      # correct no sales period
      no_sales = ifelse(no_sales > duration, duration, no_sales),
      
      # trend of num of sales
      diff_sales_mean = last_sales1 - total_num_sales_mean,
      diff_sales_max = last_sales1 - total_num_sales_max,
      diff_sales_1 = last_sales1 - last_sales2,
      diff_sales_2 = last_sales1 - last_sales3,
      diff_sales_3 = last_sales1 - last_sales4,
      diff_sales_4 = last_sales1 - last_sales5,
      
      # create flag of release month   # what is release month
      is_release_month = ifelse(duration == 0, 1, 0),
      
      # correct last num of sales of new items
      last_sales1 = ifelse(is_release_month == 1, past_total_num_sales_max, last_sales1),
      
      # month
      month = date_block_num %% 12
    ) %>%
    # replace na with 0
    dplyr::mutate_at(vars(starts_with("last_"), starts_with("price_"), starts_with("diff_")), 
                     list(~ifelse(is.na(.), 0, .)))

    # calculate dummy colomuns  # why do this? predict()??
  dummy_col <- dummyVars(~., data = data %>% dplyr::select(
    item_category_id,
    item_category_name,
    shop_name))
  data <- data %>%
    dplyr::bind_cols(predict(dummy_col, data) %>% as.data.frame())
  
  return(data)
  
}


train_data <- create_features(sub_train, 32)
valid_data <- create_features(sub_train, 33)
pred_data <- create_features(sub_train, 34)

# xgboost function
fit_xgboost <- function(X_Train, Y_Train){
  
  X_Train <- X_Train %>% as.matrix()
  Y_Train <- Y_Train %>% as.matrix()
  
  # parameter settings
  set.seed(17)
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                eta = 0.07,
                max_depth = 5,
                min_child_weight = 10,
                colsample_bytree = 1,
                gamma = 0.9,
                alpha = 1.0,
                subsample = 0.7
  )
  
  # parallel calculation
  N_cpu = detectCores()
  
  # find nrounds with cross-validation
  xgbcv <- xgb.cv(param = param, data = X_Train, label = Y_Train,
                  nrounds = 100,
                  nfold = 5,
                  nthread = N_cpu
  )
  
  # modling
  set.seed(17)
  model_xgb <- xgboost(param = param, data = X_Train, label = Y_Train,
                       nrounds = which.min(xgbcv$evaluation_log$test_rmse_mean),
                       nthread = N_cpu, importance = TRUE)
  
  return(model_xgb)
}


# preparation
x_train <- train_data %>%  # why delete some specific variables?
  dplyr::select(
    starts_with("price_"),
    -first_month,
    starts_with("last_"),
    starts_with("total_num_sales_"),
    -total_num_sales_limited,
    starts_with("total_sales_"),
    num_item,
    starts_with("diff_"),
    duration,
    no_sales,
    starts_with("diff_sales_"),
    is_release_month,
    month,
    starts_with("item_category_id"),
    -item_category_id,
    -item_category_id0,
    -last_sales2,
    -last_sales3,
    -price_sd,
    -price_min,
    -price_mean,
    -price_median,
    -price_mean_all,
    -price_median_all,
    -price_max_all,
    -last_price1,
    -last_price2,
    -last_price3,
    -last_price4,
    -last_price5
  )

y_train <- train_data %>%
  dplyr::select(total_num_sales_limited)

x_valid_train <- valid_data %>%
  dplyr::select(
    starts_with("price_"),
    -first_month,
    starts_with("last_"),
    starts_with("total_num_salse_"),
    -total_num_sales_limited,
    starts_with("total_salse_"),
    num_item,
    starts_with("diff_"),
    duration,
    no_sales,
    starts_with("diff_sales_"),
    is_release_month,
    month,
    starts_with("item_category_id"),
    -item_category_id,
    -item_category_id0,
    -last_sales2,
    -last_sales3,
    -price_sd,
    -price_min,
    -price_mean,
    -price_median,
    -price_mean_all,
    -price_median_all,
    -price_max_all,
    -last_price1,
    -last_price2,
    -last_price3,
    -last_price4,
    -last_price5
  )

y_valid_train <- valid_data %>%
  dplyr::select(total_num_sales_limited)

# calculate modeling
model_xgb <- fit_xgboost(x_train, y_train)

# output importance
imp1 <- xgb.importance(names(x_train), model = model_xgb)

imp1 %>%
  ggplot()+
  geom_bar(mapping = aes(x = reorder(Feature, Gain), y = Gain), 
           stat = "identity")+
  coord_flip()

# train RMSE
train_train.pred <- train_data %>%
  dplyr::bind_cols(pred = predict(model_xgb, newdata = x_train %>% as.matrix(), type = "response")) %>%
  dplyr::mutate(error = total_num_sales_limited - pred)

train_train.pred %>%
  dplyr::summarise(
    RMSE = sqrt(sum(abs(error^2))/n())
  )

# valid RMSE
valid_train.pred <- valid_data %>%
  dplyr::bind_cols(pred = predict(model_xgb, newdata = x_valid_train %>% as.matrix(), type = "response")) %>%
  dplyr::mutate(error = total_num_sales_limited - pred)

valid_train.pred %>%
  dplyr::summarise(
    RMSE = sqrt(sum(abs(error^2))/n())
  )




