# DocIDを取得する関数
# 参考：https://to-mi-min.hatenablog.com/entry/2020/02/10/201001

get_json <- function(start_date, end_date, api_key) {
  library(httr)
  library(jsonlite)
  # library(tidyverse)
  
  url_api <- "https://api.edinet-fsa.go.jp/api/v2/documents.json?date="
  url_type <- "&type=2"  
  
  # 日付ベクトルを作成
  date_seq <- seq.Date(from = as.Date(start_date),
                       to = as.Date(end_date),
                       by = "1 day")
  
  # 全データを格納するリストの箱を用意
  all_data <- list()
  
  # 各日付に対してループ開始
  for (date_filing in date_seq) {
    
    # date_filingの型を指定
    date_str <- format(as.Date(date_filing), "%Y-%m-%d")
    
    # URL作成
    full_url <- str_c(url_api, date_str, url_type, "&Subscription-Key=", api_key)
    
    json_text <- content(GET(full_url), "text", encoding = "UTF-8")
    
    # JSONが不正な場合のエラーハンドリング
    data_raw <- tryCatch(fromJSON(json_text), error = function(e) {
      message("JSON parse error on ", date_str)
      return(NULL)
    })
    
    # エラー等でJSONがパースできなかったら、ループを次に回して次の日に進む
    if (is.null(data_raw)) next
    
    # その日にデータがあるかを確認
    data_flag <- data_raw %>%
      purrr::pluck("metadata", "resultset", "count")
    
    # データがない日には"No data on"を表示
    if (data_flag == 0) {
      message("No data on ", date_str)
      next
    }
    
    # 有価証券報告書かつ訂正でないデータの抽出
    data_tidy <- data_raw %>% 
      purrr::pluck("results") %>% 
      as_tibble() %>% 
      slice(str_which(docDescription, "有価証券報告書")) %>% 
      
      # 訂正有価証券報告書を除きたい場合はコメントアウトを外す
      # slice(str_which(docDescription, "訂正", negate = TRUE)) %>% 
      
      dplyr::filter(!is.na(secCode)) %>% 
      select(docID, edinetCode, filerName, secCode, periodEnd) %>% 
      mutate(
        filerName = str_squish(str_remove(filerName, "株式会社")),
        filingDate = as.Date(date_filing),
        across(everything(), as.character) # 全ての列を文字列にする
      ) %>% 
      distinct() # 念のため入れておく
    
    # データを格納
    all_data[[date_str]] <- data_tidy
    
    # API叩きすぎ防止
    Sys.sleep(0.5)
  }
  
  # リストを1つのデータフレームに統合
  docID <- bind_rows(all_data) 
  
  return(docID)
}
