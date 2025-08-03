# Edinet APIから有価証券報告書を取得する関数
# Satoshi Tsuchida, 2025/08/03
# 参考にしたページ：https://to-mi-min.hatenablog.com/entry/2020/02/10/201001

# 事前にEDINET API仕様書を読み、下記の作業を行ってください（所要約10分）
# https://disclosure2dl.edinet-fsa.go.jp/guide/static/disclosure/WZEK0110.html
# 2章「APIの利用準備」

# doc_typesについては、同3-2章「書類取得API」をみてください。
# 基本的には、2と5が有用なので、c(2, 5)を指定すればOKです。
# なお、4（英文ファイル）は有価証券報告書では公表されていないため、指定不可です。

# 参考（2025/8/3日現在）
# 1：提出本文書及び監査報告書
# 2：PDF
# 3：代替書面・添付文書
# 5：CSV
# 4（指定不可）：英文ファイル＜有価証券報告書では公表なし＞

# get_Edinet関数 ------------------------------------------------------------

get_Edinet <- function(start_date, # 集計開始日（例："2025-05-01"
                       end_date,   # 集計終了日（例："2025-06-30"
                       data_path,  # データを格納するフォルダパス（例："data/"
                       doc_types,  # Edinetの書類タイプ（例：c(1,2,3,5)）※4は指定不可
                       firm_ID,    # 抽出したい企業のEdinetコード（例：c("E03606", "E03610", "E03611", "E03614", "E03615")
                       api_key     # APIキー（例："xxxxxxxxxxxx"
) {
  
  # 必要なライブラリの読み込み（なければインストール）
  packages <- c("httr", "jsonlite", "tidyverse")
  installed <- packages %in% rownames(installed.packages())
  if (any(!installed)) {install.packages(packages[!installed])}
  lapply(packages, library, character.only = TRUE)
  

# document IDの取得 ----------------------------------------------------------
  
  url_api <- "https://api.edinet-fsa.go.jp/api/v2/documents"
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
    full_url <- str_c(url_api, ".json?date=", date_str, url_type,
                      "&Subscription-Key=", api_key)
    
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
      dplyr::select(docID, edinetCode, filerName, secCode, periodEnd) %>% 
      mutate(
        filerName = str_squish(str_remove(filerName, "株式会社")),
        filingDate = as.Date(date_filing),
        across(everything(), as.character) # 全ての列を文字列にする
      ) %>% 
      distinct() # 重複削除。念のため入れておく
    
    # データを格納
    all_data[[date_str]] <- data_tidy
    
    # API叩きすぎ防止
    Sys.sleep(0.5)
  }

  # リストを1つのデータフレームに統合
  docID <- bind_rows(all_data) 


# 取得したdocument IDの書類を取得 ---------------------------------------------------

  # 取得対象を企業リストに絞る
  my_docID <- docID %>% 
    dplyr::filter(edinetCode %in% firm_ID)
  
  # docIDについてループ
  for(ii in 1:nrow(my_docID)){
    
    # docIDをベクトルとして抽出
    ID <- my_docID$docID[ii]
    
    # ファイル名の作成
    file_name <- str_c(ID, my_docID$filerName[ii], my_docID$filingDate[ii],
                       sep = "_")
    
    # 書類をそれぞれ取得
    for(type_num in doc_types){
      
      # URLの作成
      get_url <- str_c(url_api, "/", ID, "?type=", type_num,
                       "&Subscription-Key=", api_key) 
      
      # 保存先を指定
      res <- case_when(
        type_num == 1 ~ list(list(file_ext = "zip", fol_name = "1_Honbun")),
        type_num == 2 ~ list(list(file_ext = "pdf", fol_name = "2_PDF")),
        type_num == 3 ~ list(list(file_ext = "zip", fol_name = "3_Daitai")),
        type_num == 5 ~ list(list(file_ext = "zip", fol_name = "5_CSV")),
      )
      file_ext <- res[[1]]$file_ext
      fol_name <- res[[1]]$fol_name
      
      # DL先フォルダ
      DL_path <- str_c(data_path, fol_name)
      
      # フォルダが存在しない場合は作成
      if (!dir.exists(DL_path)) dir.create(DL_path, recursive = TRUE)
      
      # フルパスを作成
      file_path <- file.path(DL_path, str_c(file_name, ".", file_ext))
      
      # ダウンロード
      download.file(get_url, destfile = file_path, mode = "wb")
      
      # API叩きすぎ防止
      Sys.sleep(0.5)
    }
  }
  
  # doc_typesで5を選んだ場合、CSVを解凍して1つのdfにまとめる
  if (5 %in% doc_types){
    # 0) 一時フォルダ作成（自動で削除可能な場所）
    tmp_dir <- tempfile("data_tmp_")
    dir.create(tmp_dir)
    
    # 1) ZIPを一時フォルダに解凍
    zip_files <- list.files(str_c(data_path, "5_CSV"),
                            pattern = "\\.zip$", full.names = TRUE)
    for (zip in zip_files) {
      unzip(zip, exdir = tmp_dir)
    }
    
    # 2) "jpcrp"で始まるCSVファイルを取得
    csv_files <- list.files(file.path(tmp_dir, "XBRL_TO_CSV"),
                            pattern = "^jpcrp.*\\.csv$", full.names = TRUE)
    
    # 3) ファイル名からEdinetコード・データ日付・データ提出日を抽出する関数
    extract_info <- function(path) {
      fname <- basename(path)
      firm_ID <- str_extract(fname, "_E\\d{5}(?=-)") %>% str_remove("^_") # firm_IDの抽出
      data_date <- str_extract(fname, "\\d{4}-\\d{2}-\\d{2}") %>% ymd() # データ日付の抽出
      submit_date <- str_extract(fname, "\\d{4}-\\d{2}-\\d{2}(?=\\.csv)") %>% ymd() # データ提出日の抽出
      list(firm_ID = firm_ID, data_date = data_date, submit_date = submit_date)
    }
    
    # 4) データ読み込み、Edinetコード・データ日付・データ提出日を追加して結合
    df <- map_dfr(csv_files, function(file) {
      info <- extract_info(file)
      read_tsv(file, locale = locale(encoding = "UTF-16")) %>%
        mutate(firm_ID = info$firm_ID,
               data_date = info$data_date,
               submit_date = info$submit_date)
    }) 
    
    # 5) 一時フォルダ削除
    unlink(tmp_dir, recursive = TRUE)
  }
  
  return(df)
}


# Example -----------------------------------------------------------------
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # パス指定のおまじない
# df <- get_Edinet(start_date = "2025-05-01",
#                  end_date = "2025-06-30",
#                  data_path = "data/",
#                  doc_types = c(1, 2, 3, 5),
#                  firm_ID = c("E03606", "E03610", "E03611", "E03614", "E03615"),
#                  api_key = "xxxxxxxxxxxx")