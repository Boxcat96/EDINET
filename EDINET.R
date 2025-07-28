rm(list = ls()); gc(); graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(tictoc)
library(writexl)

tic() # 時間計測開始

# 現在の日時を保存（df_raw保存用）
time_now <- format(Sys.time(), "%Y%m%d_%H%M%S")

# 設定 ----------------------------------------------------------------------
df_maekai_file <- "df_raw_20250728_231609.xlsx" # 前回のファイル名

master_path <- "master.xlsx"    # マスタファイルのパス 
df_maekai_path <- "df_maekai/"  # 前回フォルダのパス
result_path <- "result/"        # 結果フォルダのパス


# マスタ読み込み -----------------------------------------------------------------
# マスタファイル内の全シート名を取得
sheet_names <- excel_sheets(master_path)

# すべてのシートをリストに読み込む
master_data <- map(sheet_names, ~ read_excel(master_path, sheet = .x))

# リストの名前をシート名に設定
names(master_data) <- sheet_names

# 項目マスタを格納
item_list <- master_data[["項目"]] 
# 純資産の名称情報を格納
capital <- master_data[["純資産"]] 
# 企業マスタを格納
firm_list <- master_data[["企業"]] 

# 並び替えの順番を指定
item_order <- unlist(item_list$item_name) # itemの順番を指定
firm_order <- unlist(firm_list$firm_name) # firmの順番を指定


# データ読み込み -----------------------------------------------------------------
# ０）前回ファイルの読み込み
df_maekai <- read_xlsx(str_c(df_maekai_path, df_maekai_file))

# 一時フォルダ作成（自動で削除可能な場所）
tmp_dir <- tempfile("data_tmp_")
dir.create(tmp_dir)

# 1) ZIPを一時フォルダに解凍
zip_files <- list.files("data", pattern = "\\.zip$", full.names = TRUE)
for (zip in zip_files) {
  unzip(zip, exdir = tmp_dir)
}

# 2) "jpcrp"で始まるCSVファイルを取得
csv_files <- list.files(file.path(tmp_dir, "XBRL_TO_CSV"),
                        pattern = "^jpcrp.*\\.csv$", full.names = TRUE)

# 3) ファイル名からfirm_IDとdateを抽出する関数
extract_info <- function(path) {
  fname <- basename(path)
  firm_id <- str_extract(fname, "_E\\d{5}(?=-)") %>% str_remove("^_") # firm_IDの抽出
  date <- str_extract(fname, "\\d{4}-\\d{2}-\\d{2}") %>% ymd() # dateの抽出＆日付型に変換
  list(firm_ID = firm_id, date = date)
}

# 4) データ読み込み、firm_IDとdate列を追加して結合
df_raw <- map_dfr(csv_files, function(file) {
  info <- extract_info(file)
  read_tsv(file, locale = locale(encoding = "UTF-16")) %>%
    mutate(firm_ID = info$firm_ID,
           date = info$date)
}) %>% 
  select(-"要素ID", -"期間・時点", -"ユニットID") %>% 
  rename(
    item = "項目名",
    item_ID = "コンテキストID",
    jiten = "相対年度",
    consol = "連結・個別",
    currency = "単位",
    value = "値"
  ) %>% 
  # 単体のB/Sを絞り込む
  filter(jiten == "当期末", consol == "個別") %>% 
  # "－"をNAに置換し、1億円で割り、四捨五入
  mutate(value = as.numeric(if_else(value == "－", NA, value)),
         value = round(value / 1e8)) %>% 
  left_join(firm_list, by = "firm_ID")  %>% # 企業マスタの結合
  select(date, firm_ID, firm_name, item, item_ID, jiten, consol, currency, value)
  
# df_rawを一旦保存（次回の比較のため）
write_xlsx(df_raw, str_c(df_maekai_path, "df_raw_", time_now, ".xlsx")) 

# 4) 一時フォルダ削除
unlink(tmp_dir, recursive = TRUE)

# 前回ファイルと今回ファイルの差分を確認
firm_ck <- setdiff(unique(df_raw$firm_ID), unique(df_maekai$firm_ID)) # 今期加わった企業
item_ck <- setdiff(unique(df_raw$item), unique(df_maekai$item)) # 今期加わった項目
item_ID_ck <- setdiff(unique(df_raw$item_ID), unique(df_maekai$item_ID)) # 今期加わった純資産の項目ID

# 確認結果
df_ck <- df_raw %>%
  filter(item %in% item_ck | item_ID %in% item_ID_ck)

# df_ckに値が含まれていた場合（前回との差分がある場合）、マスタ保守の必要があるため処理を中断
if(nrow(df_ck) > 0){
  stop("df_ckを確認し、マスタを保守してください")
  print(df_ck)
}


# データ処理 -------------------------------------------------------------------

df <- df_raw %>% 
  # 純資産マスタを結合
  left_join(capital, by = "item_ID") %>% 
  mutate(item = if_else(item_ID == "CurrentYearInstant_NonConsolidatedMember",
                        item, item_capital)) %>% 
  select(date, item, currency, firm_name, value) %>% 
  # 順番並び替えのための準備
  mutate(item = factor(item, levels = item_order),
         firm_name = factor(firm_name, levels = firm_order)) %>% 
  arrange(firm_name) %>% 
  distinct() %>% # 重複を削除
  pivot_wider(names_from = firm_name, values_from = value) %>% 
  arrange(item) 

# Excelに書き出し
write_xlsx(list(BS = df), str_c(result_path, "EDINET.xlsx"))

toc() # 時間計測終了

# 処理が完了したことを伝え、企業リストを表示
print("次の企業をマスタに加えてください")
print(as.data.frame(firm_ck))