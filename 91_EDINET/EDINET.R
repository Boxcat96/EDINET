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
df_maekai_file <- "df_raw_20250727_175320.xlsx" # 前回のファイル名

master_path <- "master.xlsx"    # マスタファイルのパス 
df_maekai_path <- "df_maekai/"  # 前回フォルダのパス
result_path <- "result/"        # 結果フォルダのパス

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

# 3) UTF-16のCSVを読み込んで結合 + firm列追加
extract_firm_code <- function(path) {
  # ファイル名から "Exxxxx" を抽出（企業番号＜firm_ID＞）
  str_extract(basename(path), "_E\\d{5}(?=-)") %>% 
    str_remove("^_")
}

# ４）データ読み込み＆firm列追加＆保存
df_raw <- map_dfr(csv_files, function(file) {
  firm_code <- extract_firm_code(file)
  # tsvファイルで読み込む。エンコーディングはUTF-16を指定。
  read_tsv(file, locale = locale(encoding = "UTF-16")) %>%
    mutate(firm_ID = firm_code)
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
  dplyr::filter(jiten == "当期末" & consol == "個別") %>% 
  # "－"をNAに置換し、1億円で割り込み、ラウンド
  mutate(value = as.numeric(if_else(value == "－", NA, value)),
         value = round(value / 1e8))

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
  filter(
    item %in% item_ck |
      item_ID %in% item_ID_ck
  )

# df_ckに値が含まれていた場合（前回との差分がある場合）、マスタ保守の必要があるため処理を中断
if(!nrow(df_ck) == 0){
  stop("df_ckを確認し、マスタを保守してください")
  print(df_ck)
}

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

# データ処理 -------------------------------------------------------------------

df <- df_raw %>% 
  # 純資産マスタを結合
  left_join(capital, by = "item_ID") %>% 
  mutate(item = if_else(item_ID == "CurrentYearInstant_NonConsolidatedMember",
                        item, item_capital
                        )) %>% 
  select(item, currency, firm_ID, value) %>% 
  left_join(firm_list, by = "firm_ID") %>% # 企業マスタの結合
  # 順番並び替えのための準備
  mutate(item = factor(item, levels = item_order),
         firm_name = factor(firm_name, levels = firm_order)) %>% 
  select(-firm_ID) %>% 
  arrange(firm_name) %>% 
  distinct() %>% # 重複を削除
  pivot_wider(names_from = firm_name, values_from = value) %>% 
  arrange(item) %>% 
  write_xlsx(str_c(result_path, "EDINET.xlsx"))

toc() # 時間計測終了

# 処理が完了したことを伝え、企業リストを表示
print("次の企業をマスタに加えてください")
print(as.data.frame(firm_ck))