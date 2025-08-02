# ZIP、PDFを取得する関数
get_files <-function(docID, data_path, api_key){
  
  # docIDについてループ
  for(ii in 1:nrow(docID)){
    
    # docIDをベクトルとして抽出
    ID <- docID$docID[ii]
    
    # ファイル名の作成
    file_name <- str_c(ID, docID$filerName[ii], docID$filingDate[ii],
                       sep = "_")
    
    # ZIPとPDFをそれぞれ取得
    for(type_num in c(2,5)){
      
      # URLの作成
      get_url <- str_c("https://api.edinet-fsa.go.jp/api/v2/documents/",
                       ID, "?type=", type_num, "&Subscription-Key=", api_key) 
      
      # 保存先をzip、PDFそれぞれで指定
      if(type_num == 2){
        zip_or_pdf <- "pdf"
      } else if (type_num == 5) {
        zip_or_pdf <- "zip"
      } else{
        stop("type_numを正しく指定してください")
      }
      
      # DL先フォルダ
      DL_path <- str_c(data_path, zip_or_pdf)
      
      # フォルダが存在しない場合は作成
      if (!dir.exists(DL_path)) dir.create(DL_path, recursive = TRUE)

      # フルパスを作成
      file_path <- file.path(DL_path, str_c(file_name, ".", zip_or_pdf))
      
      # ダウンロード
      download.file(get_url, destfile = file_path, mode = "wb")
      
      # API叩きすぎ防止
      Sys.sleep(0.5)
    }
  }
}