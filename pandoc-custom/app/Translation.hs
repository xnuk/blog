module Translation (translations) where

translations :: [(String, [(String, String)])]
translations =
  [ ("ko",
    [ ("paragraph_anchor", "단락 링크: %s")
    , ("footnote_backlink", "본문으로 돌아가기")
    ])
  , ("en",
    [ ("paragraph_anchor", "Link to paragraph: %s")
    , ("footnote_backlink", "Back to the article")
    ])
  ]
