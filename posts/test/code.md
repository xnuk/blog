---
title: 섹스
lang: ko
---

## 이건 쓸데없는 하스켈 코드야!
와우 `놀라워`
```haskell
import Data.Monoid ((<>))
import Data.Char (isSpace)
import Data.List (dropWhile)

main = do
  line <- getLine
  let ltrim = dropWhile isSpace line
      rtrim = reverse . dropWhile isSpace $ reverse ltrim -- lol
  putStrLn $ "Hell\no, " <> rtrim <> "!"
```

```javascript
console.log('h\ni', /^\s+/)
```

```css
#a{
  aaa: bbb;
}
.b{
  border: 3;
}
```

```html
<b> 꺄 &gt; </b>
```
