# For LLM Instructions

## 出力設定

### 言語

AIは人間にテキストを出力するときは日本語で出力してください。
しかしコードのコメントなどが日本語ではない場合は元の言語のままにしてください。

### 記号

ASCIIに対応する全角形(Fullwidth Forms)は使用禁止。

具体的には以下のような文字:

- 全角括弧 `（）` → 半角 `()`
- 全角コロン `：` → 半角 `:`
- 全角カンマ `，` → 半角 `,`
- 全角数字 `０-９` → 半角 `0-9`

## ディレクトリ構成

### プロンプト

`CLAUDE.md`は`.github/copilot-instructions.md`のシンボリックリンクです。

```
CLAUDE.md -> .github/copilot-instructions.md
```

## Markdown

### Pandoc

Markdownの処理系にはPandocを使っています。

Pandocの標準的拡張に加えて以下のように拡張を設定しています。

#### 無効化

- `Ext_pandoc_title_block`：タイトルブロックの自動生成を無効化
- `Ext_smart`：スマート引用符やダッシュの自動変換を無効化

#### 有効化

- `Ext_auto_identifiers`：自動見出し向けのIDを有効化
- `eastAsianLineBreakFilter`：改行を挟んだ時に東アジアの文字列に余計な空白が入らないようにする自作フィルタ

#### シンタックスハイライト

シンタックスハイライトにはpygmentsを使っています。
