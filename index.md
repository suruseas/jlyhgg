---
layout: cover
---



最高におもしろい、Haskellの学び方!



## Description

Miran Lipovača著の
[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
の日本語版です。
長いので、略してLYHGGと呼ぶことにします。
LYHGG日本語版は、
私[(@moutend)](https://github.com/moutend/)
のHaskellと英語の学習を兼ねて、個人的に翻訳しています。
詳しくは、このページの最下部をご覧ください。



## Prerequisites

1. [Haskell環境の構築について]({{ site.url }}chapter/00/)
  * Windows
 * Mac OSX
  * Linux
  * System.Randomモジュールについて



## Index

1. [Introduction- はじめに]({{ site.url }}chapter/01/)
  * About this tutorial - このチュートリアルについて
  * So what's Haskell? - で、Haskellって何なのさ?
  * What you need to dive in - 冒険に必要なもの
1. [Starting Out - さぁ、はじめよう]({{ site.url }}chapter/02/)
    * Ready, set, go! - 位置について、よーい、ドン!
    * Baby's first functions - よちよち関数
    * An intro to lists - リストのご紹介
    * Texas ranges - テキサス・レンジャー巣
    * I'm a list comprehension - 我はリスト内包表記
    * Tuples - タプル
1. [Types and Typeclasses - 型と型クラス]({{ site.url }}chapter/03/)
  * Believe the type - 型を信じて
  * Type variables - 型変数
  * Typeclasses 101 - 101匹型クラスちゃん
1. [Syntax in Functions - 関数におけるシンタックス]({{ site.url }}chapter/04/)
  * Pattern matching - パターンマッチング
  * Guards, guards! - ガードで防げ!
  * Where!? - whereはどこに!?
  * Let it be - letにしよう
  * Case expressions - case式
1. [Recursion - 再帰]({{ site.url }}chapter/05/)
  * Hello recursion! - 素敵な催奇
  * Maximum awesome - 良さは最高
  * A few more recursive functions - 再帰的な関数をもう少し
  * Quick, sort! - クイックソート!
  * Thinking recursively - 再帰的に考える
1. [Higher Order Functions - 高階関数]({{ site.url }}chapter/06/)
  * Curried functions - カリー化された関数
  * Some higher-orderism is in order - 高階する理由
  * Maps and filters - マッピングとフィルタリング
  * Lambdas - ッラムダ
  * Only folds and horses - 畳み込めるもの
  * Function application with $ - $を使った関数適用
  * Function composition - 関数合成
1. [Modules - モジュール]({{ site.url }}chapter/07/)
  * Loading modules - モジュールを読み込む
  * Data.List
  * Data.Char
  * Data.Map
  * Data.Set
  * Making our own modules - 独自のモジュールを作ろう
1. [Making Our Own Types and Typeclasses - 独自の型と型クラスをつくろう]({{ site.url }}chapter/08/)
  * Algebraic data types intro - 代数的データ型とは
  * Record syntax - レコードシンタックス
  * Type parameters - 型パラメータ
  * Derived instances - 産まれたてのインスタンス
  * Type synonyms - 型シノニム
  * Recursive data structures 再帰的データ構造
  * Typeclasses 102 - 102匹型クラスちゃん
  * A yes-no typeclass - ゆるふわ型クラス
  * The Functor typeclass - ファンクタ型クラス
  * Kinds and some type-foo - 型のようなkindのような
1. [Input and Output - 入力と出力]({{ site.url }}chapter/09/)
  * Hello, world! - 念願のHello, World!
  * Files and streams - ファイルとストリーム
  * Command line arguments - コマンドライン引数
  * Randomness - ランダム性
  * Bytestrings - バイト文字列
  * Exceptions - 例外
1. [Functionally Solving Problems - 関数的に問題を解決する]({{ site.url }}chapter/10/)
  * Reverse Polish notation calculator - 逆ポーランド記法電卓
  * Heathrow to London - ヒースローからロンドンへ　
1. [Functors, Applicative Functors and Monoids - ファンクタと適用可能なファンクタ、そしてモノイド]({{ site.url }}chapter/11/)
  * Functors redux - ファンクタ、再び
  * Applicative functors - 適用可能なファンクタ
  * The newtype keyword - newtypeキーワード
  * Monoids - モノイド
1. [A Fistful of Monads - ひと掴みのモナド]({{ site.url }}chapter/12/)
  * Getting our feet wet with Maybe - Maybeを思い出そう
  * The Monad type class - モナド型クラス
  * Walk the line - 綱渡り
  * do notation - do注釈
  * The list monad - リストもなど
  * Monad laws - モナドの法則
1. [For a Few Monads More - モナドについて、もう少し]({{ site.url }}chapter/13/)
  * Writer? I hardly know her! - ライター? 全然知らないなぁ!
  * Reader? Ugh, not this joke again. - リーダー? はい、すみません、冗談です、もうしません
  * Tasteful stateful computations - あじわい深い、計算における状態
  * Error error on the wall - 壁に警告が
  * Some useful monadic functions - その他のモナドっぽい便利な関数
  * Making monads - モナドを作ろう
1. [Zippers - ジッパー]({{ site.url }}chapter/14/)
  * Taking a walk - お散歩しよう
  * A trail of breadcrumbs - パン粉の道
  * Focusing on lists - リスト再考
  * A very simple file system - 超単純なファイルシステム
  * Watch your step - あなたの足跡



## Motivation

このチュートリアルを発掘した皆さんは御存知かと思いますが、
LYHGGは2012年に日本語へ翻訳されており
巷ではすごいH本として名を馳せている、
田中英行・村主崇行(訳)の
[すごいHaskellたのしく学ぼう](https://estore.ohmsha.co.jp/titles/978427406885P)
という書籍がオーム社より販売されています。
翻訳の質と内容の正確さを求める場合は、すごいH本を必ず購入してください。



## Contributing

誤字脱字、
日本語として不自然な言い回し、
表記の揺れ、
つまらないジョーク
などを指摘、あるいは修正していただけると訳者の励みになります。
どんなに些細な事でも構いません、泣いて喜びます。



### Via Issues

このチュートリアルはGitHub Pagesでホストされています。
[GitHub](https://github.com/)
のアカウント(無料)を作成していただくと、
[github.com/moutend/jlyhgg/issues](https://github.com/moutend/jlyhgg/issues/)
のNew Issueから、どこを修正すべきか報告することができるようになります。



### Via Pull Requests

コマンドラインでの操作に抵抗がない、という型は、
PR経由で直接この記事を修正することができます。
修正方法は以下のとおりです。

1 このチュートリアルのgitリポジトリをクローンします。

    git clone https://github.com/moutend/jlyhgg.git
    cd ./jlyhgg

2 修正用のブランチを切ります。ブランチ名に決まりはありませんが、わかりやすい名前にしてください。

    git checkout -b fix-typo

3 `chapter`ディレクトリにある`01.md`から`14.md`の内、該当するものを修正します。

    # 修正が終わったらコミットする
    git commit -c "Fix typo: てにをはの修正"


4 修正内容をプッシュして、プルリクエストを送信します。

    git push origin fix-typo



## LICENSE

LYHGGは、CC BY-SA 3.0で公開されています。
LYHGG日本語版もコレに従い、同じライセンスの下で公開しています。
