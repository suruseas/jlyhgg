---
layout: article
---

# Learn You a Haskell for Grat Good 日本語訳

最高におもしろい、Haskellの学び方!




## これは何?

[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
の日本語訳です。
長いので、略してLYHGGと呼ぶことにします。
LYHGG日本語版は、私
[(@moutend)](https://github.com/moutend/)
のHaskellと英語の勉強を兼ねて、個人的に翻訳しています。
詳しくは、このページの最下部をご覧ください。



## 目次

1. Introduction- はじめに
  * About this tutorial - このチュートリアルについて
  * So what's Haskell? - で、Haskellって何?
  * What you need to dive in - 冒険に必要なもの
1. Starting Out - さぁ、はじめよう
    * Ready, set, go! - 準備しよう!
    * Baby's first functions - はじめての関数
    * An intro to lists - リストのご紹介
    * Texas ranges - テキサス・レンジャー巣
    * I'm a list comprehension - 我はリスト内包表記
    * Tuples - タプル
1. Types and Typeclasses - 型と型クラス
  * Believe the type - 型を信じて
  * Type variables - 型変数
  * Typeclasses 101 - 101匹型クラスちゃん
1. Syntax in Functions - 関数におけるシンタックス
  * Pattern matching - パターンマッチング
  * Guards, guards! - ガードで防げ!
  * Where!? - whereはどこに!?
  * Let it be - letにしよう
  * Case expressions - case式
1. Recursion - 再帰
  * Hello recursion! - こんにちは再帰!
  * Maximum awesome - 良さは最高
  * A few more recursive functions - 再帰的な関数をもう少し
  * Quick, sort! - クイックソート!
  * Thinking recursively - 再帰的に考える
1. Higher Order Functions - 高階関数
  * Curried functions - カリー化された関数
  * Some higher-orderism is in order - 高階する理由
  * Maps and filters - マッピングとフィルタリング
  * Lambdas - ッラムダ
  * Only folds and horses - 畳み込めるもの
  * Function application with $ - $を使った関数適用
  * Function composition - 関数合成
1. Modules - モジュール
  * Loading modules - モジュールを読み込む
  * Data.List
  * Data.Char
  * Data.Map
  * Data.Set
  * Making our own modules - 独自のモジュールを作ろう
1. Making Our Own Types and Typeclasses - 独自の型と型クラスをつくろう
  * Algebraic data types intro - 代数的データ型とは
  * Record syntax - レコードシンタックス
  * Type parameters - 型パラメータ
  * Derived instances - 産まれたてのインスタンス
  * Type synonyms - 型シノニム
  * Recursive data structures 再帰的データ構造
  * Typeclasses 102 - 102匹の型クラスちゃん
  * A yes-no typeclass - Yes-noな型クラス
  * The Functor typeclass - ファンクタ型クラス
  * Kinds and some type-foo - 型のようなkindのような
1. Input and Output - 入力と出力
  * Hello, world! - 念願のHello, World!
  * Files and streams - ファイルとストリーム
  * Command line arguments - コマンドライン引数
  * Randomness - ランダム性
  * Bytestrings - バイト文字列
  * Exceptions - 例外
1. Functionally Solving Problems - 関数的に問題を解決する
  * Reverse Polish notation calculator
  * Heathrow to London - ヒースローからロンドンへ　
1. Functors, Applicative Functors and Monoids - ファンクタと適用可能なファンクタ、そしてモノイド
  * Functors redux - ファンクタ、再び
  * Applicative functors - 適用可能なファンクタ
  * The newtype keyword - newtypeキーワード
  * Monoids - モノイド
1. A Fistful of Monads - 一掴みのモナド
  * Getting our feet wet with Maybe - Maybeを思い出そう
  * The Monad type class - モナド型クラス
  * Walk the line - 線にそって歩こう
  * do notation - do注釈
  * The list monad - リストもなど
  * Monad laws - モナドの法則
1. For a Few Monads More - モナドについて、もう少し
  * Writer? I hardly know her! - ライター? 全然知らないなぁ!
  * Reader? Ugh, not this joke again. - リーダー? はい、すみません、冗談です、もうしません
  * Tasteful stateful computations - あじわい深い、計算における状態
  * Error error on the wall - 壁に警告が
  * Some useful monadic functions - その他のモナドっぽい便利な関数
  * Making monads - モナドを作ろう
1. Zippers - ジッパー
  * Taking a walk - お散歩しよう
  * A trail of breadcrumbs - パン粉
  * Focusing on lists - リスト再考
  * A very simple file system - 超単純なファイルシステム
  * Watch your step - あなたの足跡



## 翻訳について

このチュートリアルにたどり着いた皆さんは御存知かと思いますが、
LYHGGは既に日本語へ翻訳されており
巷ではすごいH本と呼ばれている、
[すごいHaskellたのしく学ぼう](https://estore.ohmsha.co.jp/titles/978427406885P)
という書籍がオーム社より販売されています。
翻訳の質と内容の正確さを求める場合は、すごいH本の購入を強くお勧めします。



## 翻訳の修正

誤字脱字、表記の揺れ、日本語として不自然な言い回し、つまらないジョークなどを指摘していただけると私が喜びます。
このチュートリアルはGitHub Pagesでホストされているので、
GitHubアカウント(無料)を作成していただき、
[github.com/moutend/jlyhgg/issues](https://github.com/moutend/jlyhgg/issues/)
のNew Issueから、どこを修正すべきか報告できます。

また、gitに慣れている方は、直接このチュートリアルを修正することも可能です。
修正方法は以下のとおりです。

1 このチュートリアルのgitリポジトリをクローンします。

    git clone https://github.com/moutend/jlyhgg.git
    cd ./jlyhgg

2 修正用のブランチを切ります。ブランチ名に決まりはありませんが、わかりやすい名前にしてください。

    git checkout -b fix-typo

3 。`Chapter01.md`から`Chapter14.md`の内、該当するものを修正します。

    # 修正が終わったらコミットする
    git commit -c "Fix typo: てにをはの修正"


4 修正内容をプッシュして、プルリクエストを送信します。

    git push origin fix-typo

## ライセンスについて、

LYHGGは、CC BY-SA 3.0で公開されています。
LYHGG日本語版もコレに従い、同じライセンスの下で公開しています。

