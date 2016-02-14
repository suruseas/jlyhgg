---
layout: cover
---

このチュートリアルは、Miran Lipovača著の
[Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
を日本語に翻訳したものです。
長いので、略してLYHGGと呼ぶことにします。
LYHGG日本語版は、
私[(@moutend)](//github.com/moutend/)
のHaskellと英語の学習を兼ねて、翻訳しています。
そのため、翻訳の質と内容の正確さは一切保証しません。
また、このチュートリアルを読んだことにより被ったいかなる不利益も保証しません。
詳しくは、[Motivation](#motivation)を御覧ください。


## Prerequisites

1. [環境構築について]({{ site.url }}/chapter/00/)
    1. [Haskell tool stackのインストール]({{ site.url }}/chapter/00#0.1)
          1. [Windows]({{ site.url }}/chapter/00#0.1.1)
          1. [Mac OSX]({{ site.url }}/chapter/00#0.1.2)
    1. [Haskell tool stackのセットアップ]({{ site.url }}/chapter/00/#0.2)
    1. [(補足) System.Randomモジュールについて]({{ site.url }}/chapter/00/#0.3)



## Index

1. [Introduction- はじめに]({{ site.url }}/chapter/01/)
    1. [About this tutorial - このチュートリアルについて]({{ site.url }}/chapter/01/#1.1)
    1. [So what's Haskell? - で、Haskellって何なのさ?]({{ site.url }}/chapter/01/#1.2)
    1. [What you need to dive in - 冒険に必要なもの]({{ site.url }}/chapter/01/#1.3)
1. [Starting Out - さぁ、はじめよう]({{ site.url }}/chapter/02/)
    1. [Ready, set, go! - 位置について、よーい、ドン!]({{ site.url }}/chapter/02/#2.1)
    1. [Baby's first functions - よちよち関数]({{ site.url }}/chapter/02/#2.2)
    1. [An intro to lists - リストのご紹介]({{ site.url }}/chapter/02/#2.3)
    1. [Texas ranges - テキサス・レンジャー巣]({{ site.url }}/chapter/02/#2.4)
    1. [I'm a list comprehension - 我はリスト内包表記]({{ site.url }}/chapter/02/#2.5)
    1. [Tuples - タプル]({{ site.url }}/chapter/02/#2.6)
1. [Types and Typeclasses - 型と型クラス]({{ site.url }}/chapter/03/)
    1. [Believe the type - 型を信じて]({{ site.url }}/chapter/03/#3.1)
    1. [Type variables - 型変数]({{ site.url }}/chapter/03/#3.2)
    1. [Typeclasses 101 - 101匹型クラスちゃん]({{ site.url }}/chapter/03/#3.3)
1. [Syntax in Functions - 関数におけるシンタックス]({{ site.url }}/chapter/04/)
    1. [Pattern matching - パターンマッチング]({{ site.url }}/chapter/04/#4.1)
    1. [Guards, guards! - ガードで防げ!]({{ site.url }}/chapter/04/#4.2)
    1. [Where!? - whereはどこに!?]({{ site.url }}/chapter/04/#4.3)
    1. [Let it be - letにしよう]({{ site.url }}/chapter/04/#4.4)
    1. Case expressions - case式]({{ site.url }}/chapter/04/#4.4)
1. [Recursion - 再帰]({{ site.url }}/chapter/05/)
    1. [Hello recursion! - 素敵な催奇]({{ site.url }}/chapter/05/#5.1)
    1. [Maximum awesome - 良さは最高]({{ site.url }}/chapter/05/#5.2)
    1. [A few more recursive functions - 再帰的な関数をもう少し]({{ site.url }}/chapter/05/#5.3)
    1. [Quick, sort! - クイックソート!]({{ site.url }}/chapter/05/#5.4)
    1. [Thinking recursively - 再帰的に考える]({{ site.url }}/chapter/05/#5.5)
1. [Higher Order Functions - 高階関数]({{ site.url }}/chapter/06/)
    1. [Curried functions - カリー化された関数]({{ site.url }}/chapter/06/#6.1)
    1. [Some higher-orderism is in order - 高階する理由]({{ site.url }}/chapter/06/#6.1)
    1. [Maps and filters - マッピングとフィルタリング]({{ site.url }}/chapter/06/#6.3)
    1. [Lambdas - ッラムダ]({{ site.url }}/chapter/06/#6.4)
    1. [Only folds and horses - 畳み込めるもの]({{ site.url }}/chapter/06/#6.5)
    1. [Function application with $ - $を使った関数適用]({{ site.url }}/chapter/06/#6.6)
    1. [Function composition - 関数合成]({{ site.url }}/chapter/06/#6.7)
1. [Modules - モジュール]({{ site.url }}/chapter/07/)
    1. [Loading modules - モジュールを読み込む]({{ site.url }}/chapter/07/#7.1)
    1. [Data.List]({{ site.url }}/chapter/07/#7.2)
    1. [Data.Char]({{ site.url }}/chapter/07/#7.3)
    1. [Data.Map]({{ site.url }}/chapter/07/#7.3)
    1. [Data.Set]({{ site.url }}/chapter/07/#7.4)
    1. [Making our own modules - 独自のモジュールを作ろう]({{ site.url }}/chapter/07/#7.5)
1. [Making Our Own Types and Typeclasses - 独自の型と型クラスをつくろう]({{ site.url }}/chapter/08/)
    1. [Algebraic data types intro - 代数的データ型とは]({{ site.url }}/chapter/08/#8.1)
    1. [Record syntax - レコードシンタックス]({{ site.url }}/chapter/08/#8.2)
    1. [Type parameters - 型パラメータ]({{ site.url }}/chapter/08/#8.3)
    1. [Derived instances - 産まれたてのインスタンス]({{ site.url }}/chapter/08/#8.4)
    1. [Type synonyms - 型シノニム]({{ site.url }}/chapter/08/#8.5)
    1. [Recursive data structures 再帰的データ構造]({{ site.url }}/chapter/08/#8.6)
    1. [Typeclasses 102 - 102匹型クラスちゃん]({{ site.url }}/chapter/08/#8.7)
    1. [A yes-no typeclass - ゆるふわ型クラス]({{ site.url }}/chapter/08/#8.8)
    1. [The Functor typeclass - ファンクタ型クラス]({{ site.url }}/chapter/08/#8.9)
    1. [Kinds and some type-foo - 型のようなkindのような]({{ site.url }}/chapter/08/#8.10)
1. [Input and Output - 入力と出力]({{ site.url }}/chapter/09/)
    1. [Hello, world! - 念願のHello, World!]({{ site.url }}/chapter/09/#9.1)
    1. [Files and streams - ファイルとストリーム]({{ site.url }}/capter/09/#9.2)
    1. [Command line arguments - コマンドライン引数]({{ site.url }}/chapter/09/#9.3)
    1. [Randomness - ランダム性]({{ site.url }}/chapter/09/#9.4)
    1. [Bytestrings - バイト文字列]({{ site.url }}/chapter/09/#9.4)
    1. [Exceptions - 例外]({{ site.url }}/chapter/09/#9.5)
1. [Functionally Solving Problems - 関数的に問題を解決する]({{ site.url }}/chapter/10/)
    1. [Reverse Polish notation calculator - 逆ポーランド記法電卓]({{ site.url }}/chapter/10/#10.1)
    1. [Heathrow to London - ヒースローからロンドンへ　]({{ site.url }}/chapter/10/#10.2)
1. [Functors, Applicative Functors and Monoids - ファンクタと適用可能なファンクタ、そしてモノイド]({{ site.url }}/chapter/11/)
    1. [Functors redux - ファンクタ、再び]({{ site.url }}/chapter/11/#11.1)
    1. [Applicative functors - 適用可能なファンクタ]({{ site.url }}/chapter/11/#11.2)
    1. [The newtype keyword - newtypeキーワード]({{ site.url }}/chapter/11/#11.3)
    1. [Monoids - モノイド]({{ site.url }}/chapter/11/#11.4)
1. [A Fistful of Monads - ひと掴みのモナド]({{ site.url }}/chapter/12/)
    1. [Getting our feet wet with Maybe - Maybeを思い出そう]({{ site.url }}/chapter/12/#12.1)
    1. [The Monad type class - モナド型クラス]({{ site.url }}/chapter/12/#12.2)
    1. [Walk the line - 綱渡り]({{ site.url }}/chapter/12/#12.3)
    1. [do notation - do注釈]({{ site.url }}/chapter/12/#12.4)
    1. [The list monad - リストもなど]({{ site.url }}/chapter/12/#12.5)
    1. [Monad laws - モナドの法則]({{ site.url }}/chapter/12/#12.6)
1. [For a Few Monads More - モナドについて、もう少し]({{ site.url }}/chapter/13/)
    1. [Writer? I hardly know her! - ライター? 全然知らないなぁ!]({{ site.url }}/chapter/13/#13.1)
    1. [Reader? Ugh, not this joke again. - リーダー? はい、すみません、冗談です、もうしません]({{ site.url }}/chapter/13/#13.2)
    1. [Tasteful stateful computations - あじわい深い、計算における状態]({{ site.url }}/chapter/13/#13.3)
    1. [Error error on the wall - 壁に警告が]({{ site.url }}/chapter/13/#13.4)
    1. [Some useful monadic functions - その他のモナドっぽい便利な関数]({{ site.url }}/chapter/13/#13.5)
    1. Making monads - モナドを作ろう]({{ site.url }}/chapter/13/#13.6)
1. [Zippers - ジッパー]({{ site.url }}/chapter/14/)
    1. [Taking a walk - お散歩しよう]({{ site.url }}/chapter/14/#14.1)
    1. [A trail of breadcrumbs - パン粉の道]({{ site.url }}/chapter/14/#14.2)
    1. [Focusing on lists - リスト再考]({{ site.url }}/chapter/14/#14.3)
    1. [A very simple file system - 超単純なファイルシステム]({{ site.url }}/chapter/14/#14.4)
    1. [Watch your step - あなたの足跡]({{ site.url }}/chapter/14/#14.5)



## <a name="Motivation">Motivation</a>

このチュートリアルを発掘した皆さんは御存知かと思いますが、
LYHGGは2012年に日本語へ翻訳されており
巷ではすごいH本として名を馳せている、
田中英行・村主崇行(訳)の
[すごいHaskellたのしく学ぼう](https://estore.ohmsha.co.jp/titles/978427406885P)
という書籍がオーム社より販売されています。
このチュートリアルは、冒頭でも説明したとおり、
私のHaskellと英語の勉強を兼ねて、個人的に翻訳しています。
翻訳の質と内容の正確さを求める場合は、すごいH本を必ず購入してください。



## Contributing

文法の解釈の誤り、
誤字脱字、
日本語として不自然な言い回し、
表記の揺れ、
つまらないジョーク
などを指摘していただけると、役者の励みになります。
以下、どのように指摘するのかについて説明します。



### Issues

このチュートリアルはGitHub Pagesでホストされています。
[GitHub](https://github.com/)
のアカウント(無料)を作成していただくと、
[github.com/moutend/jlyhgg/issues](https://github.com/moutend/jlyhgg/issues/)
から新しいIssueを開くことができます。
そのIssueに、指摘したい箇所を書いて下さい。



### Pull Requests

コマンドラインでの操作に抵抗がない、という型は、
PR経由で直接この記事を修正することができます。
修正方法は以下のとおりです。

1 このチュートリアルのgitリポジトリをクローンします。

    git clone https://github.com/moutend/jlyhgg.git
    cd ./jlyhgg

2 修正用のブランチを切ります。ブランチ名に決まりはありませんが、わかりやすい名前にしてください。

    git checkout -b fix-typo

3 `chapter`ディレクトリにある`01.md`から`14.md`の内、該当するものを修正して下さい。

    # 修正が終わったらコミットする
    git commit -c "Fix typo: てにをはの修正"


4 修正内容をプッシュして、プルリクエストを送信します。

    git push origin fix-typo



## LICENSE

LYHGGは、CC BY-SA 3.0で公開されています。日本語版もコレに従い同じライセンスの下で公開しています。
