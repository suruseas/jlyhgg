# お知らせ

チュートリアルを削除しました。
理由は、以下のツイートを発見したからです。

* https://twitter.com/lotz84_/status/736885466927005697

このチュートリアルはHaskellと英語の文法の勉強を兼ねて、私が私のために翻訳したものです。
内容には多くの誤りが含まれていますから、本来公開すべきものではありません。
それでも、LYHGGの日本語版というタイトルのもとで公開していたのは、このチュートリアルを見つけた親切な方が、翻訳の誤りを指摘してくれるのではないかと考えていたからです。

しかし、翻訳の誤りを指摘されることは一度もありませんでした。
更に、私の関数プログラミングについての知識不足から、適切な日本語の表現を選択できていないことに気づきました。
英文法の知識と関数プログラミングの知識が身についていない状態での翻訳は続行できないと判断したため、私は途中で翻訳作業を中断し、このチュートリアルを放置していました。
そして、上記のツイートを発見したため、このチュートリアルを削除するに至りました。

Learn You a Haskell for Great Good!の日本語版をを探しの方は、
田中英行・村主崇行(訳)の
[すごいHaskellたのしく学ぼう](https://estore.ohmsha.co.jp/titles/978427406885P)
を購入してください。
こちらの書籍はMiran Lipovača氏の許諾を得て翻訳された正式な日本語版です。

--------

# Learn You a Haskell for Great Good! - Japanese version


You're ready to learn Haskell but you are not good at English?
Never mind!
I'm working on translating the most funkiest way to learn Haskell written by Miran Lipovača into Japanese.
Please visit at:

* [https://moutend.github.io/jlyhgg/](https://moutend.github.io/jlyhgg/)



## Preview at local

You can also preview this tutorial on your machine.
(You need Ruby 2.1+ and bundler gem.)

```shell
git clone https://github.com/moutend/jlyhgg
cd ./jlyhgg
mkdir -p vendor/bundle
bundle install --path vendor/bundle
bundle exec jekyll server
```



## Motivation

To improve my English and learn Haskell.
So the quality of translation,
correctness of terms,
natural Japanese representations and fun of jokes are not guaranteed.

If you are looking for a reliable tutorial written in Japanese,
I recommend you to read a official Japanese version of LYHGG called
[すごいHaskellたのしく学ぼう！](https://estore.ohmsha.co.jp/titles/978427406885P)
at first.
Well, I think that thousand of sophisticated jokes are difficult part of translation.
It's not joke, really.



## Contributing

You can report the weird part of translation via Issues
or directly fix it and send Pull Request.
The markdown files that corresponding to each chapters are located in `chapters` directory.
And I appreciate your any contribution, any time.



## TODO

* Clarify the meaning between:
  * do, execute, run, perform, etc
  * act, play, behave, etc
  * feed, give
* Make the rules for when we use Katakana or Kanji:
  * To map is マップする.
      * It's OK and it sounds like natural Japanese.
  * To zip is ジップする.
    * ジップする sounds like closing a plastic bag.
  * Applicatives, Functors, Monoids, Monads, etc
  * Concrete types
  * They have no appropriate words responding to natural Japanese representations.
  * Ofcource, using Katakana for these words is one of the solutions.



## LICENSE

This version is licensed under the same terms of original (CC BY-SA 3.0).
