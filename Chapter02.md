---
title: "Starting Out"
layout: article
---



`ghci`でHaskellのREPLが起動する。

    % ghci
    GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
    Prelude>

プロンプトを`Prelude>`から`ghci>`に変更。

    Prelude> :set prompt "ghci> "
    ghci>

遊ぶ。

    ghci> 1 + 2
    3
    ghci> 1.23 * 4
    4.92
    ghci> -0.12 * 3.45
    -0.414

たのしい。

    ghci> 123 == 123
    True
    ghci> 123 /= 123
    False
    ghci> "hello" == "hello"
    True
    ghci> "hello" /= "hello"
    False

`123 + "hello"`はエラーになる。堅いHaskellたのしくない。

    ghci> 123 + "hello"
    <interactive>:7:5:
        No instance for (Num [Char]) arising from a use of ‘+’
        In the expression: 123 + "hello"
        In an equation for ‘it’: it = 123 + "hello"

エラー曰く、「"hello"が文字列だから123も文字列だろ、とおもったら違った」だそうです。勝手に変換したりしないのね。



## Function

実は、既にfunctionを使っていたというのに気づく。例えば、2つの引数で`*`を挟むと、それがfunctionをcallしたとみなされる。これをinfix functionという。

命令形の言語では、`関数名(1つめの引数, 2つめの引数)`と書くのに対して、Haskellでは関数名の後に` `を置いて、`関数名 1つめの引数 2つめの引数`と書く。

    ghci> succ 123
    124
    ghci> min 123 4
    4
    ghci> max 123 4
    123

ここで用語の整理

* infix function
  * `1 + 2`という書き方をする関数。実は`+`、`-`や`*`は関数。
* prefix function
  * `succ 123`のような書き方をする関数。
* function application
  * 関数名の後にスペースを挟んで、パラメータを与えること。

function applicationは最も優先度が高い。

    ghci> succ 9 + max 5 4 + 1
    16
    ghci> (succ 9) + (max 5 4) + 1
    16

この優先度を考慮しないと、succ(9 * 10)のつもりが、(succ 9) * 10になってしまう。

    ghci> succ 9 * 10
    100
    ghci> succ 9 * 10 == (succ 9) * 10
    True

9 * 10にsuccを適用したい場合、先に9 * 10をカッコで包む必要がある。

    ghci> succ (9 * 10)
    91

2つのパラメータを受け取るprefix functionは、バックティックス`\`で囲むと、infix functionとして扱える。divの場合、この記法のほうが見た目で分かりやすい。

    ghci> div 12 3
    4
    ghci> 12 `div` 3
    4



## Baby's first functions

ghciを起動したディレクトリで、次の内容を`baby.hs`として保存する。

    doubleMe x = x + x

`:l`で`baby.hs`を読み込む。

    ghci> :l baby
    [1 of 1] Compiling Main             ( baby.hs, interpreted )
    Ok, modules loaded: Main.

`+`はintegerをFloatとみなすので、`doubleMe`はどんな数にも対応できる。

    ghci> doubleMe 1
    2
    ghci> doubleMe 1.23
    2.46

次は、2つの引数を取って、それぞれ2倍した結果を足す`doubleUs`を作ってみる。

    doubleUs x y = x*2 + y*2

`baby.hs`を保存したら、もう一度`:l`で読み込むのをお忘れなく。

    ghci> :l baby
    [1 of 1] Compiling Main             ( baby.hs, interpreted )
    Ok, modules loaded: Main.

試そう。

    ghci> doubleUs 2 3
    10
    ghci> doubleUs 1.5 2.75
    8.5
    ghci> doubleUs 28 88 + doubleMe 123
    478

もちろん、関数の中で他の関数を使うこともできる。`doubleUs`を定義し直すと、次のようになる。

    doubleUs x y = doubleMe x + doubleMe y

ところで、Haskellには関数を定義する順番に制限がないので、`doubleUs`の前に`doubleMe`を定義することができる。

    doubleUs x y = doubleMe x + doubleMe y
    doubleMe x = x + x

次は`doubleSmallNumber`を定義してみる。いきなりだけど、Haskellにおけるifが登場。

    doubleSmallNumber x = if x > 100
                          then x
                          else x*2

命令型言語のifと、Haskellにおけるifの違いは、elseが必要かどうか、というところだ。Haskellでは必ずelseが必要になる。そして、もう一つの違いはHaskellにおけるifはstatementではなく、expressionである、というところだ。expressionというのは、値、あるいは値を返すコードのことで、例えば、`123`、`1 + 23`、`x * y`は、すべてexpressionになる。

Haskellにおけるifはexpressionだから、必ず値を返さなくてはいけない。だからelseを省略することはできない。ここで、ifがexpressionであることを確認してみよう。

    doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

`()`を外してみる。

    doubleSmallNumber'' x = if x > 100 then x else x*2 + 1

ご想像の通り、elseより後ろは、まだexpressionが続いているのだ、と解釈されてしまい、`12 * 2 + 1`と扱われてしまう。

    ghci> doubleSmallNumber'' 12
    25

ちなみに、Haskellでは関数の名前に`'`を使うことができる。ということで、`doubleSmallNumber'`の`'`には特別な意味はない。Haskellの慣習では、関数`foo`を少し変更したときに`foo'`と名前をつけることが多い。

もちろん、次のような名前の関数を作ることだてできる。

    conanO'Brien = "It's a-me, Conan O'Brien!"

この関数について、注目してほしいことが2つある。

1. 関数の名前を小文字から始めている
1. 関数`conanO`は引数を取らない

1.は単にHaskellの制限でそうなっている。2.のような返される値の変わらない関数は、definitionという。



## An intro to lists

さて、次はリストについて説明しよう。Haskellにおけるリストはhomogenousなデータ構造だからたとえば `[123, True, "foo"]`というリストは作れない。次のように、リストの中身は同じ種類に揃える必要がある。(`let`は値の定義に使う。後で詳しく説明)リストは`[]`で値を包む。値は`,`で区切る。`,`と値の間にスペースが挟まれてもOKだ。

    ghci> let lostNumbers = [4,8,15,16,23,42]
    ghci> lostNumbers
    [4,8,15,16,23,42]

文字列はCharのリストになっている。例えば`"hello"`は`['h', 'e', 'l', 'l', 'o']`のシンタックスシュガーになる。

    ghci> "hello" == ['h', 'e', 'l', 'l', 'o']
    True

リストを連結するには`++`を使う。

    ghci> [1,2,3,4] ++ [9,10,11,12]
    [1,2,3,4,9,10,11,12]

もちろん、1つの値しか持たないリストを連結することもできる。

    ghci> [1,2,3] ++ [4]
    [1,2,3,4]

リストの先頭に値を追加したい場合、`:` (cons operatorとも言う)を使う。

    ghci> 'A':" SMALL CAT"
    "A SMALL CAT"
    ghci> 5:[1,2,3,4,5]
    [5,1,2,3,4,5]

実は`[1,2,3]`というのもシンタックスシュガーで、実際には`1:2:3:[]`と等しい。([]は空のリスト)

    ghci> 1:2:3:[]
    [1,2,3]

リストからインデックスを指定して値を取り出すには、`!!`を使う。

    ghci> "Steve Buscemi" !! 6
    'B'
    ghci> [9.4,33.2,96.2,11.2,23.25] !! 1
    33.2

存在しないインデックスを与えるとエラーになる。

    ghci> [1,2,3] !! 4
    *** Exception: Prelude.!!: index too large

リストは、リストのリスト、のリスト...のように、ネストすることができる。

    ghci> [[1,2], [3]]
    [[1,2],[3]]

ただし、最初に説明したように、リストの中身は同じ種類にする必要があるから注意してほしい。例えば`[1, [2,3]]`が含んでいるのははIntegerとリストのリストだから、エラーになる。

    ghci> [1,[2,3]]
    <interactive>:33:1:
        Non type-variable argument in the constraint: Num [t]
        (Use FlexibleContexts to permit this)
        When checking that ‘it’ has the inferred type
          it :: forall t. (Num t, Num [t]) => [[t]]

ここで便利な機能を紹介しよう。`>`や`==`などを使って、リストとリストを比較することができる。左側のリストの0番目の値と、右側のリストの0番目の値を比較して、次に左側のリストの1番目の値と右側のリストの1番目の値を比較して...というのを繰り返し、リスト内のすべての要素が条件を満たせば、`True`となる。

    ghci> [3,2,1] > [2,1,0]
    True

他にもリストを操作できる便利な関数が用意されている。headはリストの先頭から要素を1つ取り出す。

    ghci> head [1,2,3]
    1

tailはリストから先頭の要素を取り除いたリストを返す。

    ghci> tail [1,2,3]
    [2,3]

lastはリストの末尾の要素を取り出す。

    ghci> last [1,2,3]
    3

initはリストから末尾の要素を取り除いたリストを返す。

    ghci> init [1,2,3]
    [1,2]

ちなみに、これらは`[]`に対して使うとエラーになる。エラーは実行時に初めてわかるので、`[]`は慎重に扱う必要がある。

    ghci> head []
    *** Exception: Prelude.head: empty list
    ghci> tail []
    *** Exception: Prelude.tail: empty list
    ghci> last []
    *** Exception: Prelude.last: empty list
    ghci> init []
    *** Exception: Prelude.init: empty list

lengthはリストの長さを返す。

    ghci> length [1,2,3]
    3
    ghci> length []
    0

nullはリストが`[]`か判定する。

    ghci> null [1,2,3]
    False
    ghci> null []
    True

reverseはリストを反転する。

    ghci> reverse [1,2,3]
    [3,2,1]
    ghci> reverse []
    []

takeはリストの先頭から指定した数だけ、リストとして値を取り出す。指定した数がリストの長さより大きければ、リストをそのまま返す。

    ghci> take 2 [1,2,3]
    [1,2]
    ghci> take 4 [1,2,3]
    [1,2,3]

dropはtakeと逆の働きをする。

    ghci> drop 2 [1,2,3,4,5]
    [3,4,5]
    ghci> drop 10 [1,2,3,4,5]
    []

maximumはリストの中から最大値を取り出す。

    ghci> maximum [1,2,3]
    3

sumはリストの総和を返す。

    ghci> sum [1,2,3]
    6

productはリストの層積を返す。

  ghci> product [1,2,3,4]
  24

elemはリスト内に与えられた要素が含まれているか判定する。

    ghci> elem 1 [1,2,3]
    True
    ghci> elem 4 [1,2,3]
    False

elemはinfix functionとして使うほうが読みやすい。

    ghci> 1 `elem` [1,2,3]
    True
    ghci> 4 `elem` [1,2,3]
    False



## Texas ranges

1,2,3...やa,b,c...など、列挙できる￥値のリストを作るときにはrangeを使うことができる。

    ghci> [1,2,3,4,5] == [1..5]
    True

rangeはその感覚を設定できるので、たとえば奇数のリストを作成できる。

    ghci> [1,3 .. 20]
    [1,3,5,7,9,11,13,15,17,19]

rangeは単に先頭2つの値の差を次の値に足しているだけなので、たとえば`[1,4,9,16]`というリストは作れない。

    ghci> [1,4.20]
    [1.0,4.2]

また、`[10..1]`というのは`[]`になるので`[10,9..1]`とする必要がある。

    ghci> [10..1]
    []
    ghci> [10,9..1]
    [10,9,8,7,6,5,4,3,2,1]

rangeでは実数が使えるものの、誤差が積み重なると値が正確ではなくなる。rangeでは実数を使うのはおすすめしない。

    ghci> [0.1, 0.3 .. 1]
    [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]  [

さて、ここで興味深い機能を紹介しよう。Haskellでは無限のリストを作り、値を取り出すことができる。ちなみに、`[1..]`とだけ入力すると、`[1,2,3...`と無限に結果が表示され続けてしまう。そのときは、`Ctrl-C`でキャンセルしよう。

    ghci> take 5 [1..]
    [1,2,3,4,5]

cycleは与えたリストを繰り返し、無限リストを作る。

    ghci> take 10 (cycle [1,2,3])
    [1,2,3,1,2,3,1,2,3,1]

repeatは与えた値で無限リストを作る。

    ghci> take 10 (repeat 7)
    [7,7,7,7,7,7,7,7,7,7]

replicateは第2引数で与えた値を第1引数の数だけ繰り返し、リストを作る。

    ghci> replicate 3 10
    [10,10,10]



## I'm a list comprehension

ここで、ちょっとした数学の話をしよう。大丈夫、難しい話はしないから読み続けてほしい。以下の数式は、2,4,6...という偶数を10個含む集合を表している。ちなみに、$2 \cdot x$はoutput function、$x$はvariable。$\mathbb{N}$はinput set、$x \leq 10$はpredicateという。

```math
\{2 \cdot x \mid x \in \mathbb{N}, x \leq 10 \}
```

この数式をHaskellで書いてみよう。

    take 10 [2,4..10]

うん、まぁ便利だけど、でも、もっと複雑な場合は?



### List comprehension

もう一度、2,4..という偶数を10個とりだしてみる。List comprehensionを使うと、以下のように書ける。`x`には`[1..10]`から取り出された値が入る。そして、2倍されたxのリストが得られる。

    ghci> [2*x | x <- [1..10]]
    [2,4,6,8,10,12,14,16,18,20]

いいね、期待通り！

次は、条件(predicate)を加えてみる。条件は、`,`で区切った後に書く。試しに、「2倍された値が12より大きいか等しい」という条件を加えてみる。

    ghci> [2*x | x <- [1..10], 2*x >= 12]
    [12,14,16,18,20]

おお、期待通り！じゃあ、「50から100までの自然数の内、7で割ったあまりが3になる数」を求めるには?

    ghci> [x | x <- [50..100], x `mod` 7 == 3]
    [52,59,66,73,80,87,94]

これは便利。ちなみに、リストからpredicateにもとづいて取り出す値を決めることをfilterとも言う。

次は、「10より大きいか等しい奇数は"BANG!"、10より小さい奇数は"BOOM!"、それ以外の数は、何も表示しない(リストに含めない)。」というリストを作ってみる。もちろん、functionのなかでlist comprehensionを使うこともできる。

    boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

predicateに注目。`odd x`は、`x`が奇数の場合`True~を、そうでない場合は`False`を返す。前に確認したとおり、predicateがTrueになると、リストに値が含まれ、Falseのときはリストから値が覗かれる。

    ghci> boomBangs [7..13]
    ["BOOM!","BOOM!","BANG!","BANG!"]

もちろん、複数のpredicateを使うこともできる。例として、「10から20までの数の内、13、15、19を含めないリスト」を作ってみる。

    ghci> [x | x <- [10..20], x /= 13, x /= 15, x /= 19]
    [10,11,12,14,16,17,18,20]

リストに含まれるのは、すべてのpredicateを満たした値のみ、なので注意してほしい。

次は、複数のリストから値を取り出してみよう。取り出した値は、output functionでjoinできる。例として、リスト`[1,2,3]`と`[4,5,6]`の直積を求めてみよう。

    ghci> [x*y | x <- [1,2,3], y <- [4,5,6]]
    [4,5,6,8,10,12,12,15,18]

`x*y`の組み合わせは、1と4、2と5、3と6ではなく、1と4、1と5、1と6、2と4...となるので注意。この場合、結果として、リストの長さは3 * 3 = 9になる。

次は、`x*y >= 50`というpredictを与えてみる。

    ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
    [55,80,100,110]

文字列はリストだから、この機能は文字列に対しても使える。

    ghci> let nouns = ["hobo","frog","pope"]
    ghci> let adjectives = ["lazy","grouchy","scheming"]
    ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
    ["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",
    "grouchy pope","scheming hobo","scheming frog","scheming pope"]

さて、ここで`length`を自分で作ってみよう。

    length' xs = sum [1 | _ <- xs]

上記の`_`という変数は、この変数は意味を持たない、という意味を表す。つまり、実際には使わない、ダミーの変数であることを示すのに使う。この機能を使うと、たとえば変数を受け取るものの、その変数は使わない、という関数を定義することができる。

    ghci> let mom _ = "Do your homework!"
    ghci> mom "I want to play video games!"
    "Do your homework!"
    ghci> mom "OK. How about playing video games for 10 minutes?"
    "Do your homework!"

おっと、話を元に戻そう。`length'`は、与えられたリスト`xs`に含まれる値を、すべて`1`で置き換える。その後`sum`でリストの値の合計を求める。結果として、与えたリストの長さが得られる、というわけだ。

では、与えられた文字列から小文字を取り除く、という関数を作ってみよう。

    removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

文字列もリストだから、この関数は期待通り機能するはずだ。

    ghci> removeNonUppercase "Hahaha! Ahahaha!"
    "HA"
    ghci> removeNonUppercase "IdontLIKEFROGS"
    "ILIKEFROGS"

期待通り！もちろん、predicateの中で、リストが使える。そして、リストを含むリスト、も使えることに注目してほしい。ということで、リストの形を保ったまま、奇数を取り除く関数を作ってみよう。

    ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
    ghci> [ [ x | x <- xs, even x ] | xs <- xxs]
    [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]

リストを含むリスト、は読みづらいから開業するのがおすすめだ。



## Tuples

一つ、あるいは複数の値を保持できるという点で、タプルとリストは似ている。ただし、リストは無限リストが作れるのに対し、タプルではそれができない。そして、もう一つの重要な違いは、タプルはhomogenousではない、というところだ。タプルでは、異なる方の値を保持できる。タプルは`()`で値を包んで表す。値は`,`で区切る。

ここで、二次元のベクトルをHaskellで表現する方法を考えてみよう。ひとつはリストを使う方法...[x,y]とすれば、多分、期待通りに動くと思う。じゃあ、二次元の平面上で、なにか図形を表現するとして、幾つかのベクトルを含むリストを作ったらどうなる?そうだね、`[[1,2],[8,11],[4,5]]`という書き方ができる。

    ghci> [[1,2], [3,4], [5,6]]
    [[1,2],[3,4],[5,6]]

ただし，これには問題がある。Haskellのリストは、同じ種類の値しか含めることができない。逆に言えば、例えば`[[1,2], [3,4,5], [6,7]]`というリストはエラーにはならない。「リストを含むリスト」という構造には違反していないからだ。これでは、`[3,4,5]`という3次元のベクトルが含まれるのを防げない。

    ghci> [[1,2], [3,4,5], [6,7]]
    [[1,2],[3,4,5],[6,7]]

ここで、タプルの出番だ。タプル(要素数が2つの場合はペアとも言う)は、それ自身がtypeとして振る舞う。つまり、`(1,2)`と`(1,2,3,)`は違うものとして扱われる。試してみよう。

    ghci> [(1,2), (3,4), (5,6)]
    [(1,2),(3,4),(5,6)]

おおっと、間違えて3次元のベクトル`(3,4,5)`を含めてしまった！

    ghci> [(1,2), (3,4,5), (6,7)]
    <interactive>:97:9:
        Couldn't match expected type ‘(t, t1)’
                    with actual type ‘(Integer, Integer, Integer)’
        Relevant bindings include
          it :: [(t, t1)] (bound at <interactive>:97:1)
        In the expression: (3, 4, 5)
        In the expression: [(1, 2), (3, 4, 5), (6, 7)]
        In an equation for ‘it’: it = [(1, 2), (3, 4, 5), (6, 7)]

このエラーは、「リストには要素数が2つのタプルしか含まれないはずなのに、要素数が3つのタプルが含まれている。」ということを報告している。

もちろん、タプルの要素数だけではなく、タプルに含まれるtypeの組み合わせが一致しなければならない。ということで、、`[(1,2), ("foo", "bar)]`というリストも作ることができない。

    ghci> [(1,2), ("foo", "bar")]
    <interactive>:101:3:
        No instance for (Num [Char]) arising from the literal ‘1’
        In the expression: 1
        In the expression: (1, 2)
        In the expression: [(1, 2), ("foo", "bar")]

タプルは異なるtypeの値を含めることができる。例えば、性、名、年齢をまとめたデータ構造を作ることができる。

    ghci> ("Christopher", "Walken", 55)
    ("Christopher","Walken",55)

タプルの注意点は、すべてのタプルで共通に使える関数は作れない、というところだ。そして、要素数が1つのタプルは、値そのものになる、というところだ。というより、値と、要素数が1のタプルを区別できない、というのが正しい。

    ghci> (1) == 1
    True
    ghci> ("foo") == "foo"
    True

タプルはリストのように比較できる。ただし、先程説明したとおり、すべてのタプルに対して使用できる関数は作ることができない。最初から定義されているのは、ペアに対する関数だけだ。要素数が2より大きいタプルから値を取り出す方法は後で説明する。

`fst`はペアの1つ目の値を取り出す。

    ghci> fst (8,11)
    8
    ghci> fst ("Wow", False)
    "Wow"

`snd`はペアの2つ目の値を取り出す。

    ghci> snd (8,11)
    11
    ghci> snd ("Wow", False)
    False

`zip`は、リスト1とリスト2を与えると、リスト1のn番目の要素とリスト2のn番目の要素を含むペアを作る。

    ghci> zip [1,2,3,4,5] [5,5,5,5,5]
    [(1,5),(2,5),(3,5),(4,5),(5,5)]
    ghci> zip [1 .. 5] ["one", "two", "three", "four", "five"]
    [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]

`zip`は、リストの要素数が一致しない場合、要素数は小さい方に合わせる。

    ghci> zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
    [(5,"im"),(3,"a"),(2,"turtle")]

`zip`は、無限リストに対しても使える。

    ghci> zip [1..] ["apple", "orange", "cherry", "mango"]
    [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]

では、タプルとlist comprehensionを組み合わせた問題を説いてみよう。すべての辺が10より小さいか等しく、3辺の合計が24となる直角三角形を求めよう。まずは、すべての辺が10より小さいか等しい三角形を求める。

    ghci> let triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

`triangles`は、辺の長さが10より小さい三角形を列挙した結果となる。

    ghci> triangles
    [(1,1,1),(2,1,1),(3,1,1),(4,1,1),(5,1,1),(6,1,1...

次は、直角三角形という条件を加えよう。直角三角形は$a&2 + b^2 = c^2$となる。cが3つの辺の内、最も長い変だから、aとbはそれより小さくなる。これをHaskellで書くと、こうなる。

    ghci> let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
    ghci> rightTriangles
    [(3,4,5),(6,8,10)]

あとは、3辺の合計が24、という条件を加えれば完成だ。

    ghci> let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
    ghci> rightTriangles'
    [(6,8,10)]

まずは、問題から導かれる答えの断片を集めて、それから答えの断片を組み合わせたり、変形させたり、フィルターしたりして、目的の答えを得る。これがfunctional programmingの定石だ。



# Types and Typeclasses

## Believe the type

Haskellはstaticなtype systemを備えている。そして、expressionのtypeは、コンパイル時に判明する。このおかげで、例えば`True `div` 123`というコードが実行されるのをコンパイルの時点で防げる。Haskellでは、あらゆるものがtypeを持つ。そのため、Haskellではコンパイルに時間が掛かる。

そして、Javaとは異なり、Haskellはtype inferenceという機能を備えている。`1 + 2.34`というコードには、typeについての情報がない。しかし、`3.34`という結果が得られる。Haskellがコードのtypeが何なのか、推測したのだ。type inferenceは、expressionにtypeの情報を付け加える手間を省いてくれる。

Starting Outでは、Haskellの基本について、表面的なところを学んだ。しかし、type systemはHaskwllを深く学ぶ上で、とても重要な概念だ。typeというのはexpressionごとに付けられたラベルのようなものといえる。`True`はBooleanに当てはまるし、`"hello"`はStringに当てはまる。typeは、あるexpressionが、どんな種類に当てはまるかを指し示す。ghciで`:t`と入力して、expressionのtypeを調べてみよう。

    ghci> :t 'a'
    'a' :: Char
    ghci> :t True
    True :: Bool
    ghci> :t "HELLO!"
    "HELLO!" :: [Char]
    ghci> :t (True, 'a')
    (True, 'a') :: (Bool, Char)
    ghci> :t 4 == 5
    4 == 5 :: Bool

あるexpressionに対して`:t expression`と入力すると、`expression :: type`という結果が得られる。`::`の右側にあるのが、expressionのtypeだ。typeの名前は、BoolとかCharのように、必ず大文字で始まる。

'a'`のtypeは`Char`、`True`のtypeは`Bool`になるのは理解できると思う。でも、`"HELLO"`のtypeは`[Char]`と表示されている。これは何だろう?...そう、`[]`はtypeがリストであることを意味する。つまり`[Char]`はCharを含むリスト、という意味になる。`"hello"`というStringが実際には`['h', 'e', 'l', 'l', 'o']`のシンタックスシュガーだ、というのはStarting Outで学んだ。

そして、タプルはそれ自身がtypeになるのだった。ということで、例えば`('a', True)`というexpressionは`(Char, Bool)`というtypeとみなされる。`Char`と`Bool`をまとめて、ひとつのtypeとして扱われるのだ。最後の`4 == 5`というのは、`False`になるので、typeはBoolになる。

もちろん、関数もtypeを持つ。そして、自分で関数を定義するとき、typeについての情報を明示的に付け加えることができる。これをtype decralationという。大きく複雑な関数を定義するときは、type declarationを心がけよう。...ということで、此処から先は、関数を定義するときは必ずtypeを宣言する。

List comprehensionで作った文字列から小文字を取り除く、という関数を覚えているかな? あの関数にtype decralationを加えてみよう。

    removeNonUppercase :: [Char] -> [Char]
    removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

`[Char] -> [Char]`というのは、この関数が`[Char]`を受け取り、`[Char]`を返す、ということを表している。つまり、この関数は文字列から文字列へのマッピングを行う、というのを意味している。

ちなみに、`[Char]`の代わりとして、`String`という別名が用意されている。`String`を使ったほうが読みやすいから、関数の定義を書き換えてみよう。この商の冒頭で説明したとおり、`removeNonUppercase`は、Haskellが関数のtypwを推測してくれるので、type declarationをする必要はない。でも、type declarationをする癖をつけたほうが良いから、そうしている。

    removeNonUppercase :: String -> String
    removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

次は、複数の引数を受け取る関数のtype declarationについて。引数として3つの整数を受け取り、その合計を返す関数を作ってみよう。

    addThree :: Int -> Int -> Int -> Int
    addThree x y z = x + y + z

`Int -> Int -> Int -> Int`のうち、一番右側の`Int`が、このかんすうが返す値のtypeを表している。そして、最初の3つが、引数のtypeを表している。

関数のtype declarationでは引数を`->`で区切る、というのは分かった。でも、たとえば`Int, Int, Int -> Int`という書き方ができるほうが、自然じゃない? ...なぜ不自然な書き方をするのか、その理由は後にわかる。お楽しみに。

先程、関数を作るときは、type decralationを書くべし、と言った。ただし、関数を作ろうとして、type declarationを正しく行えるか自身がなければ、type declarationを省いても大丈夫だ。関数を作ったら、その都度`:t`で関数のtypeを調べる癖をつけよう。



## Common types

ここで、一般的なtypeを紹介する。

`Int`は整数を意味する。`7`は整数だけど、`7.89`は整数ではない。`In`には最大値と最小値が存在する。32 bitのマシンでは、最大値は`2147483647`、最小値は`-2147483648.`となる。

`Integer`も、整数を意味する。`Int`との違いは、最大値と最小値が存在しない、というところだ。つまり、大きな整数が扱える。大きな、というのは本当に大きな、という意味だ。関数`fuctorial`を作ってみよう。

    factorial :: Integer -> Integer
    factorial n = product [1..n]

試してみる。

    ghci> factorial 50
    30414093201713378043612608166064768844377641568960512000000000000

`Float`は単精度の浮動小数点を意味する。関数`circumference `を作ってみる。

    circumference :: Float -> Float
    circumference r = 2 * pi * r

試してみよう。

    ghci> circumference 4.0
    25.132742

`Double`は倍精度の浮動小数点を意味する。関数`circumference'`を作ってみる。

    circumference' :: Double -> Double
    circumference' r = 2 * pi * r

結果の精度が向上するか、試してみよう。

    ghci> circumference' 4.0
    25.132741228718345

`Bool`は真偽値を意味する。値としては`True`と`False`を持つ。

`Char`は1文字を意味する。typeがCharの値を作るには、`'a'`のように、シングルクォートで囲む。

`String`は`Char`のリスト、つまり`[Char]`と等しい。typeがStringの値を作るには`"hello"`のように、ダブルクオートで囲む。

タプルは、それ自身がtypeとして振る舞う。タプルのtypeは、その中身によって決まるから、たとえば`(Int, Bool)`、`(String, String, String)`のように、無数のtypeを創りだすことができる。

ちなみに、空のタプル`()`は、空のタプル`()`というtypeとして扱われる。

    ghci> :t ()
    () :: ()



## Type variables

ところで、Starting Outで`head`という関数を紹介したのを覚えているかな?`head`はリストから先頭の要素を1つ取り出す。でも、この関数はリストの中身がどのtypeなのか、気にしない。この関数のtype decralationはどうなっているんだろう? 調べてみよう。

    ghci> :t head
    head :: [a] -> a

ハァ!? なんだこりゃ。`[a]`の`a`はtypeなのか?...いや、typeの名前は`Int`とか`String`のように、大文字で始まるから、違う。

この`a`はtype variableという。type variableは小文字で始めても良いし、(この例では1文字だけど)名前の長さに制限はない。`[a]`は、「`a`にはあらゆるtypeが当てはまる」というのを意味する。type variableは他の言語で言うところのジェネリクスに相当する。Haskellではtype variableのおかげで、あらゆるtypeに対応できる関数をジェネリクスよりもお手軽に作ることができる。

Haskellでは、関数のtype declarationにtype variableがある場合、その関数をpolymorphic functionという。

`head`のtype decralationは、あらゆるtypeのリストを受け取り、そのtypeの値を返す、ということを意味する。つまり、たとえば、`[True, True, False]`というリストが渡されると、関数headは、`[Bool] -> Bool`として振る舞うし、`"hello"`というリストが渡されると`[Char] -> Char`として振る舞うことを意味する。

次は、`fst`のtype decralationを調べてみよう。この関数は、ペアを受け取って最初の要素を返すのだった。

    ghci> :t fst
    fst :: (a, b) -> a

これで、fstがあらゆるペアに対して使える理由が分かった。fstのtype decralationは、2つのtype(どんなtypeでもOK)で構成されたペアを受け取り、その1つめの値を返す、という意味になる。

ちなみに、`(a,b)`というのは、aとbのtypeが異なることを意味する。もし、fstのtype declarationが`(a, a)`だったら、`(1, 23)`や`("hello", "world")`のように、2つの値のtypeが同じペアしか受け取ることができない、という意味になる。



## Typeclasses 101

次はtypeclassについて説明しよう。typeclassは振る舞いを定義するインターフェースの一種だ。あるtypeがtypeclassの一部である場合、そのtypeはtypeclassの振る舞いを実装したものといえる。

typeclassという概念は、オブジェクト指向に慣れている人であれば混乱すると思う。もし、Javaなどのオブジェクト指向言語に慣れ親しんでいるのであれば、typeclassは単なるインターフェースだ、と考えればいい。

さて、Haskellでは`==`も関数だ、というのはStarting Outで学んだ。このようなHaskellに最初から組み込まれている関数、`+`、`*`、`/`などを他の関数に渡す、あるいはprefix functionとして使いたい場合、関数を`()`で包む。ここで、`()`は、タプルではないので注意してほしい。

    ghci> (==) 123 123
    True
    ghci> (==) "foo" "bar"
    False

では、`==`のtypeを調べよう。

    ghci> :t (==)
    (==) :: (Eq a) => a -> a -> Bool

おっと、新しく`=>`という記号が登場した。ちなみに、`=>`の前にある部分をclass constraintという。`==`のtype declarationの読み取り方は、次のとおりだ。

`==`関数は、あらゆる2つの値を受け取る。そして、それら2つが同じtypeであるか判定し、typeがBoolの値、つまりTrueかFalseを返す。このとき、判定される2つの値は、必ず`Eq`というtypeclassに属さなければならない。

`Eq a`というのがclass constraintだ。そして、`Eq`というtypeclassは2つの値が等しいかを判定するインターフェースを提供する。Haskellでは、IOという入出力を扱うtypeを除き、標準的なすべてのtypeが`Eq`に属する。

たとえば`elem`という関数は`Eq`というtypeclassに属する値を受け取っている。渡されたリストの中に、目的の値が存在するか、`==`を使って判定しているからだ。

    ghci> :t elem
    elem :: (Eq a, Foldable t) => a -> t a -> Bool

このtype declarationは、`elem`は`Eq`に属する値とリストを受けとって、リスト内にその値が存在するか判定して、結果を`Bool`で返す、ということを意味している。(Foldableについては後で説明する。)



## Some basic typeclasses

代表的なtypeclassを紹介しよう。

`Eq`は、渡された値が等しいか判定するためのtypeclassだ

。`Eq`というtypeclassを実装している関数は`==`と`/=`の2つだ。たとえば、ある関数のtype constraintの中に`Eq a`というtype constraintがある場合、その関数は`==`または`/=`を関数の中で使用していることを意味する。

そして、IntやFloat、StringやBoolなど、関数を除くすべてのtypeは`Eq`というtypeclassに属する。試してみよう。

    ghci> 123 == 132
    False
    ghci> 1.23 /= 1.23
    False
    ghci> True == False
    False
    ghci> "foo" /= "bar"
    True

`Ord`は順番を持つtypeのためのtypeclassだ。

    ghci> :t (>)
    (>) :: (Ord a) => a -> a -> Bool

`Int`や`Float`、`Char`などのすべてのtypeは、Ordというtypeclassに属する。そして、Ordは、すべての標準的な比較関数(`>`、`<`、`>=`、`<=`)で使用されている。

`compare`という関数は、同じtypeかつOrdというtypeclassに属する2つの値を取り、`Ordering`というtypeの値として結果を返す。`Ordering`というtypeは、値として`GT`、`LT`、`EQ`を持つ。

    ghci> "foo" `compare` "bar"
    GT
    ghci> 1 `compare` 234
    LT
    ghci> 1.23 `compare` 1.23
    EQ

`Show`というtypeclassに属するものは、Stringで表せる。関数を除き、すべてのtypeがShowに属する。

`show`という関数は、Showというtypeclassを扱うのに使われる。`show`はShowに属するtypeの値を受け取って、その文字列表現を返す。

    ghci> show 3
    "3"
    ghci> show 5.334
    "5.334"
    ghci> show True
    "True"

`Read`は`Show`と逆の働きをするtypeclassだ。

関数`read`は、文字列を受け取って、Readに属するtypeの値を返す。

    ghci> read "True" || False
    True
    ghci> read "8.2" + 3.8
    12.0
    ghci> read "5" - 2
    3
    ghci> read "[1,2,3,4]" ++ [3]
    [1,2,3,4,3]

ここまでは順調。では、`read "4"`の結果はどうなる?

    ghci> read "4"
    <interactive>:1:0:
        Ambiguous type variable `a' in the constraint:
          `Read a' arising from a use of `read' at <interactive>:1:0-7
        Probable fix: add a type signature that fixes these type variable(s)

ghciのエラーメッセージは、どんなtypeの値を返せばよいかわからない、ということを表している。最初にreadの説明をした時、あえて`read "1" + 23`のような例を示した。これは、"1"という値がどんなtypeなのか、`23`という情報から推測できるようにするためだ。

Readに属するtypeはいくつかあるから、`read "1"`だけでは情報が不足している。"1"が`Int`なのか`Float`なのか、あるいは`Double`なのか、Haskellは推測できなかった。

ここで、関数`read`のtype declarationを確認しよう。

    ghci> :t read
    read :: (Read a) => String -> a

この結果から、文字列を受け取り、`Read`というtypeclassに属するtypeの値を返す、ということが読み取れる。つまり、最終的に関数readに渡された文字列がどのtypeの値になるのか、追加の情報なしには判断できない。

ここで、明示的なtype annotationが必要になる。明示的なtype annotationというのは、expressionの後ろに`::`を置き、そのexpressionのtypeを指定する、というものだ。試してみよう。

    ghci> read "1" :: Int
    1
    ghci> read "1" :: Float
    1.0
    ghci> read "1" :: Double
    1.0

ほとんどのexpressionはコンパイラがそのtypeを推測してくれる。しかし、`read "1"`のようなexpressionでは、IntなのかFloatなのかDoubleなのか、typeが推測できない。というより、返すべきtypeの候補が複数あるので何を返すべきか定まらない。

Haskellはstaticなtype systemを持っているから、コードがコンパイルされる時点で、(ghciなら、1行入力した時点で)それぞれのexpressionが持つtypeの情報を、すべて汁必要がある。

面倒だけど、明示的にtype annotation をしなければならないこともある、とおぼえておこう。

`Enum`は列挙できる、という性質を表すtypeclassだ。Enumというtypeclassの便利なところは、rangeでリストを作れる、というところだ。Bool, Char, Ordering, Int, Integer, Float, Doubleが、Enumというtypeclassに属する

`Enum`というtypeclassを実装した関数として、`succ`と`pred`がある。

    ghci> ['a'..'e']
    "abcde"
    ghci> [LT .. GT]
    [LT,EQ,GT]
    ghci> [3 .. 5]
    [3,4,5]
    ghci> succ 'B'
    'C'
    ghci> pred 'b'
    'a'

`Bounded`というtypeclassに属するtypeの値は、上限と加減を持つ。

    ghci> minBound :: Int
    -2147483648
    ghci> maxBound :: Char
    '\1114111'
    ghci> maxBound :: Bool
    True
    ghci> minBound :: Bool
    False

`Bounded`というtypeclassを実装した関数として、`minBound`と`maxBound`がある。これらの興味深いところは、polymorphicな定数である、というところ。調べてみよう。

    ghci> :t minBound
    minBound :: Bounded a => a
    ghci> :t maxBound
    maxBound :: Bounded a => a

そして、タプルは、Boundedに属する

    ghci> maxBound :: (Bool, Int, Char)
    (True,2147483647,'\1114111')

`Num`は数値として振る舞うtypeclassだ。このtypeclassに属するtypeのあ値は、数値として振る舞う。調べてみよう。

    ghci> :t 20
    20 :: (Num t) => t

すべての数値はpolymorphicな定数として振る舞う。Numに属するtypeの値は、他のNumに属するtypeとしても振る舞える。

    ghci> 20 :: Int
    20
    ghci> 20 :: Integer
    20
    ghci> 20 :: Float
    20.0
    ghci> 20 :: Double
    20.0


たとえば、関数`*`のtypeを調べると、Numに属するtypeの値を受け取ることが分かる。

    ghci> :t (*)
    (*) :: (Num a) => a -> a -> a

`*`は、Numに属する2つの同じtypeを受け取り、受け取ったtypeと同じtypeの結果を返す。ということで、IntとInt同士の計算はできても、たとえばIntとDoubleの計算はエラーになる。

    ghci> (1::Int) * (2::Int)
    2
    ghci> (1::Int) * (2::Double)
    <interactive>:199:13:
        Couldn't match expected type ‘Int’ with actual type ‘Double’
        In the second argument of ‘(*)’, namely ‘(2 :: Double)’
        In the expression: (1 :: Int) * (2 :: Double)
        In an equation for ‘it’: it = (1 :: Int) * (2 :: Double)


ある2つのNumに属するtypeを扱うには、それらがShowとEqに属している必要がある。

`Integral`というtypeclassも同じく数値を表す。`Num`は、実数と整数を含むすべての数値を表すのに対し、Integralは感覚を持つ数値を表す。このtypeclassに属するtypeは、`Int`と`Integer`だ。

`Floating`というtypeclassは浮動小数点を表し、FloatとDoubleの2つのtypeが属する。

数値を扱う上で便利なのが、`fromIntegral`という関数だ。`:t`でtype signatureを調べてみよう。

    ghci> :t fromIntegral
    fromIntegral :: (Integral a, Num b) => a -> b

`fromIntegral`は、Integral、またはNumを受け取ってNumを返す。つまり、より一般的に扱いやすいNumへIntegralを変換する関数と言える。整数と浮動小数点をを一緒に扱うときに便利だ。

たとえば、関数`length`は、リストの長さを返すのだった。ただし、Intで。なぜ結果がNumではなくIntなのかはよく知らないが、多分、Haskellの歴史的な事情があるんだろう。

    ghci> :t length
    length :: Foldable t => t a -> Int

Foldableについては後に説明するので、気にしないでほしい。

リストの長さに浮動小数を足したい、という需要があるかは知らないが、`fromIntegral`を使えばできる。

    ghci> fromIntegral (length [1,2,3]) + 1.23
    4.23

なお、class constraintは`,`で区切って複数記述できる。



# Syntax in Functions

この商では、Haskellの便利なシンタックスを紹介しよう。まずはpattern matchingから。

pattern matchingは、あるデータが、与えられたpatternに一致しているか判定し、一致していればそのpatternにもとづいてデータを分解する、という一連の流れから構成される。

関数を定義するとき、関数のbodyをpatternごとに分けて定義することができる。このおかげで、関数を読みやすくかつ、簡潔にできる。そして、pattern matchingは、どんなtypeに対しても可能だ。数値、文字、リスト、タプル...などなど。

ここで、与えた数値が7か判定する関数を作ってみよう。

    lucky :: (Integral a) => a -> String
    lucky 7 = "LUCKY NUMBER SEVEN!"
    lucky x = "Sorry, you're out of luck, pal!"

関数`lucky`を呼び出すと、patternが上から下に向かって、順番に試される。そして、patternに一致する関数のbodyが見つかると、それが使われる。

最初のpatternに一致するのは、関数に与えた値が7のときだけだ。最初のpatternに一致しなければ、次のpatternが試される。残ったpatternは、すべての値に一致するから、値は`x`にbindされる。

ところで、関数`lucky` は、Starting Outで説明した`if`を使っても実現できそうだ。

    lucky' :: (Integral a) => a -> String
    lucky' x = if x == 7
              then "LUCKY NUMBER SEVEN!"
              else "Sorry, you're out of luck, pal!"

実際、`lucky'`は期待通りに動作する。でも、たとえば1から5の値を受け取った時はそれに対応するメッセージを表示して、そうでなければ`"Not between 1 to 5`と表示する関数を作るとしたらどうだろう?pattern matchingがなければ、一々if-elseで条件を書かなければいけない。

では、pattern matchingを使って、`sayMe`という関数を作ってみよう。

    sayMe :: (Integral a) => a -> String
    sayMe 1 = "One!"
    sayMe 2 = "Two!"
    sayMe 3 = "Three!"
    sayMe 4 = "Four!"
    sayMe 5 = "Five!"
    sayMe x = "Not between 1 and 5"

いいね！if-elseを使わずに、しかも読みやすい関数の定義ができた。なお、最後のパターン(すべての数値に一致するパターン)を先頭へ移動させると、後の1、2、...5というパターンには永遠に一致しなくなる。常に最初のパターンにマッチしてしまうからだ。

ところで、`関数`factorial`を作ったのを覚えているかな? 引数としてnを与えると、1からnまでの席を求める関数だ。あの関数は、再帰的にも定義できる。

まず、`factorial 0`が`1`になる、というところからはじめよう。そして、正の整数はすべて、それ自身とそのpredecessoをかけた数として定義できる。これをHaskellで書くと、次のようになる。

    factorial :: (Integral a) => a -> a
    factorial 0 = 1
    factorial n = n * factorial (n - 1)

おめでとう、君はいま、初めて関数を再帰的に定義した。催奇は、Haskellにおいて重要な部分だから、次の賞で詳しく説明する。ここでは、簡単に、関数がどう働くかを説明しよう。

例として、`factorial 3`は`3 * (factorial 2)`となる。`factorial 2`は`2 * factorial 1`となる。`factorial 2`は`2 * factorial 1`となる。`factorial 0`は`1`だから、すべてを合わせると`3 * 2 * 1 * 1`となる。

0の場合のパターンを先に書いたのは、これがなければ`factorial`の計算が永遠に再起し続けて、`0, -1, -2, ...`のように止まらなくなるからだ。催奇の定義では、催奇を停止させるパターンから先に定義して、その他のパターンを後に書くのが一般的だ。

さて、ここでパターンマッチングが失敗する例を紹介しよう。次の関数のパターンマッチングは不完全だ。

    charName :: Char -> String
    charName 'a' = "Albert"
    charName 'b' = "Broseph"
    charName 'c' = "Cecil"

この関数に、想定外の値を与えると、こうなる。

    ghci> charName 'a'
    "Albert"
    ghci> charName 'b'
    "Broseph"
    ghci> charName 'h'
    "*** Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName

このエラーは、与えた値にマッチするパターンが見つからなかったことを報告している。パターンを作るときは、プログラムがクラッシュしないように毎回必ずすべての値にマッチするパターンを含め無くてはならない。

    charName' :: Char -> String
    charName' 'a' = "Albert"
    charName' 'b' = "Broseph"
    charName' 'c' = "Cecil"
    charName'  x  = "Anonymous"

これで、エラーは起こらなくなる。

    ghci> charName' 'h'
    "Anonymous"

パターンマッチングはタプルに対しても使うことができる。ペアとして表現されるベクトルを2つ受け取り、２次元の平面上でそのベクトルを足す、という関数を考えてみよう。ベクトルAのxとべくとるBのx、ベクトルAのyとべくとるB のyをそれぞれ足すことで、これは実現できる。

もし、パターンマッチを知らなければ、関数を次のように定義していただろう。

    addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
    addVectors a b = (fst a + fst b, snd a + snd b)

もちろん、これは正しく動作するし何の問題もない。だけど、パターンマッチングを使えば、もっとうまく書ける。

    addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
    addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

こっちのほうが分かりやすい。しかも、既にすべてのパターンにマッチしている。ということで、2つのペアを引数として受け取る、というのが保証できる。

さて、`fst`と`snd`は、それぞれペアから最初の値と、残りの値を抜き出す関数だ。でも要素が3つあるタプルに対しては、`fst`や`snd`は使えない。どうする? 自分で作ろう!

    first :: (a, b, c) -> a
    first (x, _, _) = x

    second :: (a, b, c) -> b
    second (_, y, _) = y

    third :: (a, b, c) -> c
    third (_, _, z) = z

ところで、List comprehensionで説明した`_`の意味を覚えているかな? `_`というのは、この変数には意味が無い、使われない、というのを表すのだった。そうそう、list comprehensionでもパターンマッチングが使える。試してみよう。

    ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
    ghci> [a+b | (a,b) <- xs]
    [4,7,6,8,11,4]

パターンマッチングが失敗すると、単に次のパターンへ移動する。

もちろん、パターンマッチングの中でリストを使うことだってできる。`[]`は、空のリストにマッチするし、`:`を使って、どんなリストにもマッチさせることができる。`[1,2,3]`というのは、`1:2:3:[]`のシンタックスシュガーだ、というのを覚えているかな? この記法を使うことで、たとえば`x:xs`とすれば、ある要素を先頭の要素と残りのリストに分解して、xとxsにbindすることができる。1つしか要素を持たないリストの場合は、たとえば`1:[]`のように分解され、それぞれ`x`には`1`が、`xs`には`[]`がbindされる。

Note: `x:xs`というのは多くの場面で、特に催奇で使用される。`x:xs`は要素数が1以上のリストに対してマッチする。もし、リストの最初の3津と残りを変数にbindしたければ、`x:y:z:zs`と書けばいい。これは要素数が3以上のリストにマッチする。

メモ: リストに対するパターンマッチングで`x:xs`ではなく`(x:xs)`と、かっこで包んでいますが、これはパターンマッチング用のシンタックスでありタプルではないです。

さて、リストに対するパターンマッチングの方法が分かったから、自分で`head`を定義してみよう。

    head' :: [a] -> a
    head' [] = error "Can't call head on an empty list, dummy!"
    head' (x:_) = x

動いているか確認。

    ghci> head' [4,5,6]
    4
    ghci> head' "Hello"
    'H'

期待通り!

次は、リストの先頭の要素が何か教えてくれる、ちょっとした関数を作ってみよう。

    tell :: (Show a) => [a] -> String
    tell [] = "The list is empty"
    tell (x:[]) = "The list has one element: " ++ show x
    tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
    tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

この関数は、空のリスト、singleton list、要素が2つのリスト、要素が2つより大きいリストに対してpattern matchingを行っているから安全だ。

NOTE `(x:[])`と`(x:y:[])`は、それぞれシンタックスシュガーを利用して、`[x]`と、`[x,y]`と書きなおすことができる。ただし、`(x:y:_)`を`[x,y,_]`とか空いてしまうと、これは要素が3つのリストのみにマッチするので注意。

    foo :: [a] -> String
    foo []      = "I'm an empty list."
    foo [x]     = "I'm a singleton list."
    foo [x,y]   = "I'm a list that contains 2 elems."
    foo [x,y,_] = "I'm a list that contains 3 elems."
    foo (x:xs) = "I'm a list that contains more than 3 elems."

これは期待通りに動作する。

    ghci> foo []
    "I'm an empty list."
    ghci> foo [1]
    "I'm a singleton list."
    ghci> foo [1,2]
    "I'm a list that contains 2 elems."
    ghci> foo [1,2,3]
    "I'm a list that contains 3 elems."
    ghci> foo [1,2,3,4]
    "I'm a list that contains more than 3 elems."

`length`関数は、既に自分で定義した。ここで、催奇とpattern matchingを使って、もう一度作ってみる。

    length' :: (Num b) => [a] -> b
    length' [] = 0
    length' (_:xs) = 1 + length' xs

これは`factorial`関数に似ている。まずは催奇が止まるように空のリストに対するパターンマッチングを書く。ちなみにこれをedge conditionという。次に、リストを先頭と残りの要素に分割して、、残りの要素に対してlength関数を再帰的に実行する。パターンマッチングが失敗しないよう、すべてのリストに対するパターンが用意されていることにも注目しよう。`[]`は空のリストに、`(_:xs)`は要素の数に関係なく、すべてのリストにマッチする。

関数lengthをcallすると何が起こるのか、追跡してみよう。

`[1,2,3]`を与えた場合、まずはリストを`[1]`と`[2,3]`に分割する。`[1]`の要素数は1だから、`1 + [2,3]の要素数`を返せば答えになる。では`[2,3]`の要素数は、というと、`[2]`と`[3]`に分割できるから、これも`1 + [2]の要素数`となる。残った`[3]`の要素数は、`[3]`と`[]`に分割できるから、`1 + 0`となる。結局、`[1,2,3]`の要素数は、`1 + (1 + (1 + 0))`を求めることになるから、3という正しい答えが得られる。

次は、関数`sum`を自分で定義してみよう。`sum []`は0になるから、まずはそのpatternを書いて、残りのpatternを加工。

    sum' :: (Num a) => [a] -> a
    sum' [] = 0
    sum' (x:xs) = x + sum' xs

パターンマッチングの中で使えるパターンのようなものもある。パターンにもとづいて分割したものに対し、それらすべてを含むもの、として別名をつけることができる。これはパターンの前に`@`を置くことで実現できる。例えば、`xs@(x:y:ys)`と書いた場合、`xs`は`(x:y:ys)`と等しい。以下に、簡単な例を示す。

    capital :: String -> String
    capital "" = "Empty string, whoops!"
    capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

試してみよう。

    ghci> capital "Dracula"
    "The first letter of Dracula is D"

通常、このパターンは、function bodyの中で分割されたパターンを分割される前の状態で取り出したい時に使う。これで、パターンを繰り返し書くのを防ぐことができる。

それと、`++`はパターンマッチングで使えないことに注意してほしい。仮に`x ++ y`があったとして、どこまでが、先頭で、どこまでが残りの部分になるんだろう? ということで、例えば、`(xs ++ [x,y,z])`や`(xs ++ [x])`はどうかというと、やはりリストの性質上、使うことはできない。



## Guards, guards!

パターンマッチングは、値がある構造にもとづいているか調べ、分解する方法であるのに対し、ガードは、ある値、あるいはいくつかの値のpropertyがTrueかFalseかをテストする方法を提供する。これは、if文にとても似ているように思える。ガードの場合、複数の条件をより読みやすく記述でき、パターンマッチングとの相性もいい。

メモ: 値は値だろうと思っていましたが、property of valueという言い方をしているので　Haskellでは、値の中身と値がどのように作られたか、(どのパターンにマッチするか)、という構造をふくめて値と読んでいるのでしょうか。

詳しい説明をする前に、とりあえずガードを使った簡単な関数を定義してみよう。

これから定義するのは、BMI(Body Math Index)にもとづいて、罵詈雑言を返してくれる素敵な関数だ。BMIは自分の体重を、身長(単位はメートル)の2乗で割ることで求めることができる。BMIが`BMI <= 18.5`のときは痩せすぎ、`BMI > 18.5 && BMI <= 25`のときは標準、`BMI > 25 && BMI <= 30`のときは太り気味、`BMI > 30`のときは肥満、となる。早速、BMIを計算して結果を教えてくれる関数を定義しよう。

    bmiTell :: (RealFloat a) => a -> String
    bmiTell bmi
        | bmi <= 18.5 = "You're underweight, you emo, you!"
        | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
        | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
        | otherwise   = "You're a whale, congratulations!"

ガードは関数の名前に続けて`|`で表し、その後にパラメータを置く。通常、ガードはインデントを行い、`|`の位置を揃える。(インデントは必ず` `が1つ以上ないと、エラーになる。)

ガードの条件は、それが`True`になれば、対応するbodyが使われる。`False`になれば、次のガードを調べる、というのを最後まで繰り返す。例えば、この関数に`24.3`を与えると、まず最初のガードで、18.5より小さい、あるいは等しいか判定される。これはFalseになるから、次のガードへ進む。次は25より小さいか等しい、という条件だから、これに対応する文字列が返される。

命令型の言語で同じことをしようとすると、if-elseを一々書く必要があるのに対し、ガードを使えば依り読みやすく記述できる。そして、個々の条件にあわせてif-elseを追加していくと、いずれ破綻するだろう。ガードはその代替手段として有効だ。

多くの場合、最後のガードはotherwiseとなる。`otherwise`は単に`otherwise = True`と定義されており、命令言語の`switch`文における`default`と同じ働きをしてくれる。

    ghci> otherwise
    True

ガードはパターンマッチングに似ている。パターンマッチングの場合は、パターンを満たしているか判定するのに対し、ガードは`True`か`False`を判定する。

もし、`otherwise`が用意されていなければ、すべてのガードが`False`になった場合、次のパターンへ進むことになる。これがガードとパターンマッチングの相性がいい理由だ。ただし、最終的にどのガードにも当てはまらなかった場合、エラーとなるので注意してほしい。

もちろん、複数のパラメータを受け取る関数でもguardを使うことができる。先ほどのBMI関数を、事前にBMIを計算してもらう代わりに、身長と体重を受け取って計算するように変更してみよう。

    bmiTell :: (RealFloat a) => a -> a -> String
    bmiTell weight height
        | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
        | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
        | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
        | otherwise                 = "You're a whale, congratulations!"

さあ、自分のBMIを調べてみよう。

    ghci> bmiTell 85 1.90
    "You're supposedly normal. Pffft, I bet you're ugly!"

わーい、(Haskellに嫌味を言われたけど...)、太ってなかった。

Note: ガードを使うときは、パラメータの後に`=`は書かない。Haskellに慣れていないと、よく間違えてシンタックスエラーになるので注意しよう。

もう一つの例として、`max`関数を自分で実装してみよう。関数`max`は、2つのvalueを受け取り、大きい方を返すのだった。

    max' :: (Ord a) => a -> a -> a
    max' a b
        | a > b     = a
        | otherwise = b

ガードはもちろん1行にまとめて書くこともできる。これまで開業していたのは読みやすさを優先していたからだ。小さな関数であれば、1行にまとめて書いても問題ない。関数`max`のガードを1行で書くと、こうなる。

    max' :: (Ord a) => a -> a -> a
    max' a b | a > b = a | otherwise = b

うーん、あんまり読みやすくない...。ということで、1行にまとめて書くのはおすすめしない。

続いて、`compare`関数を自分で定義してみよう。


    myCompare :: (Ord a) => a -> a -> Ordering
    a `myCompare` b
        | a > b     = GT
        | a == b    = EQ
        | otherwise = LT

では、試してみよう。

    ghci> 3 `myCompare` 2
    GT

Note: 関数を定義するとき、infix functionのほうが読みやすい場合は、`\``を使って、定義することもできる。たとえば、次のように関数を定義できる。

    ghci> let x `plus` y = x + y
    ghci> 1 `plus` 2
    3



## Where!?

先程、以下の様なBMIを計算する関数を定義した。

    bmiTell :: (RealFloat a) => a -> a -> String
    bmiTell weight height
        | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
        | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
        | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
        | otherwise                   = "You're a whale, congratulations!"

この関数で同じことを3回も繰り返しているのに気づいたかな? 3回も! 3回も!

プログラミングでは、繰り返しを避けるのが望ましい。この場合は、同じexpressionを何度も書くのではなく、BMIの計算結果を、適当な変数にbindして、使い回すべきだ。では、この関数を以下のように変更してみよう。

    bmiTell :: (RealFloat a) => a -> a -> String
    bmiTell weight height
        | bmi <= 18.5 = "You're underweight, you emo, you!"
        | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
        | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
        | otherwise   = "You're a whale, congratulations!"
        where bmi = weight / height ^ 2

通常、`where`はガードの後に書く。`where`のインデント位置は`|`と同じか、それ以上深くインデントするのがベターだ。そして、`where`に続けて変数や関数を定義する。

このとき、`where`で定義した変数や関数は、ガードの中で参照することができる。もし、BMIの計算式を少し変更したくなったら、`where`で定義した部分だけを変更すればいい。そして、これはコードの読みやすさを向上させるだけではなく、一度計算した結果を使いまわすことで、実効速度も向上する。では、ちょっと調子に乗って、次のように関数を変更してみよう。

    bmiTell :: (RealFloat a) => a -> a -> String
    bmiTell weight height
        | bmi <= skinny = "You're underweight, you emo, you!"
        | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
        | bmi <= fat    = "You're fat! Lose some weight, fatty!"
        | otherwise     = "You're a whale, congratulations!"
        where bmi = weight / height ^ 2
              skinny = 18.5
              normal = 25.0
              fat = 30.0

関数内のwhereで定義した変数は、その関数内でしか参照できないので、他の関数のnamespaceを汚染してしまう心配はない。変数名や関数名は、1行に1つしか定義できないので注意してほしい。そうでなければ、同じブロック内に定義されたものと解釈されなくなる。

whereは異なるpatternに対して、関数の定義を跨いで使うこともできる。もし、幾つかのパターンから、ある関数を共通に使いたい場合、グローバルに定義する必要がある。

`where`は、もちろんパターンマッチングにも使うことができる。先ほどの関数は、以下のようにも定義できる。

    ...
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

ここで、First NameとLast Nameを与えると、イニシャルを変えいてくれる関数を定義してみよう。

    initials :: String -> String -> String
    initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
        where (f:_) = firstname
              (l:_) = lastname

関数のパラメータに対して直接パターンマッチングができるので、完結でわかりやすくなる。

`where`ブロック内で定数を定義したように、関数を定義することもできる。健康を維持したままプログラミングができるよう、体重と身長を含むタプルを要素に持つリストを受け取って、BMIのリストを返す関数を定義してみよう。

    calcBmis :: (RealFloat a) => [(a, a)] -> [a]
    calcBmis xs = [bmi w h | (w, h) <- xs]
        where bmi weight height = weight / height ^ 2

この例でbmi関数を先に紹介したのは、関数のパラメータからbmiを直接計算することはできないからだ。BMIの計算をしてくれるヘルパー関数を先に定義して、それから全てのペアに対してそのヘルパー関数を使う必要があった。

そして、`where`はネストすることもできる。これは一般的なイディオムで、ちょっとしたヘルパー関数を定義したい時に`where`句を使う。もちろん、そのヘルパー関数内でも`where`句を使ってヘルパー関数のヘルパー関数を定義することもできる。

メモ: 上記のBMIを計算する関数、`bmi weight height = weight / height ^ 2`のように、ちょっとした計算をする関数のことを、一般的にヘルパー関数と呼んだりします。



## Let it be

`where`とよく似ているのが`let`だ。

`where`は関数の末尾で変数を定義して、ガードを含め、すべての関数から使えるようにする。それに対し`let`は、`let`自身がexpressionであり、他の関数から変数を参照することはできないので局所的な変数の定義に使うことができる。Haskellでは値に名前をつける時に`let`をよく使う。また、パターンマッチングにも使うことができる。

では、`let`を使ってみよう。以下は半径rと高さhを与えて、円筒の表面積を求める関数だ。

    cylinder :: (RealFloat a) => a -> a -> a
    cylinder r h =
        let sideArea = 2 * pi * r * h
            topArea = pi * r ^2
        in  sideArea + 2 * topArea

`let`がexpressionであることに注目してほしい。`let`で定義した変数や関数は、`in`というキーワードの後に使うことができる。今までに見たとおり、これは`where`を使っても同じことが実現できる。

Note let`で定義する変数名は1列に整列しよう。

では、`let`と`where`の違いはなんだろう? `let`は書いたその場で変数を定義して使うことができるのに対して、`where`は関数の後ろの部分で変数を定義して使う、という違いしかないように思える。

両者の違いは、`let`自身がexpressionである、というところだ。`where`はあくまでシンタックスだ。`let`と同様に`if`もexpressionであり、あらゆる場所で使うことができる、というのを説明したことを覚えているかな?

    ghci> [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]
    ["Woo", "Bar"]
    ghci> 4 * (if 10 > 5 then 10 else 0) + 2
    42

`let`も同様にexpressionとして使うことができる。

    ghci> 4 * (let a = 9 in a + 1) + 2
    42

`let`を使って、関数にローカルスコープを作り出すこともできる。

    ghci> [let square x = x * x in (square 5, square 3, square 2)]
    [(25,9,4)]

幾つかの変数を定義したい場合、複数行に分けて書くと読みやすい。しかし、1行で書きたい場合もある。そのときはセミコロン`;`で区切って変数や関数を定義する。

    ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
    (6000000,"Hey there!")

最後の変数には`;`を付ける必要はない。しかし、書くこともできる。先程述べたように、`let`をパターンマッチングと組み合わせて使うこともできる。`let`は、タプルを分解して各要素ごとに別名を付けて取り出す、という便利な使い方ができる。

    ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100
    600

もちろん、`let`をlist comprehensionと組み合わせて使うこともできる。体重と身長を持つペアを含むリストを受け取って、BMIを計算した結果を返す関数を、`where`ではなく`let`をlist comprehensionの中で使って書きなおしてみよう。

    calcBmis :: (RealFloat a) => [(a, a)] -> [a]
    calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

`let`をlist comprehensionの中だけではなく、関数のpredicateの中でも使うことができる。`let`をlist comprehensionの中で使ったときは、その変数はoutput function(`|`の後ろの部分)からも参照することができる。ということで、太った人のみBMIの値を返す、という関数を作ることができる。

    calcBmis :: (RealFloat a) => [(a, a)] -> [a]
    calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

`let`より先に定義されるため、`bmi`は`(w, h) <- xs`という部分では参照できない。

また、list comprehensionでは`in`というのを省略していることに注目してほしい。`let`で定義した変数や関数を使えるのは`in`の後だった。関数`calcBmis `では、`bmi`という変数を事前に定義しているので`in`は必要ない。しかし、`let`を関数のpredicateの中で定義して、predicateの中だけで有効な、predicateの外から参照できない変数を定義することもできる。

関数や定数を定義するとき、`in`と、その後に続く部分は、ghciを使っている時にも省略できる。`let`で定義したものは、ghciを終了するまで使うことができる。

    ghci> let zoot x y z = x * y + z
    ghci> zoot 3 9 2
    29
    ghci> let boot x y z = x * y + z in boot 3 4 2
    14
    ghci> boot
    <interactive>:1:0: Not in scope: `boot'

`let`が便利なのは分かった。でも、それなら`where`は必要ないんじゃない? しかし、`let`はexpressionであり、ガードをまたがってグローバルに変数を定義できないことを思い出してほしい。そして、関数を定義するときに`where`を使ったほうが、その関数で使う変数やヘルパー関数が一覧できてわかりやすい、という人もいる。その場合、関数のtype declarationとfunction bodyが近くにあるほうがより読みやすくなる。



## Case expressions

CやC++、Javaなどの命令形言語でのプログラミング経験がある方は、caseがどんなものかご存知だろう。caseは変数を受け取り、特定の値に対するコードのブロックを実行し、caseに当てはまらなかった場合に実行されるブロックを含むこともある。

Haskellの`case`は、命令型のcaseをちょっとよくしたものだ。名前が暗示している通り、Haskellの`case`は、`let`や`if`と同様、expressionだ。変数の値に基づいて、特定のコードを実行する、というのは`case`だけではなく、パターンマッチングでも同じことをしている。

...おや、変数を受け取って、パターンマッチングして、その変数に基づいてコードの断片を実行する、というのは聞いたことがあるぞ? 関数を定義するときの、パラメータに対するパターンマッチングだ。

そう、実は関数を定義した時に使うパターンマッチは、`case`のシンタックスシュガーだったのだ。ということで、以下のコードは等しいので、両者を交換することができる。

    head' :: [a] -> a
    head' [] = error "No head for empty lists!"
    head' (x:_) = x

    head' :: [a] -> a
    head' xs = case xs of [] -> error "No head for empty lists!"
                          (x:_) -> x

ご覧のとおり、`case`のシンタックスは単純だ。

    case expression of pattern -> result
                       pattern -> result
                       pattern -> result

expressionはパターンに対してマッチする。パターンマッチの挙動は、最初にマッチしたものを使う、という予想通りの挙動をする。すべてのパターンを試した結果、どれにもマッチしなければ、ランタイムエラーとなる。

関数のパターンマッチは、関数の定義でしか使えないのに対して、`case`のパターンマッチは、あらゆる場所で使うことができる。例えば、以下の様なことができる。

    describeList :: [a] -> String
    describeList xs = "The list is " ++ case xs of [] -> "empty."
                                                   [x] -> "a singleton list."
                                                   xs -> "a longer list."



`case`はexpressionの途中で、パターンマッチをすることができるので便利だ。関数定義でのパターンマッチは`case`のシンタックスシュガーだったから、以下のように関数を定義することもできる。

    describeList :: [a] -> String
    describeList xs = "The list is " ++ what xs
        where what [] = "empty."
              what [x] = "a singleton list."
              what xs = "a longer list."



# Recursion

再起については、前の章で少し触れた。この商では催奇について詳しく見て、Haskellでは何故催奇が重要なのか、そして、催奇の考え方を用いてエレガントかつ明快に問題を解決する方法を学ぶ。

もし、まだ催奇というものが何なのか知らなければ、次の一文を読んで欲しい。はぁ? からかってるの? 催奇は関数定義の中で、定義しようとしている関数を呼び出す方法のことだ。

数学の世界では、定義はしばしば再帰的に行われる。たとえば、フィボナッチ数列は再帰的に定義される。まず、フィボナッチ数列の最初の2つを非再帰的に定義する。$F(0) = 0$と$F(1) = 1$は、それぞれフィボナッチ数列の0と1番目の値が0と1であることを定義している。次に、他の自然数について、求めたいフィボナッチ数は、1つ前と2つ前のフィボナッチ数を足すことで得られる。つまり$F(n) = F(n - 1) + F(n - 2)$となる。たとえば、F(3) = F(2) + F(1) = (F(1) + F(0)) + F(1)となる。ここまでは非再帰的にフィボナッチ数を定義したから、安全に$(3)が2になることを確認できた。

再帰的な関数の定義の中で、再起していない要素、たとえばフィボナッチ数のF(0)やF(1)はedge conditionとよばれ、再帰関数を停止させるために重要となる。もし、edge conditionを定義していなければ、フィボナッチ数の場合はF(-1)、F(-2)...を計算し続けることになり、関数が永遠に停止できなくなり、答えが得られない。

命令型言事は異なり、Haskellにおいて催奇は重要だ。Haskellでは、なにかを求める手順ではなく、それが何なのかを宣言する。Haskellにはforやwhileループがないのは、その代わりとして催奇を使うからだ。



## Maximum awesome

`maximum`関数は、リストの中身がOrdering、type classがOrdに属していれば、その最大値を取り出す。これを命令的に定義する方法を考えてみよう。

おそらく、最大値を保持するための変数を用意して、リストの要素をwhileループを使って1つ1つ調べ、保持している最大値と取り出した要素を比較して、取り出した要素のほうが大きければそれを保持して、最終的にリスト内の最大値を得ることになると思う。

こんな簡単なアルゴリズムなのに、説明するのが大変だ。では、`maximum`関数を再帰的に定義する方法を見てみよう。

最初に、singleton listの場合は最大値がその要素と等しいから、その要素を返す、というedge conditionを定義する。次に、要素が2つ以上あるリストの場合は、リストの先頭の要素と、残ったリストを分割して、先頭のほうが大きければ、それを返す。残ったリストのほうが大きければ、それに対して`maximum`を求めた結果を返す。

以上! 早速、実装してみよう。

    maximum' :: (Ord a) => [a] -> a
    maximum' [] = error "maximum of empty list"
    maximum' [x] = x
    maximum' (x:xs)
        | x > maxTail = x
        | otherwise = maxTail
        where maxTail = maximum' xs

ご覧のとおり、パターンマッチは催奇でその効果を発揮する。殆どの命令言語はパターンマッチの機能を備えていないので、大量のif-elseを用いてedge conditionをテストすることになる。

ここでは、催奇とパターンマッチの組み合わせで問題を解決している。最初のedge conditionは空のリストに対するパターンだ。空のリストに対して最大値は求められないから、これはエラーとする。2番目のパターンもedge conditionだ。singleton listであれば、要素は一つしかないから最大値はその要素となる。

最後のパターンは、3番目のパターン、再起している部分だ。まず、パターンマッチでリストを先頭と残りの部分に分割する。残りのリストに対して最大値を求めるmaxtail関数を定義するのにwhereを使用しているが、これは、リストに対する催奇として一般的なイディオムだから、なれておこう。

そして、リストの先頭が残りの部分より大きいか調べる。もし大きければ、先頭を返す。そうでなければ、リストの残りの部分に対する`maximum`の結果を返す。

例として`[2,5,1]`をこの関数に与えた時、どのように動作するのか調べてみよう。まず、最初の2つのパターンにはマッチしない。3番目のパターンにはマッチするので、リストは`2`と、`[5,1]`に分割される。whereで定義されたmaxtailは、`[5,1]`の最大値を求めるために、maximumを求めることになる。再び3番目のパターンで、`[5,1]`は`5`と`[1]`に分割される。そして、再びwhereでは`[1]`の最大値を求めることになる。`[1]`はedge conditionだから、`1`が返される。最終的に、5と1を比較することになり、ここでは5を返すことになる。つまり、`[5,1]`のmaximumは5であることがわかる。そして、ステップを進めると、2と`[5,1]`のmaximumを求めることになる。`[5,1]`のmaximumは既に5であることが分かっているので、これは2と5を比較することになるから、5を選ぶことになる。


関数`max`を使うことで、この関数をより明確に定義できる。`max`は2つの引数を取り、大きい方を返す。`maximum`を`max`を使って書くと、以下のようになる。

    maximum' :: (Ord a) => [a] -> a
    maximum' [] = error "maximum of empty list"
    maximum' [x] = x
    maximum' (x:xs) = max x (maximum' xs)

なんてエレガントなんだ。ここで重要なのは、リストのmaximumは、リストの先頭とmaxtailに対するmaxの結果になる、というところだ。



## A few more recursive functions

さて、催奇の考え方がわかってきたから、他にも催奇を用いた関数を幾つか定義してみよう。最初に、replicateを定義してみよう。replicateはIntで繰り返しの回数と要素を受け取り、その要素を与えられた回数だけ繰り返したリストを返す。たとえば、`replicate 3 5`は`[5,5,5]`となる。

まずは、edge conditionから考えよう。ぱっと思いつくedge conditionは0か、それより小さい値だ。与えた要素がなんであれ、0回繰り返すというのは、結果として空のリスト`[]`になる。もちろん、負の数に対しても、空のリストを返すべきだろう。

    replicate' :: (Num i, Ord i) => i -> a -> [a]
    replicate' n x
        | n <= 0    = []
        | otherwise = x:replicate' (n-1) x

ここでは、条件を真偽値で判定したいので、パターンマッチングを使う代わりにガードを使った。nが0より小さいか等しければ、空のリストを返す。nが0より大きければ、リストの要素と、要素を(n - 1)回繰り返したリストを連結したリストを返す。結局、n-1という部分はedge conditionに到達することになる。

Note: `Num`というtypeclassは、`Ord`というtypeclassのサブクラスではない。つまり、数値を構成するものの中にはOrderingは含まれていない、ということを意味する。比較を行うためには、NumとOrdの両方を記述する必要があるのはそのためだ。

次は`take`を実装してみよう。takeは、リストから指定した数だけ要素を取り出して、リストとして返す。たとえば、`take 3 [5,4,3,2,1]`を実行すると、`[5,4,3]`が返される。もし、リストの要素数より小さいか、0を指定すると、空のリストが返される。空のリストに対しては、取り出す要素の数に関係なく、空のリストが返される。edge conditionは異常。早速実装してみよう。


    take' :: (Num i, Ord i) => i -> [a] -> [a]
    take' n _
        | n <= 0   = []
    take' _ []     = []
    take' n (x:xs) = x : take' (n-1) xs

最初のパターンは、リストから取り出す値として、0または0より小さい値を指定した時に空のリストを返す、というedge conditionだ。このとき、リストが何であるかは気にしないので、変数名に`_`を使用している。そして、otherwiseなしにガードを使っていることにも注目しよう。このガードの条件がFalseになったときは、次のパターンマッチに進むことを意味する。2番目のパターンは、空のリストに対してマッチするので、取り出す要素の数に関係なく、空のリストが返される。3番目のパターンは、リストを先頭と残りの部分に分割している。そして、先頭の要素と、残りの要素から、指定した数、(n - 1)だけ要素を取り出したリストを連結したリストを返すことになる。

次は、`reverse`はリストを反転させる。edge conditionを考えてみよう。ええと、どうなるだろう。まず、空のリストを反転した結果は、空のリストになる。OK。では、残りはどうなる? リストを先頭と残りの部分に分割して、残りの部分を反転したものと、先頭を連結することになると思う。

    reverse' :: [a] -> [a]
    reverse' [] = []
    reverse' (x:xs) = reverse' xs ++ [x]

Haskellでは無限リストがサポートされているから、edge conditionは必ずしも必要ではない。しかし、無限リストのような無限という構造を持つデータを扱うにはどうすればいいのだろう。無限リストを扱う方法としては、必要な部分だけを切り取る、というのが考えられる。

関数`repeat`は、受け取った値を無限に繰り返して、無限リストを返す。これを再起で定義する方法は簡単だ。見てみよう。

    repeat' :: a -> [a]
    repeat' x = x:repeat' x

`repeat 3`をcallすると、ghciの画面が`3`でうめつくされるだろう。`repeat 3`は`repeat 3`をevaluateすることになり、その`repeat 3`というのは`repeat 3`をevaluateすることになるから、これは止まらなくなる・このままでは止まらなくなるので、`take 3 (repeat 3)`のように要素をいくつ取り出すか指定する必要がある。これは、`replicate`関数で部分的に行っていることだ。

`zip`関数は、リストを2つ受け取り、それらをまとめる。zipに渡されるリストの要素っすうが一致しない場合は、短い方に合わせる。zipに空のリストを渡すと、もう一方のリストに関係なく空のリストが返される。これらがedge conditionになる。しかし、zipは2つのリストを受け取るから、実際には2つのedge conditionが必要になる。

    zip' :: [a] -> [b] -> [(a,b)]
    zip' _ [] = []
    zip' [] _ = []
    zip' (x:xs) (y:ys) = (x,y):zip' xs ys

最初の2つのパターンは、zipに渡されたリストのどちたかが空であれば、空のリストを返すことを示している。3番目のパターンは、2つのリストの先頭をタプルにまとめて、それを残りのリストにたいしてzipした結果と結合したリストお返すことを示している。`zip [1,2,3] ['a', 'b']`の結果は、最終的には`[3]`と`[]`をzipすることになる。`zip [3] []`の結果は`[]`だから、結果として`(1, 'a'):(2, 'b'):[]`することになり、期待通りの結果`[(1,'a'),(2,'b')]`が得られる。

もうひとつ、標準的なライブラリ関数である`elem`を実装してみよう。この関数は、要素とリストを渡すと、リストの中にその要素が存在するか判定する。リストに対するedge conditionは、これまで何度も見てきたように、空のリストに対するものだ。空のリストには何も要素がないから、探したい要素は含まれない。

    elem' :: (Eq a) => a -> [a] -> Bool
    elem' a [] = False
    elem' a (x:xs)
        | a == x    = True
        | otherwise = a `elem'` xs

予想通りの結果が得られるし、定義はシンプルだ。リストの先頭が目的の要素か調べ、違っていれば残りのリストにたいして`elem'`を求める。もし、目的の要素が存在しなければ、最終的には空のリストにたどり着いて、Falseが返される。



## Quick, sort!

ソート可能な要素を持つリストがある。要素のtypeclassはOrdだ。では、クイックソートを使って、要素をソートしてみよう。これは、要素をソートするための賢い方法だ。これを実装するには、命令言語では10行以上必要になるところを、Haskellでは10行より少なく、エレガントに、そしてシンプルに実装できる。クイックソートはHaskellの縮図とも言える。

では、早速実装してみよう。Haskellのエレガントさを説明するのにクイックソートはうってつけだ。　

まず、type signatureは、`(Ord a) => a -> [a] -> [a]`となる。特に驚くことはないね。edge conditionは、ご想像のとおりだ。空のリストをソートしても空のリストになる。では、本題のアルゴリズムについて。まず、ソート済みのリストはリストの先頭要素と、残りの要素で構成される。残りのリストはソート済み、かつ先頭の要素より小さいか等しい値で構成される。次に、リストの中から、先頭の要素を取り出し、ソート済みリストのすべての要素の内、先頭の要素より大きいものを取り出す。この定義では、ソートを2回行っているのに気づくと思う。つまり、催奇を2回callすることになる。そして、アルゴリズムを定義するのにあれをして、これをして、...という手順を示す代わりに動詞を使っているのにも気づいただろう。これがfunctional programmingの美しさだ。リストから先頭の要素より小さい値を取り覗いてのこったリストからもっとも大きい値を取り出すにはどうすればいい?そう、list comprehensionを使えばいい。では、関数を実際に定義してみよう。

    quicksort :: (Ord a) => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) =
        let smallerSorted = quicksort [a | a <- xs, a <= x]
            biggerSorted = quicksort [a | a <- xs, a > x]
        in  smallerSorted ++ [x] ++ biggerSorted

適当なリストを与えて、これが正しく動作するかテストしてみよう。

    ghci> quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
    [1,2,2,3,3,4,4,5,6,7,8,9,10]
    ghci> quicksort "the quick brown fox jumps over the lazy dog"
    "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"

やったね、うまく動いた! これが長々と説明したクイックソートの結果だ。

では、`[5,1,9,4,6,7,3]`をソートする過程を追跡しよう。このアルゴリズムは、まずリストから先頭の要素`5`を取り出し、残りのリストにたいして、`5`より小さいか等しい値で構成されたリストと、`5`より大きい値で構成されたリストを求める。この段階では、`[1,4,3] ++ [5] ++ [9,6,7]`となる。そして、小さい方のリスト、`[1,4,3]`がソートされても、`5`の位置はッ変化せず、先頭から4番目にとどまることに注目してほしい。小さい方のリストには`5`より小さいか等しい値しか存在しないので、`5`はリストの4番目に位置し続けることになる。次に、小さい方のリストと大きい方のリストをソートする。どちらのソートにも同じクイックソート関数を使っていることに注目してほしい。最終的に、空のリストにたどり着くが、空のリストはソート済み、と見なされるので、ソートはうまく機能する。図で説明しよう。

(分かりやすい図が掲載されているので、オリジナルのページを見てください。)

ソート済みの要素は、一度その場所に留まると動かない。左から右へ眺めると、ソート済みのリストになる。type declarationでtypeclassがOrdの要素を受け取るようにしたので、比較可能な要素で構成されたリストであれば、どんなリストに対してもソートできる。クイックソートでは、比較対象となる戦闘の要素のことをピボットと呼ぶ。図の中の、緑色で示した部分だ。なぜリストの先頭要素を選んだのかというと、パターンマッチを使って簡単に取り出せる体。図の中では、ピボットより小さいものは淡い緑色で、ピボットより大きい物は深い緑色で示している。黄色で示した部分は、クイックソートを適用していることを示している。



## Thinking recursively

再起について少し学んだので、ここで再帰的に考えるためのパターンを紹介しよう。まず、edge caseを定義して、それからある要素に対する操作を定義する。そして、残ったリストに対して関数を適用する。催奇の対象はツリーやリストに限らないので、どんなデータ構造でも問題ない。

たとえば、`sum`はリストの先頭要素と、残りのリストに対する`sum`の結果を足したものを返す。 `product`はリストの先頭要素と残りのリストのproductの結果を返す。 `length`は1と残りのリストに対する`length`の結果を足したものを返す。

もちろん、これらの関数にはedge caseが存在する。通常、edge caseは再帰的に関数を適用できない場合のために用意する。リストを扱う場合、空リストは頻繁にedge caseとなる。木構造を扱う場合であれば、子要素を持たない、というのがedge caseになるだろう。

数値を再帰的に扱う場合もこれに似ている。通常、ある数値を操作して、変更された数値に対して関数を再び適用する。以前、階乗を求める`factorial`を定義したが、あのかんすうでは、　受け取った数値`n`にたいして、その数と`factorial (n - 1)`の結果を求めていた。この再帰関数は性の数に対する会場を定義しているので、0に対して催奇を求める意味は無い。多くの場合、edge caseではそれがedge caseであることを示す値、identityを返す。

ある数値に対して1をかけるとその数値になるので、乗算のidentityは1となる。リストから要素を取り出して合計を求めるときは、空のリストがedge caseとなるが、その値は加算のidentityである、0に対応する。クイックソートではedge caseは空のリストとなるし、identityも空のリストとなる。空のリストをソートすると、空のリストになるからだ。

つまり、問題を再帰的に解決するには催奇を適用できない場合を洗い出し、それをedge caseとしてていぎして、催奇を停止させるidentityが何になるかを考える。例えば、リストの場合はパターンマッチにより先頭の要素と末尾の要素に分割して、それらを再帰的に利用する。



# Higher order functions

Haskellの関数は関数をパラメータとして受け取り、値として返すことができる。このような関数をHigher order functionという。高階関数はHaskell固有の機能ではないが、Haskellで使うことができる。

高階関数は、関数の持つ状態を変化させつつループさせたい場合に使うものであり計算の手順ではなく、その関数自体を返す。Haskellにとって高階関数は不可欠だ。これは、なにか問題を解決するときに、プログラムを考える強力な手段となる。



## Curried functions

正確に述べると、Haskellでは、すべての関数が一度に1つのパラメータしか受け取ることができない。では、これまで2つ以上のパラメータを受け取る関数をいくつか定義してきたが、あれはどうして可能なんだろう? 実は賢いトリックが隠されている。複数のパラメータを受け取ることができる関数はすべてcurried function、カリー化された関数だったのだ。...これがどういう意味７日、実際の例から理解するのが一番だろう。

さて、関数`max`を覚えているかな? この関数は、2つのパラメータを受け取り、大きい方を返しているように見える。

`max 4 5`を実行すると、まず、「4か、与えられた引数のどちらか大きい方を返す関数」を返す。次に、その関数に`5`を適用すると、4と5のどちらが大きいか判定して、望みの結果が得られる。一言で軽く説明してしまったが、これは興味深い概念だ。

つまり、以下の2つの関数呼び出しは等しい。

    ghci> max 4 5
    5
    ghci> (max 4) 5
    5


2つのものの間にスペースを挟むと、それはkfunction application、関数適用になるのだった。スペースは演算子であり、最も優先度が高い。`max`関数のtypeを調べてみよう。

    ghci> :t max
    max :: Ord a => a -> a -> a

これは`(Ord a) => a -> (a -> a)`と書くこともできる。`max`はaというtypeのパラメータを受け取ってまず、aというtypeのパラメータを受け取る関数を返し、その関数はaというtypeの値を返す。

これが、返されるtypeと関数のパラメータが、それぞれ区別なく、単に矢印`->`で句切られていた理由だ。それで、これの何が便利なんだろう? 単純にエヴァ、関数にパラメータの一部だけを与えると、部分的にパラメータが適用された関数が得られる。つまり、まだ与えていないパラメータの数だけ、パラメータを受け取れる関数が得られる。

部分適用(部分的にパラメータを与えて関数を呼び出すこと)は、手っ取り早くかつ、お手軽に関数を作る方法だ。そして、部分的にパラメータが適用された関数は、他の関数に渡すこともできるし、データとして扱うこともできる。

ここで、超単純な関数を見てみよう。

    multThree :: (Num a) => a -> a -> a -> a
    multThree x y z = x * y * z

では、`multThree 3 5 9`と` ((multThree 3) 5) 9`の違いは何だろう?

まず、スペースで区切られているので、3は`multThree 3`が適用される。すると、パラメータを1つ受け取り、関数を返す関数が作られる。続いて、5をその関数に適用する。すると、パラメータを1つ受け取り、15とそのパラメータを乗算した結果を返す関数が作られる。最後に、9をパラメータとしてその関数に適用すると、結果として、135が返される。

`multThree`は、`multThree :: (Num a) => a -> (a -> (a -> a))`と書けることを思い出してほしい。矢印`->`の前の部分は関数が受け取るパラメータを、後ろの部分は、関数が何を返すかを表しているのだった。

つまり、いま定義した関数は、typeがaのパラメータを受け取り、`(Num a) => a -> (a -> a)`を返す、という読み取り方ができる。同様に、この関数はtypeがaのパラメータを受け取り、`(Num a) => a -> a`を返すという読み取り方もできる。そしてこの関数は、最終的にはaというtypeのパラメータを受け取り、aというtypeの値を返す。

確認してみよう。

    ghci> let multTwoWithNine = multThree 9
    ghci> multTwoWithNine 2 3
    54
    ghci> let multWithEighteen = multTwoWithNine 2
    ghci> multWithEighteen 10
    180

部分的にパラメータを与えて関数を呼ぶと、それはつまり関数を作ることになる。例えば、数値を受け取り、`100`と比較する関数は、次のように定義できる。

    compareWithHundred :: (Num a, Ord a) => a -> Ordering
    compareWithHundred x = compare 100 x

この関数に99を与えれば結果は`GT`となるのは理解できると思う。等式の両辺の右側にある`x`に注目してほしい。`compare 100`が何を返すのか考えてみよう。この関数は、100とパラメータを比較して結果を返す。おや? これは`compareWithHundred`と同じことじゃないか。つまり、次のように定義できる。

    compareWithHundred :: (Num a, Ord a) => a -> Ordering
    compareWithHundred = compare 100

`compare 100`は関数を返すことに変わりはないから、`compareWithHundred`のtype declarationは全く変化していない。`compare`のtype declarationは、以下のとおりだ。`compare 100`の`100`は、type classが`Num`であることにも注目してほしい。

    ghci> :t compare
    compare :: Ord a => a -> a -> Ordering

カリー化と部分適用は重要な概念だから、しっかり理解してほしい。

さて、infix functionは、sectionを使って部分的に適用することができる。infix functionをsectionするには、関数をかっこで包み、その隣にパラメータを置く。これで、パラメータをひとつ受け取る関数が作られるので、足りていないパラメータを与えると、結果が得られる。ちょっとした関数を定義して試してみよう。

    divideByTen :: (Floating a) => a -> a
    divideByTen = (/10)

`divideByTen 200`をcallした結果は、`200 / 10`と等しい。次は、与えた文字が小文字化判定する関数を作ってみよう。

    isUpperAlphanum :: Char -> Bool
    isUpperAlphanum = (`elem` ['A'..'Z'])

sectionについて、唯一特殊な点は`-`を使う時だ。sectionの定義通りに解釈すると、`(-4)`は、数値を受け取り、4を引く関数になってしまう。しかし、利便性のために`(-4)`は、マイナス4と解釈される。ということで、ある数値から4を引く、という関数を定義したい場合は、`subtract`関数を使って`subtract 4`と定義する必要がある。

さて、ghciで`multThree`関数を`let`で変数にbindしたり、他の関数にパラメータとして渡すことなく、直接パラメータが足りない状態で呼び出すとどうなるか試してみよう。

    ghci> multThree 3 4
    <interactive>:1:0:
        No instance for (Show (t -> t))
          arising from a use of `print' at <interactive>:1:0-12
        Possible fix: add an instance declaration for (Show (t -> t))
        In the expression: print it
        In a 'do' expression: print it

ghciのメッセージは、与えられたexpressionを実行した結果、typeが`a -> a`となる関数が返されたものの、これをどのように表示すればいいかわからない。ということを表している。関数はShowというtypeclassに属していないから、文字列でどう表現すべきかわからなかったのだ。

ghciでは、たとえば`1 + 1`を計算した時、数値`2`という答えが得られた後にその文字列表現である`show 2`を求める。そして、数値`2`の文字列表現は`'2'`になるから、画面上に表示できたわけだ。



## Some higher-orderism is in order

関数は関数をパラメータとして受け取り、関数を返すことができる。これを説明するために、関数を受け取り、その関数にパラメータを2回適用する、という関数を定義してみよう。

    applyTwice :: (a -> a) -> a -> a
    applyTwice f x = f (f x)

まず、type decralationについて注目してみよう。以前、パラメータを複数受け取る関数のtype decralationでは、右から順に解釈されるので、パラメータをカッコで包む必要はない、と説明した。しかし、ここでは必須となる。まず、最初のパラメータは、それが、パラメータと同じtypeのパラメータを受け取る関数であることを示している。2番目のパラメータは、同じtypeの値を返すことを示している。

このtype decralationでは、カリー化を使う選択肢もあるが、安全を重視して、この関数は2つのパラメタを受け取り、値を返す、というように定義している。最初のパラメータは、typeが`(a -> a)`という関数であり、2番目のパラメータは、同じtypeとなる。パラメータとなる関数にはもちろん`Int -> Int`や`String -> String`など、どんなtypeを指定しても問題はない。ただし、2番目のパラメータは、最初のパラメータと同じtypeにする必要がある。

Note: 此処から先は、何か値をかえすまで、実際には部分的にパラメータが与えられた状態の関数を返すことになるが、「複数のパラメータを受け取る関数だ」ということにしよう。

既に`a -> a -> a`がカリーかされた関数であり、実際には1つのパラメータを受け取る関数で構成されているのを学んだが、説明を単純にするために、は2つのパラメータを受け取る関数だ、とみなすことに仕様。

関数のbodyは実にシンプルだ。パラメータでは`f`を関数として受け取り、パラメータ`x`にスペースを挟んで関数`f`を適用したものに、またその関数`f`を適用する。実際にこの関数を試してみよう。

    ghci> applyTwice (+3) 10
    16
    ghci> applyTwice (++ " HAHA") "HEY"
    "HEY HAHA HAHA"
    ghci> applyTwice ("HAHA " ++) "HEY"
    "HAHA HAHA HEY"
    ghci> applyTwice (multThree 2 2) 9
    144
    ghci> applyTwice (3:) [1]
    [3,3,1]

関数の部分適用の素晴らしいところは明らかだ。もし、パラメータを一つだけ受け取る関数、を受け取るようにしたい場合、部分適用した状態の関数を、必要になった箇所でその関数に渡すようにすればいい。

では、高階関数を使って、標準ライブラリにあるような便利な関数を自分で定義してみよう。それは`zipWith`という関数だ。この関数は2つのリストと関数をパラメータとして受け取り、それぞれのリストの要素に対して関数を適用した結果をリストとして結合して返す。いかが、実装方法だ。

    zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith' _ [] _ = []
    zipWith' _ _ [] = []
    zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

では、type declarationを見てみよう。最初のパラメータは、パラメータを2つ受け取る関数だ。その2つのtypeは異なるが、同じものを与えることもできる。2、3番目のパラメータはリストだ。そして、結果もリストとなる。関数がパラメータとしてtypeがaの値を受け取るので、最初のリストは、typeがaの要素で構成されたリストでなければならない。2番目のリストは、同じ理由によりtypeがbの要素で構成されたリストでなければならない。

そして、結果のリストはtypeがcの要素で構成されなければならない。`zipWith`がパラメータとして受け取る関数のtype decralationは`a -> b -> c`となているから、もちろん`a -> a-> a`というのもこれに含まれる。Haskellでは、高階関数に限らず、関数を定義するときに、そのtype declarationが正しいか自信がない時や、一々書くのが面倒なときに、type declarationを省略して、`:t`でその関数のtypeをHaskellが推測した結果を確認できる、というのを覚えているかな?

関数の挙動は、普通のzipに似ている。edge conditionも同じだ。ただし、関数を受け取っているので、そこは変数名に`_`を使って、いる。最後のパターンも、`関数の　bodyが`(x, y)`の代わりに`f(x,y)`としている点を除けば、よく似ている。A single higher order functionは、タスクが一般的であれば、、複数のタスクをこなすのに使われる。以下に、`zipWith`がどのように動作するかを示す。

    ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
     [6,8,7,9]
     ghci> zipWith' max [6,3,2,1] [7,3,1,5]
     [7,3,2,5]
     ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
     ["foo fighters","bar hoppers","baz aldrin"]
     ghci> zipWith' (*) (replicate 5 2) [1..]
     [2,4,6,8,10]
     ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
     [[3,4,6],[9,20,30],[10,12,12]]

ご覧のとおり、single higher order functionは、多彩な使い方ができる。

これを命令言語で実現しようとすると、forやwhileループを使って、何か操作してはその状態を変数に保持して、また何か操作をして...と、面倒なことになる。そして、その振る舞いをまとめて実現するためのインターフェースとして、命令言語における関数で、それを包むことになる。

これに対して、functional programmingでは、高階関数が共通のパターンに対する抽象的な処理の方法、例えば、ある2つのリストに対し、それをペアにまとめたり、あるいは別の何かとして答えを返したり、不要な要素をリストから取り除いたり、といったことができる。

次は、別の標準ライブラリ関数、`flip`を自分で実装してみよう。`flip`に関数を渡すと、関数のbodyはそのままに、パラメータを受け取る順番を逆にした関数を返す。では、実装してみよう。

    flip' :: (a -> b -> c) -> (b -> a -> c)
    flip' f = g
        where g x y = f y x

`flip'`のtype declarationを見ると、a、bという順番でパラメータを受け取る関数を受け取り、b、aの順番でパラメータを受け取る関数を返す、ということが読み取れる。ただ、すべての関数はすでにcurried functionなので、`(b -> a -> c)`は、あえてカッコで包む必要はない。また、`b -> a -> c`は`(b -> (a -> c))`とも等しい。

さて、この関数では、`g x y = f y x`と書いている。もし、それが正しければ、`f x y = g y x`ということになる。つまり、この関数は以下のように書ける。

     flip' :: (a -> b -> c) -> b -> a -> c
     flip' f y x = f x y

ここで、関数が最初からcurryされている利点が享受できる。もし、`flip`に関数だけを与えれば、パラメータを逆順で受け取る関数が返される。受け取るパラメータを逆順にした関数は、他の関数に渡される事が多いので、パラメータが完全に適用された結果がどうなるか、さきのことを考えて、高階関数を作るときに、curryingの利点が享受できる。



## Maps and filters

`map`は、関数とリストを受け取って、リストのすべての要素に関数を適用した結果からなる、新しいリストを返す。まずはtype signatureを確認して、この関数がどのように定義されているか見てみよう。

    map :: (a -> b) -> [a] -> [b]
    map _ [] = []
    map f (x:xs) = f x : map f xs

type signatureから、`a -> b`という関数と、、`[a]`というリスト、を受け取って、`[b]`というリストを返すことが読み取れる。関数のtype signatureを見るだけで、、その関数が何をしたいのかが分かる。`map`は、多彩な高階関数の一つであり、何万通りもの使い方ができる。実際に試してみよう。

    ghci> map (+3) [1,5,3,1,6]
    [4,8,6,4,9]
    ghci> map (++ "!") ["BIFF", "BANG", "POW"]
    ["BIFF!","BANG!","POW!"]
    ghci> map (replicate 3) [3..6]
    [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
    ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
    [[1,4],[9,16,25,36],[49,64]]
    ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
    [1,3,6,2,2]

`map`関数で実現したいことは、list comprehensionでも実現できる、ということにお気づきだろうか。たとえば、`map (+3) [1,5,3,1,6]`というのは、`[x+3 | x <- [1,5,3,1,6]]`と、同じだ。しかし、あるリストの要素すべてに関数を適用したい場合、`map`を使うほうがより読みやすくなる。特に、`map`に対して`map`を適用する場合は、list comprehensionで同じことをしようとすると、カッコだらけになって、読みづらくなるだろう。

`filter`はpredicate、`True`か`False`を返す関数とリストを受け取り、リストからある要素をpredicateにもとづいて取り除いたリストを返す関数だ。type signature と実装は、このようになる。

    filter :: (a -> Bool) -> [a] -> [a]
    filter _ [] = []
    filter p (x:xs)
        | p x       = x : filter p xs
        | otherwise = filter p xs

シンプルだね。predicateが`True`なら、その要素`x`と残りのリスト`xs`を`filter' p xs`おして連結した結果を返す。では、この関数の使用例を見てみよう。

    ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
    [5,6,4]
    ghci> filter (==3) [1,2,3,4,5]
    [3]
    ghci> filter even [1..10]
    [2,4,6,8,10]
    ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
    [[1,2,3],[3,4,5],[2,2]]
    ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
    "uagameasadifeent"
    ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"
    "GAYBALLS"

これらは、list comprehensionのpredicateでも同じことができる。`map`や`filter`か、list comprehensionか、どちらを選ぶべきかという明確な基準はないコードの読みやすさやコーディング規約などをもとにその都度決めればいい。list comprehensionでは複数のpredicateを記述できたが、`filter`関数の場合は、`&&`でpredicateを結合するか、`filter`を何回か適用することで代用できる。

さて、前の商でクイックソートをlist comprehensionを用いて実装したのを覚えているかな?ピボットに対して、残りのリストと比較して値がより小さいか、等しいか、大きいか、というのをlist comprehensionで調べていた。同じ機能を`filter`関数を用いて実現できる。


    quicksort :: (Ord a) => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) =
        let smallerSorted = quicksort (filter (<=x) xs)
            biggerSorted = quicksort (filter (>x) xs)
        in  smallerSorted ++ [x] ++ biggerSorted

functional programmingにおいて、`map`と`filter`はパンとバターのようなものだ。その機能をlist comprehensionを使うか、関数として使うかは問題ではない。

さて、3辺の合計がある値になる直角三角形をどのように求めたかを思い出してほしい。命令言語では、3段階ネストしたループを用いて、いまの値が目的の直角三角形を満たすかどうか、3つのパラメータをその都度計算することになる。そして、目的の値に一致すれば、それを表示する。

Functional programmingでは、これはmapとfilterを用いて実現できる。まず、パラメータを受け取って、答えとなりうるものを列挙する関数を作る。`map`でリストにたいして関数を適用し、その結果から`filter`で条件を満たさないものを取り除くことで期待通りの結果が得られる。Haskellのlazinessのおかげで、リストに対する`map`や`filter`を何回か行う場合でもリストを一度与えるだけで住む。

では、0から100000の内、3829で割り切れる最大の値を求めてみよう。単純に、答えとなりうる集合から、条件に合わないものをfilterするだけでいい。

    largestDivisible :: (Integral a) => a
    largestDivisible = head (filter p [100000,99999..])
        where p x = x `mod` 3829 == 0

まずは、降順に100000までの整数を含むリストを作る。そして、リストは降順に並んでいるから、条件に合わない要素を取り除いたリストの先頭の要素を答えとして取り出す。そして、開始時点では有限のリストを使う必要はない。

`head`で要素を取り出したいだけなので、対象となるリストが無限化有限か、ということを気にする必要はない。evaluationは、最初に条件に合致するものを見つけた時点で止まるからだ。

次は、2乗した結果が奇数となるものを全て合計した結果農地、100000より小さいものをを返す関数、を実装してみよう。まず、答えを求めるのに必要となる`takeWhile`関数を紹介する。この関数にリストとpredicateを渡すと、リストの先頭の要素から順番にpredicateを試して、それがTrueである間はその要素をリストから取り出し続ける。Falseになれば、その時点で要素を取り出すのを止める。

例えば、文字列`"elephants know how to party"`から最初の単語を取り出したければ、`takeWhile (/=' ') "elephants know how to party"`とする。結果は`elephants`となる。いいね。

では、100000より小さい、すべての2乗の結果を求める。まず、無限リストに対して`^ 2`で、2乗した結果をマッピングする。そして、その中から奇数となるものを取り除く。さらに、その中空100000より小さいものを取り出す。最後に、それらの合計を求める。この関数は、1行で定義できる。


    ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
    166650

これはすごい。初期値として、自然数1から始まる無限リストを用意して、それをmapして、filterして、最後に得られたリストを合計する。もちろん、これはlist comprehensionを使って書くこともできる。

    ghci> sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
    166650

どちらが綺麗な方法に見える化は、好みの問題だ。繰り返しになるが、HaskellのLazinessがこれを可能にしている。無限リストに対する`map`や`filter`は、実際にその場で即座に`map`や`filter`するのではなくその操作は遅延して行われる。Haskellに`sum`関数が求めた総和を表示させようとすると、`sum`は`takeWhile`に総和を求めたいリストが必要だ、と伝える。そして、`takeWhile`は、10000より大きいか等しい値に遭遇するまでマッピングとフィルタリングを続ける。

次は、自然数に対するコラッツ数列を求めてみよう。もし、ある数が偶数なら、2で割る。もし、ある数が奇数なら、その数に3をかけて1を足す。これらの結果に対して、再度同じ手順を踏むことで結果を求める。そして、自然数の連鎖が得られる。、どんな数から始めてもいいが、結果が1になった時点で、この連鎖を止める。

では、コラッツ数列が何であるか分かったところで、1から100までのコラッツ数列の内、その長さが15より大きい物はいくつ存在するか、求めてみよう。まずは連鎖を生み出す関数から実装しよう。

    chain :: (Integral a) => a -> [a]
    chain 1 = [1]
    chain n
        | even n =  n:chain (n `div` 2)
        | odd n  =  n:chain (n*3 + 1)

連鎖は1で止まるから、これは催奇のedge caseとなる。

    ghci> chain 10
    [10,5,16,8,4,2,1]
    ghci> chain 1
    [1]
    ghci> chain 30
    [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

うん、問題なく動いている。では、先ほどの問題に対する答えを求めてみよう。

    numLongChains :: Int
    numLongChains = length (filter isLong (map chain [1..100]))
        where isLong xs = length xs > 15

まず、連鎖のリストを得るために、`[1..100]`に対して`chain`関数をマッピングしている。そして、連鎖のリストから、その長さが15より大きい物を取り出す。フィルタリングが完了すれば、連鎖した結果が15より大きくなるもののリストが得られるわけだ。

Note: `length`は歴史的な理由により、その結果を`Int`として返すので、上記の関数のtype declarationでは`Int`を使用している。もし、`length`が`Num`を返すのであれば、`Int`の代わりに`Integral`という、もっと一般的なtypeを使用できる。

`map`関数は、無限リストに対して、例えば`map (*) [0..]`という事ができる。curryされた関数(と、部分適用された関数)は、値として他の関数に渡したり、リストにたいして使うことができるのだった。ただし、関数を文字列として表現できない、ということも学んだ。

さて、ここまで`map`関数はたとえば、`map (*2) [0..]`という具合に、リストにたいして一つのパラメータを受け取ってそれをリストの各要素に対してマッピングしていたので、結果のtypeは``(Num a) => [a]`となっていた。

しかし、`map (*) [1..]`というのも問題なくできる。どういうことかというと、リストの要素は、関数`*`が適用された状態になり、そのtypeは`(N、um a) => a -> a -> a`となる。

2つのパラメータを受け取る関数にひとつだけパラメータを与えると、ひとつのパラメータを受け取る関数になるのだった。つまり、リストにたいして関数`*`をマッピングすると、一つのパラメータを受け取る関数、のリストが得られる。

実際にはできないが、文字列で表現すると、`[(1 *), (2 *), (3 *)..]`というリストになるだろう。そして、そのリストのtypeは、`(Num a) => [a -> a].となる。

    ghci> let listOfFuns = map (*) [0..]
    ghci> (listOfFuns !! 4) 5
    20

リストの4番目を取り出すと、関数が得られる。その関数は`(4 *)`と等しいので、この関数に`5`を適用できる。つまり、`(*) 4 5`もしくは`4 * 5`と等しくなり、結果として`20`が得られる。



## Lambdas

lambdaは一度しか使わない場合に使う、匿名関数だ。普通、ラムダは公開関数を作るために用いる。ラムダを使うには、バックスラッシュ`\`の後にパラメータをスペースで区切って書く。ちなみに、バックスラッシュは、ギリシャ文字のラムダを意図している。そして、パラメータの後には、矢印`->`を起き、その後に関数のbodyを記述する。どこまでが関数のbodyか判断できるように、ラムダを記述するときは、それをかっこ`()`で包む。

もし、`numLongChain`関数の中で、数列の長さでフィルタリングをしたくなっったら、`where`を使って`isLong`という関数を定義するだろう。そうする代わりに、ラムダを使うことができる。

    numLongChains :: Int
    numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

ラムダはexpressionだから、上記のように`filter`へ渡すことができる。`(\xs -> length xs > 15)`というexpressionは、与えたリストの長さが15より大きいか判定した結果を返す関数だ。

カリー化や、部分適用がどのように機能するのかをよく理解していない人は、ひつようのない場面でラムダを使ってしまうことがある。たとえば、`map (+3) [1,6,3,2]`というexpressionに対して`map (\x -> x + 3) [1,6,3,2]`という書き方をしてしまう。パラメータを受け取って、それに3を足したものを返すという関数`\x -> x + 3`は、`+ 3`と等しい。言うまでもなく、この場合はラムダを使う必要はないし、`+ 3`のほうが読みやすい。

普通の関数のように、ラムダは複数のパラメータを受け取ることができる。

    ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
    [153.0,61.5,31.0,15.75,6.6]

そして、普通の関数と同様に、ラムダでもパターンマッチが使える。唯一の違いは、一つのパラメータに対して複数のパターン、例えば、`[]`と`(x:xs)`のようなパターンを調べ、これに当てはまらなければフォールスルーして次のパターンを試す、というような、記述ができない、というところだ。

もしラムダでパターンマッチが失敗すると、ランタイムエラーとなるので、ラムダの中では慎重にパターンマッチをする必要がある。

    ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
    [3,8,9,8,7]

ラムダは、意図してそうする目的がない限り、かっこで包むのが普通だ。ここで面白い例を紹介しよう。Haskellではすべての関数が最初からcurryされているから、以下の2つは等しくなる。

    addThree :: (Num a) => a -> a -> a -> a
    addThree x y z = x + y + z

    addThree :: (Num a) => a -> a -> a -> a
    addThree = \x -> \y -> \z -> x + y + z

関数をこのように記述すると、type declarationの意図が明確になる。どちらのtype declarationにも矢印`->`が登場する。ただ、もちろん最初の関数のほうが、何をする関数７日が見た目で分かりやすいし、2爪の関数はcurryの機能を説明する方法としてなら使えるだろう。

しかし、この記法を活かせる場面がある。`flip`関数の定義はその一例ではないかと思う。

    flip' :: (a -> b -> c) -> b -> a -> c
    flip' f = \x y -> f y x

これは、`flip' f x y = f y x`と同じ意味になる。しかし、ラムダを使えばこの関数が新しく関数を返すのだ、という糸を明確にできる。

flipの最も一般的な使用例は、flipに何か関数を与えて、それを`map`や`filter`に渡す、というものだ。ラムダは、その関数が部分適用されていることを明確にして他の関数にパラメータとして渡したい場合の方法として使える。



## Only folds and horses

さて、ここで催奇の話に戻ろう。催奇はリストにたいしてよく使われる。そして、リストに対するedge caseは普通、空のリストになるのだった。`(x:xs)`というのはリストを先頭と残りの部分に分割する、というのも紹介した。このようなリストに対する処理は一般的で、カプセル化されている。これらの関数は`fold`と呼ばれる。`map`関数のように、リストを受け取ってある1つの値を残すという関数だ。

`fold`関数は、binary functionとリストを受け取り、リストを畳み込む関数だ。、binary functionというのは2つのパラメータを受けとる関数だ。binary functionは、最初、あるいは最後の要素とaccumulator を伴ってcallされ、新しいaccumulatorを返す。そして、binary functionはその新しいaccumulatorと最初、あるいは最後の要素を伴ってcallされ、というのを繰り返す。リストの要素全てに対し、この操作が終わるとaccumulatorが残る。これが、リストをreduceした結果となる。

まずは、fold関数を見てみよう。foldはleft fold関数とも言う。これはリストの要素を左から順にfoldする。binary functionは、まずリストの先頭の要素と初期値に適用される。すると、新しいaccumulatorが返されるので、binary functionはそれをパラメータとして受け取り、またaccumulatorを返し、...というのを繰り返す。

では、催奇を使う代わりにfoldを使って、`sum`を実装してみよう。

    sum' :: (Num a) => [a] -> a
    sum' xs = foldl (\acc x -> acc + x) 0 xs

`[1,2,3]`をテストしてみる。

    ghci> sum' [3,5,2,1]
    11

foldlがどのように動作するのか追跡してみよう。まず、`\acc x -> acc + x`というのがbinary functionとなる。`xs`は対象のリストであり、`0`は初期値で、この値からfoldが開始される。まず、`ac`には0が、`x`には3が与えられ、最初のbinary funcitonがcallされる。`0 + 3`は`3`だから、次の段階では`ac`に`3`が与えられることになる。次の段階では、`ac`に`3`が、`x`にはリストから取り出された値である`5`が与えられた状態となる。続けると、`8`が計算した結果となるから、次の`ac`には`8`が与えられることになる。こうして、最終的には`ac`に`10`が、`x`に`1`が与えられた状態となり、`11`が得られる。おめでとう、これでfoldは完了だ。

このプロフェッショナルな図は(冗談)は、foldlが動作するさまを順番に説明したものだ。緑色の部分が`ac`に相当する。リストが左の要素から順に消費されているのが読み取れるだろう。

いやいや、ちょっとまってほしい、関数のカリー化を考慮すれば、もっとシンプルにこの関数を実装できるはずだ。

    sum' :: (Num a) => [a] -> a
    sum' = foldl (+) 0

`\x y -> x + y`というラムダは、`+`と全く同じだ。関数`foldl (+) 0`というのはリストを受け取る関数になるから、`xs`という変数を用意する必要はなくなるのだ。カリー化のおかげで、一般的な関数`foo a = bar b a`は、`foo = bar b`のように省略することができる。

とにかく、right foldの話に進む前に、他の関数を定義してみよう。`elem`関数がどんなものかは、よく知っていると思うが、foldを使ってもう一度この関数を実装してみよう。

    elem' :: (Eq a) => a -> [a] -> Bool
    elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

ちょーっと待った! ちょ基地もaccumulatorも真偽値になっているけど、これはどういうことなんだ。accumulatorもfoldlの結果も、どちらもおなじになる。初期値に何を渡せばいいのかわからない場合の方法がある。`False`から始めるという方法だ。Falseであると仮定するのだ。空のリストに対するfoldlも同様に、初期値と同じ空のリストとなる。いま取り出した要素が目的の要素か判定する。目的の要素であればaccumulatorとしてTrueを返す。そうでなければ、accumulatorを変化させない。前の段階の結果がFalseであれば、そのaccumulatorが引き継がれることになる。そして、それがTrueの場合も引き継がれる。

right foldはleft foldによく似ているが、accumulatorにわたされる要素が右から順番に取り出されるという違いがある。つまり、left foldのbinary functionは、accumulatorをひとつ目の引数、いまの値を2つめの引数として受け取るが、right foldはいまの値を一つ目の引数、accumulatorを2つめの引数として受け取る。accumulatorを受け取る順番から、right foldが右から順に要素を取り出すことになる理由がわかるだろう。

もちろん、この関数はleft foldを用いて実装することもできる。たとえば、map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs`というのは、`:`より`++`を使うほうがわかりやすいだろう。つまり、right foldはリストから新しいリストを得る場合に使うのが一般的だ。

accumulatorの値から、foldrの結果がどんなものでも構わないことが分かるだろう。数値やリスト、真偽値など、なんでも良い。ここでは、`map`関数をright foldを使って実装する。accumulatorはリストになるし、返されるのもリストになる。というわけで、初期値は空のリストから始めることに仕様。

    map' :: (a -> b) -> [a] -> [b]
    map' f xs = foldr (\x acc -> f x : acc) [] xs

たとえば、リスト`[1,2,3]`に対して、`(+3)`をマッピングすると、その順番は右からとなる。　最後の要素は`3`だから、`+3`を適用すると結果として`6`が得られる。そして、`6`をaccumulatorとして空のリスト`[]`の先頭に追加する。`6:[]`は`[6]`だから、accumulatorは`[6]`となる。次は`2`に対して`+3`を適用するから、結果の`5`をリスト`[6]`と連結して`5:[6]`だから、accumulatorは`[5,6]`となる。最後に`1`に対して`+3`を適用して、結果の`4`とリスト`[5,6]`を連結した結果の`[4,5,6]`が得られる。

もちろん、これはright foldの代わりにleft foldを使っても同じことが実現できる。実際に定義するとしたら、`map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs`となるだろう。ただ、リストを連結する関数`++`より関数、`:`のほうが扱いやすいから、リストから何か新しいリストを作るときはright foldを使うのが一般的だ。

たとえば、リストを反転させたい場合は、left foldで行うのと逆のことをright foldで行えばいい。ただ、そのひつようがない場合もある。left foldを使っても、right foldを使っても、同じ実装になる関数がある。

left foldとright foldの大きな違いのひとつは、無限リストに対するものだ。無限リストは、リストの大きさが無限だから最後の要素、というものが存在しない。従って、無限リストの範囲を区切って部分的なリストを取り出してから、最初の要素までリストをfoldすることになる。ただし、left foldの場合は、どれが最後の要素なのか判断できないので、やはりright foldと同様に、無限リストに対する操作の前には、範囲を指定して部分的なリストとして取り出しておく必要がある。

foldは、受け取ったリストの各々の要素に対して何か操作をして結果を返す関数を実装するときに役立つ。リストをトラバースしようとしているなら、foldを活かせるチャンスだ。`map`や`filter`に加えて、foldはfunctional programmingが最もその効果を発揮する一例だ。

さて、`foldr1`と`foldl1`を紹介しよう。唯一異なるのは、foldr`や`foldl`では初期値を指定する必要がない、というところだ。これらの関数は、リストの先頭の要素、あるいは末尾の要素を初期値とみなして、次の要素に対してfoldを行う。つまり、関数`sum`は、`sum = foldl1 (+)`と実装できる。ただし、foldされるリストは少なくとも1つの要素を持つ必要があるから、空のリストを与えるとランタイムエラーとなる。一方、`foldr`と`foldl`は、空のリストにたいしてfoldづることができる。foldが空のリストに対して、どのように動作するか考えてみよう。定義しようとしている関数が、空のリストを受け取る必要がなければ`foldr`か`foldl`を選択するのが理にかなっているだろう。

いかにfoldが強力か示すために、いくつか標準ライブラリ関数を自分で定義してみる。

    maximum' :: (Ord a) => [a] -> a
    maximum' = foldr1 (\x acc -> if x > acc then x else acc)

    reverse' :: [a] -> [a]
    reverse' = foldl (\acc x -> x : acc) []

    product' :: (Num a) => [a] -> a
    product' = foldr1 (*)

    filter' :: (a -> Bool) -> [a] -> [a]
    filter' p = foldr (\x acc -> if p x then x : acc else acc) []

    head' :: [a] -> a
    head' = foldr1 (\x _ -> x)

    last' :: [a] -> a
    last' = foldl1 (\_ x -> x)

`head`はパターンんマッチでも実装できるが、ここでは例を示すためにfoldを使って実装した。特に`reverse`の定義は、かなり賢いと思うね。最初は空のリストからはじめて左の要素から結果となるリストの戦闘に追加していく。すると、最終的にはリストを反転させることになる。ラムダ`\acc x -> x : acc`というのは、パラメータを入れ替えているだけだ。だから、この`reverse`関数は、`flip`を使って定義することもできる。

right foldとleft foldを説明する他の方法は、こうだ。まず、`z`を初期値とするbinary function、`f`について考えよう。right foldの場合、`[3,4,5,6]`というリストをfoldするということは、`f 3 (f 4 (f 5 (f 6 z)))`をしていることになる。`f`は、リストの最後の要素と`z`を受け取り、その結果を次の`f`がパラメータとして受け鳥、その結果をッ次の`f`がパラメータとして受け取り、...というのを繰り返す。

たとえば、`f`として`+`を与えれば、`3 + (4 + (5 + (6 + 0)))`を計算した結果が得られる。prefix functionとして書き表すなら、、`(+) 3 ((+) 4 ((+) 5 ((+) 6 0)))`となる。

同様に、left foldの場合は、binary functionとして`g`を受け取る場合、`g (g (g (g z 3) 4) 5) 6`を求めることになる。たとえば、`flip (:)`をbinary functionとして、空のリスト`[]`をaccumulatorとしてleft foldに与えた場合、`flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6`を求めることになる。もちろん、ご想像の通り結果は`[6,5,4,3]`となる。

`scanr`と`scanl`は、`foldr`と`foldl`と同じ働きをするが、計算途中の様子をリストとして返す関数だ。もちろん、`foldr1`と`foldl1`に類似した`scanr1`と`scanl1`もある。

    ghci> scanl (+) 0 [3,5,2,1]
    [0,3,8,10,11]
    ghci> scanr (+) 0 [3,5,2,1]
    [11,8,3,1,0]
    ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
    [3,4,5,5,7,9,9,9]
    ghci> scanl (flip (:)) [] [3,2,1]
    [[],[3],[2,3],[1,2,3]]

`scanl`を使った場合、その結果は`scanr`の結果の先頭の要素が、結果のリストの末尾の要素となる。

scanは、foldを使った関数を実装するとき、その進行状況を監視するのに使われる。では、自然数の平方根の内、それらを合計していった結果が1000を超えないとき、その要素数はいくつになるか、というのを求めてみよう。

まず、すべての自然数の平方根を得るために、`map sqrt [1..]`を求める。そして、これらの合計を求めるにはfoldすればOKだ。しかし、今回はその途中の様子を知りたいので、foldではなくscanを使う。scanが完了すると、1000以下となる合計の値がいくつ存在するか、というのが得られる。`sqrt 1`は`1`だから、最初のscan結果は、`1`となるだろう2番目の結果は、その結果に`sqrt 2`を加えた結果、つまり`1 + (sqrt 2)`となるだろう。3番目の結果は、その結果に`sqrt 3`を加えた結果になるだろう。そして、結果が1000を超えた時点で、それまでの結果を取り出せば、求めていた答えとなる。

    sqrtSums :: Int
    sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
    ghci> sqrtSums
    131
    ghci> sum (map sqrt [1..131])
    1005.0942035344083
    ghci> sum (map sqrt [1..130])
    993.6486803921487

無限リストに対しては`filter`は使えないので、ここでは、`takeWhile`をその代わりとして使用している。この例では、リストは昇順に並んでいるから、最初に合計が1000を超えた時点で、その無限リストを切り取ることになる。



## Function application with $

さて、次は`$`関数についてだ。これは、function applicationとも呼ばれる。なにはともあれ、まずは`$`がどのように定義されているか調べよう。

    ghci> :t ($)
    ($) :: (a -> b) -> a -> b

なんだこれ。この関数の用途は何なんだ?

これは、ただのfunction applicationに見えるが、実は違う。通常のfunction applicationは、関数とパラメータの間にスペースを置くことで適用され、その優先度は最高となるが、この関数は逆に、優先度が最も低くなる。

通常は左から順に適用されるので関数適用`f a b c`というのは、`(((f a) b) c)`という意味になるが、関数`$`は右から順に適用される。

`$`の使い方は分かったけど、これの何が便利なんだ?

まず、`map sqrt [1..130])`というexpressionを例に考えてみよう。`$`の優先度は最も低いから、先ほどのexpressionは、`sum $ map sqrt [1..130]`と書きなおすことができる。キー入力の手間を省けるわけだ。`$`に出くわすとその右側のexpressionは、左側の関数にパラメータとして適用される。では、`sqrt 3 + 4 + 9`というのはどうだろう?これは、3の平方根に4と9を足す、という意味になる。もし、`3 + 4 + 9`に対して平方根を求めたい場合は、`sqrt (3 + 4 + 9)と記述するか、または、`$`の優先度が最も低いから、`sqrt $ 3 + 4 + 9`と記述することができる。`$`は他のどのoperatorよりも優先度が低いから、`$`をexpressionの前に置くだけで、expressionの両端をかっこで包むという手間を省ける。

では、`sum (filter (> 10) (map (*2) [2..10]))`というexpressionはどうだろう? `$`はright-associativeだから、`f (g (z x))`というのは、`f $ g $ z x`というのと等しい。つまり、`sum (filter (> 10) (map (*2) [2..10]))というのは、`sum $ filter (> 10) $ map (*2) [2..10]`というように書きなおすことができる。

`$`がfunction applicationとして扱われるのは、expressionからかっこを取り除くときに、そう扱われる必要がある体。これにより、例えば関数のリストに対して、function applicationをマッピングできる。

    ghci> map ($ 3) [(4+), (10*), (^2), sqrt]
    [7.0,30.0,9.0,1.7320508075688772]



## Function composition

数学の世界では、function compositionは、$(f * x)(x) = f(g(x))$のように定義される。`g)(x) = f(g(x)),`というのは、2つの関数を合成して、ひとつの新しい関数にすることを意味している。そして、その関数に`x``を与えることは、`g(x)`の結果に対して、`f`をcallすることと同じである、というのを意味している。
`x`は`g(x)`

Haskellでは、関数合成は、同じように行える。関数合成は、`.`で関数を連結することで行える。

    ghci> :t (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c

type declarationに注目してほしい。`f`が受け取るパラメータのtypeは、`g`が返す値のtypeと同じである必要がある。そして、合成された関数も、`f`と`g`のtypeと同じtypeにする必要がある。たとえば、`negate.(*3)`はパラメータを受け取り、それに3をかけたものに`-1`をかけた結果尾を返す。

関数合成の便利なところは、他の関数に渡すための関数を即席で作れる、というところだ。もちろんラムダを使っても実現できるが、関数合成はそれより手軽でわかりやすい。たとえば、数値で構成されたリストの要素をすべて負の数にする、というのを試してみよう。一度`abs`でそれぞれの絶対値を求めてから負の数にする、というのが一つの方法だ。

    ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
    [-5,-3,-6,-7,-3,-2,-19,-24]

このようにラムダを使う、という方法もあるが、このラムダが意味するところは`.`を使って関数合成したのと全く同じだ。

    ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
    [-5,-3,-6,-7,-3,-2,-19,-24]

すばらしい。そして、関数合成は、right-associativeだから、`.`を使って、関数をその場で次々に合成することができる。たとえば、`f (g (z x))`というexpressionは、`f.g z x`というのと等しい。これを覚えたうえで、例えば次の関数を見てほしい。

    ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
    [-14,-15,-27]

これを書き直すと、こうなる。

    ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]
    [-14,-15,-27]

ところで、複数のパラメータを受け取る関数の関数合成はどうなるんだろう? もし、そのような関数で関数合成がしたい場合は、その関数がひとつのパラメータを受け取る関数になるまで部分適用してから、関数合成をする必要がある。

たとえば、`sum (replicate 5 (max 6.7 8.9))` というのは書き直しができて、`(sum . replicate 5 . max 6.7) 8.9`または、`sum .replicate 5 . max 6.7 $ 8.9`という記述ができる。

ここで何が起きているかというと、まず、`max 6.7`という関数を受け取り、`replicate 5`に適用させた関数が作られる。そして、その結果を`sum`に適用させて、関数合成が完了する。これでパラメータを一つ受け取る関数が出来上がったので、最後に、その関数に`8.9`を渡してcallする。ここまで複雑な説明をしてしまったが、この関数は、`8.9`を`max`に適用した結果を、`replicate 5`に適用して、最後にその合計を求めるため、`sum`に適用する、という読み取り方をするのが普通だ。

例示した2番目の関数合成のexpressionを、ドット`.`の代わりにかっこを使って書きなおすとするなら、expressionの右はじにあるパラメータを、まずは`$`の後ろ、一番深くにある関数に与えて、それから、残りの関数をパラメータがひとつ足りていない状態のまま並べていき、最後にそれらをドット`.`で連結していけば、関数合成の完成となる。たとえば、`replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))`というのは、`replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]`のように書きなおすことができる。もし、関数が3つ以上の閉じカッコで終わっているなら、それは関数合成を使って書き直せるチャンスだ。

関数合成の使いみちとしては、他に`point free`あるいは`pointless`とよばれる記述をするときに使われる。先程定義した関数を例に説明しよう。

    sum' :: (Num a) => [a] -> a
    sum' xs = foldl (+) 0 xs

`xs`は両サイドに露出している。しかし、カリー化のおかげで、`foldl (+) 0`というのがリストを受け取る関数を作り出すことになるから、両サイドの`xs`というのを省略することができる。　たとえば、`sum' = foldl (+) 0`のような関数は、point freeで記述されていると言える。

では、次の関数をpoint freeで記述するとどうなるだろう。

    fn x = ceiling (negate (tan (cos (max 50 x))))

右側の`x`は省略することができない。関数のbodyにある`x`はその後ろにかっこが続いている。このため、`(max 50)`が意味を成さなくなってしまう。この関数には余分なところがない。

ここでできるのは、`fn`を関数の合成として書き直す、という方法だ。

    fn = ceiling . negate . tan . cos . max 50

すばらしい。 多くの場合、point freeで記述したほうがより読みやすくかつ完結になる。なぜなら、データがどのように扱われるかを考える代わりに目的の結果を返す関数をどのように合成するかを考えることに集中できるからだ。

関数合成は、かんたんな関数を組み合わせて複雑な関数に仕立てる、接着剤のようなものだ、と考えるのがいいだろう。しかし、point freeで関数を記述しようとすると、読みやすさを失って、複雑なものになってしまう。長い関数合成の連鎖を作るのは、複雑さを回避してpoint freeの恩恵をうけるためだ。

ただ、これに対して`let`を使って、解決したい問題を小さな問題に分割して、それらの答えを求めたうえで、答えの断片をまとめ上げて最終的な答えとする方法もあり、巨大な関数合成の連鎖を作る代わりに、小さな問題を解決する関数の集まりを定義するというのも一つの方法ではある。

さて、maps and filtersの商では、1000未満の間は、2乗した結果が奇数になるものを合計し続けて、その結果を得る、という関数を定義した。この問題を個別の関数に切り出して答えを求めようとすると、たとえば以下のようになるだろう。

    oddSquareSum :: Integer
    oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

関数合成の楽しさを伝えるべく、この関数を定義し直すと、こうなる。

    oddSquareSum :: Integer
    oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

しかし、これにはもっと読みやすくできる余地が残されている。こんなふうに書けるだろう。

    oddSquareSum :: Integer
    oddSquareSum =
        let oddSquares = filter odd $ map (^2) [1..]
            belowLimit = takeWhile (<10000) oddSquares
        in  sum belowLimit

コードゴルフ大会に出場する場合を除いて、自分や他人のためにも関数合成を用いてより読みやすい関数を定義しよう。



# Modules

Haskellのモジュールはtypeとtypeclassに関係する関数の集まりだ。Haskellのプログラムは、メインモジュールが他のモジュールを読み込んでそこに定義されている関数を使ってなにかする、というものになる。コードをモジュールに分割する利点は大きい。モジュールが十分に一般的であれば、その関数を他のプログラムで使い回すことができる。もし、あなたのコードがお互いに依存しないモジュールに分割できるなら、後からそれらを再利用できる。目的別にコードを分割することで、管理しやすくかつ特定の問題に対する答えを導きやすくなる。

Haskellの標準ライブラリはモジュールに分割されており、それらのモジュールには、typeや一般的な目的別の関数が含まれている。たとえばリストを操作するためのモジュールや、並行処理のためのモジュール、複素数を扱うモジュールなどがある。いままで扱ってきた関数や、type、typeclassはPreludeという標準で組み込まれているモジュールの一部だった。この商では、いくつか役に立つモジュールと、それに含まれる関数を紹介する。まずは、モジュールの読み込み方法から説明する。

Haskellのスクリプトからモジュールを読み込むためのシンタックスは、`import <module name>`だ。これは他のどの関数の定義より先に行う必要があるので、通常、ファイルの一番先頭に`import ...`というのを記述する。もちろん、ひとつのスクリプト内で複数のモジュールを読み込むことができる。
`import`というのを複数行に記述するだけだ。

では、`Data.List`というモジュールを読み込んでみよう。このモジュールは、リストに対する操作を提供する便利な関数が含まれており、たとえば、リストからユニークな値がいくつあるかを教えてくれる関数などがある。

    import Data.List
    
    numUniques :: (Eq a) => [a] -> Int
    numUniques = length . nub

インポートされたモジュールに含まれる関数は、グローバルなネームスペースで参照できるようになるので、スクリプト内のどこからでもそれらの関数を使えるようになる。関数`nub`は、受け取ったリストから重複した要素を刈り取ったリストを返すので、`length.nub`という関数合成は、リストが持つユニークな要素の数を返す関数になる。

もちろん、ghciでもグローバルにモジュールを読み込むことができる。ghciの中で、`Data.List`の関数を使いたければ、こうする。

    ghci> :m + Data.List

ghciの中で複数のモジュールを読み込みたければ、一度に複数のモジュールを読み込むことができるので、何度も`:m`をタイプする手間を省ける。

    ghci> :m + Data.List Data.Map Data.Set

もちろん、一度モジュールを読み込んだ後は、`:m`を入力する必要はない。モジュール内の特定の関数をいくつか取り出し使いたければ、モジュール名の後に関数名をタプルの形式で書く。

    import Data.List (nub, sort)

ある特定の関数を除く、すべての関数をモジュールから読み込む、というのも可能だ。すでにモジュール内に存在する関数と同じ名前の関数を定義していて、名前の衝突を避けたい場合にこの機能が役立つ。たとえば、既に`nub`という関数を定義していて、その関数名を除くすべての関数を`Data.List`から読み込みたい場合は、以下のように記述する。

    import Data.List hiding (nub)

他に名前の衝突を避ける方法として、qualified  importというのがある。たとえば、ghciを起動すると最初に読み込まれるPreludeモジュールには`filter`や`map`という関数が定義されている。しかし、`Data.Map`というキーからバリューを求めるような関数を集めたモジュールにも同じ名前の関数が定義されている。このままでは、PreludeかData.Mapか、どちらで定義された関数を使うべきか判断できない。そこで、以下のように解決する。

    import qualified Data.Map

これで、最初から読み込まれている`filter`関数はそのままに、Data.Mapモジュールに組み込まれているfilter関数は、`Data.Map.filter`として使えるようになる。ただ、このままでは毎回`Data.Map.`というプレフィックスをつけることになって面倒なので、読み込まれる関数の名前を別の短い名前に変更したほうが便利だ。

    import qualified Data.Map as M

これで、`Data.Map.filter`を`M.filter`として使える。

Haskellの知識を深めるには、標準ライブらいに定義されている関数を概観するのが手っ取り早く勝つ有効な手段となる。もちろん、それぞれのモジュールのソースコードを見ることができる。他のモジュールのソースコードをみて学ぶというのは、自分でモジュールを書くときの手助けとなるだろう。

どこに目的の関数が定義されているかを調べるには、Hoogleを使うのが便利だ。これは、関数名やモジュール名、type signatureから目的の関数を探すための検索エンジンだ。



## Data.List

`Data.List`はリストに関するモジュールで、リストを操作するための便利な関数が含まれている、実は既にそのモジュールに含まれる関数を幾つか使っている。`map`や`filter`は、その例だ。Preludeモジュールでは利便性を考慮していくつかの関数を`Data.List`からあらかじめ読み込んでいる。そのため、改めて`Data.List`をimportする必要はない。

では、まだ使ったことのない関数を幾つか紹介しよう。

`intersperse`関数は、ある要素とリストを受け取り、リストの要素と要素の間に受け取った要素を挟んだリストを返す、という関数だ。試してみよう。

    ghci> intersperse '.' "MONKEY"
    "M.O.N.K.E.Y"
    ghci> intersperse 0 [1,2,3,4,5,6]
    [1,0,2,0,3,0,4,0,5,0,6]

`intercalate `関数は、リストのリスト、とリストを受け取って、そのリストを、リストのリストのそれぞれの間に挟んで、最後にそのリストをflattenしたリストを返す、という関数だ。

    ghci> intercalate " " ["hey","there","guys"]
    "hey there guys"
    ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
    [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

`transpose`関数はリストのリストを受け取ってtransposeする関数だ。2次元をリストで表現した場合、その行と列を入れ替えたリストが返される。つまり、行列にたいする転置行列を得る関数だ。

    ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]
    [[1,4,7],[2,5,8],[3,6,9]]
    ghci> transpose ["hey","there","guys"]
    ["htg","ehu","yey","rs","e"]

たとえば、3次元の多項式があったとして、$3x^2 + 5x + 9$、$10x^3 + 9$、$8x^3 + 5x^2 + x - 1$を足したいとする。これをHaskellのリストで表現すると`[0,3,5,9], [10,0,0,9], [8,5,1,-1]`となる。そして、これらを足すにはこのようにする。

    ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
    [18,8,6,17]

それぞれの行と列が入れ替わるので、目的の多項式が得られる。この場合、変数xの係数が次元の高い順に3,2,1,0と並んでいたのが、3,3,3,3,、2,2,2,2、...という順に入れ替わることになるので、結果としてその次元の係数を足し合わせることになる。　

`foldl'`と、`foldl1'`は、foldrとfoldlをより厳密にしたものだ。巨大なリストに対してfoldをすると、頻繁にスタックオーバーフローが起こる。この問題の原因は、実際にaccumulatorが計算されるのはその対象がfoldされる時点になってから、という特性による。実際にfoldで何が起こっているのかというと、accumulatorは、必要になったら計算された結果を返す、という約束のようなものを生成している。ちなみに、これはthunkとも言う。つまり、このthunkが計算の途中で大量に生成されてしまい、結果としてスタックを使い果たすことになるのだ。strict foldは、スタックを食い尽くす代わりに、計算の途中結果を逐一計算していく。foldを使っていてスタックオーバーフローに出くわしたら、`foldr'`や`foldl'`を試そう。

`concat`はリストのリストをリストへと平坦化する。

    ghci> concat ["foo","bar","car"]
    "foobarcar"
    ghci> concat [[3,4,5],[2,3,4],[2,1,1]]
    [3,4,5,2,3,4,2,1,1]

`concat`が平坦化するのは1段階までだから、リストのリスト...、のリストを完全に平坦化したい場合は`concat`を複数回行う必要がある

`concatMap`は、まず与えた関数をリストにマッピングしてから`concat`を行う。

    ghci> concatMap (replicate 4) [1..3]
    [1,1,1,1,2,2,2,2,3,3,3,3]

`and`は真偽値のリストを受け取り、すべてTrueだった場合にTrueを返す。

    ghci> and $ map (>4) [5,6,7,8]
    True
    ghci> and $ map (==4) [4,4,4,3,4]
    False

`or`は、`and`のようにBoolのリストを受け取り、どれかがTrueであればTrueを返す。

    ghci> or $ map (==4) [2,3,4,5,6,1]
    True
    ghci> or $ map (>4) [1,2,3]
    False

`any`と`all`はpredicateとリストを受け取り、文字通りある要素、あるいはすべての要素がpredicateを見たしたとき、`True`を返す。通常、predicateを`map`する代わりに`any`や`all`を使う。

    ghci> any (==4) [2,3,5,6,1,4]
    True
    ghci> all (>4) [6,9,10]
    True
    ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
    False
    ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
    True

`iterate`は、関数と初期値を受け取り、関数に初期値を適用した値、関数にその値を適用した値、関数に...という具合に、関数に初期値を適用した値をもとにして適用を続け、それを無限リストとして返す。

    ghci> take 10 $ iterate (*2) 1
    [1,2,4,8,16,32,64,128,256,512]
    ghci> take 3 $ iterate (++ "haha") "haha"
    ["haha","hahahaha","hahahahahaha"]

`splitAt`は、数値とリストを受け取り、その数値でリストを分割した結果をタプルとして返す。

    ghci> splitAt 3 "heyman"
    ("hey","man")
    ghci> splitAt 100 "heyman"
    ("heyman","")
    ghci> splitAt (-3) "heyman"
    ("","heyman")
    ghci> let (a,b) = splitAt 3 "foobar" in b ++ a
    "barfoo"

`takeWhile`は非常に便利な関数だ。`takeWhile`は、predicateが`True`の間、リストから要素を取り出し続け、predicateを満たさなくなった時点で、リストを切り取る。これがいかに便利か、見てみよう。

    ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
    [6,5,4]
    ghci> takeWhile (/=' ') "This is a sentence"
    "This"

ここで、3乗した結果が10000以下の数を総和するとしよう。`map (^3)`を無限リスト`[1..]`に適用すれば、3乗した結果の無限リストは得られる。しかし、`filter`が扱えるのは有限のリストのみだそこで、リストが昇順に並んでいることを利用して、`takeWhile`を使う。

    ghci> sum $ takeWhile (<10000) $ map (^3) [1..]
    53361

無限リストに対して`(^3)`を適用して、その後10000より大きい数値に遭遇したらその時点で無限リストを切り取る。これで、総和が簡単に求められる。

`dropWhile`も似たような関数で、こちらはpredicateが`True`の間、リストから要素を取り除き続ける。そして、predicateが`False`になった時点でのこりのリストを返す。これも愛すべき便利な関数だ。

    ghci> dropWhile (/=' ') "This is a sentence"
    " is a sentence"
    ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
    [3,4,5,4,3,2,1]

ここで、日付ごとの株価を表すリストを与えるとしよう。リストは、最初が株価、2番目が年、3番目が月、4番目が日にちを表すタプルで構成される。株価がはじめて1000ドルを超えたのはいつか、求めるとしよう。

    ghci> let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
    ghci> head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
    (1001.4,2008,9,4)

`span`も`takeWhile`と似たようなものだが、こちらはリストのペアを返す。ペアに含まれる最初のリストは、`takeWhile`にpredicateを与えて得られるリストと同じものを含み、2番目のリストは、残りのリストが含まれる。

    ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest
    "First word: This, the rest: is a sentence"

`span`はpredicateが`True`の間、リストを切り取るのに対して、`break`は、predicateがはじめて`True`になった時点でリストを切り取る。つまり、`break p`というのは`span (not . p)`というのとおなじだ。

    ghci> break (==4) [1,2,3,4,5,6,7]
    ([1,2,3],[4,5,6,7])
    ghci> span (/=4) [1,2,3,4,5,6,7]
    ([1,2,3],[4,5,6,7])

`break`した結果、2番目のリストに含まれる先頭の要素は、predicateを満たした要素から始まる。

`sort`はその名の通り、リストをソートする。ただしソートするためにはリスト内の要素同士を比較する必要があるので、ソート対象のリストに含まれる要素は`Ord`typeclassに属していなければならない。

    ghci> sort [8,5,3,2,1,6,4,2]
    [1,2,2,3,4,5,6,8]
    ghci> sort "This will be sorted soon"
    "    Tbdeehiillnooorssstw"

`group`は、受け取ったリスト内の隣り合う要素が等しければ、それらをまとめたサブリストを返す。

    ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
    [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

リストを`group`するまえに`sort`しておけば、各々の要素がリスト内で南海出現するか知ることができる。

    ghci> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
    [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]

`inits`と`tails`は、`init`と`tail`に似ているが、これらはリストから要素がなくなるまで再帰的に適用される。では、観察してみよう。

    ghci> inits "w00t"
    ["","w","w0","w00","w00t"]
    ghci> tails "w00t"
    ["w00t","00t","0t","t",""]
    ghci> let w = "w00t" in zip (inits w) (tails w)
    [("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]

ここで、リストから目的のリストを検索する関数を、foldを使って実装してみよう。

    search :: (Eq a) => [a] -> [a] -> Bool
    search needle haystack =
        let nlen = length needle
        in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

まずは、検索対象のリストにたいして`tails`を適用する。そして、`tails`した結果のリストにたいして、そのリストが目的のリストと同じ要素から始まっているかを調べる。

いま実装した関数の振る舞いは、`isInfixOf`と同じだ。`isInfixOf`は、検索対象のリスト内のどこかに目的のリストが存在すれば`True`を返すという関数だ。

    ghci> "cat" `isInfixOf` "im a cat burglar"
    True
    ghci> "Cat" `isInfixOf` "im a cat burglar"
    False
    ghci> "cats" `isInfixOf` "im a cat burglar"
    False

`isPrefixOf`と`isSuffixOf`は、それぞれ検索対象のリスト内の先頭、あるいは末尾に目的のリストが存在するか調べる。

    ghci> "hey" `isPrefixOf` "hey there!"
    True
    ghci> "hey" `isPrefixOf` "oh hey there!"
    False
    ghci> "there!" `isSuffixOf` "oh hey there!"
    True
    ghci> "there!" `isSuffixOf` "oh hey there"
    False

`elem`と`notElem`は、与えた要素が検索対象のリスト内に存在するか調べる。

`partition`は、predicateとリストを受け取り、リストのペアを返す。ペアに含まれる1番目のリストには、predicateを満たす要素が、2番目のリストにはpredicateを一度も満たさなかった要素が含まれる。

    ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
    ("BOBMORGAN","sidneyeddy")
    ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]
    ([5,6,7],[1,3,3,2,1,0,3])

これが`span`や`break`とどう違うのかを理解するのは重要だ。　

    ghci> span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
    ("BOB","sidneyMORGANeddy")

`span`や`break`は、一度predicateを満たさない、あるいは満たす要素に遭遇した時点でリストを分割するのに対して、`partition`は、リスト内のすべての要素に対してpredicateを満たすかどうか判定し、その結果のリストをタプルとして返す。

`find`はpredicateとリストを受け取り、リストから最初にpredicateを満たした要素を返す。ただし、結果は`Maybe`に包まれたものとして返される。次の賞で、algebraic data typesについては詳しく説明するので、とりあえず今知っておくべきことは、`Maybe`で包まれた値は`Just その値`もしくは`Nothing`になる、ということだ。リストが空のリスト`[]`もしくはある要素を含むリストの両方を表現できるように、Maybeな値も、要素そのもの、もしくは要素がない状態、というのを表現できる。また、たとえば整数のリストのtypeは、`[Int]`になるように、Maybeな整数のtypeも`Maybe Int`になる。さて、話を戻して`find`を試してみよう。

    ghci> find (>4) [1,2,3,4,5,6]
    Just 5
    ghci> find (>9) [1,2,3,4,5,6]
    Nothing
    ghci> :t find
    find :: (a -> Bool) -> [a] -> Maybe a

`find`のtypeに注目してほしい。結果が`Maybe a`となっている。`Maybe a`とすることで、あるtypeの要素もしくは要素が存在しないということを表現できる。一方、リストを使えば空のリストで要素がないことを表現できるものの、1つ以上の要素を含むリストも作れてしまう。

ところで、株価が初めて1000ドルを超えた日付を求めたのを覚えているかな? `head (dropWhile (\(val,y,m,d) -> val < 1000) stock)`として求めたのだった。そして、`head`というのが安全ではない、というのも覚えているかな? もし、株価が1000ドルを超えなかったら何が起こるだろうか。`dropWhile`は空のリストを返し、空のリストに対する`head`はエラーを引き起こす。しかし、これを`find (\(val,y,m,d) -> val > 1000) stock`のように書きなおせば、より安全になる。もし株価が1000ドルを超えなかったとしても、つまり、predicateを満たす要素が存在しなくても、`Nothing`という結果が得られる。しかも、目的の要素が見つかった場合には、`Just (1001.4,2008,9,4)`という適切な結果が得られる。

`elemIndex`は、結果がBoolではない、というのを除いて`elem`と同じだ。もしリスト内に探している要素があれば、そのインデックスを`Maybe`で包んだ値として返すし、見つからなければ`Nothing`を返す。

    ghci> :t elemIndex
    elemIndex :: (Eq a) => a -> [a] -> Maybe Int
    ghci> 4 `elemIndex` [1,2,3,4,5,6]
    Just 3
    ghci> 10 `elemIndex` [1,2,3,4,5,6]
    Nothing

`elemIndices`は、`elemIndex`と似ているが、こちらはリスト内に目的の要素が複数ある場合、それらのインデックスをまとめてリストとして返す。ここでは、結果をインデックスのリストとして表現しているので`Maybe`を使う必要はない。空のリストが要素が見つからなかった、ということを表現できるため、`Nothing`と類似した働きができるのだ。

    ghci> ' ' `elemIndices` "Where are the spaces?"
    [5,9,13]

`findIndex`は、`find`と似ているが、最初にpredicateを満たした要素を`Maybe`な値として返す。`findIndices`は、リストからpredicateを満たす要素をすべて取り出し、リストとして返す。

    ghci> findIndex (==4) [5,3,2,1,6,4]
    Just 5
    ghci> findIndex (==7) [5,3,2,1,6,4]
    Nothing
    ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
    [0,6,10,14]


さて、`zip`と`zipWith`はもう学んだね。、どちらも2つのリストを受け取り、2つのパラメータを受け取るbinary functionによってタプルにまとめるのだった。でも、3つのリストをまとめたい、あるいは3つのパラメータを受け取る関数を使ってまとめたい場合はどうしよう?そうだね、実は`zip3`や`zip4`という関数がある。`zipWith3`、`zipWith4`もある。そして、これらは7まである。ただ、この方法は小細工に見える。もちろんこれらは機能するが、8つのリストをまとめる場面というのはそうそうないだろう。実は、リストを無限に受け取って、それらをまとめる賢い方法がある。しかし、その方法を知るにはまだ知識が足りない。

    ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
    [7,9,8]
    ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
    [(2,2,5,2),(3,2,5,2),(3,2,3,2)]

なお、普通の`zip`や`zipWith`と同様、`zip3`や`zipWith3`に与えたリストの長さが異なる場合、短い方に合わせて、残りの要素は取り除かれる。

`lines`は、ファイルなどからの読み込みを扱うのに便利な関数だ。これは、文字列を受け取り、行ごとの文字列としてリストに分割する。

    ghci> lines "first line\nsecond line\nthird line"
    ["first line","second line","third line"]

`'\n'`というのは、UNIXにおける開業を意味する。Haskellでは、バックスラッシュ`\`は特別な意味を持つ。

`unlines`は、`lines`と逆の働きをし、文字列のリストを受け取り、それらを文字列にまとめる。

    ghci> unlines ["first line", "second line", "third line"]
    "first line\nsecond line\nthird line\n"

`words`と`unwords`は、文字列を単語に区切り、単語のリストから文字列へまとめる、とても便利な関数だ。

    ghci> words "hey these are the words in this sentence"
    ["hey","these","are","the","words","in","this","sentence"]
    ghci> words "hey these           are    the words in this\nsentence"
    ["hey","these","are","the","words","in","this","sentence"]
    ghci> unwords ["hey","there","mate"]
    "hey there mate"

さて、`nub`は既に紹介した。リストから重複した要素を取り除き、要素の重複がないリストを返すという無味乾燥な関数だ。しかし、変な名前の関数だ。"nub"というのは、"a small lump or essential part of something"という意味だ。古の言葉より、現実的な関数の名前にすべきだと思うね。

    ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
    [1,2,3,4]
    ghci> nub "Lots of words and stuff"
    "Lots fwrdanu"

`delete`は削除対象の要素とリストを受け取り、リスト内ではじめてその要素が出現した時、その要素を取り除いたリストを返す。

    ghci> delete 'h' "hey there ghang!"
    "ey there ghang!"
    ghci> delete 'h' . delete 'h' $ "hey there ghang!"
    "ey tere ghang!"
    ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
    "ey tere gang!"

`\\`は、リストの差を求める関数だ。この関数は、差集合を求めるような関数であり、左側のリスト内のようその内、右側のリスト内に存在する要素をすべて取り除く。

    ghci> [1..10] \\ [2,5,9]
    [1,3,4,6,7,8,10]
    ghci> "Im a big baby" \\ "big"
    "Im a  baby"

`[1..10] \\ [2,5,9]`というのは、`delete 2 . delete 5 . delete 9 $ [1..10]`というのと同じだ。

`union`も同様に集合を扱う関数だ。これは、2つのリストの和集合を返す。2番目のリスト内の要素すべてにたいして、1番目のリスト内に存在するか調べ、まだ存在しなければ、その要素を1番目のリストへ追加する。ただし、2番目のリストの重複した要素は取り除かれるので注意が必要だ。

    ghci> "hey man" `union` "man what's up"
    "hey manwt'sup"
    ghci> [1..7] `union` [5..10]
    [1,2,3,4,5,6,7,8,9,10]

`intersect`は積集合のように振る舞う。どちらのリストにも存在する要素を返す関数だ。

    ghci> [1..7] `intersect` [5..10]
    [5,6,7]

`insert`は、ソート可能な要素と対象のリストを受け取り、その要素より小さいか等しい要素の隣に、その要素を挿入する。言い換えれば、`insert`は、リストの先頭から挿入したい要素より大きいか等しい要素を調べ続け、もし条件を満たす要素が見つかれば、その要素の前に目的の要素を挿入する。

    ghci> insert 4 [3,5,1,2,8,2]
    [3,4,5,1,2,8,2]
    ghci> insert 4 [1,3,4,4,1]
    [1,3,4,4,4,1]

最初の例では、`4`は、`3`と`5`の間に挿入され、2番目の例では、`3`と`4`の間に挿入されている。

もし、ソート済みのリストにたいして`insert`した場合、結果のリストもソート済みとなる。

    ghci> insert 4 [1,2,3,5,6,7]
    [1,2,3,4,5,6,7]
    ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']
    "abcdefghijklmnopqrstuvwxyz"
    ghci> insert 3 [1,2,4,3,2,1]
    [1,2,3,4,3,2,1]


`length, take, drop, splitAt, !! replicate`、これらの関数は、パラメータとして、あるいは結果を返す際に`Int`を使用するが、`Int`の代わりにより汎用的なtypeclassである`Integral`や`Num`を受け取ったり返したりできたほうが、より便利に使えるだろう。`Int`を使用するのは、Haskellの歴史的な理由による。これらを修正すれば、既存のコードを破壊しかねない。そこで、`Data.List`には、これらをより汎用的にした`genericLength, genericTake, genericDrop, genericSplitAt, genericIndex, genericReplicate`が用意されている。たとえば、.`length`のtype signatureは、`length :: [a] -> Int`となっている。例えば、リスト内の要素の平均を求めようとして、`let xs = [1..6] in sum xs / length xs`とすると、`/`は`Int`に対して使用できないので、type errorが起こってしまう。一方、`genericLength`のtype signatureは、`genericLength :: (Num a) => [b] -> a`となっている。`Num`は、浮動小数としても振る舞うから、`let xs = [1..6] in sum xs / genericLength xs`としても大丈夫だ。

`nub, delete, union, intersect, group`より汎用的にした`nubBy, deleteBy, unionBy, intersectBy, groupBy`という関数が用意されている。両者の違いは、前者の関数が要素の比較に`==`という関数を使うのに対し、後者の関数は比較用の関数を受け取って要素を比較するところにある。つまり、`group`は`groupBy (==)`と等しい。


たとえば、ある関数の値を1秒ごとに記録したリストがあるとしよう。そして、値が0を下回った、あるいは0を超えた時にそれまでの値をまとめたいとしよう。普通の`group`を使った場合、隣り合うリスト同士でまとめられてしまう。しかし、ここで実現したいのは、ある値が負の数になったとき、あるいはそうではなくなった時にそれまでの要素をまとめる、というものだ。そこで、`groupBy`の出番となる。比較用の関数として、、2つの要素が成果不可を判定しそれらを比較する関数を与えると、、2つの要素がどちらも同じ基準を満たせば`True`となるので、負の数から正の数、あるいは正の数から負の数、という時点でリストをまとめることができる。

    ghci> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
    ghci> groupBy (\x y -> (x > 0) == (y > 0)) values
    [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]

これで、どの部分が正負なのか、はっきりした。2つの要素を受け取る比較用の関数は、どちらの要素も正あるいは負のとき`True`を返す。この比較用の関数は、最初の関数のほうが読みやすいと思うが、`\x y -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)`という書き方もできる。そして、よりわかりやすい比較用の関数を書く方法としては、`Data.Function`からインポートした`on`関数を使う方法がある。`on`は次のように定義されている。

    on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    f `on` g = \x y -> f (g x) (g y)

つまり、`(==) `on` (> 0)`というのは、`\x y -> (x > 0) == (y > 0)`という意味になる。たとえば、次のようなことができるので、`on`は、比較用の関数としてよく使われる。

    ghci> groupBy ((==) `on` (> 0)) values
    [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]

超わかりやすいね! これは、次のような意味になる。それぞれの要素が0より大きいか判定し、その結果が等しければ、その時点で要素をまとめる。

同様に、`sort, insert, maximum, minimum`を依り一般的にしたものがある。`groupBy`は、2つの要素を受け取り等しいかを判定する関数を受け取る。`sortBy, insertBy, maximumBy, minimumBy`は、1つの要素を受け取り、より大きい、等しい、あるいはより小さいか判定する関数を受け取る。`sortBy`のtype signatureは、`sortBy :: (a -> a -> Ordering) -> [a] -> [a]`となっている。`Ordering`が値として`GT, EQ, LT`を持つというのを覚えていれば、`sort`というのは、`sortBy compare`と等しいと考えていい。`compare`は、typeclassが`Ord`の値を2つ受け取り、判定結果を`Ordering`の値として返すからだ。

リストも比較可能だが、比較する際の順番は辞書順となる。もし、リストのリストに対してその中身ではなく、リストの長さに基づいてリストをソートするとしたら、どうだろう、そうだね、ご想像の通り`groupBy`関数を使うだろう。

    ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
    ghci> sortBy (compare `on` length) xs
    [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]

すばらしい! `compare \`on\` length`というのは自然な英語のようだ。もし、これが何をしているのかよくわからなければ、`compare \`on\` length`というのは、`\x y -> length x `compare` length y`と等しい、と考えればいい。等しさを判定する関数を受け取る比較用の関数を扱う場合、通常、`(==) \`on\` something`というのがよく使われる。また、順序を扱う関数を`比較用の関数に渡す場合には`compare `on` something`というのをよく使うことになる。



## Data.Char

`Data.Char`モジュールは、その名が示す通り、文字を扱う関数を提供する。また、文字列は文字のリストだから、文字列に対するフィルタリングやマッピングなどを助ける関数もこれに含まれる。

`Data.Char`は文字に対するpredicateを数多く提供してくれる。つまり、文字を受け取って、何かに基づいてその文字が`True`か`False`かを判定してくれる関数が、このモジュールには多数あるということだ。さっそく、見てみよう。

`isControl`は、その文字が制御文字か判定する。

`isSpace`は、その文字がスペースやタブ、開業などの空白文字かどうかを判定する。

`isLower`は、その文字が小文字か判定する。

`isUpper`は、その文字が大文字化判定する。

`isAlpha `は、その文字がアルファベットか判定する。

`isAlphaNum`は、その文字がアルファベットまたは数字かどうか判定する。

`isPrint`は、その文字が印刷可能か判定する。たとえば制御文字は印刷できない。

`isDigit`は、その文字が10進数の数字かどうか判定する。

`isOctDigit`は、その文字が8進数か判定する。

`isHexDigit`は、その文字が16進数か判定する。

`isLetter`は、その文字が英語を含めた文字化判定する。

`isMark`は、ユニコードの記号文字か判定する。これは、文字の前に付けられる記号であり、文字と組み合わせて使用されるものであり、、たとえばフランス語で使われる記号などがそれに該当する。

`isNumber`は、その文字が数値化どうか判定する。

    ghci> isDigit '5'
    True
    ghci> isDigit '５'
    False
    ghci> isNumber '5'
    True
    ghci> isNumber '５'
    True
    ghci> isNumber '五' --- これも5として扱ってほしい気がする
    False

`isPunctuation`は、その文字が句読点か判定する。

`isSymbol`は、その文字が、数学の記号化通貨の記号化を判定する。

`isSeparator`は、ユニコードの区切り文字か判定する。

`isAscii`は、ユニコードの文字セットの内、最初の128種類のASCII文字か判定する。

`isLatin1` は、その文字がユニコードの最初の256文字に該当するか判定する。

`isAsciiUpper` は、そのもじがASCII文字かつ大文字化判定する。

`isAsciiLower`は、その文字がASCII文字かつ小文字化判定する。

これらのpredicateは、すべて`Char -> Bool`というtype signatureとなっている。これらは、文字列などから特定の文字をフィルタリングするのに役立つ。たとえば、ユーザーネームを受け取るプログラムを作っていて、ユーザーネームとして使えるのは英数字のみだ、としよう。ユーザーネームが適切なものか、`Data.List`に含まれる関数と`Data.Char`に含まれるpredicateの組み合わせで判定することができる。

    ghci> all isAlphaNum "bobby283"
    True
    ghci> all isAlphaNum "eddy the fish!"
    False

クールだ! `all`は受け取ったリストの全ての要素に対してpredicateを満たすか判定する、というのは覚えておこう。

また、`isSpace` を使って、`Data.List`の`words`関数をシミュレートできる。

    ghci> words "hey guys its me"
    ["hey","guys","its","me"]
    ghci> groupBy ((==) `on` isSpace) "hey guys its me"
    ["hey"," ","guys"," ","its"," ","me"]

うーん、`words`のように機能しているけど、スペースが残ってしまった。さて、どうしよう? フィルターしよう。

    ghci> filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"
    ["hey","guys","its","me"]

`Data.Char`は、`Ordering`のようなtypeも提供している。`Ordering`というtypeは、`LT, EQ, GT`という値を持つ。これは、ある種のenumerationだ。`LT, EQ, GT`というのは、2つの要素を比較した結果から発生しえる、答えの候補を示している。`GeneralCategory`というtypeも、enumerationの一種だ。これは、ある文字がどの種類に当てはまるか、というのを示している。そして、ある文字の種類を調べるための関数が、`generalCategory`となる。`generalCategory`のtypeは、　`generalCategory :: Char -> GeneralCategory`となる。`GeneralCategory`は、31種類もあるので、全ては紹介できないが、とりあえずこの関数を試してみよう。

    ghci> generalCategory ' '
    Space
    ghci> generalCategory 'A'
    UppercaseLetter
    ghci> generalCategory 'a'
    LowercaseLetter
    ghci> generalCategory '.'
    OtherPunctuation
    ghci> generalCategory '9'
    DecimalNumber
    ghci> map generalCategory " \t\nA9?|"
    [Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

`GeneralCategory`のtypeは,`Eq`というtypeclassの一部だから、`generalCategory c == Space`というような比較ができる。

`toUpper`は、スペース、数字などを除き、小文字を大文字へ変換する。

`toLower`は、大文字を小文字に変換する。

`toTitle`は、は、文字をtitle-caseに変換する。殆どの文字で、title-caseは大文字とおなじになる。

`digitToInt`は、文字を`Int`の数字に変換する。ただし、変換するには文字が`['0'..'9']`、`['a'..'f']`、`['A'..'F']`の範囲でなければならない。

    ghci> map digitToInt "34538"
    [3,4,5,3,8]
    ghci> map digitToInt "FF85AB"
    [15,15,8,5,10,11]

`intToDigit`は、`digitToInt`と逆の働きをする。`[0..15]`の範囲で数字を受け取り、`['0'..'f']`として結果を返す。

    ghci> intToDigit 15
    'f'
    ghci> intToDigit 5
    '5'

`ord`と`chr`は、文字をそれに対応した数字に、数字をそれに対応した文字に変換する。

    ghci> ord 'a'
    97
    ghci> chr 97
    'a'
    ghci> map ord "abcdefgh"
    [97,98,99,100,101,102,103,104]

`ord`で求めた値の差は、ある文字と文字がユニコード表上でどれだけ離れているかを示している。

Caesar cipherは、原始的なメッセージのエンコード方法であり、アルファベット上で、メッセージに含まれる文字を固定長だけずらす。もし対象の文字列がアルファベットに限るのであれば、Caesar cipherは、簡単に実装することができる。

    encode :: Int -> String -> String
    encode shift msg =
        let ords = map ord msg
            shifted = map (+ shift) ords
        in  map chr shifted


まず、文字列を数字のリストへ変換する。そして、数字のリストを文字列に戻す前に、シフト量をそれらの数字に加える。もし、あなたが関数合成オタクなら、`map (chr . (+ shift) . ord) msg`という書き方をするだろう。では、幾つかメッセージをエンコードしてみよう。

    ghci> encode 3 "Heeeeey"
    "Khhhhh|"
    ghci> encode 4 "Heeeeey"
    "Liiiii}"
    ghci> encode 1 "abcd"
    "bcde"
    ghci> encode 5 "Marry Christmas! Ho ho ho!"
    "Rfww~%Hmwnxyrfx&%Mt%mt%mt&"

問題なくエンコードで着ている。エンコードした文字列をデコードしたい場合は、エンコードするときに指定したシフト量だけ、逆方向にずらせばいい。これで、元の文字列が得られる。

    decode :: Int -> String -> String
    decode shift msg = encode (negate shift) msg

    ghci> encode 3 "Im a little teapot"
    "Lp#d#olwwoh#whdsrw"
    ghci> decode 3 "Lp#d#olwwoh#whdsrw"
    "Im a little teapot"
    ghci> decode 5 . encode 5 $ "This is a sentence"
    "This is a sentence"



## Data.Map

ディクショナリとも呼ばれる連想リストは、キーと値を保存するリストであり、リスト内の要素の順番は保証されない。たとえば、電話番号と持ち主の名前を、電話番号をキー、名前を値として保持する連想リストがあるとしよう。ここで重要なのは、リスト内の要素の順番ではなく、電話番号というキーから、持ち主の名前という値が正しく得られるうことだ。

Haskellで連想リストを表現する最も明確な方法は、ペアのリストを作る、という方法だ。ペアの1つめの要素がキー、2つめの要素が値となる。では、電話番号を使った連想リストの例を見てみよう。

    phoneBook =
        [("betty","555-2938")
        ,("bonnie","452-2928")
        ,("patsy","493-2928")
        ,("lucille","205-2928")
        ,("wendy","939-8282")
        ,("penny","853-2492")
        ]

この不格好なインデントで表現しているのは、単なるペアのリストだ。連想リストで重要なのは、キーから値が取り出せる、ということだ。では、キーを与えて値を取り出す関数を定義してみよう。

    findKey :: (Eq k) => k -> [(k,v)] -> v
    findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

かなり単純だね。この関数は、キーとリストを受け取って、キーをもとにフィルターして、、キーが一致した要素のみが残る。そして、残った要素の内、最初の要素からキーを下に値を取り出し、それを返している。しかし、探しているキーが連想リスト内に存在しなかった場合、何が起こるだろう? うーむ。ここでは、空のリストに対して`head`を求めることになるので、ランタイムエラーが発生してしまう。しかし、簡単にプログラムのクラッシュを防ぐ方法を、我々は既に知っている。そう、Maybeを使うのだ。キーに一致する値が見つからなければ、`Nothing`を返す。見つかれば、それに対応する値を`Just その値`として返せばいい。

    findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
    findKey key [] = Nothing
    findKey key ((k,v):xs) = if key == k
                                then Just v
                                else findKey key xs

type declarationを見てみよう。連想リストのキーを受け取って、値をMaybeに包んで返す。これは正しそうだ。これは、リストを扱う関数として、催奇のお手本のような関数だ。リストを先頭の要素と残りのリストに分割して、残りのリストにたいして関数を呼び出す。これは、典型的なfoldのパターンだ。では、これをfoldでどのように実装するのか見てみよう。

    findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
    findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

Note: 標準的なリストに対する催奇のパターンは、明示的に再起を書くのではなく、通常はfoldを使って書いたほうが良い。なぜなら、foldを使ったほうが、読みやすく理解しやすいからだ。foldが使われているのを見つけたら、それが実際には再帰しているというのが理解できる。

    vGghci> findKey "penny" phoneBook
    Just "853-2492"
    ghci> findKey "betty" phoneBook
    Just "555-2938"
    ghci> findKey "wilma" phoneBook
    Nothing

いい感じに動いている。もし女の子の電話番号があれば、その名前または`Nothing`が得られる。

さて、今実装した検索を行う関数は`Data.List`の機能を利用している。キーに対応する値を得るには、リストの要素すべてに対して、キーが見つかるまでトラバースをしなくてはいけない。`Data.Map`モジュールは、内部の実装が木構造となっているためより高速な連想リストを提供する。また、多くのユーティリティ関数も提供している。、ここからは、連想リストの代わりにマップを使うことに仕様。

`Data.Map`が提供する関数は、`Prelude`と`Data.List`モジュールと衝突するのでqualifiedインポートをしよう。

    import qualified Data.Map as Map

このインポート文をスクリプト内に書いてから、ghciでもう一度ロードする。

では、`Data.Map`に格納されている関数を概観してみよう。以下に、基本的な関数をまとめる。

`fromList`は、連想リストをリストとして受け取り、同じものを連想マップとして返す。

    ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
    fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
    ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]
    fromList [(1,2),(3,2),(5,5)]

元の連想リスト内に重複したキーが含まれていれば、そのキーは捨てられる。`fromList`のtype signatureは次のとおりだ。

    Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v

`k`と`v`というtypeのペアのリストを受け取り、`k`というtypeのキーに対して`v`というtypeの値をマッピングした、マップを返す。普通のリストを連想リストとして扱う場合はそのキーは等しさが確かめられさえすれば、(`Eq`というtypeclassに属してさえいれば)問題なかった。しかし、マップのキーは`Ord`にも属している必要がある。これは、`Data.Map`モジュールに不可欠な製薬となる。キーが順序を持つことで、それらを木構造へと整形できるのだ。


キーのtypeが`Ord`に属していないとき以外は、キーと値の組合せに対して常に`Data.Map`を使うほうがいいだろう。

`empty`は、空のマップを表現する。これは、引数を取らず、空のマップを返す。

    ghci> Map.empty
    fromList []

`insert`は、キー、値、そしてマップを受け取り、元のマップの構造を保ったまま、キーと値が挿入された新しいマップを返す。

    ghci> Map.empty
    fromList []
    ghci> Map.insert 3 100 Map.empty
    fromList [(3,100)]
    ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))
    fromList [(3,100),(4,200),(5,600)]
    ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
    fromList [(3,100),(4,200),(5,600)]

さて、独自の`fromList`を`insert`と空のマップ、そしてfoldを使って実装することができる。見てみよう。

    fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
    fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

このfoldは実に直感的だ。まず、空のマップから開始して、リストの右からキーと値のペアを`ac`として取り出し、それをマップへと挿入しながら畳み込んでいく。

`null`は、マップが空のマップか判定する。

    ghci> Map.null Map.empty
    True
    ghci> Map.null $ Map.fromList [(2,3),(5,5)]
    False

`size`は、マップの大きさを返す。

    ghci> Map.size Map.empty
    0
    ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
    5
`singleton`は、キーと値を受け取り、1つの要素のみを持つマップを返す。

    ghci> Map.singleton 3 9
    fromList [(3,9)]
    ghci> Map.insert 5 9 $ Map.singleton 3 9
    fromList [(3,9),(5,9)]

`lookup`は、`Data.List`の`lookup`のように機能するが、こちらはマップに対してのみ機能し、もし目的の値が見つかれば`Just その値`、見つからなければ`Nothing`を返す。

`member`は、キーとマップを受け取り、そのマップ内に目的のキーが存在するか判定するpredicateだ。

    ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
    True
    ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)]
    False

`map`と`filter`は、リストに対するそれと同じように機能する。

    ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
    fromList [(1,100),(2,400),(3,900)]
    ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
    fromList [(2,'A'),(4,'B')]

`toList`は、`fromList`と逆の働きをする。

    ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3
    [(4,3),(9,2)]

`keys`と`elems`は、それぞれキーと値のリストをを取り出す。これは、それぞれマップに対して[`fst . Map.toList`と`snd . Map.toList`を求めるのと等しい。

`fromListWith`は、ちょっとおもしろい関数だ。これは`fromList`のように機能するが、こちらは重複した要素を捨てることなく渡した関数によってその重複をどう扱うかを決めることができる。では、女の子が複数の電話番号を持てるように、連想リストを次のようにセットアップしてみよう。

    phoneBook =
        [("betty","555-2938")
        ,("betty","342-2492")
        ,("bonnie","452-2928")
        ,("patsy","493-2928")
        ,("patsy","943-2929")
        ,("patsy","827-9162")
        ,("lucille","205-2928")
        ,("wendy","939-8282")
        ,("penny","853-2492")
        ,("penny","555-2111")

さて、ここで通常の`fromList`を使うと、いくつかの電話番号が失われてしまう。そこで、以下のようにする。        

    phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
    phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

    ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
    "827-9162, 943-2929, 493-2928"
    ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
    "939-8282"
    ghci> Map.lookup "betty" $ phoneBookToMap phoneBook
    "342-2492, 555-2938"

もし重複が見つかれば、渡した関数がそれらの値を受け取り、別の値へと結合する。また、最初に連想リストのすべての値をシングルトンリストにしてから、それらを`++`で結合することもできる。

    phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
    phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

    ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
    ["827-9162","943-2929","493-2928"]

美しい。他の用途としては、数値を含む連想リストからマップを作る際に重複が見つかった場合、重複の中から最も大きい値のみを残す、というのが挙げられ得る。

    ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
    fromList [(2,100),(3,29),(4,22)]

あるいは、同じキーに対しては、その値を足し合わせる、という用途もある。

    ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
    fromList [(2,108),(3,62),(4,37)]

`insertWith`は、`fromList`に対する`fromListWith`のようなものだ。この関数はキーと値のペアをマップへと挿入するが、すでにマップ内に重複が存在する場合の判断は、渡された関数によって決めることができる。

    ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
    fromList [(3,104),(5,103),(6,339)]

これらは、`Data.Map`に含まれるほんの一部の関数でしかないから、すべての関数について知りたい場合はドキュメントを参照してほしい。



## Data.Set

`Data.Set`モジュールは、集合に関係するものを提供する。集合というのは、数学における集合だ。集合はリストとマップが交わったようなものだ。集合に含まれる要素には重複がない。また、その要素は内部的には木構造で実装されているので(`Data.Map`のマップのように)順序を持つ。集合に対する要素の挿入や削除などは、リストに対するそれより高速に行われる。集合に対する一般的な操作としては、要素の挿入や要素が集合に含まれるかの判定、集合からリストへの変換などがある。

`Data.Set`に含まれる名前は`Prelude`や`Data.List`と衝突するのでqualifiedインポートする。

スクリプトに以下の`import`文を追加しよう。

    import qualified Data.Set as Set

それから、ghciで再読込しよう。

さて、ある2つの文章から、どちらにも含まれる部分を抽出するとしよう。

    text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
    text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

`fromList`は、期待どおりの機能をする。これは、リストから集合へと変換する。

    ghci> let set1 = Set.fromList text1
    ghci> let set2 = Set.fromList text2
    ghci> set1
    fromList " .?AIRadefhijlmnorstuy"
    ghci> set2
    fromList " !Tabcdefghilmnorstuvwy"

ご覧のとおり、それぞれの要素には重複がなく、また、順序がある。では、`intersection`関数を使って、両方の集合が共有している要素を取り出してみよう。

    ghci> Set.intersection set1 set2
    fromList " adefhilmnorstuy"

`difference`関数は、2津の集合の内、1爪には存在するが、2つめには存在しない、また、その逆も満たす要素を返す。

    ghci> Set.difference set1 set2
    fromList ".?AIRj"
    ghci> Set.difference set2 set1
    fromList "!Tbcgvw"

また、どちらの文章にも存在する要素を重複なく取り出すには`union`を使う。

    ghci> Set.union set1 set2
    fromList " !.?AIRTabcdefghijlmnorstuvwy"

`null, size, member, empty, singleton, insert, delete`、これらの関数は期待通りの動作をする。

    ghci> Set.null Set.empty
    True
    ghci> Set.null $ Set.fromList [3,4,5,5,4,3]
    False
    ghci> Set.size $ Set.fromList [3,4,5,3,4,5]
    3
    ghci> Set.singleton 9
    fromList [9]
    ghci> Set.insert 4 $ Set.fromList [9,3,8,1]
    fromList [1,3,4,8,9]
    ghci> Set.insert 8 $ Set.fromList [5..10]
    fromList [5,6,7,8,9,10]
    ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]
    fromList [3,5]

そして、ある集合の部分集合や真部分集合を調べることもできる。たとえば、集合Aは集合Bの部分集合であるとき、集合Aに含まれるすべての要素は集合Bにも含まれる。また、集合Bは集合Aのすべての要素に加えて集合Aがとり得る要素の組み合わせも含むとき、集合Aは集合Bの真部分集合となる。


    ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
    True
    ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
    True
    ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
    False
    ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
    False

もちろん集合に対しても`map`や`filter`が使える。

    ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
    fromList [3,5,7]
    ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
    fromList [3,4,5,6,7,8]

集合は、リストの重複を取り除くのによく使われ、一旦イストを集合へ変換した後重複を取り除き、再度リストへへんかんすることで、目的のリストが得られる。すでに、`Data.List`の`nub`がその機能を担っているが、巨大なリストに対しては、一旦リストを集合へ変換した後、重複を取り除いてからリストへ要素を詰め込むほうが、`nub`を使うよりも高速に処理できる。ただし、`nub`で重複を取り除きたい場合は、リスト内の要素が`Eq`というtypeclassに属してさえいればいいのに大し、集合を使って重複を取り除く場合は、`Ord`にも属している必要がある。

    ghci> let setNub xs = Set.toList $ Set.fromList xs
    ghci> setNub "HEY WHATS CRACKALACKIN"
    " ACEHIKLNRSTWY"
    ghci> nub "HEY WHATS CRACKALACKIN"
    "HEY WATSCRKLIN"

`setNub`は、巨大なリストに対しては概ね高速に動作するが、先程確認したとおり、`nub`は処理後の要素の順番が保たれるのに対して、`setNub`は保たれない。



## Making our own modules

ここまで、クールなモジュールをいろいろ見てきたが、独自のモジュールを作りたくなった場合はどうするんだろう? 多くのプログラミング言語では、プログラムを小さなプログラムへ分割することができるが、Haskellもそれと違いはない。同じような目的で使用する関数やtypeclassなどを作った場合は、それをモジュールにまとめるのが一般的に有効な手段だ。そして、そのモジュールを別のプログラムでインポートすることで、使い回しができる。[

早速、体積と面積を求めたりするちょっとした関数が格納された独自のモジュールをどのように作るのか見てみよう。まず、`geometry.hs`というファイルを作るところからはじめよう。

関数をエクスポートするモジュールといった場合、インポートすればエクスポートされた関数が仕様できる、というのを意味する。モジュール内で呼び出される関数を定義できもちろんモジュールの内部でのみ呼び出される関数が定義できるし、外部から見えるのは、一度エクスポートされた関数のみとなる。

まず、モジュールはその名前を指定するところから始める。`geometry.hs`というファイル名に対しては、`Geometry`というモジュール名を指定しよう。そして、どの関数をエクスポートするかを指定してから関数を書き始める。つまり、以下のように始める。

    module Geometry
    ( sphereVolume
    , sphereArea
    , cubeVolume
    , cubeArea
    , cuboidArea
    , cuboidVolume
    ) where

ご覧のとおり、急、立方体、直方体、それぞれの表面積と堆積を求める関数をエクスポートしている。では、それらの関数がどう定義されるか見てみよう。

    module Geometry
    ( sphereVolume
    , sphereArea
    , cubeVolume
    , cubeArea
    , cuboidArea
    , cuboidVolume
    ) where
    
    sphereVolume :: Float -> Float
    sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)
    
    sphereArea :: Float -> Float
    sphereArea radius = 4 * pi * (radius ^ 2)
    
    cubeVolume :: Float -> Float
    cubeVolume side = cuboidVolume side side side
    
    cubeArea :: Float -> Float
    cubeArea side = cuboidArea side side side
    
    cuboidVolume :: Float -> Float -> Float -> Float
    cuboidVolume a b c = rectangleArea a b * c
    
    cuboidArea :: Float -> Float -> Float -> Float
    cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2
    
    rectangleArea :: Float -> Float -> Float
    rectangleArea a b = a * b

標準的な方法を挙げてみた。ここで、幾つか注目すべきことがある。立方体は、直方体の特殊な場合だから、すべての辺の長さが等しい直方体として定義している。また、`rectangleAre`という名前の長方形の面積をそのへんの長さに基づいて計算するヘルパー関数も定義している。これは、ただの乗算だから取るに足りない関数だ。この関数は、モジュール内の`cuboidArea`と`cuboidVolume`の中で使用されているが、外部にエクスポートはされていない。このモジュールがエクスポートしたいのは、3次元の物体を扱う関数だけだから、`rectangularArea`をエクスポートする必要はない。

通常、モジュールを作成するときに外部へエクスポートされるのはインターフェースとなる関数だけだから、その内部の実装は隠される。誰かが今作成したモジュールを使う場合も、エクスポートされていない関数について考える必要はない。たとえば、今作成したモジュールの内部で使われている関数を削除したり、完全に新しい物へ書き換えることもでき、`rectangularArea`をただの`*`に書き換えることもできる。そして、この関数はエクスポートされていないから、誰もこの関数について心配する必要はないのだ。

さて、作成したモジュールを使うには、以下のようにする。

    import Geometry

`geometry.hs`は、それをインポートするプログラムが置かれているディレクトリと同じ場所に配置しなければならない。

モジュールは階層的な構造にすることもできる。それぞれのモジュールがサブモジュールを持つことができ、そのサブモジュールもそれぞれのサブモジュールを持つことができる。早速、先ほどのGeometryモジュールを3種類の物体に対するサブモジュールへと切り分けてみよう。

まず、`Geometry`という名前のディレクトリを作成する。名前は大文字ではじめるのを忘れずに。そして、`Sphere.hs, Cuboid.hs, Cube.hs`という3種類のファイルをそのディレクトリ内に作成する。それぞれのファイルの中身は、こうなる。

* `Sphere.hs`

    module Geometry.Sphere
    ( volume
    , area
    ) where
    
    volume :: Float -> Float
    volume radius = (4.0 / 3.0) * pi * (radius ^ 3)
    
    area :: Float -> Float
    area radius = 4 * pi * (radius ^ 2)



* `Cuboid.hs`

    module Geometry.Cuboid
    ( volume
    , area
    ) where
    
    volume :: Float -> Float -> Float -> Float
    volume a b c = rectangleArea a b * c
    
    area :: Float -> Float -> Float -> Float
    area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2
    
    rectangleArea :: Float -> Float -> Float
    rectangleArea a b = a * b



* `Cube.hs`

    module Geometry.Cube
    ( volume
    , area
    ) where
    
    import qualified Geometry.Cuboid as Cuboid
    
    volume :: Float -> Float
    volume side = Cuboid.volume side side side
    
    area :: Float -> Float
    area side = Cuboid.area side side side

以上! まずは`Geometry.Sphere`から見てみよう。まず`Geometry`というディレクトリを作ってから、モジュールを`Geometry.Sphere`と定義しているのに注目しよう。同じことを`Cuboid`にも行う。また、それぞれのサブモジュール内で同じ名前の関数を定義していることにも注目してほしい。これが可能なのは、それぞれのモジュールが分離されているからだ。しかし、`Geometry.Cube`から`Geometry.Cuboid`ないにある関数を使いたくても、`Geometry.Cube`と`Geometry.Cuboid`内にある関数は、名前がおなじなので直接インポートすることはできない。常にqualifiedインポートをしたほうが良いのは、これが理由だ。

そこで、`Geometry`ディレクトリ内にある、つまり同じ階層にあるモジュールでは以下のようなことができる。

    import Geometry.Sphere

そして、、`area`や`volume`を呼び出せば、球体の表面積や体積が得られる。また、複数のモジュールを同時にうまく扱いたい場合は、それらがエクスポートしている関数の名前が同じなので、qualifiedインポートをする必要がある。つまり、以下のようにする。

    import qualified Geometry.Sphere as Sphere
    import qualified Geometry.Cuboid as Cuboid
    import qualified Geometry.Cube as Cube

これで、`Sphere.area`で球体の表面積が、`Sphere.volume`で球体の体積が、`Cuboid.area`で直方体の表面積...などなど、それぞれの物体に応じた表面積や体積が得られるようになった。

これからは、大量の関数が格納された巨大なスクリプトを作成するときは同じような目的の関数がないかを調べて、モジュールにまとめてみよう。そうすれば、自戒からは自分のプログラムで同じような機能が必要になったとき、自作したモジュールをインポートするだけで済むようになる。



# Making Our Own Types and Typeclasses

これまでの商では、Haskellの既存のtypeやtypeclassについて概観してきた。この商では、独自のtypeやtypeclassを作成し、それをどのように扱うのかを説明しよう。



## Algebraic data types intro

ここまで、`Bool, Int, Char, Maybe`などのデータtypeを扱ってきた。しかし、自分で定義するにはどうするんだろう? 一つの方法としては、`data`キーワードを用いる方法がある。では、`Bool`というtypeが標準ライブラリの中でどのように定義されているのか見てみよう。

    data Bool = False | True

`data`は、新しくデータtypeを定義する、という意味だ。`=`の前の部分は、これが`Bool`というtypeであることを示している。`=`の後ろは、値のコンストラクタだ。ここでは、typeが持つことのできる異なる値を指定している。`|`は、orと読み替えても良い。つまり、これは`Bool`というtypeは`True`または`False`という値を持つ、と読むことができる。typeの名前と値のコンストラクタは、どちらも大文字からはじめなければならない。

同様に、`Int`というtypeは、次のような定義になりそうだ。

    data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

値コンストラクタの最初と最後の値は、`Int`が取り得る値の最小値と最大値となるだろう。もちろん実際にこのような定義がされているわけではなく、説明のためにそう書いただけだ。

では、Haskellで図形を表現する方法を考えてみよう。ひとつは、タプルを使う方法だ。たとえば、円は、`(43.1, 55.0, 10.4)`と示すことができ、1、2番目は円の中心座標、3番目は円の半径となる。これは良さそうに見える。しかし、これでは3次元のベクトルなどと解釈されかねない。解決策としては、図形を表現する独自のデータtypeを作る、という手がある。では、円または長方形を表現するデータtypeを定義してみよう。

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float 

さて、これは何なのか? 次のように考えてみよう。まず、`Circle`というコンストラクタは、それぞれ`Float`の値を受け取る3つのフィールドを持つ。値コンストラクタの後には、任意で型を追加することができ、これは値コンストラクタが持つことのできる値の型を示している。ここでは、最初の2つが座標、残りが円の半径となる。そして、長方形の値コンストラクタは、`Float`のみ受け取る4つのフィールドを持つ。最初の2つは長方形の左上の座標を、残りの2つは右下の座標を示している。

ところで、私はフィールドをパラメータであるかのように説明した。実は、値コンストラクタは関数であり、最終的にそのデータtypeの値を返す関数となっている。では、これら2つの値コンストラクタのtype signatureを確認してみよう。

    ghci> :t Circle
    Circle :: Float -> Float -> Float -> Shape
    ghci> :t Rectangle
    Rectangle :: Float -> Float -> Float -> Float -> Shape

やばいね。値コンストラクタは本当にすべて関数だった。すごくない? 次は図形を受け取ってその表面積を返す関数を定義してみよう。

    surface :: Shape -> Float
    surface (Circle _ _ r) = pi * r ^ 2
    surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

まず注目すべきは、型宣言だ。`Shape`を受け取り`Float`を返す、と宣言している。なお、`Circle -> Float`と書くことはできないのは、`Circle`が型ではないからだ。これは`True -> Int`という型宣言ができないのと同じ理屈だ。次に注目すべきは、コンストラクタに対してパターンマッチングを行っている、というところだ。実は、コンストラクタに対して(なんと毎回)パターンマッチングを行っていたのだ。たとえば`[]`や`False`、`123`などという値はフィールドが1つもない。コンストラクタを書いてから、値を名前へと束縛していたのだ。ここで注目しているのは円の半径だから、円が座標上のどこにあるのか教えてくれる最初の2つのフィールドを考える必要はないのだ。

    ghci> surface $ Circle 10 20 10
    314.15927
    ghci> surface $ Rectangle 0 0 100 100
    10000.0

よし、動いた。しかし、たとえば`Circle 10 20 5`をプロンプトに直接表示しようとするとエラーになる。これは、Haskellに我々が作成したデータtypeを文字列でどのように表現すればよいのかを、伝えていないのが原因だ。思い出してほしい。Haskellのプロンプトにある値を表示させるときHaskellはその値の文字列表現を得るために、まずは`show`関数を実行するのだった。すると、その文字列が表示される。では、`Shape`を`Show`という型クラスに属するよう、以下のように変更しよう。

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

今のところは、まだ`deriving`について考えなくても良い。データ宣言の後ろに`deriving (Show)`というのを追加するだけで、Haskellが自動的にその型を`Show`という型クラスの一部にしてくれる、と考えることに仕様。つまり、こうなる。

    ghci> Circle 10 20 5
    Circle 10.0 20.0 5.0
    ghci> Rectangle 50 230 60 90
    Rectangle 50.0 230.0 60.0 90.0

さて、値コンストラクタは関数だから、マップしたり部分適用したり、なんでもできる。たとえば、半径が異なる同心円のリストがほしい場合、次のように求めることができる。

    ghci> map (Circle 10 20) [4,5,6,6]
    [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

このデータ型はよくできていると思う。が、まだ良くすることができる。では、2次元の空間上のある点を定義する中間的なデータ型を作ってみよう。これを使えば、`Shape`がより理解しやすくなる。

    data Point = Point Float Float deriving (Show)
    data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

`Point`を定義したときに、データ型の名前とコンストラクタの名前が同じである、ということに注目してほしい。これは特別な意味があるわけではなく、データ型とコンストラクタの名前を同じにすることは、値コンストラクタが1つしかない場合によく行われる。さて、`Circle`のフィールドは2つとなり、ひとつは`Point`、もうひとつは`Float`となる。これで、何をしているのかが理解しやすくなった。そして、長方形も同じようにする。この変更が反映される用、表面積を求める関数を調整する必要がある

    surface :: Shape -> Float
    surface (Circle _ r) = pi * r ^ 2
    surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

変更の必要があったのはパターンマッチングの箇所のみだった。円に対するパターンでは、すべての点を無視した。長方形に対するパターンでは、2点のフィールドを得るためにネストされたパターンマッチングを使用した。何かの理由で、2つの点に対する参照が欲しい場合には、`as`パターンを使うことができる。

    ghci> surface (Rectangle (Point 0 0) (Point 100 100))
    10000.0
    ghci> surface (Circle (Point 0 0) 24)
    1809.5574

図形を小突く関数はどうだろう?その関数は図形とx軸方向の移動量とy軸方向の移動量を受け取り、新しい図形を、次元は保ったまま、新しい場所に移動させたものを返す。

    nudge :: Shape -> Float -> Float -> Shape
    nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
    nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

かなり直感的だ。図形の位置を示す点に、小突いた量を加えている。

    ghci> nudge (Circle (Point 34 34) 10) 5 10
    Circle (Point 39.0 44.0) 10.0

`Point`を直接扱いたくなければ、ある大きさの原点に位置する図形を作成するための補助関数を作ってからそれらの図形を小突いてもいい。

    baseCircle :: Float -> Shape
    baseCircle r = Circle (Point 0 0) r
    
    baseRect :: Float -> Float -> Shape
    baseRect width height = Rectangle (Point 0 0) (Point width height)

    ghci> nudge (baseRect 40 100) 60 23
    Rectangle (Point 60.0 23.0) (Point 100.0 123.0)

もちろん、モジュール内にある自分で作ったデータ型を公開できる。そのためには、公開する関数と一緒にデータ型も記述してその後にかっこを加えて公開する予定の値コンストラクタをカンマ`,`で区切って記述する。もし、自作したデータ型に与えるすべての値コンストラクタを公開したい場合は、その部分を単にドット`...`と書く。

ここまで定義してきたデータ型や関数をモジュールにまとめて公開したい場合、以下のように書き始める。

    module Shapes
    ( Point(..)
    , Shape(..)
    , surface
    , nudge
    , baseCircle
    , baseRect
    ) where

`Shape(...)`とすると`Shape`の値コンストラクタをすべて公開する、という意味になるので、今作成したモジュールを読み込んだ人は誰でも`Circle`や`Rectangle`という値コンストラクタを使って図形を作ることができる。ちなみに、これは`Shape (Rectangle, Circle)`と書くのと同じだ。

また、値コンストラクタを1つも公開したくない場合は、エクスポート文に`Shape`と書くこともできる。すると、私達のモジュールを読み込んだ人は、`baseCircle`と`baseRect`という補助関数を使う場合のみ図形を作ることができるようになる。`Data.Map`は、この手法を使っている。たとえば、マップを`Map.Map [(1,2),(3,4)]`として作ることができないのは、値コンストラクタが公開されていないからだ。しかし、補助関数の一つである`fromList`を使うことでマップを作ることができる。思い出してほしい、値コンストラクタはただの関数であり、パラメータをフィールドとして受け取り、たとえば`Shape`などというtypeの値を結果として返す関数であることを。つまり、値コンストラクタを公開しないということはそれらの関数を使ってモジュールを読み込むのを防ぐことになるが、独自のデータ型を返す関数を公開していればその関数を経由して独自のデータ型を作ることができるのだ。

値コンストラクタを公開しないということは、独自のデータ型をより抽象的にして、内部の実装を隠すことになる。加えて、値コンストラクタに対して、私達のモジュールを使用する誰もパターンマッチングを行うことができなくなる。



## Record syntax

OK、じゃあ次は人物を表すデータ型を作る、というのをやってみよう。人物について保持したい情報は次の通り: 姓、名、年齢、慎重、電話番号、そして好きなアイスクリームの味だ。君のことは知らないが、私が他人について知りたいことといえばこれが全てだ。さあ、はじめよう。

    data Person = Person String String Int Float String String deriving (Show)

いいね。最初のフィールドが名、次が姓、その次が年齢、...となっている。では、人物を表す値を作ってみよう。

    ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
    ghci> guy
    Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

まぁこれで良いといえば良いかもしれないが、ちょっと読みづらい。では、人物から個々の情報を得るための関数を作成するとしたらどうだろう? 人物から姓を取り出す関数、名を取り出す関数、...などなど。そうだね、それらの関数は次のように定義する必要がある。

    firstName :: Person -> String
    firstName (Person firstname _ _ _ _ _) = firstname
    
    lastName :: Person -> String
    lastName (Person _ lastname _ _ _ _) = lastname
    
    age :: Person -> Int
    age (Person _ _ age _ _ _) = age
    
    height :: Person -> Float
    height (Person _ _ _ height _ _) = height
    
    phoneNumber :: Person -> String
    phoneNumber (Person _ _ _ _ number _) = number
    
    flavor :: Person -> String
    flavor (Person _ _ _ _ _ flavor) = flavor

はぁー、こんな書き方したくないんだよ! かなり面倒で退屈な書き方にもかかわらず、この方法は問題なく動作する。

    ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
    ghci> firstName guy
    "Buddy"
    ghci> height guy
    184.2
    ghci> flavor guy
    "Chocolate"

もっとマシな書き方があるに違いない、と思ったそこのあなた! ...ないんだな、これが。

いや、冗談、冗談。Haskellを開発した人達は非常に賢いからこのような状況になることは織り込み済みだ。彼らはデータ型を記述する別の方法を用意した。以下に、record syntaxを使用して、どのように上記の目的を達成するのかを示そう。

    data Person = Person { firstName :: String
                         , lastName :: String
                         , age :: Int
                         , height :: Float
                         , phoneNumber :: String
                         , flavor :: String
                         } deriving (Show)

、スペースで区切りながら1つ1つフィールドのtypeを命名する代わりに波括弧を使用する。まず、フィールド名(たとえば`firstName`など)を書いて、その後に2つのコロン`::`を続け、最後にtypeを指定する。このデータ型は、以前書いたものと全くおなじ結果になる。この方法のメリットは、フィールド名から値を探しだす関数がデータ型に作成されるところだ。データ型を作成するときにrecord syntaxを使うことで、Haskellは自動的に、`firstName, lastName, age, height, phoneNumber, flavor`という関数を作成してくれるのだ。

    ghci> :t flavor
    flavor :: Person -> String
    ghci> :t firstName
    firstName :: Person -> String

もう一つ、record syntaxを使う別の利点がある。データ型に`Show`を引き継がせると、record syntaxを使った場合は表示のされ方が異なり、型をインスタンス化する。たとえば、車を表現するデータ型があるとする。そこで、、、車の製造会社名、モデル名、そして製造された歳を追跡したい、としよう。

    data Car = Car String String Int deriving (Show)

    ghci> Car "Ford" "Mustang" 1967
    Car "Ford" "Mustang" 1967

record syntaxを使って定義した場合は、新しい車を以下のように作ることができる。

    data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

    ghci> Car {company="Ford", model="Mustang", year=1967}
    Car {company = "Ford", model = "Mustang", year = 1967}

新しい車を作るときは、すべての値を指定する場合に限り、フィールドに指定する値を順番に入力する必要はない。しかし、record syntaxを使わなければ、毎回フィールド名に値を順番通りに入力する必要がある。

record syntaxは、コンストラクタのフィールドが複数あって、どれがどれだか判断しづらい場合に使うとよい。たとえば、3次元のベクトルを作るために`data Vector = Vector Int Int Int`と定義しても、これはどのフィールドがどの要素なのか、というのは明らかだ。しかし、人物や車を表すdata typeは、どのフィールドがどの要素なのか明らかではないから、record syntaxの利点が最大限に享受できる。



## Type parameters

値コンストラクタは、ある値をパラメータとして受け取り、新しい値を生成する。たとえば、車のコンストラクタは、3つの値を受け取り、`Car`という新しい値を生成する。同様の方法で、型コンストラクタは、型をパラメータとして受け取り、新しい型を生成する。最初はこれが少々メタっぽく思えるかもしれないが、それほど複雑ではない。もし、C++のテンプレートに馴染みがあるのであれば、類似点がわかるだろう。型パラメータがどのように機能するのかを明らかにするために、既に見てきた型がどのように定義されているのかを見てみよう。

    data Maybe a = Nothing | Just a
i
これが型パラメータだ。そして、`a`という方パラメータがあるので`Maybe`を方コンストラクタと呼ぶ。`Maybe`に`Nothing`ではない、どのような型を保持させたいかにも依るが、この型コンストラクタは、最終的に`Maybe Int, Maybe Car, Maybe String,`などの新しい型を生成する。どんな値も`Maybe`という型を持つことができないのは、それ自身が型なのではなく、型コンストラクタだからだ。このため、ある値は実際に何らかの方の一部である必要があり、また、その型パラメータがすべて満たされている必要がある。

つまり、`Char`を型パラメータとして`Maybe`に渡せば、`Maybe Char`という型が得られる。たとえば`Just 'a'`という値は`Maybe Char`という型を持つのだ。

気づいていなかったと思うが、実は型パラメータを受け取る型を`Maybe`を紹介する前に使っていた。そう、リストという型だ。いくつかシンタックスシュガーはあるが、リスト型は具体的な型を生成するために、パラメータを受け取る。値の型は、`[Int]`や`[Char]`、`[[String], [String]]`などとなるが、値の型が`[]`となることはできない。

Maybe型を概観してみよう。

    ghci> Just "Haha"
    Just "Haha"
    ghci> Just 84
    Just 84
    ghci> :t Just "Haha"
    Just "Haha" :: Maybe [Char]
    ghci> :t Just 84
    Just 84 :: (Num t) => Maybe t
    ghci> :t Nothing
    Nothing :: Maybe a
    ghci> Just 10 :: Maybe Double
    Just 10.0

data typeに含めたい型によって異なる型を作ることができるから、型パラメータはとても便利だ。たとえば、`:t Just "Haha"`とすると、型推論エンジンがその型が`Maybe [Char]`であることを発見してくれるが、、`Just a`の`a`には文字列が入るので、`Maybe a`の`a`も同様に文字列型`[Char]`が入るのだ。

そして、`Nothing`の型が`Maybe a`となっているのに注目してほしい。これはpolymorphic typeだ。たとえばある関数がMaybeな値を受け取るとして、`Nothing`を受け取ったとしてもそれには何の値も入っていないので問題にはならない。`Maybe a`という型は、`123`が`Int`や`Double`として振る舞うのと同様に、`Maybe Int`としても、`Maybe Double`としても機能する。それと似たように、空のリストは`[a]`として振る舞う。空のリストは、どんな型のリストとしても振る舞えるのだ。これにより、`[1,2,3] ++ []`や`["ha","ha","ha"] ++ []`ということができる。

型パラメータを使うのはかなり有益であり、それらを使わない手はない。通常、あるデータ型の値が、型を持たなくても動作するようにしたい場合に型パラメータを使用し、たとえば`Maybe a`という型のように、型を内包させる。定義したい型がある種の箱とみなせるなら、型パラメータを使用すると良い。先程定義した車についてのデータ型を変更してみよう。

    data Car = Car { company :: String
                   , model :: String
                   , year :: Int
                   } deriving (Show)

変更後は、こうなる。

    data Car a b c = Car { company :: a
                         , model :: b
                         , year :: c
                         } deriving (Show)

しかし、これは本当に便利なのか? そうとはいえなさそうだ。なぜかといえば、以前は`Car String String Int`という方に対してのみ動作する関数を定義するだけで良かったからだ。たとえば、元々の車についてのデータ型はちょっとしたテキストを入力するだけで、車についての情報を表示する関数が簡単に作ることができた。

    tellCar :: Car -> String
    tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

    ghci> let stang = Car {company="Ford", model="Mustang", year=1967}
    ghci> tellCar stang
    "This Ford Mustang was made in 1967"

ちょっとした関数だけど、良い感じだ。型宣言も良い感じに機能している。さて、`Car a b c`は、どうしよう?

    tellCar :: (Show a) => Car String String a -> String
    tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

上記のように、関数に対して`(Show a) => Car String String a`という型を受け取るよう強制することになる。ご覧のとおり、型シグネチャがより複雑になり、実際に享受できる利便性は、`Car a b c`の`c`として、Showという型クラスのインスタンスであれば何でも受け入れられる、という店のみだ。

    ghci> tellCar (Car "Ford" "Mustang" 1967)
    "This Ford Mustang was made in 1967"
    ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")
    "This Ford Mustang was made in \"nineteen sixty seven\""
    ghci> :t Car "Ford" "Mustang" 1967
    Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t
    ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"
    Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]

現実の世界では、結局殆どの場面で`Car String String Int`という型を使うことになるから、車というデータ型が型を受け取れるようパラメータ化する価値は、あまりないと言えそうだ。通常、ある型の中に多数の値コンストラクタが含まれていて、それらの型がどう機能するのかが重要ではない時に型パラメータを使う。あるリストが何かのリストであるとき、その何かが、何であるのかについては問題とはならない。もし、 数値のリストからその合計を求めたいのであれば、合計を求める関数は数値のリストを受け取る、という具合に後で定義すれば良い。これはMaybeにも当てはまる。`Maybe`は、何かがある、または何もない、という選択肢を表現する。その何かというのがどんな方であるのかは問題にはならない。

既に紹介した中で、型がパラメータ化されている例といえば、`Data.Map`の`Map k v`だ。`k`はマップのキーとなる型であり、`v`は、それに対応する値の型となる。これは型パラメータがどれほど役立つかを示すには格好の例となる’。キーの型が`Ord`という型クラスの一部であるかぎり、方のパラメータ化は、多数の型から別の多数の型へと対応付けるのを可能にしてくれる。そして、マップの型を定義したいのであれば、データ宣言に型クラス制限を追加したい、と思うかもしれない。

    data (Ord k) => Map k v = ...

しかし、Haskellの根強い監修により、データ宣言の中では型の成約を追加することは不可能となっている。なぜかって? そうだね、そうする利点がないからだ。しかも、型の成約を加えたくなかったとしても、結局それを加える事になる。仮に`Map k v`のデータ宣言にキーが`Ord`に属する、という製薬を加えたとしても加えなかったとしてもマップのキーは`Ord`の一部であること、という成約を関数に加えなければいけなくなる。また、キーが順番を持つかどうかを気にしない関数であれば`(Ord k) =>`を関数の型宣言に加える必要はないし、その制約をデータ宣言に加える必要もない、ということになる。そのような関数の例としては`toList`がある。これはマップを受け取り連想リストへと変換する関数だ。その関数の型シグネチャは`toList :: Map k a -> [(k, a)]`となっている。もし、`Map k v`がデータ宣言の中で型の制約を設けていたら、`toList`は`toList :: (Ord k) => Map k a -> [(k, a)]`のようになるだろう。キーの順番を元に比較したりすることはない関数であるにもかかわらず、だ。

つまり、データ宣言の中に制約を追加する必要はない、ということであり、たとえ理にかなっているように見えたとしても、どの道、関数に型宣言を加えなくてはいけなくなるのだ。

では、3次元のベクトルを表す型を実装して、それを操作する関数も追加してみよう。通常、ベクトルが保持するのは数値の型となるからそれらに対応するために、ここではパラメータ化された型を使用する。

    data Vector a = Vector a a a deriving (Show)
    
    vplus :: (Num t) => Vector t -> Vector t -> Vector t
    (Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
    
    vectMult :: (Num t) => Vector t -> t -> Vector t
    (Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)
    
    scalarMult :: (Num t) => Vector t -> Vector t -> t
    (Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

`vplus`は、2つのベクトルを足し合わせる。2つのベクトルを足し合わせる、というのは、単純にそれぞれの要素を足し合わせればよい。`scalarMult`は、2つのベクトルのスカラー積を、`vectMult`は、ベクトルをスカラーで乗算した結果を求める。これらの関数は、`Int`のベクトル、`Integer`のベクトル、`Float`のベクトルなど、ベクトルが`Num`という型クラスに属してさえいればどんなベクトルに対する操作であっても行える。また、これらの関数の型宣言を確認すると、操作が行えるのは同じ型で構成されたベクトルのみであり、ベクトルが保持する数値の方も、その型と同じでなければならない、というのに気づくだろう。繰り返しになるが、`Num`というクラスに属する、という制約をデータ宣言に加える事ができないのは、そのデータ型を使用する関数でも同じ制約を加える事になるからだ。

ここでもう一度言おう。値コンストラクタと型コンストラクタの違いはとても重要である、と。データ型を宣言するときの、`=`の前にある部分が型コンストラクタであり、その後ろ、(あるいはパイプ`|`の後ろ)に続く部分が値コンストラクタとなる。たとえば、ベクトルを受け取る関数の方として、`Vector t t t -> Vector t t t -> t`という宣言をするのは誤りであり、なぜかといえばベクトルの値コンストラクタは3つのパラメータを受け取るのに対して、型コンストラクタが受け取るのは1つのみだからだ。では、今作ったベクトルを試してみよう。


    ghci> Vector 3 5 8 `vplus` Vector 9 2 8
    Vector 12 7 16
    ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
    Vector 12 9 19
    ghci> Vector 3 9 7 `vectMult` 10
    Vector 30 90 70
    ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
    74.0
    ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
    Vector 148 666 222



## Derived instances

Type classes 101という商では、型クラスの基本について学んだ。そこでは、型クラスというのは何らかの振る舞いを定義するある種のインターフェースである、と説明した。ある振る舞いをする型は、型クラスに対するインスタンスと言える。たとえば、`Int`という型は、`Eq`という型クラスのインスタンスであり、なぜかといえば型クラス`Eq`は等しさが比較できる、という振る舞いを定義するからだ。そして、`Intという型が、その方同士で等しさを比較できるのは型クラス`Eq`に属しているからだ。そして、型クラス`Eq`のインターフェースに付属する便利な関数といえば、、`==`と`/=`だ。たとえば、ある型が型クラス`Eq`に属していれば、`==`という関数をその型の値にたいして使用することができる。これが、`4 == 4`や`"foo" /= "bar"`に対して型検査ができる理由だ。

そして、Java、C++やPythonが備えているクラスとも異なるため、それが混乱を招いてしまい多くの人を落胆させるということにも言及した。それらの言語では、クラスはオブジェクトを作る際の設計図であり、そのオブジェクトというのはいくつかの状態を持ち、何らかの動きをするものだ。どちらかというと、型クラスはインターフェースだ。型クラスからデータを作ることはしない。その代わり、先にデータ型をつくり、その後にそれがどう振る舞うのかを考える。たとえば、それが等しさを比較できるようにしたければ、`Eq`のインスタンスとする。また、それが順番を持つようにしたければ、`Ord`という型クラスのインスタンスとすれば良い。

次は、型クラスによって定義された関数を実装することで、独自の方クラスのインスタンスとなる型を手動で作る方法を見ていこう。しかし、まずはEq, Ord, Enum, Bounded, Show, Read`という型クラスについて、ある型に対してHaskellがどのように、かつ自動的にインスタンス化を行なっているのかを見てみよう。あるデータ型を作るときに`deriving`キーワードを使うことで、、Haskellはその文脈内にて肩クラスの振る舞いを引き継がせる。

では、以下のデータ型を考えてみよう。

    data Person = Person { firstName :: String
                         , lastName :: String
                         , age :: Int
                         }

これは人物を表現している。次は、同姓同名かつ同じ年令の人物がいないことを前提としてみよう。そのとき、、2人の人物についてのレコードがあったとして、それが同じ人物を表していることが確認できるだろうか? もちろんできる。その2人に対して等しいか比較してみて、その結果を得ることができる。だから、以下のデータ型が型クラス`Eq`に属しているのは理にかなっている。属性をインスタンスへと引き継ぐのだ。

    data Person = Person { firstName :: String
                         , lastName :: String
                         , age :: Int
                         } deriving (Eq)

ある方に対して型クラス`Eq`をいんすたんすとして引き継がせ、ある方の値に対して`==`または`/=`で比較をすると、Haskellはまず値コンストラクタ(ここでは`Person`という1つの値コンストラクタ)が両方共一致するかを見て、それから両方の値が保持している中身について、それぞれのフィールドの組合せが一致するかを調べる。そして、これが上手くいくには、それぞれのフィールドの方は、すべて型クラス`Eq`に属している必要がある。しかし、両者は`Int`と`String`だから、問題ない。では、`Eq`のインスタンスをテストしてみよう。

    ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
    ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
    ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
    ghci> mca == adRock
    False
    ghci> mikeD == adRock
    False
    ghci> mikeD == mikeD
    True
    ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
    True

もちろん、`Person`は`Eq`に属しているから`Person`という型の値に対して、型シグネチャに`Eq`という型の制約がある関数は何でも、たとえば`elem`などを使うことができる。

    ghci> let beastieBoys = [mca, adRock, mikeD]
    ghci> mikeD `elem` beastieBoys
    True

`Show`と`Read`という型クラスは、それぞれある値を文字列へ、あるいは文字列からある値へ変換する`Eq`と同様に、ある方を`Show`と`Read`のインスタンスとした場合は、その型の型コンストラクタが持つフィールドも、それぞれ`Show`と`Read`に属している必要がある。では、`Person`というデータ型を`Show`と`Read`にも属させてみよう。

    data Person = Person { firstName :: String
                         , lastName :: String
                         , age :: Int
                         } deriving (Eq, Show, Read)

これで、人物データを表示できるようになった。

    ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
    ghci> mikeD
    Person {firstName = "Michael", lastName = "Diamond", age = 43}
    ghci> "mikeD is: " ++ show mikeD
    "mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"

もし、人物データの型を`Show`に属させる前に、ターミナルへ表示させようとしたら、Haskellは人物のデータを文字列で表現する方法を知らない、と文句をつけてきただろう。だが、もう`Person`に`Show`を引き継がせたから、怒られることはない。

`Read`は`Show`とは正反対の型クラスだ。`Show`は`Person`という型の値を文字列へと変換してくれるが、`Read`は文字列から値へと変換する。覚えているかもしれないが、`read`を使って文字列からある値へ変換するときは、Haskellに結果として返してほしい目的の方を伝えるために、明示的な型注釈をする必要があるのだった。明示的に、結果の型を指定しなければ、Haskellはどんな型の値を返せばよいのか判断できない。

    ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
    Person {firstName = "Michael", lastName = "Diamond", age = 43}

もし、`read`で読み取った結果を同じ型とともに後で使うのであれば、Haskellは文字列から読み取った値の型を推論できるので、明示的な型注釈をしなくてもよい。

    ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
    True

また、パラメータ化された方も読み取れるが、型パラメータをすべて満たす必要がある。つまり、`read "Just 't'" :: Maybe a`できないが、`read "Just 't'" :: Maybe Char`はできる。

さて、型クラス`Ord`をインスタンスとして引き継ぐことができその型の値は順番を持つのだった。もし、異なるコンストラクタで作られた2つの値を比較した場合、先に定義されたコンストラクタで作られた値のほうがより小さいと判断される。たとえば、値として`True`か`False`のどちらかを持つ`Bool`という型を考えてみよう。それぞれの値を比較した場合にどう振る舞うかを調べる目的のために、`Bool`が以下のように実装されているものと仮定しよう。

    data Bool = False | True deriving (Ord)

`False`という値コンストラクタが先に定義され、その後に`True`という値コンストラクタが定義されているので、`True`のほうが`False`より大きいと判断される。

    ghci> True `compare` False
    GT
    ghci> True > False
    True
    ghci> True < False
    False

`Maybe a`というデータ型は、`Just a`の前に`Nothing`という値コンストラクタが指定されているので、`Nothing`という値は、たとえ`Just a`の`a`がマイナス1億でも1兆でも関係なく常に`Nothing`のほうが小さいとみなされる。しかし、`Just a`という値同士を比較した場合は、中味の`a`の部分の値をもとに比較される。

    ghci> Nothing < Just 100
    True
    ghci> Nothing > Just (-49999)
    False
    ghci> Just 3 `compare` Just 2
    GT
    ghci> Just 100 > Just 50
    True

しかし、たとえば`Just (*3) > Just (*2)`のような比較は行えず、これは`(*3)`などの関数は、型クラス`Ord`に属していないからだ。

さて、`Enum`や`Bounded`という型クラスのおかげで、列挙可能な代数的データ型を簡単に作ることができる。では、以下のデータ型を考えてみよう。

    data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

すべての値コンストラクタがパラメータ、すなわちフィールドに引数を取らないので、型クラス`Enum`に属させることができる。型クラス`Enum`は、predecessorとsuccessorを持つもの、つまり前後関係があるものを対象とする。また、下限の値と上限の値を持つものに対しては型クラス`Bounded`に属することができる。では、上記で紹介したものに加えて、その他の継承可能な型クラスを引き継がせて、インスタンスを作ると、その結果がどうなるのか見てみよう。

    data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
               deriving (Eq, Ord, Show, Read, Bounded, Enum)

これは型クラス`Show`と`Read`に属しているから、このデータ型の値を文字列へ、あるいは文字列から変換できる。

    ghci> Wednesday
    Wednesday
    ghci> show Wednesday
    "Wednesday"
    ghci> read "Saturday" :: Day
    Saturday

型クラス`Eq`と`Ord`に属しているから、日付同士を比較できる。

    ghci> Saturday == Sunday
    False
    ghci> Saturday == Saturday
    True
    ghci> Saturday > Friday
    True
    ghci> Monday `compare` Wednesday
    LT

また、型クラス`Bounded`にも属しているから、曜日の最小値と最大値が得られる。

    ghci> minBound :: Day
    Monday
    ghci> maxBound :: Day
    Sunday

加えて、型クラス`Enum`にも属しているから、ある曜日の前後の曜日を得ることができるし、レンジを使って曜日のリストを作ることもできる。

    ghci> succ Monday
    Tuesday
    ghci> pred Saturday
    Friday
    ghci> [Thursday .. Sunday]
    [Thursday,Friday,Saturday,Sunday]
    ghci> [minBound .. maxBound] :: [Day]
    [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

いやあ、素晴らしいね。



## Type synonyms

以前、型の書き方について説明した時、`[Char]`というのは`String`と同じであり、交換可能であると説明した。これは、、型シノニムにより実装されている。型シノニム自体が何かすることはなく、単純にある型に対して異なる名前を与えるものであり、コードの可読性を向上させたり、ドキュメンテーションを読みやすくしたりする。以下に、`[Char]`の型シノニムとして`String`が標準ライブラリでどのように定義されているかを示す。

    type String = [Char]

`type`キーワードを紹介しよう。このキーワードはミスリードを誘う。なぜかといえば新しく型を作るには`data`キーワードが必要であり、これはあくまで既存の型の別名を作るのであり、新しい型を作ることはないからだ。

たとえば、文字列を大文字へと変換する`toUpperString`のような関数をつくるとしたら、その方宣言は`toUpperString :: [Char] -> [Char]`または、`toUpperString :: String -> String`となるだろう。どちらの関数も本質的には同じだが、後者のほうが読みやすい。

さて、以前`Data.Map`モジュールを扱ったときは、マップへと変換する前に、まずは電話帳を表現するものとして連想リストを作った。そして、連想リストというのはキーと値のペアで構成されたものである、というのを学んだ。ここで、その電話帳をもう一度見てみよう。


    phoneBook :: [(String,String)]
    phoneBook =
        [("betty","555-2938")
        ,("bonnie","452-2928")
        ,("patsy","493-2928")
        ,("lucille","205-2928")
        ,("wendy","939-8282")
        ,("penny","853-2492")
        ]

電話帳の型は`[String, String)]`となっている。これは、連想リストが文字列から文字列へと対応しているのを示しているが、リストについての情報はそれだけだ。では、型シノニムを使ってもう少し型宣言の情報が伝わるようにしてみよう。

    type PhoneBook = [(String,String)]

これで、電話帳の型宣言は`phoneBook :: PhoneBook`とすることができる。では、`String`の代わりにこの型シノニムを使ってみよう。

    type PhoneNumber = String
    type Name = String
    type PhoneBook = [(Name,PhoneNumber)]

Haskellプログラマーが、ある関数の中で使われている文字列が何を表現しているのかを明らかにしたいときに`String`という方に型シノニムを与える、という方法を使用する。

それでは、名前と番号を受け取って、その組合せが先ほどの電話帳に存在するか確認する関数を実装するとしよう。私たちはわかりやすくかつ説明的な型宣言を行うことができる。

    inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
    inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook


もし型シノニムを使わないと決めたのなら、関数は`String -> String -> [(String,String)] -> Bool`という型にしなくてはいけなくなる。そうすると、型宣言では、型シノニムのほうが理解しやすいというアドバンテージを得られる。しかし、型シノニムについて敏感になってはいけない。型シノニムは、関数の中に現れる既存の型について説明するものであり(そして、それが型宣言をより良いドキュメントへと変化させてくれるのであり)また、`[(String,String)]`を長々と繰り返すような型を持つ関数についてその型が何なのかを説明するものだという紹介をしたが、型シノニムは関数の文脈の中でより特定した何かを表すのだった。

型シノニムもパラメータ化できる。たとえば、連想リストを表す型がほしいとして、それをより汎用的にさせたい、つまり、どんな方でもキーやその値として使いたい場合には、以下のようにすることができる。

    type AssocList k v = [(k,v)]

さて、これで連想リストからキーをもとに値を得る関数の型は`(Eq k) => k -> AssocList k v -> Maybe v`となる。この`AssocList`は型コンストラクタであり、2つの型を受け取って、具体的な型、たとえば`AssocList Int String`という型を生成する。

？？？　「具体的な型の話をしたとき、あ、いや、それとpolymorphicな関数を扱うときにも話したっけ、具体的な型、っていうのは`Map Int String`みたいに全て適用された型っていう意味で話したんだ。ああ、それからさ、俺もたまに間違えるんだけど、あいつら`Maybe`は型だって言ってたでしょ? あれ間違ってて、型、じゃなくて型コンストラクタ、なんだよね。`Maybe Int`みたいに、なんか型を適用すれば具体的な型になるわけ。あとさ、知ってると思うけど、値の方ってのは具体的な型になるから。はい、じゃあ、まとめ!人生太く短く...じゃなくて、誰かと話すときは型と型コンストラクタを間違えない、間違えさせない。」

関数へ部分的に関数を与えれば、新しい関数が生成されるように、型コンストラクタに型パラメータを与えれば、新しい型が得られる。また、関数をパラメータが足りていない状態で呼び出せば新しい関数が得られうように、型コンストラクタへ部分的にパラメータを指定すると、部分適用された型コンストラクタが得られる。たとえば、(`Data.Map`の)マップについて、`Int`から何かへひも付けたければ、以下のように、どちらの方法を選択しても良い。

    type IntMap v = Map Int v

もしくは、以下のようにする。

    type IntMap = Map Int

どちらの方法にせよ、`IntMap`という型コンストラクタは、`Int`から何へ紐付けるか、という1つのパラメータを受け取る。

それから、コレを実装するなら`Data.Map`をqualified importすることになると思うが、qualified importするときは型コンストラクタのほうもモジュール名を先に付け加える必要がある。つまり、`IntMap = Map.Map Int`のようにする必要がある。


値コンストラクタと型コンストラクタの違いをしっかりと理解するようにしよう。なぜかと言えば、`IntMap`や`AssocList`という型シノニムを作ったが、`AssocList [(1,2),(4,5),(7,9)]`のようなことはできないからだ。これらが意味するのは、ある方を異なる名前で参照できるということだ。つまり、たとえば`IntMap`の場合、`[(1,2),(3,5),(8,9)] :: AssocList Int Int`のように`Int`型を前提として作ることになるが、これは、ただ単純に整数のペアを保持する普通のリストとしても使える。型シノニム(と一般的な型)は、、Haskellにおける型の一部だ。また、(`data`を使うときや型宣言をして新しい方を定義するときも常にHaskellの型の一部となり、また、`a ::`の後ろに位置するものは、すべて型となる。そう、コロン`::`を使うのは、型宣言か型注釈のときだ。

もうひとつ、2つのパラメータを受け取るデータ型として興味深いのが`Either a b`という型だ。こんな風に定義されている。

    data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

値コンストラクタが2つある。`Left`が使われれば、その中味である`a`という型が使われ、`Right`が使われれば、その中身である`b`という型が使われる。つまり、この型を使うことで、ある値の型をどちらか一方に包み隠して、`Either a b`という型の値を得ることになる。通常、`Left`と`Right`が異なるのを前提に、両方に対してパターンマッチングを行う。

    ghci> Right 20
    Right 20
    ghci> Left "w00t"
    Left "w00t"
    ghci> :t Right 'a'
    Right 'a' :: Either a Char
    ghci> :t Left True
    Left True :: Either Bool b

ここまで、`Maybe`は、計算の結果が失敗したか否かを表現する手段として役立つということを見てきた。しかし、`Maybe`でも十分ではない場合があり、なぜかと言えば、`Nothing`は何かが失敗した、ということ以上の情報を伝えてくれないからだ。関数の中に失敗する恐れのある箇所が1つしかない場合や、関数がなぜ、どのように失敗したのかについて関心が無いのであれば、その関数にとって、`Maybe`はとても便利だ。たとえば、`Data.Map`のマップ内に探しているキーが存在しない場合のみ、検索が失敗するので何が起きたのかについて完全に把握する必要はない。しかし、ある関数がなぜ、どのように失敗したのかについて関心がある場合は、通常、結果の型として`Either a b`というのを使い、`a`には失敗の原因を伝えるための型を`b`には、成功した計算結果の型を格納する。従って、結果には`Right`を使うので、エラーは値コンストラクタ`Left`を使う。

たとえば、ある高校には生徒がコミケの戦利品をしまっておくためのロッカーがあるとしよう。そして、それぞれのロッカーには番号がついている。生徒が新しいロッカーが必要になったことを管理人に伝えると、生徒は新しいロッカーの番号を教えてもらう。しかし、誰かがそのロッカーを既に使っていた場合、管理人はそのロッカーが使用中であると生徒に伝えて、別の使われていないロッカーを選ぶ。ということで、これは、ロッカーの番号とそれが使われているかどうかのペアを持つマップとなる。

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

分かりやすい。まず、ロッカーが使われているかを表現する新しいデータ型を作り、続いてロッカーの暗証番号を表現する型シノニムを作った。加えて、整数からロッカーの暗証番号と使用中かどうかを表すペアへとひも付けたマップの型シノニムも作った。では、ロッカーのマップから暗証番号を探す関数を作るとしよう。そこで、結果を表すのに`Either String Code`型を使用するが、コレはなぜかというと、この探索は失敗する場合が2通りあるからだ。ロッカーが使用中の場合は暗証番号を返すことはできないし、そもそもロッカーの番号が存在しないことだってある。そこで、探索が失敗した場合は、`String`を使って何が起きたのかを伝えるのだ

    lockerLookup :: Int -> LockerMap -> Either String Code
    lockerLookup lockerNumber map =
        case Map.lookup lockerNumber map of
            Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
            Just (state, code) -> if state /= Taken
                                    then Right code
                                    else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


まず、マップに対して通常の`lookup`を行う。その結果が`Nothing`であれば、ロッカーが存在しない、という文字列を`Left String`型の値として返す。もしロッカーが見つかれば、更にそのロッカーが使用中かどうかというのをチェックする。そして、ロッカーが使用中であれば、`Left String`として使用中である、という文字列を返す。また、ロッカーが使用中でなければ、`Right Code`という型の値として、ロッカーの番号に対応する暗証番号を生徒へと渡す。このとき、実際には結果が`Right String`となるが、事前に型宣言がドキュメントとなるように型シノニムを導入している。では、マップの例を見てみよう。

    lockers :: LockerMap
    lockers = Map.fromList
        [(100,(Taken,"ZD39I"))
        ,(101,(Free,"JAH3I"))
        ,(103,(Free,"IQSA9"))
        ,(105,(Free,"QOTSA"))
        ,(109,(Taken,"893JJ"))
        ,(110,(Taken,"99292"))

では、空いているロッカーを探してみよう。

    ghci> lockerLookup 101 lockers
    Right "JAH3I"
    ghci> lockerLookup 100 lockers
    Left "Locker 100 is already taken!"
    ghci> lockerLookup 102 lockers
    Left "Locker number 102 doesn't exist!"
    ghci> lockerLookup 110 lockers
    Left "Locker 110 is already taken!"
    ghci> lockerLookup 105 lockers
    Right "QOTSA"

もちろんロッカーを探した結果を`Maybe`で表現することもできるがなぜロッカーが見つからなかったのかという情報は得られない。しかし、上記のような型を使えば失敗の原因を伝えられる。



## Recursive data structures

既に見てきたように、代数的データ型のコンストラクタは、(全くフィールドがないか)幾つかのフィールドを持ち、それぞれのフィールドは具体的な型である必要があった。ということで、ある型のコンストラクタのフィールドをその型と同じにすることができるのだ。そうすると、再帰的なデータ型を作ることができ、ある方の値は、その型の値と同じ型の値を保持することができ、ある値が保持している値も、その値と同じ型の値を保持する、などということができる。

`[5]`というリストについて考えてみよう。これは、`5:[]`のシンタックスシュガーだ。コロン`:`の右側には値があり、左側にはリストがある。この場合、リストは空のリスト`[]`だ。では、`[4,5]`というリストについてはどうだろう? そう、これも`4:5:[]`のシンタックスシュガーだ。最初のコロン`:`を見てみると、その左側には`4`という値があり、その右側には`5:[]`というリストがある。同じことが`[3,4,5,6]`にも当てはまり、(コロン`:`は右結合だから)このリストは`3:4:5:6:[]`とも書くことができる。

つまり、リストは空のリストになることができ、コロン`:`を使って要素と別のリスト(空のリストでもそうでなくても良い)を結合することができる、といえる。

では、独自のリストを実装するのに、代数的データ型を使ってみよう。

    data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

これは、前の段落の内容をそのままコードにしてリストを定義したものだ。このデータ宣言は、空のリストか、ある値とリストの組合せのどちらかであることを表現している。これが何をしているのかわからなければ、record syntaxを使えば理解しやすくなるかもしれない。

    data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

`Cons`コンストラクタのせいで、また混乱させてしまったかもしれない。`Cons`は、コロン`:`の別名だ。ご覧のとおり、コロン`:`は、実際には値とリストを受け取り、新しいリストを返すコンストラクタだ。これで、新しい独自のリストが使えるようになった。言い換えれば、2つのフィールドを持ち、ひとつは`a`という型、もう一方は`[a]`という型である、と言える。

    ghci> Empty
    Empty
    ghci> 5 `Cons` Empty
    Cons 5 Empty
    ghci> 4 `Cons` (5 `Cons` Empty)
    Cons 4 (Cons 5 Empty)
    ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))
    Cons 3 (Cons 4 (Cons 5 Empty))

`Cons`コンストラクタをinfixにして呼び出すことができる。つまり、コロン`:`と同じようになる。`Empty`が空のリスト`[]`であり、`4 \`Cons\` (5 \`Cons\` Empty`が、`4:5:[]`に相当する。

特殊な文字を加える事で、自動的にその関数をinfixな関数として定義できる。また、同じことをコンストラクタに対しても行うことができる。では、そのデータ型を返す関数を定義してみよう。以下のとおりだ。

    infixr 5 :-:
    data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

まず最初に新しいシンタックスについて気づいたと思う。infixr宣言だ。ある関数を演算子として定義するとき、そこに`fixity`を加える(が、それだけでほかは何もしない。)`fixity`というのは、演算子がどれほど右結合、あるいは左結合に強く束縛されるかというのを示す。たとえば、乗算の関数`*`は`infixl 7 *`という強度、加算の関数`+`の強度は`infixl 6 +`となっている。これは、どちらも右結合であることを意味し、`(4 * 3 * 2)`は、`((4 * 3) * 2)`と等しいが、`fixity`の値が大きいため乗算`*`は、加算`+`より強力に束縛されるので、`5 * 4 + 3`は、`(5 * 4) + 3`となる。

そうしなければ、`Cons a (List a)`と書く代わりに`a :-: (List a)`と書く必要がアアル。ということで、上記のリストの型を使って、以下のようにリストを書き表すことができる。

    ghci> 3 :-: 4 :-: 5 :-: Empty
    (:-:) 3 ((:-:) 4 ((:-:) 5 Empty))
    ghci> let a = 3 :-: 4 :-: 5 :-: Empty
    ghci> 100 :-: a
    (:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))

上記の型のように`Show`を継承したとき、演算子をカッコで囲むと`4 + 3`は`(+) 4 3`となるから、Haskellは、コンストラクタがprefix関数であるかのように表示する。

では、我々のリストを結合する関数を作ってみよう。以下に、普通のコロン`:`がどのように定義されているか示す。

    infixr 5  ++
    (++) :: [a] -> [a] -> [a]
    []     ++ ys = ys
    (x:xs) ++ ys = x : (xs ++ ys)

では、上記の定義を拝借するとしよう。そして、関数の名前は`.++`とする。

    infixr 5  .++
    (.++) :: List a -> List a -> List a
    Empty .++ ys = ys
    (x :-: xs) .++ ys = x :-: (xs .++ ys)

動いているか確認数r。

    ghci> let a = 3 :-: 4 :-: 5 :-: Empty
    ghci> let b = 6 :-: 7 :-: Empty
    ghci> a .++ b
    (:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))

よしよし、良い感じだ。もし、そうしたければ、リストに対するすべての演算子は我々のリストに対しても実装することができる。

`(x :-: xs)`に対してどのようにパターンマッチングが機能するか注目してほしい。これが機能するのはパターンマッチングが実際にはコンストラクタに対して行われているからだ。`:-:`は我々のリスト型のコンストラクタだからパターンマッチングができる。同様に、`:`は最初から組み込まれたリスト型のコンストラクタだから、これに対してもパターンマッチングができる。また、同じことが空のリスト`[]`にも当てはまる。なぜかと言えば、パターンマッチングはコンストラクタ(のみ)に対して機能するからだ。そのようなコンストラクタや、普通の`Just 123`のようなコンストラクタ、あるいは`'a'`や`1.23`などのコンストラクタに対してもパターンマッチングは機能するが、後者は実際には文字`Char`や数値のコンストラクタとなっている。

さて、ここで二分探索木を実装するとしよう。もし、あなたがC言語などのユーザーかつ二分探索木についてよく知らないのであれば、以下の様なものだ、と考えよう。ある要素は、2つの要素を指し示し、1つは左側の要素、もう1つは右側の要素となる。また、それに対して左側の要素は小さく、右側の要素は大きい。加えて、左右の要素は(要素を持たないか、1つ、あるいは)2つの要素を指し示すことができる。結果として、ある要素は2つまで部分木をもつことができる。二分探索木の興味深い点は、すべての要素についての関係を知ることができる、という点であり、たとえば`5`という要素の左側にある部分木が持っている左側の要素はすべて`5`より小さくなる。また、右側の部分木が持っている右側の要素もすべて`5`より大きくなる。つまり、たとえば`8`という要素を探したい時、`5`より小さいから`5`という要素の右側へと進む。すると`7`に行き着くが、これも`8`より小さいから右側へと進む。これで、目的の要素へと3段階で辿りつけた。もし、これが普通のリスト(あるいは上記のルールに従っていない普通の木構造)であれば、3段階で探索できるところが、`8`という要素を見つけるのに7段階も必要になってしまう。

> メモ: 図の色が原因でよく見えなかった。おそらく
>
>        5
>       x 7
>    x x x 8
>
> という構造のツリーになっているはず。コレをそのままリストにすると`[5, x, 7, x, x, x, 8]`となるので、たしかに8が見つかるまで7ホップ必要。

`Data.Map`のマップや`Data.Set`の集合は、木構造を用いて実装されているが、普通の二分木の代わりに、常に2つの要素を持つ平衡二分木を使用している。しかし、ここでは普通の二分探索木を実装することにする。

以下のように実装するとしよう。あるツリーは、何も持たないか、あるいは、ある値の要素と、それに対応する2つのツリーを保持する。これは完璧な代数的データ型のように聞こえる。

    data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

いいね、良さそうだ。では、手動でツリーを構築する代わりに、ツリーと要素を受け取って、要素をツリーへと挿入する関数を作るとしよう。これを実現するには、ツリーのルートとなるノードと挿入したい要素の値を比較して、値が小さければ左側へ、大きければ右側へ進んでいく。これを空のツリーに到達するまでノードのノードへと順々に続ける。そして、空のツリーに到達した時点で、空のツリーの代わりに要素を挿入する。

たとえば、Cのような言語の場合は、ツリーに含まれる要素のポインタを変更することで、これを実現することになるだろう。しかし、Haskellでは、実際にはツリーを変更することはできないので、挿入用の関数が返す結果の新しいツリーに加えて、挿入用の関数が内部でツリーの左へ、あるいは右へ進む、と決めた瞬間にその都度内部では新しいツリーが生成されることになる。Haskellにはポインタの概念は存在せず、値のみが存在するからだ。従って、我々のツリーへ要素を挿入するための関数の型は、`a -> Tree a - > Tree a`のようになるだろう。要素とツリーを受け取って、その要素と同じ型を持つ新しいツリーを返すのだ。これは非効率的に思えるかもしれないが、HaskellのLazinessがその点の問題を解決してくれる。

そこで、2つの関数を用意する。ひとつはシングルトンツリー(要素を1つしか持たないツリー)を作るためのユーティリティ関数であり、もうひとつは、実際にツリーへ要素を挿入するための関数だ。

    singleton :: a -> Tree a
    singleton x = Node x EmptyTree EmptyTree
    
    treeInsert :: (Ord a) => a -> Tree a -> Tree a
    treeInsert x EmptyTree = singleton x
    treeInsert x (Node a left right)
        | x == a = Node x left right
        | x < a  = Node a (treeInsert x left) right
        | x > a  = Node a left (treeInsert x right)

`singleton`関数は、要素と空のツリー2つを含むツリーを作るための単なるショートカットだ。挿入用の関数では、まず境界条件となるパターンを定義する。空の部分木に到達したら、それは目的のツリーに到達したことを意味するから、その部分を空のツリーの代わりに挿入したいシングルトンツリーへと置き換える。また、空のツリーではないツリーへ要素を挿入する場合は、いくつか検査をする必要がある。まず、ツリーのルート要素と挿入したい要素が等しい場合、挿入前と同じツリーを返す。もし、要素のほうが小さければ、挿入対象のルート要素とツリーの右側の部分木はそのままに、左側の部分木に目的の要素が挿入された状態のツリーを返す。同じことを(同じ方法ではないが)、挿入したい要素が大きい場合に右側の部分木に対しても行う。

次は、ある要素がツリー内に存在するか探索する関数を作るとしよう。まずは、境界条件から定義する。もし空のツリーに対してある要素が存在するか検査した場合は、確実に要素は見つからない。いいね。ところで、これはリストに対して要素を探す場合と同じ境界条件となっていることに気づいただろうか。空のリストに対して要素を探したとしても、必ず見つからない。おっと、話を戻そう。要素を探す対象が空のツリーではない場合、いくつか検査を行う必要がある。もし、ルート要素が探している要素と一致すれば、それが答えだ。そうでなければ、さて、どうしよう? ここで、ツリー内の左側の要素は必ずルート要素より小さい、というルールの恩恵が得られる。探している要素がルート要素より小さければ、左側の部分木の要素と比較する。同様に、ルート要素より大きければ、右側のツリー内の要素に目的の要素があるかを調べる。

    treeElem :: (Ord a) => a -> Tree a -> Bool
    treeElem x EmptyTree = False
    treeElem x (Node a left right)
        | x == a = True
        | x < a  = treeElem x left
        | x > a  = treeElem x right

前の段落の内容をそのままコードとして書くだけで良かった。では、我々が作ったツリーのおもしろさを確認してみよう。(もちろんそうしても良いが)手動でツリーを構築する代わりに、`foldr`を使ってリストからツリーを生成してみよう。リストの中味を1つ1つ取り出して、最終的にある種の値をえる方法としてはfoldが使えることを思い出してほしい。まず空のツリーから開始して、リストから要素を取り出すたびに、それをツリーへと挿入して、その挿入済みのツリーをaccumulatorとして次のステップで使用するのだ。

    ghci> let nums = [8,6,4,1,7,3,5]
    ghci> let numsTree = foldr treeInsert EmptyTree nums
    ghci> numsTree
    Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

上記の`foldr`では、`treeInsert`が、(要素とツリーを受け取って、ツリーを生成する)畳み込み用の関数であり、`EmptyTree`がaccumulatorの初期値であり、`nums`はもちろん、ツリーへと変換させたいリストとなっている。

コンソールへ生成されたツリーを表示させてみると、とても読みやすいとは言えない。しかし、ツリーの構造がどうなっているのか、というのは把握できる。たとえば、ルート要素は`5`であり、また、部分木を2つ持ち、一方はルート要素が`3`のツリー、もう一方はルート要素が`7`のツリーである、などというのが読み取れる。

    ghci> 8 `treeElem` numsTree
    True
    ghci> 100 `treeElem` numsTree
    False
    ghci> 1 `treeElem` numsTree
    True
    ghci> 10 `treeElem` numsTree
    False

ツリーに要素が含まれるかを調べる関数も機能している。素晴らしいね。

ご覧のとおり、代数的データ構造は、Haskellにおける強力な機能だ。これを使えば、真偽値から曜日を表す列挙型、そして二分探索木までなんでも作ることができるのだ。



## Typeclasses 102

ここまで、Haskellの標準的な型クラスについて、また、それに含まれる型について学んできた。そして　、Haskellに依頼することで、標準の型クラスを自分で定義した型にたいして自動的に継承させる方法も学んだ。この節では、手動で独自の型クラスを定義してから、ある型をその型クラスのインスタンスにする方法を学ぶことにしよう。

では、軽く型クラスについておさらいしよう。型クラスはインターフェースのようなものだ。型クラスは、(等しさを比較したり、順番を比較したり、列挙できるようにするなどの)振る舞いを定義し、それに従う振る舞いをする型は、その型クラスのインスタンスとなる。型クラスの振る舞いは、ある関数を定義したり、型宣言を実装したりすることで達成される。つまり、ある型がある肩クラスのインスタンスである、といった場合、それは型クラスが定義する関数をその肩に対して使えることを意味する。

JavaやPythonなどのクラスとは異なり、Haskellの型クラスは実際のところ何もしない。これが多くの人を混乱させる。ここからは、命令的言語のクラスについて知っていることの全てを完全に忘れてほしい。

たとえば、`Eq`という型クラスは、等しさを比較するための型クラスだ。また、`Eq`は`==`と`/=`という関数を定義する。車を表す型があり、2つの車に対して等しさを調べる関数`==`で比較するのは理にかなっている。そして、車という型が`Eq`という型クラスのインスタンスとなるのも理にかなっている。

以下に、`Eq`という型クラスが標準モジュールの`Prelude`でどのように定義されているかを示す。

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)

おっと、幾つか見慣れない奇妙な文やキーワードが登場した。が、大丈夫だ。すぐにこれらの意味は明らかになる。まず、最初に`class Eq a where`というのを書く。これは、新しく`Eq`という名前の型クラスを定義する、という意味だ。`a`は型変数であり、これが`Eq`という型クラスのインスタンスである型がどんな役割をするのかを示す。また、`a`の部分を`a`のように1文字で書く必要はないが、この部分は小文字で書く必要がある。続いて、幾つかの関数を定義する。ここで関数の本体を実装するのは必須ではないが、関数の型宣言だけは、必ず指定し無くてはならない。

ちなみに、等しさを表現する`Eq`というクラスを書いた後に`(==) :: equatable -> equatable -> Bool`という型宣言をするほうが良いと考える人もいるだろう。それでも問題ない。

おっと、話を戻そう。ところで、`Eq`が定義する関数について、我々はその関数の本体を実装したが、関数は、それぞれ相互に再起している。2つの型クラス`Eq`のインスタンスに対して、それらが等しくなければ、それらは異なり、それらが異ならなければ、それらは等しいということを言っている　。実際、こんなことをする必要はないが、後にこれが我々を助けてくれることに気づくだろう。

もし、`class Eq a where`と書いた後に、そのクラスの方宣言の中で、`(==) :: a -> -a -> Bool`のように定義したとして、その後に関数の型を調べたとしたら、以下の様な型`(Eq a) => a -> a -> Bool`を持つことになるだろう。

さて、クラスを得たのは良いが、これで何ができるんだろう? そうだね、実は何もできない。しかし、その型クラスのインスタンスを作り始めた瞬間から、いくつかの便利な機能を享受できるようになる。では、以下の型を見てみよう。

    data TrafficLight = Red | Yellow | Green

上記は、信号の状態を定義している。ここで、インスタンスとして型を一切継承していないことに注目してほしい。`Eq`や`Show`などの型クラスを継承しようと思えばできるのにもかかわらず、そうしないのはインスタンスをこれから主導で書き表すからだ。以下に、それらを`Eq`のインスタンスとする方法を示す。

    instance Eq TrafficLight where
        Red == Red = True
        Green == Green = True
        Yellow == Yellow = True
        _ == _ = False

手動で行うには、`instance`キーワードを使う。つまり、`class`は、新しく型クラスを定義するために`instance`は、その型クラスのインスタンスとなる型を定義するために使用する。

以前、型クラス`Eq`を定義したとき、`class Eq a where`のように書いて、`a`が、その型クラスのインスタンスである型を代表して、どんな型としても振る舞う、ということを言った。ここで、その背景が明らかになった。インスタンスを作るときに`instance Eq TrafficLight where`のように書くからだ。

型宣言の中で、`==`は、`/=`を反対にしたものとして定義されているから、インスタンス宣言の中で、それらのどちらか一方を上書きすれば良い。これが、最小の実装し無くてはならない関数であり、これが、最小で完全な型クラスの定義となる。つまり、我々の型は、肩クラス`Eq`のインスタンスとして振る舞うことができるのだ。上記のように、`Eq`を依り単純に定義する場合は、型クラス`Eq`の最小限の定義を完全に満たすためには、`==`または`/=`のどちらかを上書きすればよい。しかし、以下の場合、

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool

Haskellは、これら2つの関数がどのように関係しているのかを知らないので、この型とこの型のインスタンスを作るときは、それぞれの関数を両方とも実装する必要がある。ということで、最小かつ完全な定義は、`==`と`/=`ということになる。

さて、パターンマッチングにより、`==`を単純に実装できる、というのがわかったと思う。複数のケースが考えられるので、2つの信号が等しくない場合ではなく、さきに2つの信号が等しい場合を定義してから、前のケースに一致しなかったものすべてをパターンマッチングにより捉えることで、2つの信号が異なる場合を定義することができるのだ。

では、`Show`のインスタンスを主導で作ってみよう。`Show`の最小で完全な定義を満たすために、`show`という値を受け取り、それを文字列へと変換する関数のみを定義する必要がある。

    instance Show TrafficLight where
        show Red = "Red light"
        show Yellow = "Yellow light"
        show Green = "Green light"

目的を達成するためにはパターンマッチングを使うのだった。では、これがどのように機能するか見てみよう。

    ghci> Red == Red
    True
    ghci> Red == Yellow
    False
    ghci> Red `elem` [Red, Yellow, Green]
    True
    ghci> [Red, Yellow, Green]
    [Red light,Yellow light,Green light]

いいね。(手動は面倒なので、実際には`deriving`を使うことになるが)これで、`deriving`で`Eq`を継承した場合と同じ効果を得ることができた。しかし、`Show`の継承については、直接値コンストラクタを値へと変換することになる。そのため、たとえば、信号の例のように`Red`を`Red Light`のように表示させたければ、手動でインスタンス宣言をする必要がある。

また、他の型クラスのサブクラスの型クラスを定義することもできる。型クラス`Num`の宣言はやや長くなるが、最初の部分は以下のようになる。

    class (Eq a) => Num a where

以前にも触れたとおり、クラスの制約を詰め込める箇所は、たくさんある。つまり、`class Num a where`のように書くこともでき、`a`という型は、`Eq`のインスタンスでなければならない、というのを表明できる。`Num`のインスタンスを作る前に`Eq`のインスタンスを作らなければならない、という本質的なところに変わりはない。ある型が数値として扱えるかを考慮する前に、どの型の値が等しさを判定できるのか否かを決定するというのは、理にかなっている。サブクラスについて言えることは以上だ。これは、単なるクラス宣言におけるクラスの制約だ。クラス宣言の中で関数の本体を定義するときやインスタンス宣言を定義するときに、`a`という型は、`Eq`に属している前提となるから、その型の値に対して`==`を使うことができる。

しかし、`Maybe`やリスト型を型クラスのインスタンスにするには、どうするんだろう? `Maybe`と`TrafficLight`の違いはなにかといえば、`TrafficLight`は`Maybe`を内包しており、それ自体は具体的な方ではない、という点だ。(`Char`などの型を受け取って)具体的な型(`Maybe Char`などの型)を生成したりする、1つの型パラメータを受け取る型コンストラクタに過ぎないのだ。ここで、もう一度`型クラス`Eq`を確認してみよう。

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)

関数の中では、すべての型が具体的な型である必要があるから、上記の型宣言における`a`は、具体的な型として扱われる、ということを学んだ。(たとえば、関数の型を`a -> Maybe`とすることはできないが`a -> Maybe a`や、`Maybe Int -> Maybe String`などと定義することはできる、というのを思い出してほしい。)そのため、以下の様なことはできない。

    instance Eq Maybe where

なぜかと言えば今まで見てきたとおり、`a`は具体的な型でなければならないのに、`Maybe`は具体的な型ではないからだ。`Maybe`は、型パラメータを1つ受け取って具体的な型を生成する型コンストラクタだ。しかし、たとえば型クラスを作るたびに`instance Eq (Maybe Int) whereや、`instance Eq (Maybe Char) where`などというのを一々書くのは退屈極まりないだろう。ということで、これは以下のように記述することができる。

    instance Eq (Maybe m) where
        Just x == Just y = x == y
        Nothing == Nothing = True
        _ == _ = False

これは、すべての`Maybe 何か`という具体的な型を型クラス`Eq`のインスタンスにする、という意味になる。ということで、本当に`(Maybe something)`と記述することもできるが、Haskellの監修に習って、1文字を選択するのが普通だ。`(Maybe m)`が`a`に相当し、`class Eq a where`という役割を果たす。`Maybe`は具体的な型ではないから、`Maybe m`とするのだ。型パラメータを(小文字で)指定することにより、`m`はすべての型を意味し、`Maybe m`という具体的な型をすべて型クラス`Eq`のインスタンスにしたい、ということが伝えられる。

ただし、この方法には1つ問題がある。何が問題か、発見できたかな? そう、`Maybe`の中味に対して`==`を使っているが、`Maybe`の中味を`Eq`のそれに対して使える、という保証はできないのだ。　そこで、上記のインスタンス宣言を以下のように変更する必要がある。

    instance (Eq m) => Eq (Maybe m) where
        Just x == Just y = x == y
        Nothing == Nothing = True
        _ == _ = False

そう、クラス制や区を追加する必要があったのだ。この宣言は、次のようなことを言っている。すべてのMaybe型を、`Maybe m`という形で、型クラス`Eq`のインスタンスにする。ただし、`Maybe`の中味の型として保持するのは、型クラス`Eq`に属する型のみとする。これは、実際にはHaskellが型クラスを`deriving`したときに行っていることだ。

多くの場面で、クラス制約やクラス宣言は、ある肩クラスを別の型クラスのサブクラスにするときに使用し、インスタンス宣言におけるクラス制約はある型の中味を要求していることを表明するために使用される。たとえば、上記のように、`Maybe m`が`Eq`に属するとともに、その中身の`m`も`Eq`に属する、というのを指定することができる。

インスタンスを作るとき、たとえば、`a`が`a -> a -> Bool`のように、型宣言の中で型が具体的な型として使われているのを見つけたらカッコを追加して、型パラメータを指定する必要がある。そうして、最終的に具体的な型にするのだ。

クラス宣言では、ある型をインスタンスにしようとしている箇所は、パラメータとして置き換えられないかを検討してほしい。たとえば、`class Eq a where`の`a`というのは、インスタンスにしようとしている具体的な型で置き換えられるから、その型は関数の型宣言に移動できないかを考慮に入れてほしい。それから、`(==) :: Maybe -> Maybe -> Bool`というのは機能しないが、`(==) :: (Eq m) => Maybe m -> Maybe m -> Bool`というのは機能する。しかし、これについてはいくつか考慮すべきことがありなぜかといえば、`==`は常に`(==) :: (Eq a) => a -> a -> Bool`という型になるから、作ろうとしているインスタンスは問題にはならない。

おっと、最後にもう一つ、確認することがある。もし、型クラスのインスタンスとして何があるのかを確認したければ、ghciに`:info 型クラス名`と入力する。たとえば、`:info Num`とん有力すれば、その型クラスが定義している関数と、その型クラスに属している型のリストが表示される。また、`:info`は、型や型コンストラクタに対しても機能する。たとえば、`:info Maybe`と入力すると、Maybeがどの型クラスのインスタンスなのか、という型クラスのリストが表示される。加えて、`:info`は関数の型宣言についても表示してくれる。いやぁ、便利だね。



## A yes-no typeclass

JavaScriptなどの弱い肩づけがされた言語ではif文にほぼなんでも置くことができる。たとえば、以下はすべて可能だ。

    if (0) alert("YEAH!") else alert("NO!")
    if ("") alert ("YEAH!") else alert("NO!")
    if (false) alert("YEAH") else alert("NO!)

そして、これらはすべて`"NO!"`というアラートを表示する。JavaScriptは空の文字列`""`を真らしい値として扱うからだ。

    if ("WHAT") alert ("YEAH") else alert("NO!")

ということで、上記は`"YEAH!"`とアラートが表示される。

もちろん、Haskellでは真偽値として厳密には`Bool`を使うほうが良いが、ここではJavaScriptっぽい振る舞いを実装してみよう。まずはクラス宣言からはじめる。

    class YesNo a where
        yesno :: a -> Bool

単純だ。型クラス`YesNo`は、ひとつの関数を定義する。この関数は、真らしいものとかんがえられる型の値を受け取り、それが`True`か否かを確認して表示してくれる。ところで、関数の中で`a`というのを使っているが、`a`は具体的な型となるので気をつけよう。

次は、いくつかインスタンスを定義しよう。数値については、JavaScriptっぽく、0ではない数値はすべて真らしいものとみなし、0については、偽らしいものとみなす。

    instance YesNo Int where
        yesno 0 = False
        yesno _ = True

空のリスト(に加えて文字列)は、noっぽい値、空ではないリストはyesっぽい値とする。

    instance YesNo [a] where
        yesno [] = False
        yesno _ = True

リストの中味の型については何の仮定もしていないにもかかわらず、リストを具体的な型にするため、そこへ型パラメータを置いていることに注目してほしい。他には、。`Bool`自身は、真らしいものと偽らしいものを保持しており、どちらがどちらなのか明確だ。

    instance YesNo Bool where
        yesno = id

はァ? `id`って何だ?`id`はHaskellの標準ライブラリに含まれる関数であり、受け取ったパラメータと全く同じものを返す。つまり、今まで書いていたものとおなじになる。

では、`Maybe a`をインスタンスにしてみよう。

    instance YesNo (Maybe a) where
        yesno (Just _) = True
        yesno Nothing = False

Maybeの中味については特に仮定していないので、クラス制約を設ける必要はない`Just`という値は真らしいもの、`Nothing`という値は偽らしいものとするだけでよい。また、ここでも`(Maybe a)`の代わりに`Maybe`と記述することはできない。(`Maybe`は具体的な型ではないので)ご想像の通り、`Maybe a -> Bool`というのは全く問題ないが、`Maybe -> Bool`という関数は存在できないからだ。しかし、これは素晴らしい。`Maybe something`の`something`の部分がどんな型であるかは、問題にはならないからだ。

ところで、以前、二分探索木を表現するために、ツリーの肩`Tree a`というのを定義した。空のツリーについては偽らしいもの、それ以外のツリーについては真らしいものと言えそうだ。

    instance YesNo (Tree a) where
        yesno EmptyTree = False
        yesno _ = True

では、信号を表す肩もyesかnoを表す値になれるだろうか? もちろんだ。赤は止まれ、青は進め、黄色は...私は生き急いでいるから進む。

    instance YesNo TrafficLight where
        yesno Red = False
        yesno _ = True

いいね。では、いくつかインスタンスが揃ったので、これらを試してみよう。

    ghci> yesno $ length []
    False
    ghci> yesno "haha"
    True
    ghci> yesno ""
    False
    ghci> yesno $ Just 0
    True
    ghci> yesno True
    True
    ghci> yesno EmptyTree
    False
    ghci> yesno []
    False
    ghci> yesno [0,0,0]
    True
    ghci> :t yesno
    yesno :: (YesNo a) => a -> Bool

よしよし、良い感じに動いている。では、if文を真似る関数を定義してみよう。

    yesnoIf :: (YesNo y) => y -> a -> a -> a
    yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

かなり直感的だ。この関数は、yes-noな値と、2つの値を受け取る。yes-noな値がyesっぽければ、2つの値の中から最初の方を返す。それ以外の場合は、2番目の値を返す。

    ghci> yesnoIf [] "YEAH!" "NO!"
    "NO!"
    ghci> yesnoIf [2,3,4] "YEAH!" "NO!"
    "YEAH!"
    ghci> yesnoIf True "YEAH!" "NO!"
    "YEAH!"
    ghci> yesnoIf (Just 500) "YEAH!" "NO!"
    "YEAH!"
    ghci> yesnoIf Nothing "YEAH!" "NO!"
    "NO!"



## The Functor typeclass

ここまでで、多くの標準ライブラリにある肩クラスと遭遇してきた。たとえば、`Ord`は、何か順番を持つものを表す。`Eq`も習った。これは、何か等しさが比較できるものを表す。`Show`は、ある肩の値を文字列で表現するためのインターフェースを提供する。`Read`は文字列からある型の値へと変換してくれる、我々の友達だ。では、ここからはあるものがマッピングされることができることを表す型クラス`Functor`について見ていくことにしよう。マッピング、と聞いてリストを思い浮かべたかもしれないが、リストはHaskellにおいて重要なイディオムだ。実際その通りで、リストはファンクタに属している。

型クラス`Functor`を知って、どのように実装されているかを理解する良い方法は何だろう? ちょっと除いてみることだ。

    class Functor f where
        fmap :: (a -> b) -> f a -> f b

これだけだ。`fmap`という関数が定義されているが、その関数のデフォルトの実装は含まれていない。`fmap`の型は興味深い。ここまでの型クラスにおける定義では、　`(==) :: (Eq a) => a -> a -> Bool`における`a`のように、　型変数は、型クラスにおける具体的な型となる役割を担っていた。しかし、`f`は(`Int`や`Bool`、`Maybe String`などの値を持つことができる)具体的な型ではなく、1つのパラメータを受け取る型コンストラクタとなっている。　何度もしつこいようで恐縮だが、`Maybe Int`などは具体的な型であり、`Maybe`は、パラメータとして1つの型を受け取る型コンストラクタだ。話を戻そう。`fmap`は、ある型から別のある型にマップする関数を受け取り、ファンクタがある型を適用し、ファンクタが別のある型を適用したものを返す。

なんだか紛らわしく聞こえるかもしれないが、心配しないで欲しい。いくつか例を確認してから、また見直すことに仕様。うーむ、しかし、この`fmap`の肩宣言はなにかひっかかる。`map`の肩シグネチャは`map :: (a -> b) -> [a] -> [b]`となっている。

ああ、なるほど。`map`は、ある型から別の型にする関数と、ある型のリストを受け取り、別の型のリストを返している。これはまさしく私達の友達であるファンクタではないか。事実、`map`というのはリストにたいする`fmap`といえる。以下に、リストが型クラス`Functor`のインスタンスであることを示す。

    instance Functor [] where
        fmap = map

以上だ。ここで、`instance Functor [a] where`のように記述できないことに注目してほしい。なぜなら、`fmap :: (a -> b) -> f a -> f b`と、定義されており、`f`は1つの型を受け取るコンストラクタとなっているからだ。`[a]`は既に(中味にどんな型でも保持できる)具体的な型となっているが、`[]`は1つの型を受け取って`[Int]`や`[String]`、あるいは`[[String]]`さえも生成する型コンストラクタとなっている。

リストに対する`fmap`は`map`と同じだから、リストに対してそれを使っても同じ結果が得られる。

    map :: (a -> b) -> [a] -> [b]
    ghci> fmap (*2) [1..3]
    [2,4,6]
    ghci> map (*2) [1..3]
    [2,4,6]

では、空のリストにたいして`map`か`fmap`すると、何が起きるだろう?そうだね、空のリストが帰ってくるだけだ。 [a]`という型の空のリストを`[b]`という型の空のリストに変えるだけだ。

ある種の箱とみなせる型は、ファンクタとして振る舞う。リストは仕切りが無限に入れられる箱のようなものと考えることができる。その箱は、空でもいいし、仕切りの1つはいっぱいになっているが、ほかは空でもいいし、すべてがいっぱいになっていても良い。では、他に箱のようなものとみなせるものは何があるだろう? その一つが`Maybe`だ。`Maybe`は、何もない、というのを持つことができる箱とみなせて、その場合保持する、値は`Nothing` となり、あるいは1つのアイテム、たとえば値が`Just "HAHA"`となる`"HAHA"`というものを保持することができる。では、`Maybe`がファンクタであることを確認してみよう。

    instance Functor Maybe where
        fmap f (Just x) = Just (f x)
        fmap f Nothing = Nothing

もう一度、`instance Functor Maybe where`ではなく`Maybe`や`YesNo`を扱ったときと同様に`instance Functor (Maybe m) where`となっていることに注目してほしい。ファンクタは、具体的な型ではなく、パラメータを1つ受け取る型コンストラクタを求める。もし、`f`をすべて`Maybe`に置き換えたくなったら、`fmap`は、この型に対して　`(a -> b) -> Maybe a -> Maybe b`のように振る舞う。これは良さそうだ。しかし、`f`を`(Maybe m)`で置き換えた場合、これは`(a -> b) -> Maybe m a -> Maybe m b`のように振る舞うと見なされ、意味を成さなくなってしまう。なぜなら、`Maybe`は1つの型パラメータしか受け取らないからだ。

ところで、`fmap`の実装は至ってシンプルうだ。`Nothing`という空の値であれば、単に`Nothing`を返す。空のリストに対してマッピングすれば空のリストが得られるように、空の箱に対してマッピングをすれば、空の箱が得られる、というのは理にかなっている。もし、空の値でなければ、と言うより、`Just`に単一の値が詰め込まれていたら、`Just`の中味の値に対して関数を適用する。

    ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
    Just "Something serious. HEY GUYS IM INSIDE THE JUST"
    ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
    Nothing
    ghci> fmap (*2) (Just 200)
    Just 400
    ghci> fmap (*2) Nothing
    Nothing

他に`Functor`のインスタンスとなり、マッピングできるものと言えば、`Tree a`という型がある。(値がないか、いくつかの値を保持するので)これはある種の箱とみなすことができ、たしかに`Tree`という型のコンストラクタは1つの型パラメータを受け取る。もし、`fmap`が`Tree`に対してのみ機能する関数だとしたら、`fmap`の肩シグネチャは`(a -> b) -> Tree a -> Tree b`のようになるだろう。これについては、再帰を使うことになる。空のツリーに対するマッピングは、空のツリーを生成する。空ではないツリーに対してマッピングすると、ルートの値と左右のサブツリーに関数を適用した値を保持するツリーになり、その左右のサブツリーもマッピングされたツリーになる。

    instance Functor Tree where
        fmap f EmptyTree = EmptyTree
        fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

    ghci> fmap (*2) EmptyTree
    EmptyTree
    ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])
    Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree

いいね。では、`Either a b`はどうだろう? これはファンクタになれるのか? 型クラス`Functor`は、1つの型パラメータを受け取る肩コンストラクタを要求するが、`Either`は2つ受け取る。いや、1つにする方法は知っているぞ。そう、`Either`へ部分的にパラメータを与えれば1つが空く。以下に、標準ライブラリでは`Either`がファンクタであることを示す。

    instance Functor (Either a) where
        fmap f (Right x) = Right (f x)
        fmap f (Left x) = Left x

ちょっと待った。
これは何をしているんだ? `Either`ではなく`Either a`をインスタンスにしている。これは、`Either`は2つ受け取れるが、`Either a`はパラメータを1つ受け取る型コンストラクタとなっているからだ。もし、`fmap`が`Either a`に限定していたら、その方シグネチャは、`(b -> c) -> Either a b -> Either a c`となるだろう。なぜかと言えば、`(b -> c) -> (Either a) b -> (Either a) c`と同じだからだ。しかし、実装では、値コンストラクタ`Right`に対してマッピングしているが、`Left`にはしていない。なぜかって? では、`Either a b`がどのように定義されているか確認してみよう。以下のとおりだ。

    data Either a b = Left a | Right b

もし、`a`と`b`の両方に関数をマッピングしたければ、それらを同じ型にする必要がある。たとえば、文字列から文字列へマッピングを行う関数があっても、`a`が数値で`b`が文字列の場合は機能しない、ということだ。また、`fmap`の型が`Either`の値のみに対して機能する場合、2番目のパラメータの型は変更できても1番目の型は同じ型を保つ必要があり、最初のパラメータは値コンストラクタ`Left`により具体的な型にされている必要がある。

これは、我々の箱に例える方法と合致していて、`Left`の部分をある種の空の箱とみなし、`Right`にはなぜ空の箱になるのか、というエラーメッセージを持たせる。

`Data.Map`の`Map`も、値があるかないか、というのを保持できるのでファンクタになる。`Map k v`の場合、`fmap`は`v -> v'`という関数を`Map k v`という型にマッピングして、`Map k v'`という型のマップを生成する。

以前も説明したが、型や値の命名について、`'`は特別な意味を持たないので注意してほしい。`'`は、ちょっとした変更を加えたときに名前の後ろにつける。

`Map k`がファンクタのインスタンスであることを、確認するのは宿題にしよう。

さて、　型クラス`Functor`を使うことで、型クラスが高階関数のような概念を表すことを学んだ。それから、型を部分適用することで、インスタンスにする実用的な方法も学んだ。後の章では、ファンクタに適用されるいくつかの法則について学ぶとしよう。

それから、もう一つ、ファンクタはいくつかの規則に従わなくてはいけないので、いくつかのプロパティを持つが、それらについて我々は特になにも考えなくて良い。たとえば、`fmap (+1)`をリスト`[1,2,3,4]`にマッピングすると、その結果は`[2,3,4,5]`になることを期待しても、`[5,4,3,2]`になることは期待しないだろう。もし、`fmap (\a -> a)`という(与えられたパラメータをそのまま返すだけの)関数をリストにマッピングしたら、結果は元と同じリストになることを期待するだろう。たとえば、`fmap`をツリーの左側のサブツリーはノードの値より小さく、右側のサブツリーは　ノードの値より大きいという我々の`Tree`型に間違ったファンクタを与えると、ノードが正しく配置されていないツリーを生成するかもしれない。ファンクタに適用される規則の詳細については後の章で説明する。



## Kinds and some type-foo

型コンストラクタは他の型をパラメータとして受け取り、最終的に具体的な型を生成する。これは、値を受け取り値を返す関数のようだ。また、型コンストラクタは関数のような(たとえば、`Either String`は、パラメータとして1つの型を受け取り、最終的に`Either String Int`のような具体的な型を生成する、という)部分適用が可能であることを学んだ。これは非常に興味深い。この節では、型宣言を使用することで値が関数によってどのように適用されるかを厳密に定義したようにある型が型コンストラクタによって、どのように適用されるかを厳密に定義する方法について見ていく。Haskellをめぐる冒険を続けるためにこの節を読み進める必要はないし、理解する必要もないから、心配しないで欲しい。しかし、ここを読めば、Haskellが備える型システムの真髄が理解できるようになるだろう。

そう、`3`や`"YEAH"`、そして(関数は関数に渡すことができるから、関数も値となるので)`takeWhile`のような値はすべて各々の型を持つ。型は値に付けられたラベルのようなものであり、これにより値を推論することができる。しかし、型もkindというラベルのようなものを持っている。kindというのは、型の型のようなものだ。これは奇妙で紛らわしく聞こえるかもしれないが、かなり興味深い概念だ。

kindとは何で、kindの何が良いんだろう? では、型のkindについて`:k`というコマンドをghciに入力して実際に調べてみよう。

    ghci> :k Int
    Int :: *

スター`*`が表示された。何だこりゃ? これはどういう意味なんだ? スター`*`というのは、型が具体的な型であることを意味する。具体的な型というのは、型パラメータを一切受け取らない型であり、値が保持できるのは具体的な型のみとなる。(これまで必要に迫られた経験はないが)私が`*`について声に出して説明するとしたら、`*`は型そのものだ、と言うだろう。

わかった。次は`Maybe`のkiindは何かを見てみよう。

    ghci> :k Maybe
    Maybe :: * -> *

型コンストラクタ`Maybe`は1つの、具体的な型(`Int`など)を受け取り、具体的な型(`Maybe Int`など)を返す。これが、上記の結果が意味するところだ。ちょうど`Int -> Int`というのが`Int`を受け取って`Int`を返す関数を意味するように、`* -> *`というのは、型コンストラクタが1つの具体的な型を受け取って1つの具体的な型を返すことを意味する。では、`Maybe`に型パラメータを与えてみて、kindがどうなるか見てみよう。

    ghci> :k Maybe Int
    Maybe Int :: *

まさに期待通りだ。`Maybe`に型パラメータを適用すると、具体的な型が得られる。これが`* -> *`の意味するところだ。(型とkindは異なるものであり、両者は等しくないので)並列する`:t isUpper`と`:t isUpper 'A'`の型は異なる。`isUpper 'A'`という値は`True`になるので、`isUpper`の型は、`Char -> Bool`であり、`isUpper 'A'`の型は`Bool`となる。しかし、両者のkindはどちらも`*`となる。

`:t`を使って値の型を得るように、`:k`を使って型のkindを得る。先程、型は値のラベルのようなものであり、kindは型のラベルのようなものであるが、両者の間には並列の関係がある、ということを言った。

では、他のkindも見てみよう。

    ghci> :k Either
    Either :: * -> * -> *

ああ、これは、`Either`が、型パラメータとして具体的な型を2つ受け取り、具体的な型を生成する、ということを言っている。これは、2つのパラメータを受け取って値を返す関数の型宣言のようにも見える。型コンストラクタも関数のようにカリー化できるので、部分適用が可能となっている。

    ghci> :k Either String
    Either String :: * -> *
    ghci> :k Either String Int
    Either String Int :: *

`Either`を型クラス`Functor`のインスタンスにするとき、部分適用をしておく必要があるが、これは`Either`は2つパラメータを受け取るのに対し、`Functor`が1つのパラメータを受け取る型を要求するからだ。言い換えれば、`Functor`は、`* -> *`という型を要求するので、`Either`を元の`* -> * -> *`から`* -> *`というkindの型を得るために部分適用しておく必要がある。ここで、もう一度型クラス`Functor`の定義を確認しよう。

    class Functor f where
        fmap :: (a -> b) -> f a -> f b

`f`という型変数が具体的な型を受け取り、具体的な型を生成する、ということが読み取れる。また、関数の中で、変数の型を使っているから、具体的な型を生成する必要がある、ということを我々は知っている。ということで、ファンクタの友達になりたがっている型は`* -> *`というkindでなくてはいけない、という推測ができる。

次の型クラスをミてみよう。これは、今適当に考えたものだ。

    class Tofu t where
        tofu :: j a -> t a j

何か、変な感じだ。ある型をこの奇妙な型クラスのインスタンスにするには、どうしたらいいんだろう?では、そのkindがどうなれば良いのかをミてみよう。`j a`は、`tofu`関数がパラメータとして受け取る値の方として使用されているから、`j a`は`*`というkindを持つ必要がある。`*`は`a`い対応する前提だから、`j`は`* -> *`というkindを持つことを推測できる。また、`t`は2つの型を受け取り、具体的なアタいを生成することになる、というのが読み取れる。ここまでで、`a`のkindは`*`であり、`j`のkindは`* -> *`であることが分かったから、`t`は`* -> (* -> *) -> *`というkindを持つ必要がある、と推測できる。つまり、具体的な型(`a`)を受け取り、型コンストラクタは具体的な型(`j`)を1つ受け取り、具体的な型を生成する、ということが読み取れる。

いいね。では、``* -> (* -> *) -> *`というkindを持つ型を作ってみよう。以下は実現方法の1つだ。

    data Frank a b  = Frank {frankField :: b a} deriving (Show)

どうやって、この型のkindが`* -> (* -> *) -> *`であることを知るんだろう? ADTのフィールドは、値を保持するために作られたから、そのkindは明らかに`*`となる。`*`は`a`のために、と仮定したが、これは、1つの型パラメータを受け取るから、そのkindは`* -> *`となる。さて、これで`a`と`b`の両方のkindが明らかになった。これらは`Frank`のパラメータだから、`Frank`のkindは`* -> (* -> *) -> *`となることが分かる。最初の`*`は`a`を表しており、`(* -> *)`は`b`を表している。いくつか`Frank`の値を作って、その型を調べてみよう。

    ghci> :t Frank {frankField = Just "HAHA"}
    Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
    ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
    Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
    ghci> :t Frank {frankField = "YES"}
    Frank {frankField = "YES"} :: Frank Char []

うーむ。
`frankField` は、`a b`という形の型を持つから、その値も同じ形の型を持つ必要がある。つまり、これらは`Maybe [Char]`という型を持つ`Just "HAHA"`という値や、`[Char]`という型を持つ`['Y','E','S']`という値になれる。(もし、独自のリスト型を作っていたら、`List Char`のような型を持つだろう。)また、`Fるrank`という型の値は、`Frank`のkindに対応していることが読み取れる。`[Char]`は`*`というkindを持ち、`Maybe`は`* -> *`というkindを持つ。値を持つには、具体的な型である必要があるから、すべて適用されている必要があり、`Frank blah blaah `という値は`*`というkindを持つことになる。

`Frank`を`Tofu`のインスタンスにするのは至って簡単だ。`Tofu`は`a j`とう(という形式の型の例は`Maybe Int`など)を受け取って、`t a j`を返す。つまり、`Frank`を`j`で置き換えれば、結果の型は`Frank Int Maybe`のようになる。

    instance Tofu Frank where
        tofu x = Frank x

    ghci> tofu (Just 'a') :: Frank Char Maybe
    Frank {frankField = Just 'a'}
    ghci> tofu ["HELLO"] :: Frank [Char] []
    Frank {frankField = ["HELLO"]}

とても役立つとは言えない。しかし、型という筋肉が柔軟になった。では、もう少し`data`について見てみよう。

    data Barry t k p = Barry { yabba :: p, dabba :: t k }

そして、我々は今これを`Functor`のインスタンスにしたいとする。`Functor`は`* -> *`というkindの型を要求するが、とてもそのようなkindには見えない。では、どんなkindにすれば良いのだろう? そうだね、これは3つの型パラメータを受け取るから、`something -> something -> something -> *`とすることにしよう。これは、`p`が具体的な型であり、`*`というkindを持つといえるので安全だ。`k`については、`*`であると仮定して、ついでに`t`は`* -> *`であるとしよう。これでこれらのkindをプレースホルダとして使用した`something`で置き換えると、そのkindが`(* -> *) -> * -> * -> *`となる。では、ghciで確かめてみよう。

    ghci> :k Barry
    Barry :: (* -> *) -> * -> * -> *

おお、我々は正しかったようだ。しっかり満たされている。では、この型を`Functor`の一部にするために、最初の2つの型パラメータを部分適用する必要がある。そうすれば、`* -> *`が残るこれは、インスタンス宣言が`instance Functor (Barry a b) where`のように始まることを意味する。もし、`fmap`が`Barry`のためだけに作られていたら、その型は、`fmap :: (a -> b) -> Barry c d a -> Barry c d bのようになるだろう。なぜなら、`Functor`の`f`を`Barry`の`c d` で置き換えたからだ。`Barry`の3番目の型パラメータは変更する必要があり、それが自身のフィールドにあることが確認できる。

    instance Functor (Barry a b) where
        fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

やったね。`f`を最初のフィールドだけにマップで来た。

この節では、型パラメータがどのように機能するか学び、また、関数のパラメータを型宣言によって厳密にしたように、型パラメータをkindによって厳密にする方法を学んだ。そして、型コンストラクタと関数には興味深い共通点があることを見てきた。しかし、これらは全く異なるものだ。実世界でHaskellを使うときは、kindを散らかしたり、手動でkindを推測することは、まずないだろう。独自の型クラスを標準ライブラリの型クラスのインスタンスにするときに`*`や`* -> *`を部分適用したりする必要があるが、それが実際には、なぜ、どのように機能するかを知っている、というのは良いことだ。加えて、ある型自身もまたある種の型を持っている、というのも興味深い。繰り返しになるが、このチュートリアルを読み進めるために、この節を理解する必要は全くないkindがどのように機能するか理解できたなら、Haskellの型システムについて確実に把握できたといえるだろう。



# Input and Output

以前、Haskellは純粋な関数プログラミング言語である、と紹介した。命令的な言語では、一連の実行したい処理のまとまりをコンピュータに与えることで何か結果を得ていたのに大して、関数プログラミングでは、それが何であるかを定義する。Haskellにおける関数は、変数の中味を変更するなどの、状態を変更する操作を行うことができない。(関数が状態を変更するとき、その関数は副作用を持つ、と言う。)Haskellにおける関数ができることというのは、与えられたパラメータにもとづいて結果の値を返すことだけだ。同じパラメータを与えて関数を2回呼び出せば、2回とも同じ結果が得られる必要がある。これは、命令的な世界からやってきた人々にはやや厳しい制約のように思えるかもしれないが、実はこれが素晴らしいことを学んだ。命令的な言語では、数値を受け取って何かする関数が結果をかえすまでの間に、あなたの自宅を全焼させ、愛犬を連れ去り、車にジャガイモを投げつけたりするかもしれないが、そうならない、とは保証してくれないのだ。たとえば、二分探索木を造ったときに、要素を挿入するためにツリーへ変更を加える事はしなかった。古いツリーを変更することはできないので、我々の二分探索木へ挿入を行う関数は、実際には新しいツリーを返していた。

我々のプログラムがどのように動作するか推測できるという点で関数が状態を変更できない、というのは良いことだが、ひとつ問題がある。もし関数が本当に何も変更できないとしたら、どうやって計算結果を我々に伝えるんだろう? 計算結果を伝えるには、必ず出力デバイス(通常はスクリーン)の状態を変更しなくてはならないが、これは我々の脳に混乱の光子を撒き散らし、我々の心境という状態を変更させてしまう。

絶望しなくていい。すべては失われていない。これを打ち消すために、Haskellは副作用を持つ関数をうまく扱うための非常に賢いシステムを備えており、我々のプログラムを純粋な部分と純粋ではない部分、たとえばキーボードやスクリーンとやり取りするなどの機能へと綺麗に分離してくれる。2つの部分へと分離することで、純粋なプログラムとして推測可能かつ、外部の世界とやり取りするときに純粋さが要求されるlazinessや頑健性、モジュラリティなどのアドバンテージを得ることができる。



## Hello, world!

今までは、関数をテストして試すために毎回ghciへ直接読み込ませていた。それから、標準ライブラリの関数なども一通り見てきた。しかし第8章からは、ついに本物の、最初のHaskellプログラムを書き始める。やったね。我々は`"hello, world"`というギャグをかますのに十分な年をとった。

さて、この章の目的のために、私はあなたがunixっぽい環境でHaskellを学んでいることを仮定している。もし、あなたがWindowsを使っているなら、cygwinというWindows用のLinuxらいくな環境をダウンロードすることをおすすめする。

まずはお好みのテキストエディタで以下の内容を打ち込んで欲しい。

    main = putStrLn "hello, world"

何の変哲もないように見えるが、そうではない。後にすぐ分かるだろう。とりあえず、`helloworld.hs`という名前でこれを保存しよう。

それから、これを実行する前にしておくことがある。我々のプログラムをコンパイルするのだ! いやぁ、ドキドキするね! では、ターミナルを開いて`helloworld.hs`があるディレクトリへ移動し、以下を実行しよう。

    $ ghc --make helloworld
    [1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )
    Linking helloworld ...

OK。では、幸運にも上記のように表示されてしまったら、あとは`./helloworld`と入力してプログラムを実行するだけだ。

    $ ./helloworld
    hello, world

こうして、我々がコンパイルした最初のプログラムは、ターミナルになにか表示する。なんて退屈なんだ!

では、我々が書いたものを調査してみよう。まずは、関数`putStrLn`の型を確認する。

    ghci> :t putStrLn
    putStrLn :: String -> IO ()
    ghci> :t putStrLn "hello, world"
    putStrLn "hello, world" :: IO ()

`putStrLn`の型は、こう読み取ることができる。`putStrLn`っは文字列を受け取り、結果の型が(空のタプル、あるいはユニットと呼ばれる)`()`となるI/Oアクションを返す。I/Oアクションというのは、その動きに副作用が伴うものであり、(通常は、入力からの読み取りやスクリーンへの出力など)ある種の戻り値をその中に保持するようなものだ。ターミナルに文字列を表示する、というのは意味のある戻り値を返すことができないから、ダミーの値として`()`が使われるのだ。

空のタプルは`()`という値であり、その型も`()`となる。

では、I/Oアクションは何をするのだろう? そうだね、`main`がしていることは、こうだ。`main`と名付けられたI/Oアクションがするのは、我々のプログラムを実行することだ。

それから、プログラム全体で1つのI/Oアクションしか使わないようにすべきだが、これは制約のように思えるかもしれない。そこで、`do`シンタックスを使って、いくつかのI/Oアクションを1つにくっつけるのだ。以下の例を見てみよう。

    main = do
        putStrLn "Hello, what's your name?"
        name <- getLine
        putStrLn ("Hey " ++ name ++ ", you rock!")

おお、新しいシンタックスだ。おもしろい。これは命令的な言語にそっくりだ。これをコンパイルして試せば、想像通りの振る舞いをするだろう。`do`と言った後に、一連のステップを命令言語のように連ねていることに注目してほしい。これらのステップはそれぞれI/Oアクションだ。`do`シンタックスの後にこれらをまとめて置くことで、1つのI/Oアクションにまとめることができるのだ。このアクションの型は`IO ()`となるが、
`do`の中にある最後のI/Oアクションがその型となるからだ。

ということで、`main`は常に`main :: IO something`という`something`には具体的な型が入る型シグネチャを持つ。また、習慣として、`main`には型宣言を指定しない。

まだ言及していない興味深いことと言えば、3行目で述べられている`name <- getLine`という箇所だ。これは、`name`と呼ばれる変数に入力から読み取った1行を保存しているように見える。本当にそうだろうか? では、`getLine`の型を確認してみよう。

    ghci> :t getLine
    getLine :: IO String

ああ、いいね。`getLine`は、結果の型として`String`を保持するI/Oアクションだ。ターミナルでユーザーが何か入力するのを待ってから、その入力されたものが文字列になる、というのは理にかなっている。では、`name <- getLine`では、何が起こっているんだろう? このコードのかけらは、次のように読み取ることができる。I/Oアクションの`getLine`を実行して、その結果を`name`に束縛する。`getLine`は`IO String`という型を持つから、`name`は`String`という型を持つことになる。I/Oアクションというのは、実世界へ言って、なにか(たとえば壁に落書きするとか)をしてから、何らかのデータを持って帰ってくるかもしれない、小さな箱だと思えばいい。そして、一度データをあなたの元へ撮ってきたら、箱のなかにあるデータを取り出すために、`<-`を使う。これが箱からデータを取り出す唯一の方法だ。それから、I/Oアクションの外へデータを持ち出したい場合は、他のI/Oアクションの中に限っては、持ち出すことができる。こうして、Haskellは我々のコードの中で、純粋な部分と純粋ではない部分を綺麗に分離して管理する。`getLine`は2回実行された時、その結果がおなじになることは保証できないという意味で純粋ではない。これが、型コンストラクタ`IO`がある種の汚染になる理由であり、I/OのデータはI/Oにしか持ち込めない理由となっている。また、I/OのコードはI/Oに頼っているすべての計算を汚染するから、 I/Oのデータも結果が汚染されることになる。

私は汚染という言葉を使ったが、純粋なコードの中では結果がI/Oアクションとなるものをこれから絶対に使ってはいけない、ということを意味しているわけではない。いや、I/Oアクションの中で`name`にデータを束縛するときは、一時的だが汚染されていない。`name <- getLine`するとき、単純に箱の中身が何かを表しているだけだから、`name`は普通の文字列となる。たとえば、あなたの名前を(普通の)文字列でパラメータとして受け取り、名前に基づいた運勢と将来の人生のすべてについて伝えてくれる、とても複雑な関数を作ることができる。

    main = do
        putStrLn "Hello, what's your name?"
        name <- getLine
        putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name

`tellFortune`(または、名前を与える何らかの関数)は、I/Oについて何も知る必要はないので、普通の`String -> String`という関数だ。

では、以下のコードのかけらをみてみよう。これは正しい?

    nameTag = "Hello, my name is " ++ getLine

正しくないと答えたそこのあなたにはクッキーを、正しいと答えたそこのあなたは、溶岩を飲もうか。いや、ちょっとからかってみただけだ。これが機能しない理由は、`++`は同じ型に対してのみ機能するからだ。左のパラメータは普通の`[Char]`あるいは`String`という型を持つが、`getLine`は`IO String`となる。文字列と`IO String`を連結することはできない。I/Oアクションの外で`String`型の値として結果を得ようとしているが、これを達成する唯一の方法は、別のI/Oアクションで`name <- getLine`のようにすることだ。純粋ではないデータを扱いたければ、純粋ではない環境で扱う必要がある。純粋ではないというお宣言がゾンビのような恐怖を撒き散らすので、I/Oの部分を出来る限り小さく保つことがベストだ。

すべてのI/Oアクションはその結果が得られるまで振る舞いは隠される。これにより、上記のコードは以下のようにも記述することができる。

    main = do
        foo <- putStrLn "Hello, what's your name?"
        name <- getLine
        putStrLn ("Hey " ++ name ++ ", you rock!")

しかし、`foo`は`()`という値を持つから、これは議論の余地がある。また、最後の`putStrLn`は何にも束縛していないことにも注目してほしい。これは、`do`ブロックの中では最後のアクションは最初の2つのようにナマへへと束縛できないのが原因だ。どうしてそうなっているのか、詳細については後でモナドの冒険をするときに説明しよう。今のところはとりあえず、`do`ブロックでは、自動的に最後のアクションから値を抽出して、ブロックの結果として束縛するからだ、と考えることに仕様。

最後の行を除いて、`do`ブロック内のすべての行は、束縛のように書くことはできるが、束縛を行わない。つまり、`putStrLn "BLAH"`は、`_ <- putStrLn "BLAH"`と書くことができる。が、何の役にも立たない。ということで、`putStrLn`などの重要な意味のある結果を保持しないI/Oアクションについては`<-`を取り除こう。

初心者は、ときどき以下のように考える。

    name = getLine

入力から読み取って、値を`name`に束縛したいのだろう。しかし、想像通りに機能しない。これは、`getLine`というI/Oアクションを別の名前`name`に束縛するだけだ。思い出してほしい。I/Oアクションの結果を外部へ持ち出すには、別のI/Oアクションの中で`<-`を使って束縛をする必要があるのだった。

`main`という名前を与えられた場合か、`do`ブロックにより組み合された巨大なI/Oアクションの中にある場合のみI/Oアクションは実行される。また、`do`ブロックにより、いくつかのI/Oアクションを結合することができ、それを別の`do`ブロック内でI/Oアクションとして使い回すこともできる。どちらの方法にせよ、最終的には`main`の中で実行されることになる。

ああ、それからもうひとつ、I/Oアクションが実行される場合があった。ghciの中でI/Oアクションを入力して、リターンキーを押すと、実行される。

    ghci> putStrLn "HEEY"
    HEEY

数値や関数をghciに入力してリタンキーを押せば、値を(必要に応じて)計算して、それを`show`関数でターミナルへ文字列として表示するが、`putStrLn`が暗黙のうちに使われている。

ところで、`let`による束縛を覚えているかな?覚えていなければ、記憶をリフレッシュしてこの節を読んで欲しい。`let`による束縛は式であり、`where`による束縛は、式に名前を与えるものであり、式は評価される対象の式となる。同じことはlist comprehensionについても言ったが、この場合`in`は必要なかった。list comprehensionでもできたように、`do`ブロック内でもこれらを同様に使うことができる。確認してみよう。

    import Data.Char
    
    main = do
        putStrLn "What's your first name?"
        firstName <- getLine
        putStrLn "What's your last name?"
        lastName <- getLine
        let bigFirstName = map toUpper firstName
            bigLastName = map toUpper lastName
        putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

`do`ブロック内のI/Oアクションはどのように並べられているだろう? また、`let`による束縛はI/Oアクションの中で他と比較して、どのように並べられているだろうか? Haskellではインデントが重要になるから、これは良い実例だ。さて、`toUpper`を`firstName`にマッピングするとたとえば、　`"John"`をよりかっこいい文字列`"JOHN"`にする。この大文字にした名前を別の変数名に束縛し、その文字列は後でターミナルへ出力するときに使用する。

ところで、いつ`let`を使い、いつ`<-`を使えばいいのか迷うかもしれない。そうだね、`<-`は、(今のところは)I/Oアクションを実行して結果を変数に束縛するためのものだ。　しかし、`firstName`に対するマッピングはI/Oアクションではない。これはHaskellにおける純粋な式だ。ということで、`<-`はI/Oアクションの結果を束縛したいときに使い、`let`は純粋な式を名前に束縛したいときに使用する。もし、`let firstName = getLine`のようなことをすると、これは単に`getLine`というI/Oアクションをを別の名前にする、というのを呼び出すだけであり、これを実行するには`<-`を使用しなければならない。

では、入力を受け取り続け、受け取った文字列と同じ文字列を左右反転したものを表示し続ける、というプログラムを作ってみよう。プログラムの実行は空の行を入力した時に停止する。これがプログラムだ。

    main = do
        line <- getLine
        if null line
            then return ()
            else do
                putStrLn $ reverseWords line
                main
    
    reverseWords :: String -> String
    reverseWords = unwords . map reverse . words

これが実際にどのように機能するか、コードの説明をする前に試してみよう。

Protip: プログラムを実行するために`ghc --make helloworld.hs`でコンパイルした後に`./helloworld`で実行可能ファイルを実行する方法の他に、`runhaskell`コマンドを使って、`runhaskell helloworld.hs`とすればその場でプログラムを実行できる。

まずは、関数`reverseWords`について見てみよう。これは、`"hey there man"`などの文字列を受け取る普通の関数であり、`["hey","there","man"]`のように、文字列を単語へ区切って文字のリストにする。それから、そのリストにたいして`reverse`をマッピングして、`["yeh","ereht","nam"]`というリストにする。最後にそのリストを`unwords`で文字列へと結合して、最終的な結果として`"yeh ereht nam"`という文字列を得る。関数合成がどのように使用されているか確認できるだろう。関数合成を使わなければ、`reverseWords st = unwords (map reverse (words st))`のように書かなくてはいけないだろう。

`main`についてはどうだろう? まず、`getLine`を実行することで、ターミナルから1行を`line`として得る。次に、条件分岐の式が登場する。思い出してほしい。Haskellでは、すべての`if`が、それに対応する`else`を持たなくては行けないが、これはすべての式が何らかの値を持たなければならないからだ。`if`の条件が`True`のとき(我々の場合、入力された行が空の行の時)1つのI/Oアクションを実行し、`True`でない場合には、 `else`より下のI/Oアクションが実行される。 これが、IOの`do`ブロック内では、`if`は`if condition then I/O action else I/O action`のような形式でなくてはいけない理由だ。

では、まずは`else`句の後で何が起こっているのかを見てみよう。`else`の後では必ず1つのI/Oアクションしか配置できないから、2つのI/Oアクションを1つにまとめるために`do`ブロックを使用する。また、この部分は以下のようにも記述できる。

    else (do
        putStrLn $ reverseWords line
        main)

`do`ブロックが1つのI/Oアクションのように見えるから、これはより明示的になるが、読みづらい。ところで、`do`ブロックの中では、`getLine`で受け取った行に対して、`reverseWords`を呼び出して、それをターミナルに表示している。その後、`main`を実行している。これは再帰的に呼びだされているが、`main`自身がI/Oアクションなので、問題ない。つまり、ある意味ではプログラムのはじめに戻る、と言える。

では、空の行が入力されて、`True`になったら何が起こるだろう?これは、`then`の後が実行されるケースだ。そこを見てみると、`then return ()`と書かれている。CやJava、Pythonなどの命令的な言語の経験があれば、この`return`が何をするのか想像して、このクソ長い段落を読み飛ばせるチャンスだぜ、と思うかもしれない。しかし、Haskellにおける`return`は、他の命令的言語のそれとは異なり、特に何もしない。どちらも同じ名前だから、多くの人々を混乱させるが、実際のところこれらは全く別ものだ。通常、命令的な言語では、`return`はサブルーチンやメソッドの実行を終わらせるために使用される。そして、呼び出し元にその結果を報告する。Haskellでは、もっと正確にはI/Oアクションでは、`return`は純粋な値としてI/Oアクションを外に出す。以前、箱に例えることをしたが、箱は値をその中に包むのだった。I/Oアクションの結果は実際には何もしないが、I/Oアクションはその結果を値として保持する。つまり、I/Oの文脈では、`return "haha"`とういうのは`IO String`という型を持つ。純粋な値を実際には何もしないI/Oアクションへと変形させるのはなぜなのか? なぜ、I/Oを使う我々のプログラムをもっと汚染する必要があるのか? そうだね、空の行が入力された場合のI/Oアクションをどこかへ持ち出す必要がある体。実際には何もしないI/Oアクションを`return ()`としているのは、これが理由だ。


`return`を使っても、`do`ブロック内のI/Oの実行を停止させるわけではない。たとえば、以下のプログラムは幸運にも一番下の行まで実行され続ける。

    main = do
        return ()
        return "HAHAHA"
        line <- getLine
        return "BLAH BLAH BLAH"
        return 4
        putStrLn line

すべての`return`は、I/Oアクションを作るだけであり、結果を保持することを除き、特に何もしない。そして、その結果は束縛されていないので、捨てられることになる。また、名前に束縛する`<-`と`return`を組み合わせて使うこともできる。

    main = do
        a <- return "hell"
        b <- return "yeah!"
        putStrLn $ a ++ " " ++ b

見ての通り、`return`は`<-`と真逆のことをする。`returnは値を受け取って、箱に入れるのに対して、`<-`は箱から中味の値を取り出して、それを名前へと束縛する。しかし、こんなことをするのは冗長だから、特にこの場合は`do`ブロックの中で`let`による束縛を使用したほうが良い。

    main = do
        let a = "hell"
            b = "yeah"
        putStrLn $ a ++ " " ++ b

`do`ブロックの中でI/Oを扱う場面においては、実際には何もしないI/Oを作成するときに`return`を使用する。また、`return`は、`do`ブロックの結果が最後のI/Oアクションの結果となるところを変更したい場合に使用する。つまり、I/Oアクションの中で`return`を使うときは、`do`ブロックが臨んだとおりの結果を保持できるようにするために`return`を最後に配置する。

また、`do`ブロックは1つのI/Oアクションだけを持つこともできる。その場合、単にI/Oアクションを書いたこととおなじになる。`then do return ()`という書き方を好む人もいるが、これは、`else`も`do`を持つことになるからだ。では、ファイルについての話をする前に、I/Oを扱うための便利な関数をいくつか紹介しよう。

`putStr`は、`putStrLn`のようなもので、パラメータとして文字列を受け取り、ターミナルへ表示してI/Oアクションを返す。ただし、`putStrLn`は文字列を表示した後に改行するが、`putStr`はしない。

    main = do   putStr "Hey, "
                putStr "I'm "
                putStrLn "Andy!"

    $ runhaskell putstr_test.hs
    Hey, I'm Andy!

この関数の方シグネチャは`putStr :: String -> IO ()`となっているので、I/Oアクションの結果はユニットとして隠されることになる。つまり、結果の値を束縛しても肩透かしを食らうことになる。

`putChar`は文字を受け取りターミナルへと表示して、I/Oアクションを返す。

    main = do   putChar 't'
                putChar 'e'
                putChar 'h'

    $ runhaskell putchar_test.hs
    teh

`putStr`は、実際には`putChar`のヘルパーとして再帰的に定義されている。`putStr`の境界条件は空の文字列となるので、空の文字列を表示するときは、`return`を使って実際には何もしないI/Oアクションを返す。空の文字列でなければ、最初の文字を`putChar`で表示して、残りを`putStr`で表示する。

    putStr :: String -> IO ()
    putStr [] = return ()
    putStr (x:xs) = do
        putChar x
        putStr xs

どのようにして、純粋なコードであるかのように再帰の中でI/Oが扱われているかを見てほしい。純粋なコードであるかのように、まずは境界条件を定義してから、実際には結果がどうなるのかを考えている。先に最初の文字を表示してから、残りの文字列を表示する、というアクションになっている。

`print`は、`Show`のインスタンスである型の値、つまり、文字列での表現方法を知っている値を何でも受け取り`sho`でその値を文字列に変換してからターミナルへと表示する。 基本的には`putStrLn . show`と同じだ。まずは値に対して`sho`を実行してから、その値を表示してI/Oアクションを返す`putStrLn`にくわせている。

    main = do   print True
                print 2
                print "haha"
                print 3.2
                print [3,4,3]

    $ runhaskell print_test.hs
    True
    2
    "haha"
    3.2
    [3,4,3]

ご覧のとおり、とても使い勝手の良い関数だ。思い出してほしい、`main`の中、あるいはghciのプロンプト上で評価を行うとき、どのようにI/Oアクションと対話していただろうか? (`3`や`[1,2,3]`のような)値を入力してリターンキーを押すろｔ，ghciは実際にはghciはその値をターミナルに表示するために、`print`を使用する。

    ghci> 3
    3
    ghci> print 3
    3
    ghci> map (++"!") ["hey","ho","woo"]
    ["hey!","ho!","woo!"]
    ghci> print (map (++"!") ["hey","ho","woo"])
    ["hey!","ho!","woo!"]

文字列を表示したい時、ダブルクオートで囲まれた文字列は表示したくないので、通常は`putStrLn`を使用するが、他の型の値をターミナルへ表示する場合は、`print`が最もよく使われる。

`getChar`は、入力から文字を読み取るI/Oアクションだ。I/Oアクションの結果として保持されるのは`Char`だから、従って、その方シグネチャは`getChar :: IO Char`となる。注意: バッファリングの都合により、ユーザーがリターンキーを押すまで入力は読み取られない。

    main = do
        c <- getChar
        if c /= ' '
            then do
                putChar c
                main
            else return ()


このプログラムは入力から文字を読み取り、それがスペースかどうかを判定しているように見える。
入力がスペースでなければ、入力と同じ内容を表示しプログラムの、実行を続ける。
しかし、似たような動きはするものの、[期待通りの動作はしない。確認してみよう。

    $ runhaskell getchar_test.hs
    hello sir
    hello

2行めが入力だ。`hello sir`と入力して、リターンキーを押している。バッファリングの都合により、プログラムの実行が始まるのは、なにかキーをおした時ではなく、リターンキーを押したあとになる。しかし、リターンキーを押しても我々が入力したものとは違うものが表示される。このプログラムに同情してほしい。

`when`関数は、`Control.Monad`にある。(読み込むには、`import Control.Monad`とする。)`when`は`do`ブロックの中では制御文のように見えるが、実際には普通の関数なので、非常に興味深い。`when`は`Bool`型の値とI/Oアクションを受け取り、`Bool`型の値が`True`の場合は、`when`に与えたものと同じI/Oアクションを返す。しかし、`False`の場合は、`return ()`という、実際には何もしないI/Oアクションを返す。以下に、上記ででもしたコードのかけらを`when`を使ってどのように書きなおすかを示す。

    import Control.Monad
    
    main = do
        c <- getChar
        when (c /= ' ') $ do
            putChar c

ご覧のとおり、`when`は`if 何らかの条件 then do 何らかのI/Oアクション else return ()`というパターンを隠すのに役立つ。

`sequence`は、I/Oアクションのリストを受け取り、それらを順番に実行してからI/Oアクションを返す。結果のリストには、それぞれのI/Oアクションが実行された結果が保持される。この関数の方シグネチャは、`sequence :: [IO a] -> IO [a]`となる。

    main = do
        a <- getLine
        b <- getLine
        c <- getLine
        print [a,b,c]

上記のようなコードは、以下と全く同じだ。

    main = do
        rs <- sequence [getLine, getLine, getLine]
        print rs

つまり、`sequence [getLine, getLine, getLine]`というのは、I/Oアクションを3回実行する。このアクションを名前に束縛すると、結果はそれらを実行した結果のリストになるので、上記の場合は、ユーザーがプロンプトに入力したものが3つ保持される。

`sequence`を使う時の一般的なパターンは、`print`や`putStrLn`のような関数をリストに対してマッピングすることだ。たとえば、`map print [1,2,3,4]`としてもI/Oアクションは作成されない。これは、`[print 1, print 2, print 3, print 4]`と書くのと同じだから、I/Oアクションのリストを作成することになる。I/OアクションのリストをI/Oアクションに変換したければ、`sequence`を使う必要がある。

    ghci> sequence (map print [1,2,3,4,5])
    1
    2
    3
    4
    5
    [(),(),(),(),()]

最後の`[(),(),(),(),()]`というのは何だ? そうだね、I/Oアクションをghciの中で評価したから、それが実行されて、それぞれ何も表示しない、ということを意味する`()`を含むリストが表示されたわけだ。(`putStrLn "hehe"`が保持している結果は`()`だから)ghciで`putStrLn "hehe"`を評価すると`"hehe"`となるのはそのためだ。しかし、ghciで`getLine`をすると`getLine`は`IO String`という型を持つので、I/Oアクションの結果が表示される。

リストに対して関数をマッピングして、順番にI/Oアクションの結果を得るというのは一般的なので、`mapM`と`mapM_`というユーティリティ関数が用意されている。`mapM`は関数とリストを受け取り、リストに関数をマッピングしてからそれを順番に実行する。`mapM_`も同じことをするが、実行した結果を捨て去る。通常、`mapM_`は順番に実行されるI/Oアクションの結果について気にしない場合に使う。

    ghci> mapM print [1,2,3]
    1
    2
    3
    [(),(),()]
    ghci> mapM_ print [1,2,3]
    1
    2
    3


`forever`は、I/Oアクションを受け取り、I/Oアクションを無限に繰り返すI/Oアクションを返す。これは`Control.Monad`に用意されている。以下の小さなプログラムは、ユーザーから入力を受け取り、それを大文字にしたものを返す、というのを無限に続ける。

    import Control.Monad
    import Data.Char
    
    main = forever $ do
        putStr "Give me some input: "
        l <- getLine
        putStrLn $ map toUpper l

`forM`は、`mapM`と同じだが、受け取るパラメータの順番が入れ替わっている。最初のパラメータがリストで、2番目はリストにたいしてマッピングする関数となるる。これの何が役立つんだろう? そうだね、`do`による注釈とラムダを使って創造的なことができる。

    import Control.Monad
    
    main = do
        colors <- forM [1,2,3,4] (\a -> do
            putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
            color <- getLine
            return color)
        putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
        mapM putStrLn colors

`(\a -> do ...)`という関数は、数値を受け取って、I/Oアクションを返す関数だ。これをカッコで囲む必要があるのは、かっこがないと最後の2つのI/Oアクションがラムダのものだ、と解釈されてしまうからだ。また、`do`ブロックの中で、`return color`としているのにも注目してほしい。`do`ブロックで定義されているI/Oアクションは、`color`の結果をその中に保持する。`getLine`は既にその結果を保持しているので、実際にはそうする必要はない。`color <- getLine`してから、`return color`とすることで、`getLine`の結果を取り出してから、また結果を保持するようにしているので、ただ単に`getLine`するのと同じだ。(2つのパラメータを与えて呼び出した)`forM`は、その結果が`colors`に束縛されたII/Oアクションを生成する。`colors`は文字列を保持する普通のリストだ。　最後に、それらの色を`mapM putStrLn colors`とすることで表示している。

`forM`というのは、リストの要素に対してそれぞれI/Oアクションを作る、と考えても良い。それぞれのI/Oアクションが何をするかは、それぞれのI/Oアクションを作るのに使われる要素に依存する。最後に、それらを実行して、その結果を何かに束縛する。束縛する必要がなければ、その結果は捨てても良い。

    $ runhaskell form_test.hs
    Which color do you associate with the number 1?
    white
    Which color do you associate with the number 2?
    blue
    Which color do you associate with the number 3?
    red
    Which color do you associate with the number 4?
    orange
    The colors that you associate with 1, 2, 3 and 4 are:
    white
    blue
    red
    orange

これは`forM`を使わなくても実現できるが、`forM`を使ったほうがより読みやすいコードになる。マッピングしてI/Oアクションを順番に実行したいとき、`do`記法を使ってその場で定義するのに`forM`を使うのが一般的だ。ということで、最後の行は、`forM colors putStrLn`としても同じ結果が得られる。

この節では、入力と出力についての基本を学んだ。そして、I/Oアクションが実行されることが、実際に入力や出力を可能にしている、ということも発見した。繰り返しになるが、I/Oアクションも他のHaskellにおける値と同様の値だ。パラメータとしてI/Oアクションを関数に渡すこともできるし、関数の結果として返すこともできる。I/Oアクションが特別なところは、`main`にそれが入り込むとき(あるいは結果がghciの中にあるとき)それが何らかの副作用を実行する、というところだ。たとえば、スクリーンに何か表示したり、スピーカーから騒々しいサックスの音を鳴らしたりする。そして、それぞれのI/Oアクションは実世界から持ち帰った結果を`IO`としてカプセル化する。

`putStrLn`は、文字列を受け取ってターミナルへ表示する関数だ、と考えては行けない。これは、文字列を受け取ってI/Oアクションを返す関数だ。そして、そのI/Oアクションが実行されるとターミナルに美しい詩を表示するのだ、と考えよう。



## Files and streams

`getChar`は、ターミナルから1文字を読取るI/Oアクションだった。`getLine`は、ターミナルから1行読み取るI/Oアクションだった。これらは直感的であり、多くのプログラミング言語において同じ機能をする関数化、あるいは文が用意されている。では、`getContents`を紹介しよう。`getContents`は、標準入力からあらゆるものを読み取るI/Oアクションであり、ファイルの終端を表す文字に遭遇するまで読み取りを続ける。この関数の型は、`getContents :: IO String`となっている。`getContents`の素晴らしい点は、I/Oがlazyなところだ。`foo <- getContents`とすると、すべての入力を一度に読み取るのではなく、一度メモリに格納してから`foo`に束縛する。そう、これがlazyだ。たとえば、`"Yeah yeah`というのがターミナルに入力されたとして、読み取りをするのはそれが必要になってからだ。

`getContents`は、あるプログラムの出力をパイプして我々のプログラムの入力とするときにとても役立つ。もし、unixのパイプについて知らなければ、ここで軽く説明しよう。以下のちょっとした俳句をテキストファイルに保存しよう。

    I'm a lil' teapot
    What's with that airplane food, huh?
    It's so small, tasteless

よし、良い句が読めたぞ。誰か良い俳句のチュートリアルを知っていたら教えてくれ。

では、`forever`関数を紹介した時に書いた、サンプルプログラムを思い出そう。ユーザーからの入力を受け取り、それを大文字にして表示し、また入力を受け取って、というのを無限に繰り返すのだった。いや、スクロールする必要はない。ここに同じ内容を貼ろう。

    import Control.Monad
    import Data.Char
    
    main = forever $ do
        putStr "Give me some input: "
        l <- getLine
        putStrLn $ map toUpper l

上記のプログラムを`capslocker.hs`などの名前で保存して、コンパイルしよう。それから、直接我々のプログラムにテキストファイルを食わせるためにunixパイプを使う。そこで、`cat`という引数として与えられたファイルの中身を表示するプログラムを使う。では、確認してみよう。

    $ ghc --make capslocker
    [1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
    Linking capslocker ...

    $ cat haiku.txt
    I'm a lil' teapot
    What's with that airplane food, huh?
    It's so small, tasteless

    $ cat haiku.txt | ./capslocker
    I'M A LIL' TEAPOT
    WHAT'S WITH THAT AIRPLANE FOOD, HUH?
    IT'S SO SMALL, TASTELESS
    capslocker <stdin>: hGetLine: end of file

ご覧のとおり、パイプ文字`|`を使うことで、あるプログラムの出力(上記の場合は`cat`の表示結果)を別のプログラムの入力(上記の場合は`./capslocker`)に渡すことができる。これと同じ動作を主導で完璧に真似するなら、ターミナルに俳句を入力してそれから(通常は`Ctrl-D`を入力することで)入力が終了したことを伝えるファイル終端の文字を入力する。これは、`cat`に`haiku.txt`を渡した時に「待った! その表示内容はターミナルじゃなくて`capslock.hs`に渡してくれ。」と言っているようなものだ。

つまり、`forever`を使って実際にしていることは、入力を受け取り出力へと変換することだ。ということで、`getContents`を使うのは、上記のプログラムをより短く、より良くするためだ。

    import Data.Char
    
    main = do
        contents <- getContents
        putStr (map toUpper contents)

`getContents`というI/Oアクションを実行して、それが生成する文字列に名前をつけている。それから、その文字列に`toUpper`をマッピングしてターミナルへ表示している。文字列は基本的にはリストであることを思い出してほしい。そして、文字列はlazyであり、`getContents`もlazyなI/Oだから、すべてを一度に読み取ることはせず、大文字に変換した文字列を表示する前にメモリへ格納する。というより、ほんとうに必要になった時だけ入力から1行読み取るので、読み取りすると大文字に変換したバージョンを表示する、といえる。

    $ cat haiku.txt | ./capslocker
    I'M A LIL' TEAPOT
    WHAT'S WITH THAT AIRPLANE FOOD, HUH?
    IT'S SO SMALL, TASTELESS

よし、動いた。では、`capslocker`単体で実行して、入力を主導で行ったらどうなる?

    $ ./capslocker
    hey ho
    HEY HO
    lets go
    LETS GO

終了するには`Ctrl-D`を押す。めっちゃ良いね! ご覧のとおり1行ごとに、入力を大文字にして表示している。`getContents`の結果が`contents`に束縛されると、メモリに入るのは実際の文字列ではなく、最終的に文字列を生成することを約束する何か、だ。また、`contents`に`toUpper`をマッピングすると、最終的な`contents`に関数をマッピングすることを約束する。そして、最終的に`putStr`が、それらの約束に対して、「おーい、大文字にした文字列が必要なんだ!」と呼びかける。`putStr`は、まだ1行も保持していないから、`contents`に対して、「おーい、ターミナルから受け取った文字列はどんあだ?」と呼びかける。なにか具体的なものを生成するよう依頼するコードに対して文字列を与える。というのが、`getContents`がターミナルから入力を読み取るとき実際にしていることだ。そして`map`が`toUpper`を文字列にマッピングして、それを表示する`putStr`に与える。それから、`putStr`が、「おーい、次の行が必要なんだ! さあ来い!」と呼びかけて、ファイルの終端を表す文字により入力がもう来ないことが指示されるまで、これを繰り返し続ける。

では、入力された文字列が10文字より短い場合のみ、その内容を表示するというプログラムを作ってみよう。こんな感じになる。

    main = do
        contents <- getContents
        putStr (shortLinesOnly contents)
    
    shortLinesOnly :: String -> String
    shortLinesOnly input =
        let allLines = lines input
            shortLines = filter (\line -> length line < 10) allLines
            result = unlines shortLines
        in  result

上記のプログラムでは、I/Oの部分をできるだけ短くするようにしている。我々のプログラムはなにか入力を受け取り、それに基づいて出力すると仮定しているので、`contents`から読み取るのを実装してから、それに対して関数を実行して、関数が返したものを表示している。

関数`shortLinesOnly`の働きは、こうだ。まず、"short\nlooooooooooooooong\nshort again"のような文字列を受け取る。この文字列は3行あるが、そのうちの2行は短く、真ん中の1行は長い。そこで、`lines`関数をこの文字列に大使て実行し、`["short", "looooooooooooooong", "short again"]`に変換した上で、これを`allLines`という名前で束縛する。それから、この文字列のリストをフィルターして、文字数が10文字より短いものだけをリストに残して、`["short", "short again"]`というリストを生成する。最後に、`unlines`を使って、開業により区切られた一つの文字列へと結合して、`"short\nshort again"`という文字列を得る。では、試してみよう。

    i'm short
    so am i
    i am a loooooooooong line!!!
    yeah i'm long so what hahahaha!!!!!!
    short line
    loooooooooooooooooooooooooooong
    short

    $ ghc --make shortlinesonly
    [1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )
    Linking shortlinesonly ...

    $ cat shortlines.txt | ./shortlinesonly
    i'm short
    so am i
    short

`shortlines.txt`の中味をパイプで`shortlinesonly`に入力することで、短い行のみが出力される。

入力からなにか文字列を受け取り、その文字列を関数で変形させ、出力するというパターンは、かなり一般的なので、これをより簡単にするための`interact`と呼ばれる関数が存在する。`interact`は、パラメータとして`String -> String`という型の関数を受け取り、なにか入力を受け取るI/Oアクションを返し、受け取った関数をそのI/Oに対して実行し、その関数の結果を表示する。では、我々のプログラムを`interact`を使うように変更してみよう。

    main = interact shortLinesOnly
    
    shortLinesOnly :: String -> String
    shortLinesOnly input =
        let allLines = lines input
            shortLines = filter (\line -> length line < 10) allLines
            result = unlines shortLines
        in  result


(読みやすさを損なうこともなく)上記で示したように、かなり少ないコードで実現しているが、我々の関数合成のスキルを証明するためにこれをさらに短くするよう、少し変更してみよう。

    main = interact $ unlines . filter ((<10) . length) . lines

おお、なんと1行に減らすことができた。やばいね。

`interact`は、プログラムをパイプから受け取った内容を表示するようにさせる。あるいは、`interact`はプログラムをユーザーからの入力を行単位で受け付けて、その入力に基づいた結果を返して、また入力を受付させたりする。実際にはこれら2つの違いはないようなもので、これはユーザーがどのように入力を行うかという前提による。

では、行単位で無限に読み取りを続け、入力された行が回文か否かを判定するプログラムを作ってみよう。単純に`getLine`を使って、1行を読み取り、それが回文か否かをユーザーに伝え、`main`に戻る、というのを繰り返すこともできる。しかし、`interact`を使うほうが簡単だ。`interact`を使えば、入力をどのように変形させ、期待する出力にさせるかを考えるだけで済む。我々の場合、入力された行をそれぞれ回文か、あるいは回文ではないかというのに置き換える。つまり、たとえば`"elephant\nABCBA\nwhatever"`のような入力を`"not a palindrome\npalindrome\nnot a palindrome"`というのに変形させる関数を書く必要がある。では、以下のようにしてみよう。

    respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
    where   isPalindrome xs = xs == reverse xs

これをポイントフリーで加工。

    respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
        where   isPalindrome xs = xs == reverse xs

かなり直感的だ。まず、たとえば`"elephant\nABCBA\nwhatever"`を`["elephant", "ABCBA", "whatever"]`に変えて、それをラムダでマッピングして、`["not a palindrome", "palindrome", "not a palindrome"]`に帰る。そして、リストを`unlines`により、開業で区切られた一つの文字列へと結合する。では、これを試してみよう。

    main = interact respondPalindromes

テストしてみる。

    $ runhaskell palindromes.hs
    hehe
    not a palindrome
    ABCBA
    palindrome
    cookie
    not a palindrome

入力された1つの巨大な文字列を変形するようなプログラムを書いたにも関わらず1行ごとに変形するようなプログラムを書いたかのように振舞っている。これはHaskellがlazyで、結果となる文字列の最初の1行目を表示しようとするが、入力された文字列の最初の1行をまだ保持していないので、Haskellはそれができないからだ。つまり、入力としてプログラムに最初の1行を与えるとすぐに出力の最初の1行目を表示する。また、終端を表す文字を与えることでプログラムを停止させる。

ファイルをパイプすることでもこのプログラムを使うことができる。

    dogaroo
    radar
    rotor
    madam

では、上記の内容を`words.txt`として保存しよう。以下にパイプを使う方法を示す。

    $ cat words.txt | runhaskell palindromes.hs
    not a palindrome
    palindrome
    palindrome
    palindrome

繰り返しになるが、プログラムを実行して手動で入力した場合と、標準入力から読み込んだ結果はおなじになる。単語を主導で入力するのではなく、入力はファイルの内容から読み込まれているので、我々は`palindromes.hs`に入力された内容を見ることはない。

さて、lazyなI/Oについて、lazyなI/Oがどのように機能しその利点をどのように活かすかを理解できたかもしれない。あなたは与えられた入力に対して、どのような出力をするかというのを仮定し、そのような変形を行う関数を書くだけで良い。lazyなI/Oでは、今この瞬間に表示しようとしているものは入力に依存するので、完全に変形された状態になるまで、何も表示されない。

ここまで、ターミナルから読み込んだ内容をターミナルに表示する、ということをしてきた。では、ファイルから読み込んだり書き込んだりするには、どうするんだろう? 実は既にしているのだ。ターミナルから入力を読み取ること、を考える一つの方法としては、(なにか特殊な)ファイル化ら読み込んでいるのだ、と考える方法がある。同じことがターミナルに書き出すことにも当てはまり、これはある種の特殊なファイルへの書き込みといえる。これら2つのファイルは`stdin`と`stdout`と呼ばれ、それぞれ標準入力と標準出力を意味する。ファイルに書き込んだり、読み込んだりするということは標準出力に書き込んだり、標準入力から読み込んだりすることと似ている、というのを覚えてほしい。

では、`girlfriend.txt`というファイルを開く単純なプログラムからはじめよう。これはアヴリル・ラヴィーンのヒット曲、Girlfriendからの引用だ。これをターミナルに表示しよう。以下が`girlfriend.txt`の中味だ。

    Hey! Hey! You! You!
    I don't like your girlfriend!
    No way! No way!
    I think you need a new one!

そして、以下がプログラムだ。

    import System.IO
    
    main = do
        handle <- openFile "girlfriend.txt" ReadMode
        contents <- hGetContents handle
        putStr contents
        hClose handle

実行すると、期待通りの結果が得られる。

    $ runhaskell girlfriend.hs
    Hey! Hey! You! You!
    I don't like your girlfriend!
    No way! No way!
    I think you need a new one!

では、1行ずつ解説しよう。1行目は、私の気を引くための文句だ。2行目で、アヴリルは私とロマンティックな関係にあるパートナーが気に入らない、ということを表明している。4行目で、別のパートナーを探すように、と示唆されているにもかかわらず、3行目では、私という存在が受け入れがたいと強調している。

ついでに、プログラムの方も1行ずつ解説しよう。我々のプログラム内にあるいくつかのI/Oアクションは、`do`ブロックにより結合されている。まず、`do`ブロック内の1行目で、`openFile`という新しい関数に気づくだろう。この関数の方シグネチャは、`openFile :: FilePath -> IOMode -> IO Handle`だ。これを言葉で説明するなら、`openFile`は、ファイルのパスと`IOMode`を受け取り、ファイルを開いてそれに関連した処理をする、という結果が隠されたI/Oアクションを返す。


`FilePath`は、以下のように定義された`String`の単なる型シノニムだ。

    type FilePath = String

`IOMode`という型は、以下のように定義されている。

    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

この型は、列挙できる。以前、我々が定義した1周間に含まれる7つの曜日を表現する値を持つ型のように、この型はファイルを開いて何がしたいのかを表現している。かなり単純だ。また、この型は`IO Mode`ではなっく、`IOMode`であるところに注目してほしい。`IO Mode`とすると、結果は`Mode`という型の値を持つI/Oアクションになってしまうが、`IOMode`であれば単なる列挙となる。

最後に、`openFile`は指定されたファイルを指定されたモードで開くI/Oアクションを返す。もしこのアクションを何かに束縛すれば、`Handle`が得られる。`Handle`型の値は、我々のファイルがどこにあるのかを表す。つまり、`Handle`を使えばどこにファイルがあるのか分かる。ファイルに対して何もできなくなるから、ファイルを読み込んでいるのに`handle`に束縛しないのはおかしいだろう。ということで、上記のプログラムでは処理を`handle`に束縛している。

次の行には、`hGetContents`と呼ばれる関数が見える。この関数は`handle`を受け取るので、どのファイルから内容を読み込むのかを知っており、`IO String`としてファイルの中身を保持したI/Oアクションを返す。この関数は、ほとんど`getContents`のようだ。唯一の違いは、`getContents`は、自動的に標準入力(ターミナルからの入力)を読み込むのに対して、`hGetContents`は、どのファイルから読み込むのかを伝えるファイルハンドルを受け取る。その他すべての側面については、どちらも同じ働きをする。また、`getContents`のように、`hGetContents`も一度にファイルを読み込んでメモリに格納したりはせず必要に応じて読み込む。メモリにすべて読みこんでいるわけではないのにそれがファイルの中味すべてであるかのように扱えるので、かなり便利だ。つまり、もしこれが巨大なファイルであれば、`hGetContents`は、我々のメモリをぶっ壊したりはせず、ファイルからの読み込みが必要になったときに限って、ファイルから読み込む。

我々のプログラム内の`handle`に束縛されている中味について、ファイルを識別する、ということとファイルの中味を識別するということの違いに注目してほしい。あくまで`handle`はどれがどのファイル化を識別するものだ。想像してみよう。ファイルシステム全体が巨大な一冊の本であり、それぞれの章がそれぞれのファイルに対応する、と。そして、`handle`は今読んでいる、あるいは書き込みをしている章を示すしおりのようなものだ。しかし、実際にしおりが示すのは、章の内容となる。

`putStr contents`とすることで、標準出力にその内容を表示して、それから、ハンドルを受け取りファイルを閉じるI/Oアクションである`hClose`を行う。`openFile`でファイルを開いた場合は、自分でファイルを閉じなければならない。

同じことを実現する別の方法としては、`withFile`関数を使う方法がある。型シグネチャは`withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a`となっている。この関数は、ファイルのパスと`IOMode`、それからハンドルを受け取り何らかのI/Oアクションを返す関数を受け取る、この関数が返すI/Oアクションは、ファイルを開き、ファイルに対して行いたい何らかの操作を実行し、そのファイルを閉じる、というものだ。この関数が返す最後の結果を含んだI/Oアクションは、我々がこの関数に与えた関数の中で戻されるI/Oアクションの結果と同じだ。これは少々複雑に聞こえるかもしれない。しかし、特にラムダがしていることは単純だ。以下に、`withFile`を使って書き直したプログラムを示す。

    import System.IO
    
    main = do
        withFile "girlfriend.txt" ReadMode (\handle -> do
            contents <- hGetContents handle
            putStr contents)

ご覧のとおり、以前書いたコードとかなり似ている。`(\handle -> ...)`という関数は、ハンドルを受け取り、I/Oアクションを返す。また、`withFile`に与える関数として、ラムダを使うのが一般的だ。`withFile`がただ単にI/Oアクションを受け取り、それを実行してファイルを閉じるのではなく、I/Oアクションを返す関数を受け取らなくてはならない理由は、I/Oアクションだけを渡しても、それがどのファイルを操作すればいいのかわからないからだ。関数を受け取る方法であれば、`withFile`はファイルを開いた後にハンドラを関数へ渡せる。そして、その関数からI/Oアクションが返されたら、`withFile`はファイルを閉じるだけのI/Oアクションを作る。以下に、独自の`withFile`関数を作る方法を示す。

    withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
    withFile' path mode f = do
        handle <- openFile path mode
        result <- f handle
        hClose handle
        return result

結果がI/Oアクションになることは分かっているから、まずは`do`から説明しよう。まずは、ファイルを開いてそのハンドラを得る。それから、ハンドルを渡された関数に適用して、I/Oアクションを得る。そのアクションは`result`に束縛して、ハンドルを閉じてから、`result`という結果を返す。関数`f`から得た、結果をI/Oアクションで包んだものを返すことにより、`withFile'`が返すものを関数`f`が返すものと同じ、つまり結果をI/Oアクションで包んだものを返すようにしている。たとえば`f handle`が標準入力から何行か読み取って、その内容をファイルへと書き出した上で、読み取った内容を保持するI/Oアクションを返す場合、`withFile'`を使えば、`withFile'`の結果も同じく読み取った内容を保持するI/Oアクションを返すことになる。

`getContents`のように機能する`hGetContents`を使っているが、これはファイル専用だ。ということで、`hGetLine, hPutStr, hPutStrLn, hGetChar`なども用意されている。それぞれ、関数名から`h`を取り除いたものと対になる働きをする。ただし、これらの関数はそれぞれ標準入力や標準出力を操作する代わりに、ハンドルを受け取り、特定のファイルに対して操作を行う。たとえば、`putStrLn`関数は文字列を受け取り、それをターミナルへ表示して、さらに開業も行うというI/Oアクションを返す関数だ。一方、`hPutStrLn`関数は、ハンドルと文字列を受け取り、ハンドルによってファイルに文字列を書き込み、さらに開業も書き込むというI/Oアクションを返す。同様に、`hGetLine`もハンドルを受け取り、特定のファイルから1行読み込んだ結果を保持するI/Oアクションを返す。

ファイルから読み込んで、その内容を文字列として扱うというのは一般的なので、この作業を簡単にしてくれる、ちょっとした便利な関数が3つ用意されている。

`readFile`は、`readFile :: FilePath -> IO String`という型シグネチャを持つ。思い出してほしい。`FilePath`というのは、`String`のあだ名だ。`readFile`は、ファイルのパスを受け取り、ファイルを(もちろんlazyに)読み込んで、その内容を文字列として何かに束縛する、というI/Oアクションを返す。この関数を使うほうが、`openFile`にハンドルを渡して、その内容を`hGetContents`で読み取ってなにかに束縛するよりも簡単だ。以前書いたサンプルコードを`readFile`を使って書き直したものが、これだ。

    import System.IO
    
    main = do
        contents <- readFile "girlfriend.txt"
        putStr contents

ただし、この関数ではファイルを識別するためのハンドルは得られないので、手動でファイルを閉じることはできないかだ、Haskellが自動的に行なってくれる。

`writeFile`は、`writeFile :: FilePath -> String -> IO ()`という型シグネチャを持つ。この関数は、ファイルのパスと、そのファイルに書き込むための文字列を受け取り、書き込みを行うI/Oアクションを返す。もし書き込もうとしているファイルが既に存在する場合は、書き込みする前にファイルの中味を削除する。以下に、`girlfriend.txt`の内容を大文字にした`girlfriendcaps.txt`に変換する方法を示す。

    import System.IO
    import Data.Char
    
    main = do
        contents <- readFile "girlfriend.txt"
        writeFile "girlfriendcaps.txt" (map toUpper contents)

    $ runhaskell girlfriendtocaps.hs
    $ cat girlfriendcaps.txt
    HEY! HEY! YOU! YOU!
    I DON'T LIKE YOUR GIRLFRIEND!
    NO WAY! NO WAY!
    I THINK YOU NEED A NEW ONE!

`appendFile`も`writeFile`と同じような型シグネチャを持つが、こちらは既にファイルが存在する場合、ファイルの末尾に内容を追加する。

では、`todo.txt`という、1行ごとにしなければいけないタスクを書いたTODOリストのファイルが有るとしよう。では、標準入力から読み込んだタスクをTODOリストのファイルに書き込むというプログラムを作ってみよう。

    import System.IO
    
    main = do
        todoItem <- getLine
        appendFile "todo.txt" (todoItem ++ "\n")

    $ runhaskell appendtodo.hs
    Iron the dishes
    $ runhaskell appendtodo.hs
    Dust the dog
    $ runhaskell appendtodo.hs
    Take salad out of the oven

    $ cat todo.txt
    Iron the dishes
    Dust the dog
    Take salad out of the oven

`getLine`から得た文字列には改行が含まれないので、それぞれの行に`\n`を`++`で加える必要がある。

おっと、それともう一つ。　`contents <- hGetContents handle`が何をするのか説明した時に、ファイルの中味すべてを一度に読み込んで、メモリに格納するのではない、と説明した。これが、lazyなI/oだ。つまり、以下の様なことができる。

    main = do
        withFile "something.txt" ReadMode (\handle -> do
            contents <- hGetContents handle
            putStr contents)

これは、ファイルを何らかの出力へパイプで接続しているかのようだ。リストがストリームであると考えるように、ファイルもストリームであると考えられる。このプログラムは、一度に1行を読み込んで、それをターミナルへ出力する。そこで、あなたは高質問するだろう。ディスクにアクセスする頻度はどれくらいですか? パイプの幅はどれくらいですか? そうだね、テキストファイルに限っては、デフォルトで行バッファを使うのが一般的だ。つまり、ファイルから読み込むのは1度に1行までということだ。上記のプログラムで、1行読み込んではそれを表示して、また1行読み込んではそれを表示して、というのを繰り返しているのは、そのためだ。バイナリファイルについては、通常はデフォルトのバッファ方法がブロックバッファとなる。これは、ファイルの中身をチャンクごとに読み取ることを意味する。チャンクの大きさについては、OSが最適と考えるものを使用する。

`hSetBuffering`関数を使うことで、正確にバッファの大きさを設定することができる。
この関数は、ハンドルと`BufferMode`を受け取り、バッファの大きさを設定するI/Oアクションを返す。
`BufferMode`は列挙できるデータ型であり、
`NoBuffering, LineBuffering, BlockBuffering (Maybe Int)`
という値を保持する。
`Maybe Int`は、チャンクの大きさを1バイト単位で指定する。
`Nothing`を指定すると、OSがチャンクの大きさを決定する。
`NoBuffering`は、バッファリングをせず、1文字単位で読み込むことを意味する。
通常、`NoBuffering`はディスクへのアクセスが頻発するので、
バッファを吸い上げるモードといえる。

では、以前書いたプログラムを1行ずつ読み込むのではなく、ファイル全体を2048バイトのチャンクごとに読み込むようにしたものを示す。

    main = do
        withFile "something.txt" ReadMode (\handle -> do
            hSetBuffering handle $ BlockBuffering (Just 2048)
            contents <- hGetContents handle
            putStr contents)

チャンクのサイズを大きくしてファイルを読み込むことは、ディスクへのアクセス、あるいは低速なネットワーク越しのリソースへのアクセスを減らす。

また、`hFlush`という関数は、ハンドルを受け取り、ハンドルに関連したファイルのバッファを削除するというI/Oアクションを返す。たとえば、行バッファを行うときは、1行読み込むたびにそのバッファを削除する。ブロックバッファの場合は、1チャンクを読み込んだ後にバッファを削除する。また、ハンドルを閉じた後もバッファの削除が行われる。つまり、改行文字に到達すると、読み込み(あるいは書き込み)のメカニズムにより、データが尽きたことが報告される。しかし、`hFlush`を使えば、強制的にデータが尽きたことを報告できる。データが削除されると、同時に実行されている他のプログラムからデータへアクセスできるようになる。

ファイルをブロックバッファで読み込むというのは、次のように例えられる。まず、トイレの便器に1ガロンの水が貯まると、自動的に流すという設定をする。そして、トイレに水を注ぎ始めて、一度1ガロンに達すると、自動的に流される。その流された水、というのが読み取られるデータだ。しかし、便器に溜まった水はトイレのボタンを押すことで、手動で流すこともできる。こうすると、トイレに溜まった水(という名のデータ)を流し去る(読み取る)ことができる。まだ気づいていない方のために説明すると、手動でトイレの水を流すというのは`hFlush`のメタファーだ。これはプログラミングの説明における標準的な例え方ではないが、実世界の物事に例えることで、興味を引くことを狙っている。

我々は巣でに`todo.txt`というTODOリストに新しいアイテムを追加するプログラムを書いた。では、アイテムを削除するプログラムを作ってみよう。まず先にコードを見せるが、これからこのコードについての説明をするから、心配しないで欲しい。このプログラムでは、`System.Directory`に含まれるいくつかの関数と、`System.IO`に含まれる新しい関数を1つ使っているが、これらについては後で説明する。

ということで、以下が`todo.txt`からアイテムを削除するプログラムだ。

    import System.IO
    import System.Directory
    import Data.List
    
    main = do
        handle <- openFile "todo.txt" ReadMode
        (tempName, tempHandle) <- openTempFile "." "temp"
        contents <- hGetContents handle
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
        putStrLn "These are your TO-DO items:"
        putStr $ unlines numberedTasks
        putStrLn "Which one do you want to delete?"
        numberString <- getLine
        let number = read numberString
            newTodoItems = delete (todoTasks !! number) todoTasks
        hPutStr tempHandle $ unlines newTodoItems
        hClose handle
        hClose tempHandle
        removeFile "todo.txt"
        renameFile tempName "todo.txt"

まず、`todo.txt`を読み取りモードで開き、そのハンドルを`handle`に束縛している。

続いて、今までに見たことのない`System.IO`の`openTempFile`という関数を使っている。この関数の働きは、その名が示すとおりだ。この関数は、一時的なディレクトリのパスとファイル名を受け取り、一時的なファイルを開く。一時的なディレクトリのパスとして、`"."`というのを使っているが、どんなOSでもドット`.`というのは、カレントディレクトリを意味するからだ。また、`"temp"`という名前を一時的なファイルの名前として使っているが、`temp`というのはtemporaryの意味であり、この名前の後にランダムな文字列が続く。この関数は一時的なファイルを作るI/Oアクションを返し、I/Oアクションが保持する結果は、一時的なファイルの名前とそのハンドル、という値のペアとなる。もちろん、普通のファイルとして例えば`todo2.txt`のようなファイルを開くこともできるが、`openTempFile`を使えばすべてを上書きしてしまう心配はないので、これを使うのがベターだ。

一時的なファイルを開くために、`getCurrentDirectory`を使って、カレントディレクトリを得てから、それを`openTempFile`に直接渡すのではなく、`.`を`openTempFile`に渡している理由は、Windowsやunixらいくなシステムどちらでも、カレントディレクトリを参照するからだ。

では次。`todo.txt`の内容を文字列として`contents`に束縛している。それから、その文字列をそれぞれの行で分割して、文字列のリストにしている。つまり、TODOのタスクは、`["Iron the dishes", "Dust the dog", "Take salad out of the oven"]`というリストになる。そして、文字列のリストに対して`0`から始まる数値を`3`のような数値と`"hey"`のような文字列を受け取り、`"3 - hey"`を返すような関数でマッピングする。つまり、`numberedTasks`の役割は、["0 - Iron the dishes", "1 - Dust the dog" ....`というリストを作ることだ。最後に、文字列のリストを`unlines`で開業により区切られた一つの文字列へと結合して、それをターミナルに表示する。もちろん、こうする代わりに`mapM putStrLn numberedTasks`としても同じことが実現できる。

そして、数字を入力してもらうことで、ユーザーにどのタスクを削除したいかを尋ねる。では、1番目のタスク、`Dust the dog`というタスクを削除するとしよう。ということで、`1`を入力する。これで、`numberString`は`"1"`になったが、我々がほしいのは文字列ではなく数値なので、`read`を`numberString`に対して実行し、`number`に束縛する。

`Data.List`の`delete`と`!!`を思い出して星い。`!!`はリストからあるインデックスの要素を返し、`delete`は、リストから最初に一致した要素を削除し、その要素を取り除いた新しいリストを返すのだった。`(todoTasks !! number)`は、(既に`number`が`1`になっているので)"Dust the dog"`という文字列を返す。`todoTasks`から"Dust the dog"`という最初に一致したものを取り除いたリストを、`newTodoItems`に束縛して、我々が開いた一時ファイルに書き込みをする前に、`unlines`を使って、一つの文字列へと結合している。これで、古いファイルを変更することなく、1行が削除されていることを除いて、一時ファイルには元のファイルと同じ内容が保持された。

その後、元のファイルと一時ファイルの両方を閉じてから、見ての通り、パスを受け取ってそれを削除する`removeFile`を使って、元のファイルを削除する。元のファイルを削除した後に、`renameFile`を使って、一時ファイルの名前を`todo.txt`に変更する。(ちなみに、`removeFile`と`renameFile`は`System.Directory`にある。)`removeFile`と`renameFile`は、どちらもパラメータとしてハンドルではなくファイルのパスを受け取る関数なので注意してほしい。

以上! かなり少ない行数でこれを実現できたが、既に存在するファイルを上書きしないよう、慎重に操作しており、OSに対して一時ファイルをどこへ保存すべきかを丁寧に訪ねている。では、確認してみよう。

    $ runhaskell deletetodo.hs
    These are your TO-DO items:
    0 - Iron the dishes
    1 - Dust the dog
    2 - Take salad out of the oven
    Which one do you want to delete?
    1

    $ cat todo.txt
    Iron the dishes
    Take salad out of the oven

    $ runhaskell deletetodo.hs
    These are your TO-DO items:
    0 - Iron the dishes
    1 - Take salad out of the oven
    Which one do you want to delete?
    0

    $ cat todo.txt
    Take salad out of the oven



## Command line arguments

ターミナル上で動くスクリプト、あるいはアプリケーションを作るなら、コマンドライン引数を扱うのは必須といえる。幸運にも、Haskellの標準ライブラリには、コマンドライン引数をイイ感じに扱う方法が用意されている。

前の節では、TODOリストにタスクを追加するプログラムと、TODOリストからタスクを削除するプログラムを作った。ところで、我々が採用したアプローチには2つの問題がある。ひとつは、`todo.txt`というファイルの名前をプログラム内にハードコードしている点だ。我々が`todo.txt`というファイルに決めてしまったので、ユーザーは複数のTODOリストを管理することができない。

これに対する一つの解決策としては、毎回ユーザーにTODOリストのファイル名を尋ねる、というのがある。このアプローチは、TODOリストからアイテムを削除するときに採用した方法と同じだ。ということで、これは機能する。しかし、ユーザーがプログラムを実行すると、プログラムからなにか尋ねられるのを待って、それに答えるように、と要求されるのは便利とはいえない。これはインタラクティブなプログラムと呼ばれ、インタラクティブなコマンドラインのプログラムを使ってバッチスクリプトのようにプログラムの実行を自動化しようとするのは、ややこんなんだ。バッチスクリプトは、1つあるいはいくつかのプログラムwお実行するのにいは向いているが、インタラクティブなプログラムを作るのには向いていない。

このため、プログラムを一度実行してからユーザーに尋ねるのではなく、ユーザーがプログラムに何をしてほしいかを伝えるほうが良い。そして、プログラムの実行時にユーザーがプログラムに何をして欲しいかを伝える手段としては、コマンドライン引数を使う方法がある。

モジュール`System.Environment`にはかなり便利なI/Oアクションが用意されている。ひとつは`getArgs`で、`getArgs :: IO [String]`という型を持っており、このI/Oアクションは、プログラムが実行される時の引数を受け取り、結果として引数をリストとして保持する。`getProgName`は、`getProgName :: IO String`という型を持っており、そのI/Oアクションはプログラム自身の名前を保持する。

以下に、これら2つの関数がどのように機能するかを示す。

    import System.Environment
    import Data.List
    
    main = do
       args <- getArgs
       progName <- getProgName
       putStrLn "The arguments are:"
       mapM putStrLn args
       putStrLn "The program name is:"
       putStrLn progName

`getArgs`と`progName`を`args`と`progName`に束縛している。これで、`putStrLn`で`args`を表示することで、引数をすべて表示できる。最後に、プログラムの名前も表示する。では、これを`arg-test`として、コンパイルしよう。

    $ ./arg-test first second w00t "multi word arg"
    The arguments are:
    first
    second
    w00t
    multi word arg
    The program name is:
    arg-test

いいね。この知識で武装したあなたは、素晴らしいコマンドラインアプリを作成できるだろう。では実際に、なにか作ってみよう。前の節では、タスクを追加するプログラムと、タスクを削除するプログラムを別々に作った。では、コマンドライン引数に基づいて実行するように、2つのプログラムを結合することにしよう。加えて、`todo.txt`だけではなく、別のファイルも操作できるようにする。

このプログラムを`todo`と呼ぶことにして、異なる3つのことができるようにしよう。

* タスクの確認
* タスクの追加
* タスクの削除

とりあえず、今はまだ不正な入力について気にしないことにしよう。

たとえば、我々のプログラムを使って、魔法の剣を探す、というタスクを`todo.txt`に追加するにはターミナルに`todo add todo.txt "Find the magic sword of power"`と打ち込むようにする。また、ユーザーがタスクを確認するには、`todo view todo.txt `、2番目のタスクを削除するには`todo remove todo.txt 2`と入力するようにする。

まず、連想リストを`dispatch`するところから始める。これは、コマンドライン引数がキーで、それに対応する関数が値となる単純な連想リストにするとしよう。これらの関数の型は、`[String] -> IO ()`となる。これらの関数は、引数のリストをパラメータとして受け取り、タスクの確認、追加、あるいは削除を行うI/Oアクションを返す。

    import System.Environment
    import System.Directory
    import System.IO
    import Data.List
    
    dispatch :: [(String, [String] -> IO ())]
    dispatch =  [ ("add", add)
                , ("view", view)
                , ("remove", remove)
                ]

まだ`main`、`view`、`add`それから`remove`を定義していないから、まずは`main`から定義しよう。

    main = do
        (command:args) <- getArgs
        let (Just action) = lookup command dispatch
        action args

まず、引数を`(command:args)`に束縛している。パターンマッチングについて覚えているだろうか。これは、引数の1つめを`command`に、残りの引数を`args`に束縛することを意味する。たとえば、`todo add todo.txt "Spank the monkey"`のようにしてプログラムを実行すると、`command`は`"add"`になり、`args`は、`["todo.txt", "Spank the monkey"]`になる。

次の行では、ディスパッチリストからコマンドを探している。`"add"`は`add`を指し示しているから、`Just add`という結果を得る。ここで、`Maybe`から関数を抽出するため、パターンマッチングを再度使用している。では、ディスパッチリストにコマンドがなければ何が起こるだろう? そうだね、`lookup`は`Nothing`を返す。しかし、最初に断ったように、今回はその点については華麗にスルーするので、パターンマッチングは失敗して、プログラムはクラッシュする。

最後に、残った引数のリストを使って、`action`を呼び出す。この関数は、アイテムを追加したり、アイテムを表示したり、アイテムを削除したりした結果を保持するI/Oアクションを返す。また、これらは``main`のdo`ブロック内にあるので、アクションは実行される。ここまでの具体的な例に従うなら、`action`に相当する関数は`add`であり、(引数`["todo.txt", "Spank the monkey"を使って呼び出され)猿をこらしめる、というタスクを`todo.txt`に追加するI/Oアクションを返す。

すばらしい! これで全て説明し終えたから、確認、追加、削除を実装しよう。まずは`add`からだ。

    add :: [String] -> IO ()
    add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

例えば、プログラムを`todo add todo.txt "Spank the monkey"`のように呼び出すと、`"add"`は、最初にメインブロック内でマッチしたコマンドを束縛し、`["todo.txt", "Spank the monkey"]`というリストをディスパッチリストから得た関数に渡す。また、不正な入力については今のところ扱わないので、関数`add`は、これら2つの要素を持つリストにたいしてパターンマッチングをするだけでよく、開業とともにファイルの末尾へ追加するI/Oアクションを返す。

次は、TODOリストの中味を確認する関数を実装しよう。ファイルの中味を確認したい場合は、`todo view todo.txt`とする。つまり、最初のパターンマッチングでは、`command`は`view`に、`args`は`["todo.txt"]`に束縛される。

    view :: [String] -> IO ()
    view [fileName] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
        putStr $ unlines numberedTasks

我々は、タスクを削除するだけのプログラムで、タスクを表示して、ユーザーがどれを削除するか選択できるようにするというプログラムで、既に同じことをしている。ただし、ここではタスクを表示しているだけだ。

最後に、`remove`を実装することに仕様。これは、タスクを削除するだけのプログラムとかなりに多様なものになるので、ここでは、どのようにアイテムを削除するかについて理解する必要はない。プログラムの説明だけを確認しよう。最も大きな違いといえば、`todo.txt`をハードコードしていないところだ。その代わり、引数から読み取る。また、プロンプトでユーザーにどのタスクを削除するか尋ねるのではなく、これも引数から読み取っている。

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

まず、ファイル名に基づいたファイルと、一時ファイルを開き、インデックスで指定された、ユーザーが削除したい行を削除し、それを一時ファイルに読み込み、オリジナルのファイルを削除し、一時ファイルを元のファイル名へと変更する。

以下に、これら全てをまとめた、栄光のプログラムを示す。

    import System.Environment
    import System.Directory
    import System.IO
    import Data.List
    
    dispatch :: [(String, [String] -> IO ())]
    dispatch =  [ ("add", add)
                , ("view", view)
                , ("remove", remove)
                ]
    
    main = do
        (command:args) <- getArgs
        let (Just action) = lookup command dispatch
        action args
    
    add :: [String] -> IO ()
    add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
    
    view :: [String] -> IO ()
    view [fileName] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
        putStr $ unlines numberedTasks
    
    remove :: [String] -> IO ()
    remove [fileName, numberString] = do
        handle <- openFile fileName ReadMode
        (tempName, tempHandle) <- openTempFile "." "temp"
        contents <- hGetContents handle
        let number = read numberString
            todoTasks = lines contents
            newTodoItems = delete (todoTasks !! number) todoTasks
        hPutStr tempHandle $ unlines newTodoItems
        hClose handle
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName

我々の提案をまとめると、コマンドから引数を受け取り、I/Oアクションを返す関数への連想リストとしてディスパッチリストを用意した。我々はコマンドが何であるかを確認し、そしてディスパッチリストからコマンドを元に、適切な関数を取得した。それから、残りのコマンドライン引数を使ってその関数を呼び出し、適切なI/Oアクションを得てから、それを実行した。

他の言語では、これは巨大な`switch-case`文などにより実装されることになるだろう。しかし、高階関数を使うことで、ディスパッチリストから、適切な関数をとり出すことができ、そのコマンドライン引数を受取る関数からI/Oアクションを受け取ることができる。

では、我々のアプリを試してみよう。

    $ ./todo view todo.txt
    0 - Iron the dishes
    1 - Dust the dog
    2 - Take salad out of the oven

    $ ./todo add todo.txt "Pick up children from drycleaners"

    $ ./todo view todo.txt
    0 - Iron the dishes
    1 - Dust the dog
    2 - Take salad out of the oven
    3 - Pick up children from drycleaners

    $ ./todo remove todo.txt 2

    $ ./todo view todo.txt
    0 - Iron the dishes
    1 - Dust the dog
    2 - Pick up children from drycleaners

このプログラムの他に面白いところは、機能を簡単に抽出できる、というところだ。ディスパッチリストにエントリと、それに対応する関数を追加するだけで、望みが叶う。エクササイズとして、`bump`関数を実装してみるのはどうだろう。ファイルとタスクの番号を受け取り、そのタスクをタスクリストの先頭へと移動するI/Oアクションを返す関数だ。

また、このプログラムが不正な入力を受け取った時に、より優雅に失敗するように(たとえば、誰かが`todo UP YOURS HAHAHAHA`などと入力したときに)エラーの内容(たとえば、`errorExit :: IO ())を報告するI/Oアクションを作ることで、入力がエラーを含むかどうかを判定し、入力がエラーを含んでいればそのエラー報告をするI/Oアクションを実行する。あるいは、この後すぐに説明する例外を使う方法がある。



## Randomness

プログラミングをしていると、ランダムなデータが必要になることが多々ある。それはサイコロを投げるゲームを作っている時かもしれないし、プログラムをテストするためにテストデータを生成する必要があるときかもしれない、プログラミングにおけるランダムなデータの使いみちは多々ある。片手にチーズを持った一輪車に乗っている猿が真のランダム差の厳選であることはご存知かと思うが、`pseudo-random`の厳選は、実際には異なる。この節では、Haskellではどのようにランダムに見えるデータを生成するのか、について見ていく。

他のプログラミング言語の多くでは、ランダムな値を返す関数が用意されているだろう。その関数を呼び出すたびに毎回異なるランダムな値が返される(であろう)ことが期待できる。では、Haskellはどうか? そうだね、Haskellは純粋な関数プログラミング言語である、ということを思い出してほしい。これは、Haskellが参照透過性を持つことを意味する。つまり、同じパラメータを与えれば、関数を2回よびだしても2回とも同じ結果が生成されることを意味する。これが素晴らしいのは、個々のプログラムを推論可能にしてくれたり、ほんとうに必要になるまで評価を遅らせることができるからだ。たとえば、関数を呼び出しても、結果を返すまでの間に関数が変なことをしない、という確証が持てる。つまり、関数の結果のみに集中できる。しかし、この制約のおかげで、ランダムな値の取得方法が裏ワザのようになってしまう。たとえば、以下の様な関数があるとしよう。

    randomNumber :: (Num a) => a
    randomNumber = 4

常に`4`を返すので、これは乱数の関数としては全然役に立たない。しかしながら、`4`という数字は、私がサイコロを振って決めたので、この関数が返す数字がランダムであることは保証できる。

では、他のプログラミング言語では、どのようにランダムなように見える値を生成しているのだろう? そうだね、あなたのコンピューターから取得できる様々な情報、たとえば現在の時刻、マウスカーソルの移動量、あなたのPCから発生している騒音などの情報に基づいて、本物の乱数のようにみえる数値を返しているのだ。これらの要因の組合せ(つまり、ランダム性)というのは、時間軸上のある瞬間において異なるものであり、これにより、ランダムな数値が得られる。

ああ、そうか! つまり、Haskellではランダムな数値を作ってから、そのランダム性をパラメータとして受け取り、ランダムな数値(あるいはデータ型)を返す関数を作れば良いのか。

では、`System.Random`モジュールに踏み込もう。このモジュールには、我々のランダム性の要求を満たす関数が含まれている。モジュールが公開している関数の内の一つである、`random`という名前通りの関数に飛び込んでみよう。この関数の型は、`random :: (RandomGen g, Random a) => g -> (a, g)`だ、おお、型宣言の中にいくつか新しい型クラスが登場した! `RandomGen`は、ランダム性の要素として振る舞う型のための型クラスだ。型クラス`Random`は、ランダムな値を受け取るためのものだ。たとえば真偽値は、`その名の通りTrue`か`False`か、というランダムな値を受け取れる。また、数値も同様に、過剰なほどランダムな値を受け取ることができる。では、関数はランダムな値を受け取れるのか? いや、受け取れないとは思えない! 関数`random`の肩宣言を英語に翻訳するとしたら、こんな感じになるだろう。`random`は乱数生成器(ランダム性の厳)を受け取り、ランダムな値と新しい乱数生成器を返す。なぜ、ランダムな値だけではなく、新しい乱数生成器も一緒に返すのだろう? これについては、すぐ後に説明する。

さて、`random`関数を使うには、なんらかの乱数生成器を入手する必要がある。`System.Random`モジュールは、型クラス`RandomGen `のインスタンスである、`StdGen`という、その名の通りの素晴らしい型を公開している。`StdGen`は、手動で作るか、あるいは、ランダムな何かに基づいた`StdGen`を一つくださいな、とシステムに伝えることもできる。

乱数生成器を主導で作るには、`mkStdGen`という関数を使う。この関数は、`mkStdGen :: Int -> StdGen`という型を持つ。この関数は、整数を受け取り、それに基づいた乱数生成器を返す。いいね。では、`random`と`mkStdGen`を使って(面倒だけど)ランダムな数値を取得してみよう。

    ghci> random (mkStdGen 100)
    <interactive>:1:0:
        Ambiguous type variable `a' in the constraint:
          `Random a' arising from a use of `random' at <interactive>:1:0-20
        Probable fix: add a type signature that fixes these type variable(s)

なんだこれ? あっ、そういうことか。`random`関数が返す値は、型クラス`Random`に属する型のあたいだから、Haskellに、どの型の値がほしいのか、という情報を伝えなくてはいけない。それから、`random`関数が返すのは、ランダムな値と乱数生成器のペアであることもお忘れなく。

    ghci> random (mkStdGen 100) :: (Int, StdGen)
    (-1352021624,651872571 1655838864)

ついに! ランダムなように見える数値だ! タプルの最初の要素が目的の数値であり、2つめの要素は新しい乱数生成器の文字列表現となっているでは、`random`関数を同じ乱数生成器をを与えて呼び出すと、何が起きるだろう?

    ghci> random (mkStdGen 100) :: (Int, StdGen)
    (-1352021624,651872571 1655838864)

もちろん、同じパラメータは同じ結果を生成する。では、異なる乱数生成器をパラメータとして与えてみよう。

    ghci> random (mkStdGen 949494) :: (Int, StdGen)
    (539963926,466647808 1655838864)

その通り、素晴らしい。違う数値が得られた。型注釈を使うことで、異なる型で`random`関数が返す値を受け取ることができる。

    ghci> random (mkStdGen 949488) :: (Float, StdGen)
    (0.8938442,1597344447 1655838864)
    ghci> random (mkStdGen 949488) :: (Bool, StdGen)
    (False,1485632275 40692)
    ghci> random (mkStdGen 949488) :: (Integer, StdGen)
    (1691547873,1597344447 1655838864)

では、コインを3回投げるのをシミュレーションしてみよう。もし`random`が、ランダムな値とともに新しい乱数生成器を返さなかったとしたら、この関数はパラメータとして3つの乱数生成器を受け取るようにする必要がある。それから、それぞれのコイントスの結果を返すことになる。しかし、これは間違っている気がする。もし、1つの乱数生成器が`Int`型の(異なる値を(継ぎ足すことで)ランダムな値を生成できるとしたら、(正確には8つの組合せを持つことになる)コイントスが3回できるはずだ。`random`関数がランダムな値とともに乱数生成器を返すのは、このような利便性があるからだ。

では、コインの裏表を`Bool`で表現するとしよう。表が`True`で、裏が`False`だ。

    threeCoins :: StdGen -> (Bool, Bool, Bool)
    threeCoins gen =
        let (firstCoin, newGen) = random gen
            (secondCoin, newGen') = random newGen
            (thirdCoin, newGen'') = random newGen'
        in  (firstCoin, secondCoin, thirdCoin)

`random`をジェネレータを与えて呼ぶと、パラメータとしてコインと新しいジェネレーターが得られる。それから、2番目のコインを得るために、その新しいジェネレータを使って、再度関数を呼び出す。同じことを、3番目のコインにも行う。もし、同じジェネレータを使って、毎回関数を呼び出したら、全てのコインが同じ値を持つことになるから、`(False, False, False)`あるいは`(True, True, True)`の、どちらかの結果しか得られなくなるだろう。

    ghci> threeCoins (mkStdGen 21)
    (True,True,True)
    ghci> threeCoins (mkStdGen 22)
    (True,False,True)
    ghci> threeCoins (mkStdGen 943)
    (True,False,True)
    ghci> threeCoins (mkStdGen 944)
    (True,True,True)

`random gen :: (Bool, StdGen)`とする必要がないところに注目してほしい。これは、既に関数の型宣言で、`Bool`の値が欲しいことを指定しているからだ。Haskellがこの場合においては真偽値であると推論できるのはそのためだ。

では、4枚のコインをひっくり返したいとしたら? あるいは、5枚の場合は? そうだね、`randoms`と呼ばれる関数があり、この関数はジェネレーターを受け取り、そのジェネレータに基づいた値を無限に生成し続けるリストを返す。

    ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
    [-1807975507,545074951,-1015194702,-1622477312,-502893664]
    ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
    [True,True,True,True,False]
    ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
    [7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]

なぜ`randoms`は、リストとともに新しいジェネレータを返さないのだろう? `randoms`関数は、以下のように簡単に実装できる。

    randoms' :: (RandomGen g, Random a) => g -> [a]
    randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

再帰的な定義だ。現在のジェネレーターから、ランダムな値と、新しいジェネレーターを取得する。それから、そのランダムな値をリストの先頭、そして、そのランダムな値に基づいた新しいジェネレーターをリストの末尾とするリストを作る。`randoms`が新しい乱数のジェネレーターを返すことができないのは、ランダムな数値を無限に生成できるような余地を残すためだ。
もちろん、以下のように有限のランダムな値のストリームを作り、新しいジェネレーターを返す関数を作ることもできる。

    finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
    finiteRandoms 0 gen = ([], gen)
    finiteRandoms n gen =
        let (value, newGen) = random gen
            (restOfList, finalGen) = finiteRandoms (n-1) newGen
        in  (value:restOfList, finalGen)

これも再帰的な定義だ。たとえば、ランダムな値を0個求める場合は、この関数は、空のリストと与えられたジェネレーターをそのまま返すだけでよい。ランダムな値を1つ以上求める場合は、最初に、先頭の値となる1つのランダムな値と、新しいジェネレーターを求める必要がアアル。すると、新しいジェネレーターによって生成された末尾の要素数は`n - 1`となる、最後に、リストの先頭と末尾を結合したものと、`n - 1`番目のランダムな数値から求めたジェネレーターを返す。

では、ランダムな値のレンジ、のようなものはどうだろう? ここまでのランダムな整数というのは、極端に大きいか、小さかったりする。もし、サイコロを振りたい場合はどうするのだろう? そうだね、`randomR`はこの目的のために使う。この関数は、`randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)`という型を持つ。これは、`random`のような関数であることを意味するが、上限と下限の境界線を設定する値のペアを最初のパラメータとして受け取り、最後に、その境界内にある値を生成する。

    ghci> randomR (1,6) (mkStdGen 359353)
    (6,1494289578 40692)
    ghci> randomR (1,6) (mkStdGen 35935335)
    (3,1250031057 40692)

`randomRs`という関数もあり、こちらはある境界内の範囲で、ランダムな値を生成して、無限リストとして返す。

    ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
    "ndkxbvmomg"

いいね! 秘密のパスワードみたいだ。

ところで、この節はI/Oアクションと一緒にする必要があるのか? と、あなたは自問自答するかもしれない。ここまで、特にI/Oについて考えることはしなかった。そうだね、ここまでは任意の整数を使うことで、常に我々は乱数生成器を主導で作ってきた。実際にプログラミングするときの問題点は、これらの関数は常に同じランダムな値を返すという点であり、使い勝手が良くないということで、`System.Random`は、`IO StdGen`という型を持つI/Oアクションを返す`getStdGen`関数を提供している。プログラムが実行されると、システムに対して、最適な乱数生成器を訪ね、それをグローバルジェネレーターとして保存する。`getStdGen`を何かに束縛すると、グローバルジェネレーターを持ってくる。

以下は、ランダムな文字列を生成する単純なプログラムだ。

    import System.Random
    
    main = do
        gen <- getStdGen
        putStr $ take 20 (randomRs ('a','z') gen)

    $ runhaskell random_string.hs
    pybphhzzhuepknbykxhe
    $ runhaskell random_string.hs
    eiqgcxykivpudlsvvjpg
    $ runhaskell random_string.hs
    nzdceoconysdgcyqjruo
    $ runhaskell random_string.hs
    bakzhnnuzrkgvesqplrx

しかし、注意しなくてはいけないのが、`getStdGen`を2回実行するということは、システムに同じグローバルジェネレーターを2回要求することになる、という点だ。

    import System.Random
    
    main = do
        gen <- getStdGen
        putStrLn $ take 20 (randomRs ('a','z') gen)
        gen2 <- getStdGen
        putStr $ take 20 (randomRs ('a','z') gen2)

もし上記のようなことをすると、同じ文字列が2回表示されることになる。文字数が20文字の異なる文字列を連結する一つの方法としては、無限ストリームを用意して、最初の20文字を取り出して、それを1行で表示して、それから2番目の20文字を取り出して、それを2行目に表示する。この場合、`Data.List`の`splitAt`関数を使う。この関数は、リストをあるインデックスで分割して、リストの最初の部分を最初の要素に、2番目の部分を2番目の要素とするタプルとして返す。

    import System.Random
    import Data.List
    
    main = do
        gen <- getStdGen
        let randomChars = randomRs ('a','z') gen
            (first20, rest) = splitAt 20 randomChars
            (second20, _) = splitAt 20 rest
        putStrLn first20
        putStr second20

べつのほうほうとしては、`newStdGen`というI/Oアクションを使う方法がある。これは、現在の乱数生成器を2つの乱数生成器へと分割する。そして、分割されたどちらかのジェネレーターで現在のグローバルジェネレーターを更新して、もう一方の値を包んで返す。

    import System.Random
    
    main = do
        gen <- getStdGen
        putStrLn $ take 20 (randomRs ('a','z') gen)
        gen' <- newStdGen
        putStr $ take 20 (randomRs ('a','z') gen')

新しいランダムジェネレーターを取得するときだけではなく、`newStdGen`をなにかに束縛するときも、グローバルジェネレーターが更新される。つまり、`getStdGen`を再びなにかに束縛すると、`gen`とは同じでない、新しいジェネレーターを取得できる。

以下は、ユーザーが考えている数字を当てずっぽうで応えるという、ちょっとしたプログラムだ。

    import System.Random
    import Control.Monad(when)
    
    main = do
        gen <- getStdGen
        askForNumber gen
    
    askForNumber :: StdGen -> IO ()
    askForNumber gen = do
        let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
        putStr "Which number in the range from 1 to 10 am I thinking of? "
        numberString <- getLine
        when (not $ null numberString) $ do
            let number = read numberString
            if randNumber == number
                then putStrLn "You are correct!"
                else putStrLn $ "Sorry, it was " ++ show randNumber
            askForNumber newGen
    jack of diamonds

我々は数値を尋ねる関数を作った。関数は、乱数生成器を受け取り、ユーザーにプロンプトを表示するI/Oアクションを返す。そして、ユーザーに推測したものが当たりかどうかを伝える。この関数では、まず、パラメータとして受け取ったジェネレーターに基づいたランダムな値と新しいジェネレータを生成する。これらは`randNumber`と`newGen`と呼ぶことにする。では、`7`により数値が生成されるとしよう。そして、ユーザーに、我々が考えている数値が何かを推測してもらう。`getLine`を実行して、取得した結果は`numberString`に束縛する。ユーザーが`7`を入力すると、`numberString`は`7`になる。次に、`when`を使って、ユーザーが入力した文字列が空の文字列かを判定する。もしそうであれば、`()`を返す空のI/Oアクションを実行して、プログラムを終了する。そうでなければ、`do`ブロック内にあるI/Oアクションが実行される。数値を変換するために`read`を`numberString`に対して使用し、`7`という数値を取得する。

おっと、申し訳ない! もし、ユーザーから(`"haha"`のような)`read`で読み取れない入力を渡された場合、我々のプログラムはみっともないエラーメッセージを表示してクラッシュする。もし、エラーを含む入力に対してクラッシュしないようにしたければ、`reads`を使う。この関数は、読み取りが失敗すると、空のリストを返す。成功した場合は、シングルトンリストのタプルが返される。このタプルはひとつの要素に、期待する値が、そして、残りの要素にはまだ消費されていない文字列が含まれる。

ユーザーが入力した数値と、ランダムに生成された数値が等しいかを判定し、ユーザーに対して適切なメッセージを表示する。それから、再帰的に数値を尋ねるのを続け、ただし、今回は別のジェネレーターにもとづいて一度実行したようなI/Oアクションから取得した新しいジェネレーターを使って、実行する。

`main`は、システムからランダムジェネレータを取得して、最初のアクションとして、`askForNumber`にそれを渡して呼び出す。

以下が、プログラムの動作だ。

    $ runhaskell guess_the_number.hs
    Which number in the range from 1 to 10 am I thinking of? 4
    Sorry, it was 3
    Which number in the range from 1 to 10 am I thinking of? 10
    You are correct!
    Which number in the range from 1 to 10 am I thinking of? 2
    Sorry, it was 4
    Which number in the range from 1 to 10 am I thinking of? 5
    Sorry, it was 10
    Which number in the range from 1 to 10 am I thinking of?

以下は、同じプログラムを作る別の方法だ。

    import System.Random
    import Control.Monad(when)
    
    main = do
        gen <- getStdGen
        let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
        putStr "Which number in the range from 1 to 10 am I thinking of? "
        numberString <- getLine
        when (not $ null numberString) $ do
            let number = read numberString
            if randNumber == number
                then putStrLn "You are correct!"
                else putStrLn $ "Sorry, it was " ++ show randNumber
            newStdGen
            main

これは前のバージョンとよく似ている。しかし、ジェネレータを受け取る関数を作る代わりに、再帰的に呼び出すことで、ジェネレーターを更新している。それがすべて`main`の中で行われている。ユーザーに推測したものがあっているか間違っているかを伝えた後にグローバルジェネレーターを更新して、再び`main`を呼び出す。どちらのアプローチも適切だ。しかし、私は最初のアプローチのほうが好みだ。`main`の中味が少なくて済むのと、再利用可能な関数を提供してくれるからだ。

