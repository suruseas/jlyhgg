---
title: "Input and Output"
layout: article
---



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



## Bytestrings

WIP



## Exceptions

WIP

