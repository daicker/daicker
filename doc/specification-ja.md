
# 言語仕様

- [言語仕様](#言語仕様)
  - [関数](#関数)
    - [ストリーム記法](#ストリーム記法)
    - [式の折り返し](#式の折り返し)
    - [CLIからの関数の呼び出し](#cliからの関数の呼び出し)
  - [実行環境](#実行環境)
  - [環境依存](#環境依存)
    - [環境依存関数](#環境依存関数)
    - [ユーザ定義の環境依存関数](#ユーザ定義の環境依存関数)
  - [状態管理](#状態管理)
    - [状態管理](#状態管理-1)
    - [ステートフルな関数](#ステートフルな関数)
  - [並列処理](#並列処理)
  - [データ型](#データ型)
    - [基本型](#基本型)
    - [リテラル型](#リテラル型)
    - [篩型](#篩型)
    - [合成型](#合成型)
      - [配列](#配列)
      - [順序組](#順序組)
    - [マップ](#マップ)
      - [レコード](#レコード)
    - [型推論](#型推論)
  - [モジュール](#モジュール)
    - [import](#import)
      - [ローカルインポート](#ローカルインポート)
      - [リモートインポート](#リモートインポート)
    - [export](#export)
  - [ファイルシステム](#ファイルシステム)
  - [タグ](#タグ)
  - [例外処理](#例外処理)
  - [組み込み関数](#組み込み関数)
  - [標準ライブラリ](#標準ライブラリ)
  - [メタデータ](#メタデータ)

## 関数

関数は次のように定義する。関数名に続いて仮引数を記載する。関数の本体を`:=`の後に記述する。
多くのプログラミング言語と違い引数の括弧は記載しない。

```daic
<関数名> <引数>* = [式]
```

例えば、簡単な例で引数をそのまま返却する関数は次のように記述する。

```daic
f x = x
```

また、`echo`コマンドを呼び出してメッセージをコンソールに出力する関数は次にように記述する。
`$`は指定されたコマンドを呼び出す組み込み(`Prelude`)関数である。

```daic
echo message = $ "echo" message
```

### ストリーム記法

関数の返却値を次の関数へ引き渡す書き方です。
右ストリーム演算子`|>`を使う。

```daic
addAndPrint x y = x + y |> \ans -> print ans
```

左に結果を流す場合は左ストリーム演算子`<|`を使う。

```daic
addAndPrint x y = \ans -> print ans <| x + y
```

###　do式

do式はストリーム記法の糖衣構文である。

```daic
f x y = do
  ans <- x + y    // x + y |> \ans ->
  print ans       // print ans
```

### 式の折り返し

1段深いインデントをすると前の式の続きであるとみなす。

```daic
f x y
  = x + y
  |> \ans -> ans * 2
```

### CLIからの関数の呼び出し

実行のためのサブコマンドは次の4つがある。

* `eval`
* `eval-flag-style`
* `run`
* `run-flag-style`

コマンドライン引数から関数の引数を与える場合は次のように呼び出す。

```bash
$ daicker eval f 1 2

3
```

`run-flag-style`を用いると仮引数の名前のフラグで実引数を

```bash
$ daicker eval-flag-style f -x 1 -y 2

3
```

標準入力から関数の引数を与える場合は次のように呼び出す。

```bash
$ echo "1" | daicker run f @stdin 2

3
```

また、標準入力を最後の引数として引き渡す場合は`@stdin`を省略できる。

```bash
$ echo "2" | daicker run f 1

3
```

上記は次と同義である。

```bash
$ echo "2" | daicker run f 1 @stdin

3
```

`*-flag-style`系のサブコマンドである場合には`@stdin`は省略できない。

コマンドライン引数はデフォルトで次のルールでデコードする。

| フォーマット        | 説明                 |
| ------------------- | -------------------- |
| `null`              | `null`型とみなす。   |
| `true`または`false` | `bool`型とみなす。   |
| `\d+(\.\d+)?`       | `number`型とみなす。 |
| `@stdin`            | 標準入力とみなす。   |
| 上記以外            | `string`型とみなす。 |

デコードすることなく単純な文字列の`"1"`を引数として渡したい場合には次のように呼び出す。

```bash
$ daicker run --args-decode string f 1 2
```

標準入力はデフォルトでJSONでデコードする。
JSONでデコードすることなく単純な文字列の`"1"`を引数として渡したい場合には次のように呼び出す。

```bash
$ echo "1" | daicker run --stdin-decode string f
```

これは`f "1"`を呼び出すことと同じである。
デコードで使える形式は次の通りである。

* `json` (デフォルト)
* `yaml`
* `xml`
* `string`

## 実行環境

アプリケーションのビルドやインフラのデプロイには多くのコマンドを用いて、一連の処理を組み立てる必要がある。
通常このような処理を実施する前にはあらかじめ必要なコマンドを作業する環境にインストールしておく必要がある。

このようなコマンドはユーザ自身が管理する必要がある。
多くのコマンドを利用するようになるほどこのメンテナンスは煩雑となり、作業者間で環境を統一することも難しくなる。

Daickerではコンテナを使ってコマンドを呼び出す機能を提供する。
Docker Hubではよく開発用コマンドをインストールしたコンテナが[Developer Tools](https://hub.docker.com/search?categories=Developer+Tools)として配布されている。
このようなコンテナを使うことで直接コマンドをインストールすることなく、利用したいコマンドを実行することが可能である。
Daickerではこのようなコマンドを容易に呼び出せるようにする。

実行環境はローカルまたはコンテナを利用できる。
ローカルとは`daicker`コマンドを実行したプロセスのサブプロセスとして処理を実行する。
コンテナとはDockerのコンテナ上で処理を実行する。

## 環境依存

### 環境依存関数

環境依存関数は実行する`#tag`の形式で

```daic
build = $ #alpine:latest "echo" "ok"
```

### ユーザ定義の環境依存関数

```daic
build #image = $ #image "npm" "build"
```

## 状態管理

### 状態管理

`.daicker/state/{name}.json`

```
&count = state "count"
```

### ステートフルな関数

```daic
countUp =
  (state "count") = (state "count") + 1
  $ "echo" &count
```

```daic
countUp &count =
  &count = &count + 1
  $ "echo" &count
```

## 並列処理

並列処理

```daic
[res1, res2] = proc1 | proc2
```

依存する変数の実行のみ

```daic

  b = f a
  g b
```

## データ型

### 基本型

| 型名     | 表記     | 説明                                                                                                 |
| -------- | -------- | ---------------------------------------------------------------------------------------------------- |
| Void型   | `Void`   | ボトム型である。この型に属する値は存在しない。                                                       |
| Null型   | `Null`   | `null`のみを含む型。                                                                                 |
| Bool型   | `Bool`   | `true`と`false`を含む型。                                                                            |
| Number型 | `Number` | JSONの[Numbers](https://datatracker.ietf.org/doc/html/rfc7159#section-6)に含まれる値の全てを含む型。 |
| String型 | `String` | JSONの[Strings](https://datatracker.ietf.org/doc/html/rfc7159#section-7)に含まれる値の全てを含む型。 |

### リテラル型

プリミティブ型の特定の値のみを代入することができる型を定義できる。

String型の場合は`"yes" | "no"`のように記載することで、`"yes"`または`"no"`のみを許容する文字列型となる。
Number型の場合は`1 | 2`のように記載することで、`1`または`2`のみを許容する数値型となる。
Bool型

### 篩型

(TBD)

### 合成型

#### 配列

`Array<T>`
`[T]`
`T[]`

#### 順序組


`Tuple<T1, ... , Tn>`
`[T1, ... , Tn]`

### マップ

`Map<K, V>`
`{ K: V }`

#### レコード

`Record<K1, V1, ... , Kn, Vn>`

```daic
{
  a: string,
  b: number
}
```

### 型推論

型推論は

```daic
define a = null // aはnull型と推論される
```

```daic
define f = g // fはgと同じ型であると推論される
```

## モジュール

モジュールは


### import



#### ローカルインポート

```
```

#### リモートインポート

```
```

### export



## ファイルシステム


## タグ

```daic
before tag created =
```

```daic
after tag ignoreError = !>
```

```daic
@created $ "terraform" "apply"
```

## 例外処理

```daic
throw 1 "error"
```

```daic
exit 1 "error"
```


## 組み込み関数


| 関数                                   | 説明 |
| -------------------------------------- | ---- |
| `\|\|(bool, bool) bool`                |      |
| `&&(bool, bool) bool`                  |      |
| `!(bool) bool`                         |      |
| `+<T is number>([T, T]) number`        |      |
| `+(string, string) string`             |      |
| `+(array<T>, array<T>]) array<T>`      |      |
| `-(number, number)`                    |      |
| `*(number, number)`                    |      |
| `/(number, number)`                    |      |
| `$[*](array<string>) [string, string]` |      |

## 標準ライブラリ

## メタデータ

```daic
import terraform

deploy() =
  rg = terraform.apply({
    a: ""
  })
  acr = terraform.apply({
    b: ""
  })
  { rg, acr }

build() =

test() =
  $("npm", "ci")
  $("npm", "test")

```

```daic
```
