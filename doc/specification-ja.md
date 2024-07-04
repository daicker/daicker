
# 言語仕様

Daicker LanguageはCI/CDパイプラインとInfrastructure as Codeの両方を記述するために設計された言語です。

## データ型

### 基本型

| 型名     | 説明                                                                                                 |
| -------- | ---------------------------------------------------------------------------------------------------- |
| `null`   | `null`のみを含む型。                                                                                 |
| `bool`   | `true`と`false`を含む型。                                                                            |
| `number` | JSONの[Numbers](https://datatracker.ietf.org/doc/html/rfc7159#section-6)に含まれる値の全てを含む型。 |
| `string` | JSONの[Strings](https://datatracker.ietf.org/doc/html/rfc7159#section-7)に含まれる値の全てを含む型。 |

### 合成型

#### 配列

`array<string>`

#### 順序組

`tuple<string, int>`

### マップ

`{ [string]: int }`

#### レコード

```daic
type t = {
  a: string,
  b: number
}
```

### 型推論

型推論は

```daic
a = null // aはnull型と推論される
```

```daic
f(a) = g(a)
```

## 関数

Daickerの関数はSingle Input Single Output (SISO) である。

```daic
f(x) = x
```

SISOである理由は標準入力を関数の入力として直接与えることができるようにするためである。
しかしながら通常関数は複数の入力を受け取ることがあり、配列のパターンマッチを用いて、次のように記述することができる。

```daic
f([x, y]) = x + y
```

この糖衣構文として次のように書くこともできる。

```daic
f(x, y) = x + y
```

関数本体が複数の式からなるときは次のように書く。

```daic
f(x, y) =
  ans = x + y
  ans + 1
```

### 関数の呼び出し

コマンドライン引数から関数の引数を与える場合は次のように呼び出す。
引数はデフォルトでJSONとみなしてデコードされる。

```bash
$ daicker run f "[1, 2]"

3
```

または引数が配列である場合は次のように書くこともできる。

```bash
$ daicker run f 1 2

3
```

標準入力から関数の引数を与える場合は次のように呼び出す。

```bash
$ echo "[1, 2]" | daicker run f

3
```

JSONでデコードすることなく単純な文字列の`"1"`を引数として渡したい場合には次のように呼び出す。

```bash
$ echo "1" | daicker run f --decode string
```

これは`f("1")`を呼び出すことと同じである。
デコードで使える形式は次の通りである。

* `json` (デフォルト)
* `yaml`
* `xml`
* `string`

## パッケージ

## モジュール

### mainモジュール

### import

#### 外部モジュールのインポート


### export

## ファイルシステム

## 実行環境

環境依存関数は`[コンテナ名]`

```daic
build() = $[node:latest]("npm", "build")
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
module main

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
