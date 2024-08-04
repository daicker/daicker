
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
define a = null // aはnull型と推論される
```

```daic
define f = g // fはgと同じ型であると推論される
```

## 関数

Daickerの関数はSingle Input Single Output (SISO) である。

```daic
define f x = x
```

SISOである理由は標準入力を関数の入力として直接与えることができるようにするためである。
しかしながら通常関数は複数の入力を受け取ることがあり、配列のパターンマッチを用いて、次のように記述することができる。

```daic
define f [x, y] = x + y
```

この糖衣構文として次のように書くこともできる。

```daic
define f x y = x + y
```

関数本体が複数の式からなるときは次のように書く。

```daic
define f x y =
  ans = x + y
  ans + 1
```

### 関数の呼び出し

コマンドライン引数から関数の引数を与える場合は次のように呼び出す。

```bash
$ daicker run f 1 2

3
```

`--flag-style`

```bash
$ daicker run --flag-style f -x 1 -y 2

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

`--flag-style`が有効である場合には`@stdin`は省略できない。

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

これは`f 1`を呼び出すことと同じである。
デコードで使える形式は次の通りである。

* `json` (デフォルト)
* `yaml`
* `xml`
* `string`

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

## 実行環境

環境依存関数は`[コンテナ名]`

```daic
build = #node:latest $ "npm" "build"
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
