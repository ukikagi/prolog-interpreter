# prolog-interpreter
![Haskell CI](https://github.com/ukikagi/prolog-interpreter/workflows/Haskell%20CI/badge.svg?branch=master)

## Example
```
cabal run :prolog-interpreter sample.pl
```
```
?- mother(X, Y).
X = kobo, Y = sanae.
X = miho, Y = sanae.
X = sanae, Y = mine.
```

## swiplとの主な相違点
- 幅優先探索を使うことで完全性を得ている
- functorやvariableの識別子の先頭にアンダースコアが使えない
- 整数演算や"is"などは実装していない
- 単一化の出現チェックを実装している
- 否定やカットは無い

## 既知のバグ
- hamiltonが正しく動作しない
