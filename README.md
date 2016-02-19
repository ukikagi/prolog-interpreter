# prolog-interpreter

## 使い方。
```
ghc main.hs
./main sample.pl
```
Mac OS X (Yosemite)上のHaskell Platformにおいてコンパイルできることを確認した (2015/07/31)

事前にparsecをインストールする必要がある。
```
cabal update
cabal install parsec
```

対話環境では、基本的にPrologと同じコマンドが使える。

## swiplとの主な相違点
- 幅優先探索を使うことで完全性を得ている
- functorやvariableの識別子の先頭にアンダースコアが使えない
- 整数演算や"is"などは実装していない
- 単一化の出現チェックを実装している
- 否定やカットは無い

## 既知のバグ
- hamiltonが正しく動作しない
