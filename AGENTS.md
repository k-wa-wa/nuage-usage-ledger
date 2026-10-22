# nuage-usage-ledger Rules

このファイルは、`nuage-usage-ledger` リポジトリで動作する AI Agent 向けの動作ルールを定義する。

## 1. このリポジトリの位置づけ

- メインフレーム (MVS 3.8j on Hercules) で動く業務アプリ (JCL・REXX・COBOL) だけを持つ
- 基盤 (LXC・Hercules・MVS/CE・mvs-config・帳票プリンタの PDF 化) は `../nuage-cluster` が持つ。基盤の変更はそちらで行う
- `nix build` の出力 (`members/` `schedule` `rakf` `datasets.json` `build/`) が nuage-cluster との約束事である。形を変えるときは nuage-cluster の `nix/hosts/mainframe/mvs-config.nix` と合わせて変える
- KICKS (オンライン) の導入は nuage-cluster が持つ。このリポジトリはオンラインのプログラム・マップ・表と、それをビルドするジョブ (`build/LEDGBLD`) を持つ
- `guide/ledger.toml` は 3270 端末の案内 (cicerone3270) に足す辞書で、出力の `cicerone3270/` に入る。COBOL のレコード (`mvs/POSTING.cbl` の FD) やデータセットを変えたら、辞書のレコード定義・説明も合わせて変える

## 2. 設計の方針

- 使用量を勘定として記帳・残高管理するシステムとして設計する。設計は `docs/design.md` が正である
- **勘定系の作法を、過剰と承知のうえで意図的に真似る。** 業務日付、日次締め、元帳の世代管理 (GDG)、複式の仕訳、試算表による貸借一致の検証、統制勘定と補助元帳の照合、端数処理規則の明文化などを省略しない。目的はメインフレームの勘定系を体験し理解することにある
- 用語は勘定の用語を使う (勘定、割当、使用、残高、元帳、仕訳、試算表)。銀行に見立てた言い回しはしない
- **オンラインは照会専用にする。** 元帳を更新するのは夜間のバッチ (締め) だけで、オンラインは締めの後に作る写し (照会用ファイル) を読む

## 3. MVS の資材を書くときの制約

`nix flake check` で検査される。変更したら必ず実行する。

- ASCII のみ (カードは EBCDIC に変換される。日本語は書けない。コメントも英語で書く)
- 1 行は 72 桁まで。JCL の行 (`//` で始まる) は 71 桁まで
- `./` と `$$` で始まる行は書けない (mvs-config の IEBUPDTE が制御文・終端とみなす)
- ジョブは `MSGCLASS=A` にする (nuage-cluster の mvsctl がプリンタ 1 の出力で終了を判定する)
- 帳票は `SYSOUT=M` (プリンタ 2) に出す。nuage-cluster のプリンタ役がこれを PDF にする
- COBOL は 1968 年規格 (IKFCBL00)。`END-IF` やインライン `PERFORM` は無い
- REXX は BREXX/370。文字列の直後に `X` `B` を続けない (16 進・2 進のリテラルになる)
- COBOL-68 には部分参照 (`X(1:1)`) が無い。ブロック化した出力ファイルは WRITE 後のレコードを参照しない (WORKING-STORAGE で組み立てて `WRITE ... FROM`)
- ブロック化されたデータセットを読む FD には `BLOCK CONTAINS` を書く
- HTTP は 1 回の実行で 1 接続にする (BREXX のソケット番号は再利用されず、64 で尽きる)
- 締めの成果物は一時データセットに作り、最後の確定ステップ (`COND=(0,NE)`) で書き込む
- KICKS のプログラム (`mvs/online/`) は `EXEC KICKS ... END-EXEC` を使う COBOL-68 である。K2KCOBCL (前処理を 2 回通す) でコンパイルするため、`LENGTH` は省ける。添字付きの項目は EXEC KICKS の引数に書かず、いったん別の項目に移す
- KICKS のプログラムは、COMMAREA の長さで初回かどうかを判定する (`EIBCALEN = 0` だけで判定しない。KICKS は起動直後の最初の画面に PA2 を送ってくる)。PA キーは画面の再表示にする
- BMS マップのリテラル (`INITIAL`) は継続行にしない (1 行に収まらない見出しは複数の項目に分ける)
- MVS 3.8 の VSAM では、PUB000 の KSDS は `UNIQUE` にする (共用の VSAM 空間が無い)。`UNIQUE` と `REUSE` は併用できないので、作り直すときは DELETE / DEFINE する

## 4. 動作確認

- 検査: `nix flake check` (Git に追加していないファイルがあるときは `nix flake check "path:$PWD"`)
- MVS 上での実行は `../nuage-cluster` の `docs/mainframe.md` を参照する (`mvsctl submit` でジョブを流し、結果を受け取る)
- オンラインは `nix build` の `build/LEDGBLD` を流してから、TSO で `EXEC 'NUAGE.USAGE.CLIST(LEDGER)'` を打って確かめる。照会用ファイルはジョブ LEDGINQ で作る

## 5. その他

- **Git操作の禁止**: 特別な指示がない限り、コミットやプッシュなどの Git 操作は実行しないこと
- **言い切り調の使用**: ドキュメントは である・する調 で書く
