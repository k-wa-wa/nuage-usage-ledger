# nuage-usage-report Rules

このファイルは、`nuage-usage-report` リポジトリで動作する AI Agent 向けの動作ルールを定義する。

## 1. このリポジトリの位置づけ

- メインフレーム (MVS 3.8j on Hercules) で動く業務アプリ (JCL・REXX・COBOL) だけを持つ
- 基盤 (LXC・Hercules・MVS/CE・mvs-config・帳票プリンタの PDF 化) は `../nuage-cluster` が持つ。基盤の変更はそちらで行う
- `nix build` の出力 (`members/` `schedule` `datasets.json`) が nuage-cluster との約束事である。形を変えるときは nuage-cluster の `nix/hosts/mainframe/mvs-config.nix` と合わせて変える

## 2. MVS の資材を書くときの制約

`nix flake check` で検査される。変更したら必ず実行する。

- ASCII のみ (カードは EBCDIC に変換される。日本語は書けない。コメントも英語で書く)
- 1 行は 72 桁まで。JCL の行 (`//` で始まる) は 71 桁まで
- `./` と `$$` で始まる行は書けない (mvs-config の IEBUPDTE が制御文・終端とみなす)
- ジョブは `MSGCLASS=A` にする (nuage-cluster の mvsctl がプリンタ 1 の出力で終了を判定する)
- 帳票は `SYSOUT=M` (プリンタ 2) に出す。nuage-cluster のプリンタ役がこれを PDF にする
- COBOL は 1968 年規格 (IKFCBL00)。`END-IF` やインライン `PERFORM` は無い
- REXX は BREXX/370。文字列の直後に `X` `B` を続けない (16 進・2 進のリテラルになる)

## 3. 動作確認

- 検査: `nix flake check`
- MVS 上での実行は `../nuage-cluster` の `docs/mainframe.md` を参照する (`mvsctl submit` でジョブを流し、結果を受け取る)

## 4. その他

- **Git操作の禁止**: 特別な指示がない限り、コミットやプッシュなどの Git 操作は実行しないこと
- **言い切り調の使用**: ドキュメントは である・する調 で書く
