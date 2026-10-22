# nuage-usage-ledger

おうちクラスタ (nuage) の資源使用量を、メインフレーム (MVS 3.8j on Hercules) が勘定として記帳・残高管理する
システム。毎朝、前日の使用量を換算単位 RU に換算して元帳に記帳し、昔ながらの帳票 (1403 ラインプリンタ形式) に印字する。
課金はしない。

設計は [docs/design.md](./docs/design.md) を参照する。**勘定系の作法 (業務日付・締め・元帳の世代管理・複式の仕訳・試算表など) を、
この規模には過剰と承知のうえで意図的に真似る**のが方針である。

メインフレームの基盤 (LXC・Hercules・MVS/CE・資材の配布・帳票プリンタの PDF 化) は
[nuage-cluster](https://github.com/k-wa-wa/nuage-cluster) の `docs/mainframe.md` を参照する。
このリポジトリは、その上で動く業務アプリ (JCL・REXX・COBOL) だけを持つ。

## 全体像

```
 ┌─ MVS ─────────────────────────────────────────────────────────────┐
 │ ZTIMER (常駐の時刻起動)                                            │
 │  06:00 S RUNJOB,M=USAGERPT,L='NUAGE.USAGE.JCLLIB'  日次使用量明細  │
 │  06:10 S RUNJOB,M=LEDGER,L='NUAGE.USAGE.JCLLIB'    日次締め        │
 │                                                                   │
 │ LEDGER (日次締めのジョブネット)                                    │
 │   GET      REXX : 業務日付から前日までの使用量を HTTP で取る        │
 │   TRANGEN  COBOL: RU に換算して取引を作る (月曜は週次割当も)        │
 │   SORT                                                            │
 │   POSTING  COBOL: 旧元帳(0) + 取引 → 新元帳(+1)、試算表で検証       │
 │   COMMIT        : 全ステップ正常のときだけ新世代と業務日付を確定    │
 │   → 未登録リスト・勘定残高明細・超過通知・試算表を帳票プリンタへ   │
 └───────────────────────────────────────────────────────────────────┘
   印字された帳票は、nuage-cluster のプリンタ役が PDF にする
```

## ファイル

| ファイル | 役割 |
| :-- | :-- |
| `mvs/USAGEGET.rexx` | データ取得 (REXX)。VictoriaMetrics に PromQL を投げ、固定長レコードを書く。両方のジョブで使う |
| `mvs/USAGERPT.jcl` / `mvs/USAGERPT.cbl` | 日次使用量明細のジョブと帳票プログラム |
| `mvs/LEDGER.jcl` | 日次締めのジョブネット。`{{USAGEGET}}` `{{TRANGEN}}` `{{POSTING}}` にソースが差し込まれる |
| `mvs/TRANGEN.cbl` | 取引作成 (換算・週次割当・未登録リスト) |
| `mvs/POSTING.cbl` | 記帳 (元帳の更新・仕訳・総勘定元帳・検証・帳票) |
| `mvs/cntl/RATES` | 換算表マスタ (`NUAGE.USAGE.CNTL(RATES)`) |
| `mvs/cntl/ACCTDEF` | 勘定マスタ定義 (`NUAGE.USAGE.CNTL(ACCTDEF)`)。勘定の追加・割当額の変更はここで行う |
| `mvs/schedule` | ZTIMER の予定表 (`SYS2.CONTROL(ZTIMER)`) に足す行 |
| `mvs/rakf` | RAKF の権限表 (`SYS1.SECURE.CNTL(PROFILES)`) に足す行。`NUAGE.USAGE.*` への書き込みを許す |
| `mvs/datasets.json` | このアプリが使うデータセット (無ければ作られる。PO / PS / GDG) |
| `guide/ledger.toml` | 3270 端末の案内 [cicerone3270](https://github.com/k-wa-wa/cicerone3270) に足す辞書。データセットの説明と、勘定マスタ・仕訳・総勘定元帳・業務日付のレコード定義 (ブラウズ画面で項目名と値を出す) |
| `flake.nix` | 上をまとめて、nuage-cluster に渡す資材を作る |
| `scripts/check_mvs_sources.py` | カード (80 桁) と MVS の制約の検査 (`nix flake check`) |

## nuage-cluster との約束事

`nix build` (packages.default) の出力が、nuage-cluster の mvs-config に渡す資材である。

```
members/<データセット名>/<メンバー名>   MVS に書き込むメンバー
schedule                               ZTIMER の予定表の末尾に足す行
rakf                                   RAKF の権限表に足す行
datasets.json                          無ければ作るデータセット
```

nuage-cluster は flake input としてこのリポジトリを取り込む。変更を反映するには、
nuage-cluster 側で `nix flake update nuage-usage-ledger --flake ./nix` を実行して master に入れる。

## 運用

```bash
# 手動で締める / 日次使用量明細を出す
ssh mainframe "mvs-console \"/S RUNJOB,M=LEDGER,L='NUAGE.USAGE.JCLLIB'\""
ssh mainframe "mvs-console \"/S RUNJOB,M=USAGERPT,L='NUAGE.USAGE.JCLLIB'\""

# 帳票の PDF (tmpfs のため LXC の再起動で消える)
ssh mainframe "ls /run/mainframe-printer/"
scp mainframe:/run/mainframe-printer/LEDGER-2026-10-21.pdf .
```

- 締めが失敗したとき: 業務日付は進んでいないので、原因を直して同じジョブを流し直せばよい
- 勘定を足す: `mvs/cntl/ACCTDEF` に次の番号で行を足す (番号順を保つ)。次の締めで開設され、週次割当額が 1 回分記帳される
- 換算率を変える: `mvs/cntl/RATES` に新しい有効開始日の行を足す (過去の行は変えない)

## 使用量の項目 (仮置き)

区分 K8S は namespace ごと、区分 HOST は VictoriaMetrics の `job="external-nodes"` の NixOS ホストごと
(名前は `node_uname_info` の `nodename`)。評価時刻は対象日の翌日 0:00 (JST) で、`[1d]` の窓が対象日の 0:00〜24:00 になる。

| 列 | 意味 | K8S | HOST |
| :-- | :-- | :-- | :-- |
| CPU-H | CPU 使用量 (コア時間) | `container_cpu_usage_seconds_total` の増分 / 3600 | `node_cpu_seconds_total` (idle 以外) の増分 / 3600 |
| MEM-GBH | メモリ (平均 GB × 24 時間) | `container_memory_working_set_bytes` の平均 | `MemTotal - MemAvailable` の平均 |
| NET-GB | 通信量 (受信 + 送信) | `container_network_*_bytes_total` の増分 | `node_network_*_bytes_total` (lo を除く) の増分 |
| STOR-GB | ストレージ | PVC の要求量 (日末時点) | `/` の使用量 (日末時点) |

### 使用量レコード (REXX → SORT → COBOL)

| 桁 | 内容 | COBOL の定義 |
| :-- | :-- | :-- |
| 1-8 | 日付 (YYYYMMDD) | `PIC X(8)` |
| 9-13 | 区分 (`K8S  ` / `HOST `) | `PIC X(5)` |
| 14-37 | 名前 (大文字) | `PIC X(24)` |
| 38-73 | CPU-H, MEM-GBH, NET-GB, STOR-GB を各 9 桁 (下 2 桁が小数) | `PIC 9(7)V99` × 4 |
| 74-80 | 予備 | `PIC X(7)` |

元帳側のレコードは [docs/design.md](./docs/design.md) の 5.1 にある。

### 帳票の実物 (2026-10-21 分)

```
 NUAGE CLUSTER                   DAILY RESOURCE USAGE REPORT                             PAGE    1
 USAGE DATE: 2026-10-21                                             RUN DATE: 2026-10-22

 CLASS  NAME                              CPU-H         MEM-GBH          NET-GB         STOR-GB
 -----  ------------------------  -------------  --------------  --------------  --------------
 HOST   EGRESS-GATEWAY                     1.33            1.93            1.63            1.25
   ...
                  * CLASS TOTAL           16.71           21.92           27.59           28.19

 K8S    ARGOCD                             0.89           29.81            3.74            0.00
   ...
                  * CLASS TOTAL           22.65          703.11          822.64          126.00

                 ** GRAND TOTAL           39.36          725.03          850.23          154.19


 *** END OF REPORT ***       18 RECORDS
```

## 設計判断

| 論点 | 結論 | 理由 |
| :-- | :-- | :-- |
| 時刻を誰が持つか | MVS (ZTIMER) | 夜間バッチの主役をメインフレームにする。外から MVS へジョブを投入する口も要らない |
| データの取り方 | MVS が取りに行く (REXX の TCP 関数で HTTP GET) | 時刻を持つ場所を 1 つにする。通信は MVS から外向きだけで済む |
| ソースの分け方 | JCL・REXX・COBOL を別ファイルにし、flake で 1 本のジョブにまとめる | ソースは読みやすく分け、MVS 側は 1 メンバーで完結させる。COBOL は毎回コンパイルして実行する (COBUCG) |
| 置き場所 | アプリ専用の `NUAGE.USAGE.*` (ユーザーカタログ UCPUB000) | システムのライブラリ (SYS2.*) やマスターカタログと持ち主を分ける |
| 締めの確定 | 成果物を一時データセットに作り、最後の確定ステップでまとめて書き込む | 検証に失敗した締めを何も確定させない |

勘定の設計判断は [docs/design.md](./docs/design.md) にある。

## 実装で分かったこと

| 項目 | 内容 |
| :-- | :-- |
| 文字コード | MVS は EBCDIC。HTTP の送信前に `E2A`、受信後に `A2E` で変換する。`[` `]` などは BREXX と Hercules の変換表が合わず化けるので、URL エンコードは自前の表で行う |
| REXX | 文字列の直後に `X` や `B` が続くと 16 進・2 進リテラルになる (`'{'X`)。BREXX では `SYMBOL()` で複合変数を判定できず、ステムの既定値 (`SEEN. = 0`) で判定する |
| HTTP の接続数 | BREXX のソケット番号は TCPCLOSE しても再利用されず、64 に達すると取得できなくなる。1 回 1 接続 (HTTP/1.1 keep-alive) にし、nuage-cluster の週次の計画 IPL で番号を戻す |
| SORT | この時代の SORT は 3380/3390 を作業域に使えない (`IER042A`)。2314 の作業パック (SORTW1-6) を使う |
| COBOL | ブロック化されたデータセットを読む FD には `BLOCK CONTAINS` が要る (無いと S001)。`PERFORM ... THRU ...-EXIT` で範囲を明示する。WRITE 後のレコードは参照できない (S0C7)。部分参照は無い |
| JCL | 1 行は 71 桁まで (72 桁目は継続記号)。START コマンドでピリオドを含む値は引用符で囲む (`L='NUAGE.USAGE.JCLLIB'`) |
| 権限とカタログ | 内部リーダ経由のジョブは既定の PROD 利用者で動く。マスターカタログを更新できないため、ユーザーカタログ (別名 NUAGE) と RAKF の権限 (`mvs/rakf`) で書き込めるようにした。カタログ登録の失敗 (`NOT CATLGD`) はステップの RC に出ない |

[docs/design.md](./docs/design.md) の「実装で分かったこと」も参照する。

## 今後

| 項目 | 内容 |
| :-- | :-- |
| CI | GitHub Actions で MVS/CE の Docker イメージを立ち上げ、固定データでジョブを流して COND CODE と帳票を検証する |
| 帳票 | 勘定元帳 (勘定ごとの取引明細)、週次・月次の推移 |
| オンライン | KICKS (CICS 互換) の 3270 画面で残高照会、勘定間の割当の振替 |
| HOST 区分の対象 | mainframe と swfs-cluster が `external-nodes` に入っていない (nuage-monitoring-stack 側で足す) |
| 権限 | バッチ専用の利用者を作り、既定の利用者への ALTER の許可をやめる |
