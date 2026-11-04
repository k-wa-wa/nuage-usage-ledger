# テスト

## `nix flake check` で流れるもの

| 検査 | 中身 |
| :-- | :-- |
| `mvs-sources` (`scripts/check_mvs_sources.py`) | カード (80 桁)・JCL (71 桁)・ASCII などの MVS の制約と、勘定マスタ定義 (ACCTDEF)・換算表 (RATES) の桁位置・区分コードと区分の対応・並び |
| `usageget` (`tests/usageget_test.py`) | `mvs/USAGEGET.rexx` を Linux 上の REXX (Regina) で動かす。BREXX/370 にしか無い TCP 関数・EXECIO・`DATE('S')` は差し替え、VictoriaMetrics の代わりに決まった JSON を返す |

`usageget` で確かめること:

- セント未満の四捨五入 (CENTS)。10 進の文字列の上で丸める (`13.775` → 13.78)
- MODE=REPORT の使用量レコード (80 桁)。AGENT の数量欄は 1 つ目が USD、残りは 0。名前は大文字
- MODE=LEDGER の業務日付から前日までの日付、月曜の印、クエリの評価時刻、接続が 1 本であること
- 前日まで処理済みなら RC 4

単体で流すときは次のとおり (regina が PATH にあること)。

```bash
nix shell nixpkgs#regina -c python3 tests/usageget_test.py mvs/USAGEGET.rexx
```

## 手元の MVS で締めを流す (`tests/mvs_local.py`)

cicerone3270 の試し用の MVS (mvsce-local) に資材を入れ、締めのジョブを流す。本番の MVS には触れない。
nuage-cluster の mvsctl (`nix/hosts/mainframe/tools/mvsctl.py`) を、手元の MVS に向けて使う。

```bash
# 1. 手元の MVS を起動する (別の端末。Ctrl+C で止まる)
MVSCE_LOCAL_DIR=/tmp/mvsce-ledger nix run github:k-wa-wa/cicerone3270#mvsce-local

# 2. 資材を作って入れる (データセット・メンバー・RAKF・LEDGBLD。KICKS が未導入なら KICKS_XMI を渡す)
nix build -o /tmp/ledger-pkg
export MVS_STATE_DIR=/tmp/mvsce-ledger
python3 tests/mvs_local.py setup /tmp/ledger-pkg

# 3a. VictoriaMetrics から取って、そのまま LEDGER (→ LEDGMON → LEDGINQ) を流す
python3 tests/mvs_local.py run /tmp/ledger-pkg LEDGER

# 3b. VictoriaMetrics に届かないとき・月をまたぐ締めを試すときは、試しの使用量で流す
#     FROM は業務日付の次回処理日にする。TO の翌日が締めの実行日になり、月が替われば LEDGMON が前月を締める
python3 tests/mvs_local.py ledger /tmp/ledger-pkg 2026-10-27 2026-11-02
```

- ジョブの出力は `$MVS_STATE_DIR/ledger-test/<ジョブ名>.txt`、帳票 (出力クラス M) は `$MVS_STATE_DIR/printers/prt00f.txt` に出る
- 試しの使用量 (`USAGE`) には、K8S・HOST・AGENT の登録済みの勘定と、未登録の利用者 (SCRATCH・TMP) を入れてある。
  `python3 tests/mvs_local.py usage FROM TO` でレコードだけを出せる
- オンラインは TN3270 (127.0.0.1:3270) で TSO にログオン (IBMUSER / SYS1) し、`EXEC 'NUAGE.USAGE.CLIST(LEDGER)'` を打って確かめる
