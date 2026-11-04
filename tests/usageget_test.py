"""USAGEGET (mvs/USAGEGET.rexx) を Linux 上の REXX (Regina) で動かして確かめる。

BREXX/370 にしか無いもの (TCP 関数・E2A/A2E・EXECIO) は差し替える。
- TCPOPEN などは偽物にし、VictoriaMetrics の代わりに、クエリの指標名と評価時刻で引いた JSON を返す
- EXECIO は、DD 名をファイル名とする読み書きにする
- 今日の日付 (DATE('S')) は固定する

確かめること
- 関数: CENTS の四捨五入 (セント未満を文字列の上で丸める)、DAYS / CIVIL の往復と曜日
- MODE=REPORT: 使用量レコードの桁 (80 桁)、AGENT の数量欄 (1 つ目が USD、残りは 0)、名前の大文字化
- MODE=LEDGER: 業務日付から前日までの日付・月曜の印、クエリの評価時刻、接続が 1 本であること
- MODE=LEDGER で前日まで処理済みなら RC 4

使い方: python3 tests/usageget_test.py mvs/USAGEGET.rexx (regina が PATH にあること)
"""

import datetime
import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path

failures = []


def check(cond, msg):
    if not cond:
        failures.append(msg)


# ---------------------------------------------------------------- REXX の組み立て

STUBS = r"""
/* ---------------- TEST STUBS (NOT IN BREXX/370) ---------------- */
FAKEIO:
  PARSE ARG FIO_CMD
  PARSE VAR FIO_CMD . . FIO_OP FIO_DD '(' . FIO_STEM .
  FIO_FILE = STUB_DIR'/dd.'STRIP(FIO_DD)
  IF FIO_OP = 'DISKW' THEN DO
    CALL CHAROUT FIO_FILE, '', 1
    DO FIO_I = 1 TO VALUE(FIO_STEM'0')
      CALL LINEOUT FIO_FILE, VALUE(FIO_STEM || FIO_I)
    END
    CALL STREAM FIO_FILE, 'C', 'CLOSE'
  END
  ELSE DO
    FIO_N = 0
    IF STREAM(FIO_FILE, 'C', 'QUERY EXISTS') <> '' THEN
      DO WHILE LINES(FIO_FILE) > 0
        FIO_N = FIO_N + 1
        CALL VALUE FIO_STEM || FIO_N, LINEIN(FIO_FILE)
      END
    CALL VALUE FIO_STEM'0', FIO_N
  END
  RC = 0
  RETURN

TCPINIT:
  RETURN 0
TCPTERM:
  RETURN 0
TCPOPEN:
  STUB_OPENS = STUB_OPENS + 1
  CALL LINEOUT STUB_DIR'/log', 'OPEN' ARG(1) ARG(2)
  _FD = 3
  RETURN 0
TCPCLOSE:
  CALL LINEOUT STUB_DIR'/log', 'CLOSE' ARG(1)
  RETURN 0
TCPSEND:
  PARSE VALUE ARG(2) WITH 'GET ' STUB_PATH ' HTTP/1.1' .
  CALL LINEOUT STUB_DIR'/log', 'SEND' ARG(1) STUB_PATH
  STUB_PENDING = STUB_PENDING || STUBRESP(STUB_PATH)
  RETURN LENGTH(ARG(2))
/* GIVE THE RESPONSE BACK IN PIECES OF 50 BYTES */
TCPRECEIVE:
  IF STUB_PENDING = '' THEN RETURN -1
  _DATA = LEFT(STUB_PENDING, 50)
  STUB_PENDING = SUBSTR(STUB_PENDING, LENGTH(_DATA) + 1)
  RETURN LENGTH(_DATA)
E2A:
  RETURN ARG(1)
A2E:
  RETURN ARG(1)

STUBRESP: PROCEDURE EXPOSE STUB_DIR STUB_KEY. CRLF
  PARSE ARG P
  PARSE VAR P 'time=' T '&' .
  F = ''
  DO K = 1 TO STUB_KEY.0
    IF POS(STUB_KEY.K, P) > 0 THEN DO
      F = STUB_DIR'/resp.'K'.'T
      LEAVE
    END
  END
  B = '{"status":"success","data":{"resultType":"vector","result":[]}}'
  IF F <> '' THEN
    IF STREAM(F, 'C', 'QUERY EXISTS') <> '' THEN DO
      B = CHARIN(F, 1, CHARS(F))
      CALL STREAM F, 'C', 'CLOSE'
    END
  RETURN 'HTTP/1.1 200 OK' || CRLF || 'Content-Type: application/json',
      || CRLF || 'Content-Length: ' || LENGTH(B) || CRLF || CRLF || B
"""

# クエリを見分ける指標名 (STUB_KEY.n の順)
KEYS = [
    "container_cpu_usage_seconds_total",
    "container_memory_working_set_bytes",
    "container_network_receive_bytes_total",
    "kube_persistentvolumeclaim_resource_requests_storage_bytes",
    "node_cpu_seconds_total",
    "node_memory_MemTotal_bytes",
    "node_network_receive_bytes_total",
    "node_filesystem_size_bytes",
    "nuage_ai_agent_cost_usd_total",
]


def build_program(src, workdir, today):
    """USAGEGET に偽物の関数を足し、BREXX 固有の箇所を差し替えた REXX を作る"""
    prog = src.replace("DATE('S')", f"'{today:%Y%m%d}'")
    prog, n = re.subn(r'^(\s*)"(EXECIO [^"]*)"\s*$', r"\1CALL FAKEIO '\2'", prog, flags=re.M)
    assert n == 3, f"EXECIO の行が 3 つではない ({n})"
    head = [f"STUB_DIR = '{workdir}'", "STUB_OPENS = 0", "STUB_PENDING = ''", f"STUB_KEY.0 = {len(KEYS)}"]
    head += [f"STUB_KEY.{i} = '{k}'" for i, k in enumerate(KEYS, 1)]
    # 1 行目の /* REXX */ の注釈の後に差し込む
    first, rest = prog.split("\n", 1)
    return first + "\n" + "\n".join(head) + "\n" + rest + STUBS


def vector(label, values):
    return json.dumps(
        {
            "status": "success",
            "data": {
                "resultType": "vector",
                "result": [{"metric": {label: name}, "value": [0, v]} for name, v in values.items()],
            },
        },
        separators=(",", ":"),
    )


def epoch_day(d):
    return (d - datetime.date(1970, 1, 1)).days


def evalts(d):
    """その日の 00:00-24:00 (JST) を [1d] で覆う評価時刻 (翌日 00:00 JST)"""
    return (epoch_day(d) + 1) * 86400 - 9 * 3600


def run(src, today, args, bizdate=None, responses=None):
    """responses: {(KEYS の添字 (1 から), 日付): JSON}。結果のファイルと RC を返す"""
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        (tmp / "prog.rexx").write_text(build_program(src, tmp, today))
        if bizdate is not None:
            (tmp / "dd.BIZDATE").write_text(bizdate + "\n" if bizdate else "")
        for (k, d), body in (responses or {}).items():
            (tmp / f"resp.{k}.{evalts(d)}").write_text(body)
        p = subprocess.run(["regina", str(tmp / "prog.rexx"), args], capture_output=True, text=True)
        out = {}
        for name in ("dd.USAGE", "dd.CTLCARD", "log"):
            f = tmp / name
            out[name] = f.read_text().splitlines() if f.exists() else None
        return p.returncode, p.stdout + p.stderr, out


# ---------------------------------------------------------------- 関数の検査


def test_functions(src):
    """QUERY: 以降 (サブルーチン) だけを取り出して呼ぶ"""
    subs = src[src.index("\nQUERY:") :]
    cents_cases = {
        "149.11": 14911,
        "13.775": 1378,  # 浮動小数点では 1377.4999... になる値
        "21.364999999": 2136,
        "10.0649999999": 1006,
        "0.405": 41,
        "0.004": 0,
        "0.005": 1,
        "7": 700,
        "123.4": 12340,
        "1.5e-05": 0,
        "6.0e-03": 1,
        "2.5E+01": 2500,
        "-3.2": 0,
        "NaN": 0,
        "+Inf": 0,
        "": 0,
        "9999999.994": 999999999,
    }
    lines = ["/* REXX */", "NUMERIC DIGITS 15"]
    for s, _ in cents_cases.items():
        lines.append(f"SAY 'CENTS' '{s}' CENTS('{s}')")
    # DAYS / CIVIL: 1970-01-01 から 2100 年まで 7 日おき + 月末前後
    start = datetime.date(1970, 1, 1)
    dates = [start + datetime.timedelta(days=i) for i in range(0, 47500, 7)]
    dates += [datetime.date(2026, 10, 31), datetime.date(2026, 11, 1), datetime.date(2028, 2, 29), datetime.date(2100, 3, 1)]
    for d in dates:
        lines.append(f"SAY 'DAYS' '{d:%Y%m%d}' DAYS('{d:%Y%m%d}') CIVIL(DAYS('{d:%Y%m%d}')) YMD('{d:%Y-%m-%d}')")
    lines.append("EXIT 0")
    with tempfile.TemporaryDirectory() as tmp:
        f = Path(tmp) / "f.rexx"
        f.write_text("\n".join(lines) + subs)
        p = subprocess.run(["regina", str(f)], capture_output=True, text=True)
    check(p.returncode == 0, f"関数の呼び出しが失敗した: {p.stderr}")
    got = {}
    for line in p.stdout.splitlines():
        w = line.split()
        if w and w[0] == "CENTS":
            # 空文字列は語にならない
            got[w[1] if len(w) == 3 else ""] = int(w[-1])
        elif w and w[0] == "DAYS":
            d = datetime.datetime.strptime(w[1], "%Y%m%d").date()
            check(int(w[2]) == epoch_day(d), f"DAYS({w[1]}) = {w[2]}, 期待 {epoch_day(d)}")
            check(w[3] == f"{d:%Y-%m-%d}", f"CIVIL(DAYS({w[1]})) = {w[3]}")
            check(w[4] == w[1], f"YMD = {w[4]}")
    for s, want in cents_cases.items():
        check(got.get(s) == want, f"CENTS('{s}') = {got.get(s)}, 期待 {want}")


# ---------------------------------------------------------------- 実行の検査


def parse_usage(line):
    return {
        "date": line[0:8],
        "class": line[8:13].rstrip(),
        "name": line[13:37].rstrip(),
        "qty": [int(line[37 + 9 * i : 46 + 9 * i]) for i in range(4)],
        "rest": line[73:80],
    }


def test_report(src):
    today = datetime.date(2026, 10, 28)
    day = today - datetime.timedelta(days=1)
    responses = {
        (1, day): vector("namespace", {"argocd": "1.2345", "pechka": "0.004"}),
        (2, day): vector("namespace", {"argocd": "12.5", "pechka": "3"}),
        (5, day): vector("nodename", {"lb-1": "0.995"}),
        (
            9,
            day,
        ): vector(
            "repo",
            {
                "nuage-cluster": "149.11",
                "pechka": "21.364999999",
                "nuage-autopilot5": "13.775",
                "nuage-monitoring-stack": "10.0649999999",
                "tmp": "0.405",
            },
        ),
    }
    rc, log, out = run(src, today, "", responses=responses)
    check(rc == 0, f"MODE=REPORT の RC が {rc}: {log}")
    usage = out["dd.USAGE"] or []
    for line in usage:
        check(len(line) == 80, f"使用量レコードが 80 桁でない: {line!r}")
    recs = {(r["class"], r["name"]): r for r in map(parse_usage, usage)}
    check(len(recs) == 8, f"使用量レコードが 8 件でない: {sorted(recs)}")
    want_usd = {
        "NUAGE-CLUSTER": 14911,
        "PECHKA": 2136,
        "NUAGE-AUTOPILOT5": 1378,
        "NUAGE-MONITORING-STACK": 1006,
        "TMP": 41,
    }
    for name, cents in want_usd.items():
        r = recs.get(("AGENT", name))
        check(r is not None, f"AGENT {name} のレコードが無い")
        if r:
            check(r["qty"] == [cents, 0, 0, 0], f"AGENT {name} の数量欄: {r['qty']}")
            check(r["date"] == f"{day:%Y%m%d}", f"AGENT {name} の日付: {r['date']}")
    r = recs.get(("K8S", "ARGOCD"))
    check(r is not None and r["qty"] == [123, 1250, 0, 0], f"K8S ARGOCD: {r}")
    r = recs.get(("K8S", "PECHKA"))
    check(r is not None and r["qty"] == [0, 300, 0, 0], f"K8S PECHKA: {r}")
    r = recs.get(("HOST", "LB-1"))
    check(r is not None and r["qty"] == [100, 0, 0, 0], f"HOST LB-1: {r}")
    ctl = out["dd.CTLCARD"] or []
    check(ctl[:1] == [f"RUN 2026-10-28 {day:%Y-%m-%d} {day:%Y-%m-%d} 20261028".ljust(80)], f"CTLCARD の RUN: {ctl[:1]}")
    log_lines = out["log"] or []
    check(sum(1 for x in log_lines if x.startswith("OPEN")) == 1, "接続が 1 本でない")
    sends = [x for x in log_lines if x.startswith("SEND")]
    check(len(sends) == 9, f"クエリが 9 本でない ({len(sends)})")
    check(all(f"time={evalts(day)}&" in x for x in sends), "評価時刻が翌日 00:00 JST でない")
    agent = [x for x in sends if "nuage_ai_agent_cost_usd_total" in x]
    check(
        len(agent) == 1 and "query=sum%20by%20%28repo%29%20%28increase%28nuage_ai_agent_cost_usd_total%5B1d%5D%29%29" in agent[0],
        f"AGENT のクエリ: {agent}",
    )


def test_ledger(src):
    today = datetime.date(2026, 10, 28)
    days = [datetime.date(2026, 10, 25), datetime.date(2026, 10, 26), datetime.date(2026, 10, 27)]
    responses = {}
    for i, d in enumerate(days):
        responses[(1, d)] = vector("namespace", {"argocd": str(1 + i)})
        responses[(9, d)] = vector("repo", {"pechka": f"{i}.5"})
    rc, log, out = run(src, today, "MODE=LEDGER", bizdate="20261025 20261024", responses=responses)
    check(rc == 0, f"MODE=LEDGER の RC が {rc}: {log}")
    recs = [parse_usage(x) for x in out["dd.USAGE"] or []]
    agent = sorted((r["date"], r["qty"][0]) for r in recs if r["class"] == "AGENT")
    check(agent == [("20261025", 50), ("20261026", 150), ("20261027", 250)], f"AGENT の日ごとのレコード: {agent}")
    k8s = sorted((r["date"], r["qty"][0]) for r in recs if r["class"] == "K8S")
    check(k8s == [("20261025", 100), ("20261026", 200), ("20261027", 300)], f"K8S の日ごとのレコード: {k8s}")
    ctl = [x.rstrip() for x in out["dd.CTLCARD"] or []]
    want = [
        "RUN 2026-10-28 2026-10-25 2026-10-27 20261028",
        "DAY 20261025",
        "DAY 20261026 M",
        "DAY 20261027",
    ]
    check(ctl == want, f"CTLCARD: {ctl}")
    log_lines = out["log"] or []
    check(sum(1 for x in log_lines if x.startswith("OPEN")) == 1, "遡り処理でも接続が 1 本でない")
    check(sum(1 for x in log_lines if x.startswith("SEND")) == 27, "クエリが 3 日 x 9 本でない")
    for d in days:
        check(sum(1 for x in log_lines if f"time={evalts(d)}&" in x) == 9, f"{d} のクエリが 9 本でない")

    # 前日まで処理済み
    rc, log, out = run(src, today, "MODE=LEDGER", bizdate="20261028 20261027", responses=responses)
    check(rc == 4, f"処理済みのときの RC が 4 でない ({rc})")
    check(out["log"] in (None, []), "処理済みなのに接続した")

    # 初回 (業務日付が空) は START= から
    rc, log, out = run(src, today, "MODE=LEDGER START=2026-10-26", bizdate="", responses=responses)
    ctl = [x.rstrip() for x in out["dd.CTLCARD"] or []]
    check(rc == 0 and ctl[1:] == ["DAY 20261026 M", "DAY 20261027"], f"START= の初回: RC {rc}, {ctl}")


def main(path):
    src = Path(path).read_text()
    test_functions(src)
    test_report(src)
    test_ledger(src)
    for f in failures:
        print("NG:", f)
    if failures:
        sys.exit(1)
    print("USAGEGET: OK")


if __name__ == "__main__":
    main(sys.argv[1])
