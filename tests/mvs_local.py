"""手元の MVS (cicerone3270 の mvsce-local) で、締めのジョブを試しの使用量で流す。

本番の MVS には触れない。nuage-cluster の mvsctl (ジョブの投入・終了待ち・資材の反映・KICKS の導入) を、
手元の MVS に向けて使う。手順は tests/README.md を参照する。

    python3 tests/mvs_local.py setup PKG             資材を手元の MVS に入れる (データセット・メンバー・RAKF・KICKS・LEDGBLD)
    python3 tests/mvs_local.py ledger PKG FROM TO    試しの使用量 (FROM..TO の日) で LEDGER を流し、LEDGMON・LEDGINQ まで待つ
    python3 tests/mvs_local.py run PKG MEMBER        JCLLIB のジョブ (LEDGER・USAGERPT など) をそのまま流す (VictoriaMetrics から取る)
    python3 tests/mvs_local.py usage FROM TO         試しの使用量レコードを標準出力に出す

PKG は nix build の出力 (packages.default)。環境変数:
    MVS_STATE_DIR   手元の MVS の状態ディレクトリ (mvsce-local の MVSCE_LOCAL_DIR)
    NUAGE_CLUSTER   nuage-cluster のリポジトリ (既定 ~/ghq/github.com/k-wa-wa/nuage-cluster)
    KICKS_XMI       KICKS の配布物の XMI (nuage-cluster の kicks.nix の出力。setup で使う)
ジョブの出力は MVS_STATE_DIR/ledger-test/ に書く。帳票 (出力クラス M) は printers/prt00f.txt に出る。
"""

import base64
import datetime
import importlib.util
import json
import os
import re
import sys
import tempfile
import urllib.request
from pathlib import Path

STATE = Path(os.environ.get("MVS_STATE_DIR", Path.home() / ".local/state/mvsce-local"))
os.environ["MVS_STATE_DIR"] = str(STATE)
CLUSTER = Path(os.environ.get("NUAGE_CLUSTER", Path.home() / "ghq/github.com/k-wa-wa/nuage-cluster"))
MAINFRAME = CLUSTER / "nix/hosts/mainframe"
USER, PASSWORD = "IBMUSER", "SYS1"  # MVS/CE の初期値 (手元の MVS だけで使う)
TIMEOUT = 1800
OUT = STATE / "ledger-test"


def load_mvsctl():
    spec = importlib.util.spec_from_file_location("mvsctl", MAINFRAME / "tools/mvsctl.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    # mvsce-local の HTTP サーバーは起動のたびに作るパスワードで認証する
    user, password = (STATE / ".http-auth").read_text().split(":", 1)
    auth = base64.b64encode(f"{user}:{password}".encode()).decode()
    opener = urllib.request.build_opener()
    opener.addheaders = [("Authorization", f"Basic {auth}")]
    urllib.request.install_opener(opener)
    return mod


# ---------------------------------------------------------------- 試しの使用量

# (区分, 名前, 4 つの数量 (100 倍の整数)、日ごとに足す数)。名前の無い勘定 (SCRATCH・TMP) は未登録リストに出る
USAGE = [
    ("K8S", "ARGOCD", (125, 3050, 40, 200), 10),
    ("K8S", "KUBE-SYSTEM", (1010, 30000, 512, 1500), 0),
    ("K8S", "PECHKA", (300, 4000, 120, 800), 5),
    ("K8S", "SCRATCH", (50, 100, 0, 0), 0),
    ("HOST", "LB-1", (50, 2000, 150, 300), 0),
    ("AGENT", "NUAGE-CLUSTER", (14911, 0, 0, 0), 100),
    ("AGENT", "PECHKA", (2136, 0, 0, 0), 0),
    ("AGENT", "NUAGE-AUTOPILOT5", (1378, 0, 0, 0), 1),
    ("AGENT", "NUAGE-MONITORING-STACK", (1006, 0, 0, 0), 0),
    ("AGENT", "NUAGE-USAGE-LEDGER", (0, 0, 0, 0), 0),
    ("AGENT", "TMP", (41, 0, 0, 0), 0),
]


def days(first, last):
    d = first
    while d <= last:
        yield d
        d += datetime.timedelta(days=1)


def usage_records(first, last):
    """USAGEGET が書くのと同じ 80 桁の使用量レコード"""
    recs = []
    for n, d in enumerate(days(first, last)):
        for cls, name, qty, step in USAGE:
            q = [qty[0] + step * n] + list(qty[1:])
            recs.append(f"{d:%Y%m%d}{cls:<5}{name:<24}" + "".join(f"{v:09d}" for v in q))
    return [r.ljust(80) for r in recs]


def ctl_cards(first, last, run):
    """USAGEGET の CTLCARD: RUN の行と、日ごとの DAY の行 (月曜に M)"""
    nxt = last + datetime.timedelta(days=1)
    cards = [f"RUN {run:%Y-%m-%d} {first:%Y-%m-%d} {last:%Y-%m-%d} {nxt:%Y%m%d}"]
    cards += [f"DAY {d:%Y%m%d} {'M' if d.weekday() == 0 else ' '}".rstrip() for d in days(first, last)]
    return [c.ljust(80) for c in cards]


def test_ledger_jcl(pkg, first, last, run):
    """LEDGER の GET (VictoriaMetrics から取る REXX) を、試しの使用量を書くステップに置き換える"""
    jcl = (Path(pkg) / "members/NUAGE.USAGE.JCLLIB/LEDGER").read_text()
    start = jcl.index("//GET      EXEC RXBATCH")
    end = jcl.index("//TRANGEN  EXEC")
    get = "\n".join(
        [
            "//* TEST: THE USAGE AND THE CONTROL CARD ARE GIVEN IN THE JOB",
            "//GET      EXEC PGM=IEBGENER",
            "//SYSPRINT DD SYSOUT=*",
            "//SYSIN    DD DUMMY",
            "//SYSUT1   DD *",
            *usage_records(first, last),
            "/*",
            "//SYSUT2   DD DSN=&&RAW,DISP=(NEW,PASS),UNIT=3350,",
            "//             SPACE=(TRK,(10,10)),DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)",
            "//CTL      EXEC PGM=IEBGENER",
            "//SYSPRINT DD SYSOUT=*",
            "//SYSIN    DD DUMMY",
            "//SYSUT1   DD *",
            *ctl_cards(first, last, run),
            "/*",
            "//SYSUT2   DD DSN=&&CTL,DISP=(NEW,PASS),UNIT=3350,",
            "//             SPACE=(TRK,(1,1)),DCB=(RECFM=F,LRECL=80,BLKSIZE=80)",
            "//*",
        ]
    )
    return jcl[:start] + get + "\n" + jcl[end:]


# ---------------------------------------------------------------- 実行


def report(mvsctl, name, output):
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / f"{name}.txt").write_text(output)
    result = mvsctl.JobResult(name, output)
    # IEF142I <ジョブ> <ステップ> [<手続きのステップ>] - STEP WAS EXECUTED - COND CODE nnnn
    steps = re.findall(rf"IEF142I {name} (\S+(?: \S+)?) - STEP WAS EXECUTED - COND CODE (\d{{4}})", output)
    flushed = re.findall(rf"IEF272I {name} (\S+(?: \S+)?) - STEP WAS NOT EXECUTED", output)
    print(f"{result.summary()}  steps: {', '.join(f'{s}={c}' for s, c in steps)}"
          + (f"  not executed: {', '.join(flushed)}" if flushed else ""), flush=True)
    return result


def submit_chain(mvsctl, jcl, chain):
    """jcl を流し、続けて内部リーダで投入されるジョブ (chain) の終了まで待つ"""
    offset = mvsctl.PRINTER.stat().st_size
    first = mvsctl.jobname_of(jcl)
    results = [report(mvsctl, first, mvsctl.submit(jcl, TIMEOUT).output)]
    for name in chain:
        results.append(report(mvsctl, name, mvsctl.wait_for_job(name, offset, TIMEOUT)))
    return results


def setup(mvsctl, pkg):
    pkg = Path(pkg)
    members = {}
    # システムの設定からは RAKF の権限表と RUNJOB だけを使う。PARMLIB (時間帯。再 IPL が要る) と
    # ZTIMER (毎朝の自動起動) は手元の MVS には入れない
    for dsn, member in [("SYS1.SECURE.CNTL", "PROFILES"), ("SYS2.PROCLIB", "RUNJOB")]:
        members[(dsn, member)] = (MAINFRAME / "mvs-config" / dsn / member).read_text()
    members[("SYS1.SECURE.CNTL", "PROFILES")] += (pkg / "rakf").read_text()
    for path in sorted((pkg / "members").glob("*/*")):
        members[(path.parent.name, path.name)] = path.read_text()
    with tempfile.TemporaryDirectory() as tmp:
        for (dsn, member), content in members.items():
            (Path(tmp) / dsn).mkdir(exist_ok=True)
            (Path(tmp) / dsn / member).write_text(content)
        members = mvsctl.load_members(tmp)
    datasets = json.loads((MAINFRAME / "mvs-datasets.json").read_text()) + json.loads((pkg / "datasets.json").read_text())
    existing = mvsctl.existing_datasets([ds["dsn"] for ds in datasets], USER, PASSWORD, TIMEOUT)
    missing = [ds for ds in datasets if ds["dsn"] not in existing]
    for ds in missing:
        print(f"データセットを作る: {ds['dsn']}", flush=True)
    result = report(mvsctl, "MVSCONF", mvsctl.submit(mvsctl.build_update_job(members, missing, USER, PASSWORD), TIMEOUT).output)
    if not result.ok(max_cc=0):
        return 1
    mvsctl.hercules_command("/S RAKFPROF")
    if os.environ.get("KICKS_XMI"):
        mvsctl.install_kicks(os.environ["KICKS_XMI"], USER, PASSWORD, TIMEOUT)
    return 1 if mvsctl.run_builds(pkg / "build", STATE / "mvs-build.applied.json", TIMEOUT, force=True) else 0


def main(argv):
    cmd = argv[0] if argv else ""
    date = datetime.date.fromisoformat
    if cmd == "usage":
        print("\n".join(usage_records(date(argv[1]), date(argv[2]))))
        return 0
    mvsctl = load_mvsctl()
    if cmd == "setup":
        return setup(mvsctl, argv[1])
    if cmd == "ledger":
        first, last = date(argv[2]), date(argv[3])
        run = last + datetime.timedelta(days=1)
        results = submit_chain(mvsctl, test_ledger_jcl(argv[1], first, last, run), ["LEDGMON", "LEDGINQ"])
        return 0 if results[0].ok(max_cc=0) else 1
    if cmd == "run":
        jcl = (Path(argv[1]) / "members/NUAGE.USAGE.JCLLIB" / argv[2]).read_text()
        chain = {"LEDGER": ["LEDGMON", "LEDGINQ"], "LEDGMON": ["LEDGINQ"]}.get(argv[2], [])
        results = submit_chain(mvsctl, jcl, chain)
        return 0 if results[0].ok(max_cc=4) else 1
    print(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
