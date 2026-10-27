"""MVS に渡す資材が、カード (80 桁) と MVS の制約に合っているかを検査する。

- ASCII のみ (カードは EBCDIC に変換される。日本語は書けない)
- データの行は 72 桁まで (73-80 桁目は連番欄)
- JCL の行 (// で始まる) は 71 桁まで (72 桁目は継続記号)
- ./ と $$ で始まる行は書けない (mvs-config の IEBUPDTE が制御文・終端とみなす)
- ZTIMER の予定行は決まった桁位置に書く (時刻は 10 桁目、コマンドは 24 桁目から)
- RAKF の権限の行は決まった桁位置に書く (対象は 9 桁目、権限は 61 桁目から)
- ビルドジョブ (build/) は JOB 文で始まり、MSGCLASS=A である (nuage-cluster の mvsctl が終了を判定する)
- 勘定マスタ定義 (ACCTDEF) と換算表 (RATES) が、COBOL (TRANGEN・POSTING) の読む桁位置と規則に合っている
"""

import datetime
import json
import re
import sys
from pathlib import Path

errors = []


def check_lines(path, lines):
    for i, line in enumerate(lines, 1):
        where = f"{path}:{i}"
        if any(ord(c) > 127 for c in line):
            errors.append(f"{where}: ASCII 以外の文字がある")
        if len(line) > 72:
            errors.append(f"{where}: 72 桁を超えている ({len(line)})")
        if line.startswith("//") and len(line) > 71:
            errors.append(f"{where}: JCL が 71 桁を超えている ({len(line)})")
        if line.startswith(("./", "$$")):
            errors.append(f"{where}: ./ または $$ で始まる行は書けない")


# 区分コード (勘定番号の 1-3 桁) と区分。300 (AGENT) の金額は USD のセント
CLASS_CODES = {"100": "K8S", "200": "HOST", "300": "AGENT"}
RESOURCES = ("CPU", "MEM", "NET", "STOR")


def check_acctdef(path, lines):
    """COLS 1-10 勘定番号, 12-16 区分, 18-41 名前, 43-51 週次割当額 (TRANGEN・POSTING の FD と同じ)"""
    prev = ""
    names = set()
    for i, line in enumerate(lines, 1):
        where = f"{path}:{i}"
        if line.startswith("*") or not line.strip():
            continue
        no, cls, name, allot = line[0:10], line[11:16].rstrip(), line[17:41].rstrip(), line[42:51]
        if not (len(line) <= 51 and line[10] == " " and line[16] == " " and line[41] == " "):
            errors.append(f"{where}: ACCTDEF の書式 (勘定番号 1-10, 区分 12-16, 名前 18-41, 割当額 43-51 桁) に合わない")
            continue
        if not re.fullmatch(r"\d{10}", no):
            errors.append(f"{where}: 勘定番号が 10 桁の数字でない: {no!r}")
        elif CLASS_CODES.get(no[:3]) != cls:
            errors.append(f"{where}: 勘定番号の区分コード {no[:3]} と区分 {cls!r} が合わない ({CLASS_CODES})")
        if no <= prev:
            errors.append(f"{where}: 勘定番号の順に並んでいない、または重複している: {no}")
        prev = no
        if not re.fullmatch(r"[A-Z0-9][A-Z0-9.-]*", name):
            errors.append(f"{where}: 名前は使用量のデータと同じ大文字で書く: {name!r}")
        if (cls, name) in names:
            errors.append(f"{where}: 区分と名前が重複している: {cls} {name}")
        names.add((cls, name))
        if not re.fullmatch(r"\d{9}", allot):
            errors.append(f"{where}: 週次割当額が 9 桁の数字でない: {allot!r}")


def check_rates(path, lines):
    """COLS 1-8 有効開始日, 10-13 資源, 15-24 換算率 99999.9999 (TRANGEN の FD と同じ)"""
    seen = set()
    for i, line in enumerate(lines, 1):
        where = f"{path}:{i}"
        if line.startswith("*") or not line.strip():
            continue
        m = re.fullmatch(r"(\d{8}) (CPU |MEM |NET |STOR) (\d{5})\.(\d{4})", line.rstrip())
        if not m:
            errors.append(f"{where}: RATES の書式 (日付 1-8, 資源 10-13, 換算率 15-24 桁 99999.9999) に合わない")
            continue
        try:
            datetime.datetime.strptime(m[1], "%Y%m%d")
        except ValueError:
            errors.append(f"{where}: 有効開始日が日付でない: {m[1]}")
        key = (m[1], m[2].rstrip())
        if key in seen:
            errors.append(f"{where}: 同じ日付・資源の換算率が 2 つある: {key}")
        seen.add(key)
    for res in RESOURCES:
        if not any(r == res for _, r in seen):
            errors.append(f"{path}: 資源 {res} の換算率が無い")


def main(out):
    out = Path(out)
    for path in sorted((out / "members").glob("*/*")) + sorted((out / "build").glob("*")):
        check_lines(path.relative_to(out), path.read_text(encoding="latin-1").splitlines())
    for path in sorted((out / "build").glob("*")):
        lines = path.read_text(encoding="latin-1").splitlines()
        # JOB 文と、その継続行 (72 桁目までの末尾が , の行の次)
        card = lines[0]
        for line in lines[1:]:
            if not card.rstrip().endswith(","):
                break
            card += line
        if not re.match(r"//[A-Z@#$][A-Z0-9@#$]{0,7} +JOB ", card) or "MSGCLASS=A" not in card:
            errors.append(f"{path.relative_to(out)}: ビルドジョブは JOB 文で始め、MSGCLASS=A にする")

    schedule = (out / "schedule").read_text(encoding="latin-1").splitlines()
    check_lines("schedule", schedule)
    for i, line in enumerate(schedule, 1):
        if line.startswith("*"):
            continue
        ok = (
            len(line) > 23
            and " " not in line[0:8].rstrip()
            and line[8] == " "
            and re.fullmatch(r"\d\d\.\d\d", line[9:14])
            and line[14] == " "
            and line[15:18] in ("COM", "DSH", "DSN")
            and line[22] == " "
            and line[23] != " "
        )
        if not ok:
            errors.append(f"schedule:{i}: ZTIMER の書式 (日付 1-8 桁, 時刻 10-14 桁, 種別 16-18 桁, 内容 24 桁目から) に合わない")

    # RAKF: CLASS (8) + ENTITY (44) + GROUP (8) + ACCESS
    if (out / "rakf").exists():
        rakf = (out / "rakf").read_text(encoding="latin-1").splitlines()
        check_lines("rakf", rakf)
        for i, line in enumerate(rakf, 1):
            if line[0:8].rstrip() not in ("DATASET", "FACILITY", "DASDVOL", "TAPEVOL", "TERMINAL") or line[60:].strip() not in ("NONE", "READ", "UPDATE", "ALTER"):
                errors.append(f"rakf:{i}: RAKF の書式 (クラス 1-8 桁, 対象 9-52 桁, グループ 53-60 桁, 権限 61 桁目から) に合わない")

    cntl = out / "members" / "NUAGE.USAGE.CNTL"
    check_acctdef("ACCTDEF", (cntl / "ACCTDEF").read_text(encoding="latin-1").splitlines())
    check_rates("RATES", (cntl / "RATES").read_text(encoding="latin-1").splitlines())

    for ds in json.loads((out / "datasets.json").read_text()):
        missing = {"dsn", "unit", "volser", "space", "dcb"} - ds.keys()
        if missing:
            errors.append(f"datasets.json: {ds.get('dsn')}: {sorted(missing)} が無い")

    for e in errors:
        print(e)
    sys.exit(1 if errors else 0)


if __name__ == "__main__":
    main(sys.argv[1])
