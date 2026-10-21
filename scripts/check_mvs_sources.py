"""MVS に渡す資材が、カード (80 桁) と MVS の制約に合っているかを検査する。

- ASCII のみ (カードは EBCDIC に変換される。日本語は書けない)
- データの行は 72 桁まで (73-80 桁目は連番欄)
- JCL の行 (// で始まる) は 71 桁まで (72 桁目は継続記号)
- ./ と $$ で始まる行は書けない (mvs-config の IEBUPDTE が制御文・終端とみなす)
- ZTIMER の予定行は決まった桁位置に書く (時刻は 10 桁目、コマンドは 24 桁目から)
"""

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


def main(out):
    out = Path(out)
    for path in sorted((out / "members").glob("*/*")):
        check_lines(path.relative_to(out), path.read_text(encoding="latin-1").splitlines())

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

    for ds in json.loads((out / "datasets.json").read_text()):
        missing = {"dsn", "unit", "volser", "space", "dcb"} - ds.keys()
        if missing:
            errors.append(f"datasets.json: {ds.get('dsn')}: {sorted(missing)} が無い")

    for e in errors:
        print(e)
    sys.exit(1 if errors else 0)


if __name__ == "__main__":
    main(sys.argv[1])
