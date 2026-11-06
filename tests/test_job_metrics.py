"""scripts/job_metrics.py の単体テスト"""

import tempfile
import unittest
from pathlib import Path

from scripts.job_metrics import (
    ABEND_OR_JCL_ERROR_RC,
    is_success,
    parse_log,
    write_prom,
)


class TestJobMetrics(unittest.TestCase):
    def test_is_success_generic_jobs(self):
        # 正常終了 (RC 0)
        self.assertTrue(is_success("USAGERPT", 0, [("EXEC", 0)], False, False))
        # 異常終了 (RC > 0)
        self.assertFalse(is_success("USAGERPT", 8, [("EXEC", 8)], False, False))
        # ABEND / JCL ERROR / 空ステップ
        self.assertFalse(is_success("USAGERPT", 0, [("EXEC", 0)], True, False))
        self.assertFalse(is_success("USAGERPT", 0, [("EXEC", 0)], False, True))
        self.assertFalse(is_success("USAGERPT", 0, [], False, False))

    def test_is_success_ledger(self):
        # RC 0 (全ステップ正常: GET, TRANGEN(COB/GO), SORT, POSTING(COB/GO), COMMIT...)
        self.assertTrue(
            is_success(
                "LEDGER",
                0,
                [
                    ("EXEC", 0),
                    ("COB", 0),
                    ("GO", 0),
                    ("SORT", 0),
                    ("COB", 0),
                    ("GO", 0),
                    ("MONTHLY", 0),
                ],
                False,
                False,
            )
        )
        # 締める日がない場合の正常スキップ (EXEC RC 4 のみ)
        self.assertTrue(is_success("LEDGER", 4, [("EXEC", 4)], False, False))
        # 他のステップでの RC 4 は失敗 (例: TRANGEN の GO が 4)
        self.assertFalse(
            is_success(
                "LEDGER",
                4,
                [("EXEC", 0), ("COB", 0), ("GO", 4)],
                False,
                False,
            )
        )
        # RC 16 (POSTING の GO が 16) は失敗
        self.assertFalse(
            is_success(
                "LEDGER",
                16,
                [
                    ("EXEC", 0),
                    ("COB", 0),
                    ("GO", 0),
                    ("SORT", 0),
                    ("COB", 0),
                    ("GO", 16),
                ],
                False,
                False,
            )
        )

    def test_is_success_ledgmon(self):
        # 月次締め実行正常 (RC 0: MONSEL(COB/GO), SORT, ACCTLDG(COB/GO), COMMIT, ONLINE)
        self.assertTrue(
            is_success(
                "LEDGMON",
                0,
                [
                    ("COB", 0),
                    ("GO", 0),
                    ("SORT", 0),
                    ("COB", 0),
                    ("GO", 0),
                    ("CHIST", 0),
                    ("CMONBAL", 0),
                    ("CBIZDATE", 0),
                    ("ONLINE", 0),
                ],
                False,
                False,
            )
        )
        # 締める月がない場合の正常スキップ (COB 0, GO 4, ONLINE 0)
        self.assertTrue(
            is_success(
                "LEDGMON",
                4,
                [("COB", 0), ("GO", 4), ("ONLINE", 0)],
                False,
                False,
            )
        )
        # ACCTLDG の GO が RC 4 を返した場合（回帰テスト: ステップ名 GO だけに依存していないことの確認）
        self.assertFalse(
            is_success(
                "LEDGMON",
                4,
                [
                    ("COB", 0),
                    ("GO", 0),
                    ("SORT", 0),
                    ("COB", 0),
                    ("GO", 4),
                    ("ONLINE", 0),
                ],
                False,
                False,
            )
        )
        # MONSEL の COB が RC 4 を返した場合
        self.assertFalse(
            is_success(
                "LEDGMON",
                4,
                [("COB", 4), ("ONLINE", 0)],
                False,
                False,
            )
        )
        # ACCTLDG の検証不一致など (RC 16) は失敗
        self.assertFalse(
            is_success(
                "LEDGMON",
                16,
                [
                    ("COB", 0),
                    ("GO", 0),
                    ("SORT", 0),
                    ("COB", 0),
                    ("GO", 16),
                    ("ONLINE", 0),
                ],
                False,
                False,
            )
        )
        # ABEND は失敗
        self.assertFalse(
            is_success(
                "LEDGMON",
                4,
                [("COB", 0), ("GO", 4), ("ONLINE", 0)],
                True,
                False,
            )
        )

    def test_is_success_mvslcat(self):
        self.assertTrue(is_success("MVSLCAT", 0, [("LISTCAT", 0)], False, False))
        self.assertTrue(is_success("MVSLCAT", 4, [("LISTCAT", 4)], False, False))
        self.assertFalse(is_success("MVSLCAT", 8, [("LISTCAT", 8)], False, False))

    def test_parse_log_ledger_success_and_skip(self):
        log_ledger_skip = """
JOB 1000 $HASP100 LEDGER   ON INTRDR
JOB 1000 $HASP373 LEDGER   STARTED - INIT 1 - CLASS A - SYS NUA
JOB 1000 IEFACTRT EXEC     /00:00:01/000000/000000/4/LEDGER
JOB 1000 $HASP395 LEDGER   ENDED
"""
        jobs = {}
        now = 1700000000
        parse_log(log_ledger_skip, jobs, now)
        self.assertIn("LEDGER", jobs)
        self.assertEqual(jobs["LEDGER"]["last_rc"], 4)
        self.assertEqual(jobs["LEDGER"]["last_end_timestamp_seconds"], now)
        self.assertEqual(jobs["LEDGER"]["last_success_timestamp_seconds"], now)

    def test_parse_log_ledger_invalid_rc4(self):
        log_ledger_fail = """
JOB 1001 $HASP100 LEDGER   ON INTRDR
JOB 1001 $HASP373 LEDGER   STARTED - INIT 1 - CLASS A - SYS NUA
JOB 1001 IEFACTRT EXEC     /00:00:01/000000/000000/0/LEDGER
JOB 1001 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGER
JOB 1001 IEFACTRT GO       /00:00:01/000000/000000/4/LEDGER
JOB 1001 $HASP395 LEDGER   ENDED
"""
        jobs = {}
        now = 1700000000
        parse_log(log_ledger_fail, jobs, now)
        self.assertEqual(jobs["LEDGER"]["last_rc"], ABEND_OR_JCL_ERROR_RC)
        self.assertEqual(jobs["LEDGER"]["last_end_timestamp_seconds"], now)
        self.assertNotIn("last_success_timestamp_seconds", jobs["LEDGER"])

    def test_parse_log_ledgmon_success_skip(self):
        log_ledgmon_skip = """
JOB 2000 $HASP100 LEDGMON  ON INTRDR
JOB 2000 $HASP373 LEDGMON  STARTED - INIT 1 - CLASS A - SYS NUA
JOB 2000 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGMON
JOB 2000 IEFACTRT GO       /00:00:01/000000/000000/4/LEDGMON
JOB 2000 IEFACTRT ONLINE   /00:00:01/000000/000000/0/LEDGMON
JOB 2000 $HASP395 LEDGMON  ENDED
"""
        jobs = {}
        now = 1700000000
        parse_log(log_ledgmon_skip, jobs, now)
        self.assertIn("LEDGMON", jobs)
        self.assertEqual(jobs["LEDGMON"]["last_rc"], 4)
        self.assertEqual(jobs["LEDGMON"]["last_end_timestamp_seconds"], now)
        self.assertEqual(jobs["LEDGMON"]["last_success_timestamp_seconds"], now)

    def test_parse_log_ledgmon_full_success(self):
        log_ledgmon_full = """
JOB 2001 $HASP100 LEDGMON  ON INTRDR
JOB 2001 $HASP373 LEDGMON  STARTED - INIT 1 - CLASS A - SYS NUA
JOB 2001 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGMON
JOB 2001 IEFACTRT GO       /00:00:01/000000/000000/0/LEDGMON
JOB 2001 IEFACTRT SORT     /00:00:01/000000/000000/0/LEDGMON
JOB 2001 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGMON
JOB 2001 IEFACTRT GO       /00:00:01/000000/000000/0/LEDGMON
JOB 2001 IEFACTRT CHIST    /00:00:01/000000/000000/0/LEDGMON
JOB 2001 IEFACTRT CMONBAL  /00:00:01/000000/000000/0/LEDGMON
JOB 2001 IEFACTRT CBIZDATE /00:00:01/000000/000000/0/LEDGMON
JOB 2001 IEFACTRT ONLINE   /00:00:01/000000/000000/0/LEDGMON
JOB 2001 $HASP395 LEDGMON  ENDED
"""
        jobs = {}
        now = 1700000000
        parse_log(log_ledgmon_full, jobs, now)
        self.assertEqual(jobs["LEDGMON"]["last_rc"], 0)
        self.assertEqual(jobs["LEDGMON"]["last_success_timestamp_seconds"], now)

    def test_parse_log_ledgmon_invalid_rc4(self):
        log_ledgmon_bad_rc4 = """
JOB 2002 $HASP100 LEDGMON  ON INTRDR
JOB 2002 $HASP373 LEDGMON  STARTED - INIT 1 - CLASS A - SYS NUA
JOB 2002 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGMON
JOB 2002 IEFACTRT GO       /00:00:01/000000/000000/0/LEDGMON
JOB 2002 IEFACTRT SORT     /00:00:01/000000/000000/4/LEDGMON
JOB 2002 IEFACTRT ONLINE   /00:00:01/000000/000000/0/LEDGMON
JOB 2002 $HASP395 LEDGMON  ENDED
"""
        jobs = {}
        now = 1700000000
        parse_log(log_ledgmon_bad_rc4, jobs, now)
        self.assertEqual(jobs["LEDGMON"]["last_rc"], ABEND_OR_JCL_ERROR_RC)
        self.assertNotIn("last_success_timestamp_seconds", jobs["LEDGMON"])

    def test_parse_log_ledgmon_acctldg_rc4_failure(self):
        # ACCTLDG の GO が RC 4 を返し、後続確定がスキップされ ONLINE が RC 0 で終わるケース
        # MONSEL の正常スキップと混同せず、9999 で失敗と判定されること
        log_ledgmon_acctldg_rc4 = """
JOB 2002 $HASP100 LEDGMON  ON INTRDR
JOB 2002 $HASP373 LEDGMON  STARTED - INIT 1 - CLASS A - SYS NUA
JOB 2002 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGMON
JOB 2002 IEFACTRT GO       /00:00:01/000000/000000/0/LEDGMON
JOB 2002 IEFACTRT SORT     /00:00:01/000000/000000/0/LEDGMON
JOB 2002 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGMON
JOB 2002 IEFACTRT GO       /00:00:01/000000/000000/4/LEDGMON
JOB 2002 IEFACTRT ONLINE   /00:00:01/000000/000000/0/LEDGMON
JOB 2002 $HASP395 LEDGMON  ENDED
"""
        jobs = {}
        now = 1700000000
        parse_log(log_ledgmon_acctldg_rc4, jobs, now)
        self.assertEqual(jobs["LEDGMON"]["last_rc"], ABEND_OR_JCL_ERROR_RC)
        self.assertNotIn("last_success_timestamp_seconds", jobs["LEDGMON"])

    def test_parse_log_ledgmon_rc16_failure(self):
        # ACCTLDG の GO が RC 16 を返し、ONLINE が RC 0 で終わるケース
        log_ledgmon_rc16 = """
JOB 2003 $HASP100 LEDGMON  ON INTRDR
JOB 2003 $HASP373 LEDGMON  STARTED - INIT 1 - CLASS A - SYS NUA
JOB 2003 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGMON
JOB 2003 IEFACTRT GO       /00:00:01/000000/000000/0/LEDGMON
JOB 2003 IEFACTRT SORT     /00:00:01/000000/000000/0/LEDGMON
JOB 2003 IEFACTRT COB      /00:00:01/000000/000000/0/LEDGMON
JOB 2003 IEFACTRT GO       /00:00:01/000000/000000/16/LEDGMON
JOB 2003 IEFACTRT ONLINE   /00:00:01/000000/000000/0/LEDGMON
JOB 2003 $HASP395 LEDGMON  ENDED
"""
        jobs = {}
        now = 1700000000
        parse_log(log_ledgmon_rc16, jobs, now)
        self.assertEqual(jobs["LEDGMON"]["last_rc"], 16)
        self.assertNotIn("last_success_timestamp_seconds", jobs["LEDGMON"])

    def test_parse_log_abend_and_previous_success_retention(self):
        # 1回目: 成功
        jobs = {
            "LEDGMON": {
                "last_rc": 0,
                "last_end_timestamp_seconds": 1699990000,
                "last_success_timestamp_seconds": 1699990000,
            }
        }
        # 2回目: ABEND
        log_abend = """
JOB 2004 $HASP100 LEDGMON  ON INTRDR
JOB 2004 $HASP373 LEDGMON  STARTED - INIT 1 - CLASS A - SYS NUA
JOB 2004 IEF450I LEDGMON - ABEND S0C4 U0000
JOB 2004 $HASP395 LEDGMON  ENDED
"""
        now = 1700000000
        parse_log(log_abend, jobs, now)
        self.assertEqual(jobs["LEDGMON"]["last_rc"], ABEND_OR_JCL_ERROR_RC)
        self.assertEqual(jobs["LEDGMON"]["last_end_timestamp_seconds"], now)
        # 成功時刻は前回の値が維持される
        self.assertEqual(jobs["LEDGMON"]["last_success_timestamp_seconds"], 1699990000)

    def test_parse_log_jcl_error(self):
        log_jcl_err = """
JOB 3000 $HASP100 BADJOB   ON INTRDR
JOB 3000 $HASP373 BADJOB   STARTED - INIT 1 - CLASS A - SYS NUA
JOB 3000 IEF452I BADJOB - JOB FAILED - JCL ERROR
JOB 3000 $HASP395 BADJOB   ENDED
"""
        jobs = {}
        now = 1700000000
        parse_log(log_jcl_err, jobs, now)
        self.assertEqual(jobs["BADJOB"]["last_rc"], ABEND_OR_JCL_ERROR_RC)
        self.assertNotIn("last_success_timestamp_seconds", jobs["BADJOB"])

    def test_write_prom(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            prom_path = Path(tmpdir) / "test.prom"
            jobs = {
                "LEDGER": {
                    "last_rc": 4,
                    "last_end_timestamp_seconds": 1700000100,
                    "last_success_timestamp_seconds": 1700000100,
                },
                "LEDGMON": {
                    "last_rc": 0,
                    "last_end_timestamp_seconds": 1700000200,
                    "last_success_timestamp_seconds": 1700000200,
                },
            }
            write_prom(jobs, prom_path)
            self.assertTrue(prom_path.exists())
            content = prom_path.read_text()
            self.assertIn('mvs_job_last_rc{job_name="LEDGER"} 4', content)
            self.assertIn('mvs_job_last_rc{job_name="LEDGMON"} 0', content)
            self.assertIn('mvs_job_last_end_timestamp_seconds{job_name="LEDGER"} 1700000100', content)
            self.assertIn('mvs_job_last_success_timestamp_seconds{job_name="LEDGMON"} 1700000200', content)


if __name__ == "__main__":
    unittest.main()
