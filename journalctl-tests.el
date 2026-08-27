;;; journalctl-tests.el --- Tests for journalctl.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests for journalctl.el.

;;; Code:

(require 'ert)
(require 'journalctl)

(defun journalctl-tests--record (epoch-microseconds)
  "Return a journald-style record with EPOCH-MICROSECONDS realtime timestamp."
  (let ((record (make-hash-table :test 'equal)))
    (puthash "__REALTIME_TIMESTAMP" (number-to-string epoch-microseconds) record)
    record))

(defun journalctl-tests--extract (zone epoch-microseconds)
  "Extract display timestamp for EPOCH-MICROSECONDS with buffer-local ZONE."
  (with-temp-buffer
    (setq-local journalctl--timezone zone)
    (journalctl--extract-timestamp "__REALTIME_TIMESTAMP"
                                   (journalctl-tests--record epoch-microseconds))))

(ert-deftest journalctl-tests-extract-timestamp-utc ()
  (should (equal (journalctl-tests--extract "UTC" 1720000000123456)
                 "2024-07-03 09:46:40.123456")))

(ert-deftest journalctl-tests-extract-timestamp-fixed-zone ()
  (should (equal (journalctl-tests--extract "Asia/Tokyo" 1720000000123456)
                 "2024-07-03 18:46:40.123456")))

(ert-deftest journalctl-tests-extract-timestamp-dst ()
  "A single Olson zone name must honor DST per-record."
  (should (equal (journalctl-tests--extract "Europe/London" 1705000000000001)
                 "2024-01-11 19:06:40.000001"))
  (should (equal (journalctl-tests--extract "Europe/London" 1720000000000001)
                 "2024-07-03 10:46:40.000001")))

(ert-deftest journalctl-tests-extract-timestamp-nil-zone-is-local ()
  (should (equal (journalctl-tests--extract nil 1720000000123456)
                 (concat (format-time-string "%Y-%m-%d %H:%M:%S" 1720000000)
                         ".123456"))))

(ert-deftest journalctl-tests-resolve-timezone-local ()
  (let ((journalctl-timezone 'local))
    (should (equal (journalctl--resolve-timezone) nil))))

(ert-deftest journalctl-tests-resolve-timezone-explicit-string ()
  (let ((journalctl-timezone "UTC"))
    (should (equal (journalctl--resolve-timezone) "UTC"))))

(ert-deftest journalctl-tests-resolve-timezone-system-on-local-host ()
  "`system' on a non-remote host means local time (nil zone)."
  (let ((journalctl-timezone 'system)
        (default-directory temporary-file-directory))
    (should (equal (journalctl--resolve-timezone) nil))))

(provide 'journalctl-tests)
;;; journalctl-tests.el ends here
