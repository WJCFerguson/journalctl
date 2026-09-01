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

(defun journalctl-tests--truncate (maximum-lines line-count)
  "Truncate a buffer of LINE-COUNT lines to MAXIMUM-LINES; return contents."
  (with-temp-buffer
    (dotimes (i line-count)
      (insert (format "line-%d\n" i)))
    (let ((journalctl-buffer-maximum-lines maximum-lines))
      (journalctl--truncate-buffer))
    (buffer-string)))

(ert-deftest journalctl-tests-truncate-buffer-over-limit ()
  "Oldest lines are deleted from the top, keeping the newest."
  (should (equal (journalctl-tests--truncate 3 5)
                 "line-2\nline-3\nline-4\n")))

(ert-deftest journalctl-tests-truncate-buffer-under-limit ()
  (should (equal (journalctl-tests--truncate 5 3)
                 "line-0\nline-1\nline-2\n")))

(ert-deftest journalctl-tests-truncate-buffer-at-limit ()
  (should (equal (journalctl-tests--truncate 3 3)
                 "line-0\nline-1\nline-2\n")))

(ert-deftest journalctl-tests-truncate-buffer-nil-disables ()
  (should (equal (journalctl-tests--truncate nil 5)
                 "line-0\nline-1\nline-2\nline-3\nline-4\n")))

(defun journalctl-tests--json-line (n)
  "Return a journald json line with timestamp and message suffix N."
  (format "{\"__REALTIME_TIMESTAMP\":\"172000000000000%d\",\
\"PRIORITY\":\"6\",\"SYSLOG_IDENTIFIER\":\"test\",\"MESSAGE\":\"msg %d\"}\n"
          n n))

(ert-deftest journalctl-tests-flush-json-truncates ()
  "Flushing incoming records keeps the buffer within the line limit."
  (let ((buffer (generate-new-buffer " *journalctl-test*"))
        (process (make-process :name "journalctl-test"
                               :command (list "sleep" "30")
                               :noquery t))
        (journalctl-buffer-maximum-lines 2))
    (unwind-protect
        (progn
          (set-process-plist
           process
           (list 'partial-input (mapconcat #'journalctl-tests--json-line '(1 2 3 4))
                 'target-buffer buffer
                 'insertion-marker (with-current-buffer buffer
                                     (set-marker (make-marker) (point-max)))))
          (journalctl--flush-json process)
          (with-current-buffer buffer
            (should (equal 2 (count-lines (point-min) (point-max))))
            (should (string-match-p "msg 4" (buffer-string)))))
      (delete-process process)
      (kill-buffer buffer))))

(ert-deftest journalctl-tests-truncate-buffer-read-only ()
  "Truncation must work in the read-only journalctl buffer."
  (with-temp-buffer
    (dotimes (i 5)
      (insert (format "line-%d\n" i)))
    (setq buffer-read-only t)
    (let ((journalctl-buffer-maximum-lines 3))
      (journalctl--truncate-buffer))
    (should (equal (buffer-string) "line-2\nline-3\nline-4\n"))))

(provide 'journalctl-tests)
;;; journalctl-tests.el ends here
