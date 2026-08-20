;;; ob-janet.el --- Org-Babel support for the Janet language  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 FoAM oü

;; Author: nik gaffney <nik@fo.am>
;; Keywords: languages, tools, literate programming, janet
;; Homepage: https://codeberg.org/zzkt/ob-janet
;; Version: 1.0.4
;; Package-Requires: ((emacs "26.1") (org "9.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for Janet (https://janet-lang.org/)
;; Based on ob-template.el and previous work from DEADB17
;;  - https://github.com/DEADB17/ob-janet
;;  - https://github.com/DEADB17/ob-racket
;;
;; Setup:
;;   (add-to-list 'org-babel-load-languages '(janet . t))

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

;;; Customization

(defgroup ob-janet nil
  "Org Babel support for Janet."
  :group 'org-babel
  :prefix "ob-janet-")

(defcustom ob-janet-executable "janet"
  "Janet executable name or path."
  :type 'string
  :group 'ob-janet)

(defcustom ob-janet-path nil
  "JANET_PATH environment variable. When non-nil, sets the module search path."
  :type '(choice (const :tag "Use system default" nil) string)
  :group 'ob-janet)

(defcustom ob-janet-hline-to "nil"
  "Replacement for table hlines in Janet input."
  :type 'string
  :group 'ob-janet)

(defcustom ob-janet-nil-to 'hline
  "Replacement for Janet nil in returned tables."
  :type 'symbol
  :group 'ob-janet)

(defcustom ob-janet-output-wrapper "%s"
  "Template wrapping body for :results output."
  :type 'string
  :group 'ob-janet)

(defcustom ob-janet-value-wrapper
  "(import spork/test)\n(pp (test/suppress-stdout (do %s)))"
  "Template wrapping body for :results value."
  :type 'string
  :group 'ob-janet)

(defvar org-babel-default-header-args:janet
  '((:results . "output"))
  "Default header arguments for Janet source blocks.")

(add-to-list 'org-babel-tangle-lang-exts '("janet" . "janet"))


;;; Elisp -> Janet conversion

(defun ob-janet--to-janet (value)
  "Convert Elisp VALUE to Janet syntax."
  (cond
   ((eq value 'hline) ob-janet-hline-to)
   ((null value) "nil")
   ((eq value t) "true")
   ((numberp value) (number-to-string value))
   ((stringp value) (format "%S" value))
   ((symbolp value) (format "'%s" value))
   ;; cons
   ((consp value)
    (if (and (cdr value) (atom (cdr value)))
        (ob-janet--to-janet-tuple value)
      (concat "(tuple "
              (mapconcat #'ob-janet--to-janet value " ")
              ")")))
   ;; vector
   ((vectorp value)
    (concat "(array "
            (mapconcat #'ob-janet--to-janet
                       (append value nil) " ")
            ")"))
   ;; hash table
   ((hash-table-p value)
    (let ((pairs nil))
      (maphash (lambda (k v)
                 (push (ob-janet--to-janet-hash k v)
                       pairs))
               value)
      (concat "(table " (mapconcat #'identity pairs " ") ")")))
   ;; other
   (t (format "%S" value))))

(defun ob-janet--to-janet-tuple (value)
  "Convert Elisp VALUE to Janet tuple syntax."
  (format "(tuple %s %s)"
          (ob-janet--to-janet (car value))
          (ob-janet--to-janet (cdr value))))


(defun ob-janet--to-janet-hash (key value)
  "Convert Elisp KEY, VALUE pair to Janet syntax."
  (format "%s %s"
          (ob-janet--to-janet key)
          (ob-janet--to-janet value)))


(defun ob-janet--vars-to-defs (vars)
  "Convert alist VARS to Janet (def name value) expressions."
  (mapconcat (lambda (pair)
               (format "(def %s %s)" (car pair) (ob-janet--to-janet (cdr pair))))
             vars "\n"))


;;; Body expansion

(defun org-babel-expand-body:janet (body params &optional processed-params)
  "Expand BODY with PARAMS, or optional PROCESSED-PARAMS to avoid re-processing."
  (let ((processed (or processed-params (org-babel-process-params params))))
    (with-temp-buffer
      (when-let* ((prologue (alist-get :prologue params)))
        (insert prologue "\n"))
      (let ((vars (org-babel--get-vars processed)))
        (when vars (insert (ob-janet--vars-to-defs vars) "\n")))
      (insert body)
      (when-let* ((epilogue (alist-get :epilogue params)))
        (insert epilogue "\n"))
      (buffer-string))))


;;; Output parsing

(defun ob-janet--parse-result (result)
  "Parse Janet RESULT string, substituting nil for ob-janet-nil-to."
  (let ((parsed (org-babel-script-escape (string-trim result))))
    (if (listp parsed)
        (mapcar (lambda (el) (if (equal el 'nil) ob-janet-nil-to el)) parsed)
      parsed)))


(defun ob-janet--parse-session-output (output)
  "Parse session OUTPUT, extracting result after echoed code."
  ;; Remove any comint prompts and echoed input. keep the result
  (let ((clean (replace-regexp-in-string "^repl:[0-9]+:> " "" output)))
    (replace-regexp-in-string "\\`\n+" "" clean)))


;;; Sessions

(defun ob-janet--session-p (session)
  "Return non-nil if SESSION is a valid session name."
  (and session (not (string= session "none"))))

(defun ob-janet--initiate-session (&optional session)
  "Ensure a Janet REPL SESSION exists, return buffer name."
  (let ((name (if (ob-janet--session-p session)
                  (format "janet-%s" session) "janet"))
        (process-environment
         (append '("TERM=dumb")
                 (when ob-janet-path
                   (list (concat "JANET_PATH=" ob-janet-path)))
                 process-environment)))
    (unless (comint-check-proc (format "*%s*" name))
      (make-comint name ob-janet-executable nil "-n")
      (accept-process-output nil 1)
      (with-current-buffer (format "*%s*" name)
        (set (make-local-variable 'comint-prompt-regexp)
             "^repl:[0-9]+:> ")))
    (format "*%s*" name)))


(defun ob-janet--execute-to-session (code session)
  "Send CODE to SESSION and return output."
  (let ((buf (ob-janet--initiate-session session)))
    (with-current-buffer buf
      (let* ((proc (get-buffer-process (current-buffer)))
             (start (point-max)))
        ;; Send code with newline
        (comint-send-string proc (concat code "\n"))
        ;; Wait for prompt to appear
        (let ((deadline (+ (float-time) 5)))
          (while (and (< (float-time) deadline)
                      (not (save-excursion
                             (goto-char (point-max))
                             (re-search-backward comint-prompt-regexp nil t)
                             (> (point) start))))
            (accept-process-output proc 0.1)))
        ;; Return everything after start (includes echoed code & output)
        (buffer-substring-no-properties start (point-max))))))


(defun ob-janet--execute-to-file (expanded file)
  "Execute EXPANDED code and write output to FILE."
  (let ((result (ob-janet--execute-external
                 expanded ob-janet-executable)))
    (with-temp-file file (insert result))
    nil))

(defun ob-janet--execute-external (code cmd)
  "Run CODE via CMD."
  (let ((file (org-babel-temp-file "ob-janet-" ".janet"))
        (process-environment
         (if ob-janet-path
             (cons (concat "JANET_PATH=" ob-janet-path) process-environment)
           process-environment)))
    (with-temp-file file (insert code))
    (org-babel-eval
     (concat (shell-quote-argument cmd) " "
             (org-babel-process-file-name file)) "")))


(defun org-babel-execute:janet (body params)
  "Execute Janet code BODY with header arguments PARAMS."
  (let* ((processed     (org-babel-process-params params))
         (session       (cdr (assq :session processed)))
         (result-type   (cdr (assq :result-type processed)))
         (result-params (cdr (assq :result-params processed)))
         (cmd           (alist-get :cmd params ob-janet-executable))
         (file          (alist-get :file params))
         (expanded      (org-babel-expand-body:janet
                         body params processed)))
    ;; debug
    (if (or (assoc :debug params) (assoc :debug processed))
        (concat (if (org-babel--get-vars processed)
                    (concat (ob-janet--vars-to-defs
                             (org-babel--get-vars processed))
                            "\n")
                  "")
                (if (string= result-type "value")
                    (format ob-janet-value-wrapper body)
                  (format ob-janet-output-wrapper body)))
      ;; output format
      (when (not file)
        (setq expanded (if (string= result-type "value")
                           (format ob-janet-value-wrapper expanded)
                         (format ob-janet-output-wrapper expanded))))
      ;; session or file?
      (cond
       ((ob-janet--session-p session)
        (ob-janet--parse-session-output
         (ob-janet--execute-to-session expanded session)))
       (file (ob-janet--execute-to-file expanded file) nil)
       (t
        (let ((result (ob-janet--execute-external expanded cmd)))
          (org-babel-reassemble-table
           (org-babel-result-cond result-params result
                                  (ob-janet--parse-result result))
           (org-babel-pick-name (cdr (assq :colname-names processed))
                                (cdr (assq :colnames processed)))
           (org-babel-pick-name (cdr (assq :rowname-names processed))
                                  (cdr (assq :rownames processed))))))))))


;;; Org-babel session functions

(defun org-babel-prep-session:janet (session _params)
  "Prepare a Janet SESSION."
  (unless (ob-janet--session-p session)
    (error "Janet sessions require a :session name"))
  (ob-janet--initiate-session session))

(defun org-babel-janet-initiate-session (&optional session)
  "Initialize a Janet SESSION buffer."
  (ob-janet--initiate-session session))

(defun org-babel-janet-session-info (&optional session)
  "Return info for SESSION."
  (format "Janet REPL: %s" (or session "default")))


(provide 'ob-janet)
;;; ob-janet.el ends here
