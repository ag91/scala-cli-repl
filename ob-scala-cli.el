;;; ob-scala-cli.el --- org-babel for scala evaluation in Scala-Cli. -*- lexical-binding: t; -*-

;; Author: Andrea <andrea-dev@hotmail.com>
;; URL: https://github.com/ag91/scala-cli-repl
;; Package-Requires: ((emacs "28.1") (s "1.12.0") (scala-cli-term-repl "0.0") (xterm-color "1.7"))
;; Version: 0.0
;; Keywords: tools, scala-cli, org-mode, scala, org-babel

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; org-babel for scala evaluation in `scala-cli-repl'.

;; Note: if you use :scala-version as a block param, remember to set
;; it only on the initial block otherwise it causes a reload of the
;; REPL (losing any previous state)

;;; Code:
(require 'ob)
(require 'ob-comint)
(require 'scala-cli-repl)
(require 's)
(require 'xterm-color)

(add-to-list 'org-babel-tangle-lang-exts '("scala" . "scala"))
(add-to-list 'org-src-lang-modes '("scala" . scala))

(defcustom ob-scala-cli-default-params '(:scala-version "3.3.0" :jvm 17)
  "Default parameters for scala-cli."
  :type 'plist
  :group 'org-babel)

(defcustom ob-scala-cli-prompt-str "scala>"
  "Regex for scala-cli prompt."
  :type 'string
  :group 'org-babel)

(defcustom ob-scala-cli-supported-params '(:scala-version :dep :jvm)
  "The ob blocks headers supported by ob-scala-cli."
  :type '(repeat symbol)
  :group 'ob-babel)

(defconst org-babel-header-args:scala
  (seq-map
   (lambda (param)
     `(,(intern (substring (symbol-name param) 1)) . :any))
   ob-scala-cli-supported-params)
  "Provide headers completion for Scala src blocks.")

(defcustom ob-scala-cli-temp-dir (file-name-as-directory (concat temporary-file-directory "ob-scala-cli"))
  "A directory for temporary files."
  :type 'string
  :group 'org-babel)

(defcustom scala-cli-ob-force-kill nil
  "If non-nil, kill the Scala CLI buffer and process without asking for confirmation."
  :type 'boolean
  :group 'ob-babel)

(defvar ob-scala-cli-debug-p nil
  "The variable to control the debug message.")

(defvar ob-scala-cli--last-params nil
  "Used to compare if params have changed before restarting REPL to update configuration.")

(defun ob-scala-cli--params (params)
  "Extract scala-cli command line parameters from PARAMS.

>> (ob-scala-cli--params '(:tangle no :scala-version \"3.0.0\" :jvm \"11\"))
=> (\"--scala-version\" \"3.0.0\" \"--jvm\" \"11\")"
  (flatten-list
   (mapcar
    (lambda (param)
      (when-let ((value (plist-get params param))
                 (p (s-replace ":" "--" (symbol-name param)))) ; NOTE: this means we want `ob-scala-cli--params' to match scala-cli command line params
        (cond
         ((listp value) (mapcar (lambda (d) (list p d)) value))
         ((numberp value) (list p (number-to-string value)))
         (t (list
             p
             (or (ignore-errors (symbol-name value)) value)))
         )))
    ob-scala-cli-supported-params)))

(defun org-babel-execute:scala (body params)
  "Execute the scala code in BODY using PARAMS in org-babel.
This function is called by `org-babel-execute-src-block'
Argument BODY the body to evaluate.
Argument PARAMS the header arguments."
  (let* ((info (org-babel-get-src-block-info))
         (file (ob-scala-cli--mk-file info))
         (params (org-combine-plists ob-scala-cli-default-params (cl--alist-to-plist params)))
         (version (plist-get params ':scala-version))
         (scala-cli-params (ob-scala-cli--params params))
         (parse-response (if (s-starts-with? "3" version) 'ob-scala-cli--parse-response-3 'ob-scala-cli--parse-response-2)))
    (unless (and (scala-cli-repl-is-alive?) (equal scala-cli-params ob-scala-cli--last-params))
      (ignore-errors (ob-scala-cli-kill-buffer))
      (setq-local ob-scala-cli--last-params scala-cli-params)
      (save-window-excursion
        (let ((scala-cli-repl-program-args scala-cli-params))
          (scala-cli-repl)))
      (while (not (and (scala-cli-repl-get-buffer)
                       (with-current-buffer (scala-cli-repl-get-buffer)
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward ob-scala-cli-prompt-str nil t)))))
        (message "Waiting for scala-cli to start...")
        (accept-process-output (scala-cli-repl-get-process))))

    (with-temp-file file (insert body))
    (funcall parse-response
             file
             (ob-scala-cli--wait-for-response (format ":load %s\n" file)))))

(defun ob-scala-cli--wait-for-response (cmd)
  "Send CMD to Scala REPL, wait for REPL prompt and return all consumed output."
  (let ((result "")
        (completed nil))
    (set-process-filter
     (scala-cli-repl-get-process)
     (lambda (process str)
       (term-emulate-terminal process str)
       (let ((str (substring-no-properties (xterm-color-filter str)))) ; Make a plain text
         (when ob-scala-cli-debug-p (message "=== part ===\n%s\n=== /part ===" str))
         (setq result (concat result str))
         (when (s-ends-with? ob-scala-cli-prompt-str (s-trim-right result))
           (when ob-scala-cli-debug-p (message "=== final ===\n%s\n=== /final ===" result))
           (setq completed t)
           (set-process-filter process 'term-emulate-terminal)
           ))))

    (comint-send-string (scala-cli-repl-get-process) cmd)
    (while (not completed) (accept-process-output (scala-cli-repl-get-process)))
    result))

(defun ob-scala-cli-kill-buffer ()
  "Kill Scala CLI buffer."
  (let ((process (scala-cli-repl-get-process))
        (buffer (scala-cli-repl-get-buffer)))
    (when process
      (when scala-cli-ob-force-kill (set-process-query-on-exit-flag process nil))
      (kill-process process))
    (when buffer (kill-buffer buffer))))

(defun ob-scala-cli--parse-response-2 (file response)
  "Parse the result of a Scala 2 RESPONSE removing FILE mention."
  (->> response
       (s-split "/repl.sc...") ; the first part (loading ...) is not interesting
       last
       (s-join "")
       (s-replace ob-scala-cli-prompt-str "") ; remove "scala>"
       (s-replace (format "%s:" file) "On line ") ; the temp file name is not interesting
       s-trim
       (replace-regexp-in-string "[\r\n]+" "\n") ; removing ^M
       ))

(defun ob-scala-cli--parse-response-3 (file response)
  "Parse the result of a Scala 3 RESPONSE removing FILE mention."
  (->> response
       (s-split "/repl.sc") ; the first line is not interesting
       cdr
       (s-join "")
       (s-replace ob-scala-cli-prompt-str "") ; remove "scala>"
       (replace-regexp-in-string "^~" "") ; remove a useless line with ~
       s-trim
       (replace-regexp-in-string "[\r\n]+" "\n") ; removing ^M
       ))

(defun ob-scala-cli-lsp-org ()
  "Modify src block and enable `lsp-metals' to get goodies like code completion in literate programming.

Call this on a second block if you want to reset dependencies or
Scala version, otherwise you will lose the previous session.

This works by creating a .sc file and loading the dependencies
and scala version defined by the block parameters
`scala-cli-params'. Since `lsp-org' requires a :tangle <file>
header is defined, we set it to our temporary Scala script."
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (body (nth 1 info))
         (params (nth 2 info))
         (params (org-combine-plists ob-scala-cli-default-params (cl--alist-to-plist params)))
         (s-params (s-join " " (ob-scala-cli--params params)))
         (deps (plist-get params ':dep))
         (version (plist-get params ':scala-version))
         (file (file-truename (ob-scala-cli--mk-lsp-file info)))
         (dir (file-name-directory file))
         (default-directory dir)) ; to change the working directory for shell-command
    (when (with-demoted-errors "Error: %S" (require 'lsp-metals))
      (with-temp-file file
        (seq-doseq (it deps)
          (insert "//> using dep " it "\n"))
        (when version
          (insert "//> using scala " version "\n"))
        (insert body))
      (message "Configuring ob-scala-cli for lsp through scala-cli...")
      (message "cd %s; scala-cli clean .; scala-cli setup-ide . %s" dir s-params)
      (shell-command (format "%s %s setup-ide ." scala-cli-repl-program s-params))
      (message "Starting lsp-org via lsp-metals...")
      (lsp-org))))

(defun ob-scala-cli--mk-file (&optional info lsp)
  "Create a temporary file for the current source block."
  (let* ((info (or info (org-babel-get-src-block-info)))
         (params (nth 2 info))
         (temp-dir (file-name-as-directory (concat ob-scala-cli-temp-dir (ob-scala-cli--get-id))))
         (temp-dir (if lsp
                       (file-name-as-directory (concat temp-dir "lsp"))
                     temp-dir))
         (block-name (nth 4 (org-babel-get-src-block-info)))
         (lsp-name (or block-name (org-id-uuid)))
         (file-name (format "%s.sc" (if lsp lsp-name "repl")))
         (temp-file (concat temp-dir file-name)))
    (unless (file-exists-p temp-dir)
      (make-directory temp-dir 'parents))
    temp-file))

(defun ob-scala-cli--mk-lsp-file (info)
  "Create a temporary file for lsp or use the specified tangle file."
  (let* ((params (nth 2 info))
         (tangle (cdr (assoc :tangle params))))
    (if (or (not tangle) (string= tangle "no"))
        (let ((file (ob-scala-cli--mk-file info t)))
          (ob-scala-cli--set-tangle file) ; https://emacs-lsp.github.io/lsp-mode/manual-language-docs/lsp-org/
          file)
      tangle)))

(defun ob-scala-cli--set-tangle (file)
  (save-excursion
    (goto-char (org-babel-where-is-src-block-head))
    (end-of-line)
    (insert " :tangle " file)))

(defun ob-scala-cli--get-id ()
  (org-id-get (point-min)))

(provide 'ob-scala-cli)

;;; ob-scala-cli.el ends here

