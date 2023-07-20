;;; scala-cli-repl.el --- Scala CLI REPL in term mode.

;; Copyright (C) 2023 Andrea

;; Author: ag91 <andrea-dev@hotmail.com>
;; URL: https://github.com/ag91/scala-cli-repl
;; Package-Requires: ((emacs "24.3") (s "1.12.0") (scala-mode "0.23"))
;; Version: 0.0
;; Keywords: processes, scala-cli, term, scala

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
;; Usage
;; (add-hook 'scala-mode-hook
;;           (lambda ()
;;             (scala-cli-repl-minor-mode t)))

;;; Code:
(require 'term)
(require 'comint)
(require 'scala-mode)
(require 's)
(require 'subr-x)

(defgroup scala-cli-repl nil
  "A minor mode for a Scala-Cli REPL."
  :group 'scala)

(defcustom scala-cli-repl-buffer-name "*Scala-Cli*"
  "Buffer name for scala-cli."
  :type 'string
  :group 'scala-cli-repl)

(defcustom scala-cli-repl-program "scala-cli"
  "Program name for scala-cli."
  :type 'string
  :group 'scala-cli-repl)

(defcustom scala-cli-repl-program-args '()
  "Arguments for scala-cli command."
  :type '(repeat string)
  :group 'scala-cli-repl)

(defcustom scala-cli-repl-prompt-regex "^scala> "
  "Regex for scala-cli prompt."
  :type 'string
  :group 'scala-cli-repl)

(defcustom scala-cli-repl-run-hook nil
  "Hook to run after starting an Scala-Cli REPL buffer."
  :type 'hook
  :group 'scala-cli-repl)

(defvar scala-cli-repl-program-local-args '()
  "Local args for scala-cli term repl program.")

(defun scala-cli-repl-check-process ()
  "Check if there is an active scala-cli process."
  (unless (comint-check-proc scala-cli-repl-buffer-name)
    (error "Scala-Cli is not running")))

(defun scala-cli-repl-code-first-line (code)
  "Get the first line of CODE."
  (s-trim (car-safe (s-split "\n" code))))

;;;###autoload
(defun scala-cli-repl-send-defun ()
  "Send the definition to the scala-cli buffer."
  (interactive)
  (scala-cli-repl-check-process)
  (save-mark-and-excursion
    (let (start end)
      (scala-syntax:beginning-of-definition)
      (setq start (point))
      (scala-syntax:end-of-definition)
      (setq end (point))
      (let ((code (buffer-substring-no-properties start end)))
        (comint-send-string scala-cli-repl-buffer-name code)
        (comint-send-string scala-cli-repl-buffer-name "\n")
        (message
         (format "Sent: %s..." (scala-cli-repl-code-first-line code)))))))

;;;###autoload
(defun scala-cli-repl-send-region (start end)
  "Send the region to the scala-cli buffer.
Argument START the start region.
Argument END the end region."
  (interactive "r")
  (scala-cli-repl-check-process)
  (let ((code (buffer-substring-no-properties start end)))
    (comint-send-string scala-cli-repl-buffer-name "{\n")
    (comint-send-string scala-cli-repl-buffer-name code)
    (comint-send-string scala-cli-repl-buffer-name "\n}")
    (comint-send-string scala-cli-repl-buffer-name "\n")
    (message
     (format "Sent: %s..." (scala-cli-repl-code-first-line code)))))

;;;###autoload
(defun scala-cli-repl-send-buffer ()
  "Send the buffer to the scala-cli buffer."
  (interactive)
  (save-mark-and-excursion
    (goto-char (point-min))
    (re-search-forward "^package .+\n+" nil t)
    (scala-cli-repl-send-region (point) (point-max))))

;;;###autoload
(defun scala-cli-repl-load-file (file-name)
  "Load a file to the scala-cli buffer.
Argument FILE-NAME the file name."
  (interactive (comint-get-source "Load Scala file: " nil '(scala-mode) t))
  (comint-check-source file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (scala-cli-repl-send-buffer)))

;;;###autoload
(defun scala-cli-repl ()
  "Run an Scala-Cli REPL."
  (interactive)
  (unless (executable-find scala-cli-repl-program)
    (error (format "%s is not found." scala-cli-repl-program)))

  (unless (comint-check-proc scala-cli-repl-buffer-name)
    (ignore-errors (kill-buffer scala-cli-repl-buffer-name))

    (setq scala-cli-repl-program-local-args scala-cli-repl-program-args)

    ;; (when-let ((_ scala-cli-repl-auto-detect-predef-file)
    ;;            (path (or (locate-dominating-file default-directory scala-cli-repl-predef-sc-filename)
    ;;                      scala-cli-repl-default-predef-dir))
    ;;            (file (expand-file-name scala-cli-repl-predef-sc-filename path)))
    ;;   (setq scala-cli-repl-program-local-args
    ;;         (append scala-cli-repl-program-args `("-p" ,file))))

    (message (format "Run: %s %s"
                     scala-cli-repl-program
                     (s-join " " scala-cli-repl-program-local-args)))

    (with-current-buffer
        (apply 'term-ansi-make-term
               scala-cli-repl-buffer-name
               scala-cli-repl-program
               nil
               nil
               ;; nil
               ;; scala-cli-repl-program-local-args
               )
      (term-char-mode)
      (term-set-escape-char ?\C-x)
      (setq-local term-prompt-regexp scala-cli-repl-prompt-regex)
      (setq-local term-scroll-show-maximum-output t)
      (setq-local term-scroll-to-bottom-on-output t)
      (run-hooks 'scala-cli-repl-run-hook)))

  (pop-to-buffer scala-cli-repl-buffer-name))

;;;###autoload
(defalias 'run-scala-cli 'scala-cli-repl)

(defun scala-cli-repl--send-string (string)
  "Send the code to the scala-cli buffer.
Argument STRING the code to send."
  (scala-cli-repl-check-process)
  (comint-send-string scala-cli-repl-buffer-name "{\n")
  (comint-send-string scala-cli-repl-buffer-name string)
  (comint-send-string scala-cli-repl-buffer-name "\n}")
  (comint-send-string scala-cli-repl-buffer-name "\n")
  (message
   (format "Sent: %s..." (scala-cli-repl-code-first-line string))))

;;;###autoload

(defun scala-cli-convert-sbt-dependencies (fn)
  "Substitute sbt-style dependencies with scala-cli ones, and apply FN to that."
  (if (region-active-p)
      (thread-last
        (substring-no-properties (funcall region-extract-function))
        (s-replace-all '((" " . "") ("\"" . "") ("%" . ":")))
        (funcall fn))
    (error "You need to highlight the dependency in your region to use this")))

(defun scala-cli-conver-and-kill-deps ()
  "Kill sbt dependency in Mill format."
  (interactive)
  (funcall 'scala-cli-convert-sbt-dependencies 'kill-new))

(defun scala-cli-repl-import-ivy-dependencies-from-sbt ()
  "Try to import ivy dependencies from sbt file.
Currently only form like
libraryDependencies += \"com.typesafe.akka\" %% \"akka-actor\" % \"2.5.21\"
is available."
  (interactive)
  (when-let* ((file-name "build.sbt")
              (path (locate-dominating-file default-directory file-name))
              (file (concat path file-name)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((regex "libraryDependencies[ ]+\\+=[ ]+\"\\(.+?\\)\"[ ]+%\\{1,2\\}[ ]+\"\\(.+?\\)\"[ ]+%\\{1,2\\}[ ]+\"\\(.+?\\)\"")
            (res))
        (while (re-search-forward regex nil t)
          (add-to-list
           'res
           (format "import $ivy.`%s::%s:%s`" (match-string 1) (match-string 2) (match-string 3))
           t))
        (scala-cli-repl--send-string (s-join "\n" res))))))

(defvar scala-cli-repl-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-`") 'scala-cli-repl)
    (define-key map (kbd "C-c C-z") 'scala-cli-repl)
    (define-key map (kbd "C-c C-e") 'scala-cli-repl-send-defun)
    (define-key map (kbd "C-c C-r") 'scala-cli-repl-send-region)
    (define-key map (kbd "C-c C-b") 'scala-cli-repl-send-buffer)
    (define-key map (kbd "C-c C-l") 'scala-cli-repl-load-file)
    map)
  "Keymap while function ‘scala-cli-repl-minor-mode’ is active.")

;;;###autoload
(define-minor-mode scala-cli-repl-minor-mode
  "Minor mode for interacting with an Scala-Cli REPL."
  :keymap scala-cli-repl-minor-mode-map)

(provide 'scala-cli-repl)

;;; scala-cli-repl.el ends here
