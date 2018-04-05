;;; clush-mode.el --- Emacs mode for interactive clush sessions. -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018 Alex Vorobiev

;; Author: Alex Vorobiev <alexander.vorobiev@gmail.com>
;; URL: https://github.com/alexvorobiev/clush-mode
;; Package-Requires: ((emacs "24.1") (s "1.10.0") (dash "2.11.0"))
;; Version: 1.1.0
;; Keywords: comint ssh shell clush clustershell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Comint mode for `clush'.
;;
;; Documentation: https://github.com/alexvorobiev/clush-mode

;;; Code:

(require 'dash)
(require 's)

(defgroup clush nil
  "Comint mode for `clush' (cluster shell)"
  :prefix "clush-"
  :group 'processes
  :group 'unix)

(defcustom clush-program-name "clush"
  "*Name of `clush' executable."
  :type 'string
  :require 'clush)

(defvar clush-arguments
  ;; Let emacs do the shell syntax highlighting
  '("--color=never"
    ;; No ssh noise
    "--quiet"
    ;; Merge identical output from multiple servers
    "-b"
    ;; Suppress the ssh banner among other things
    "--options=-oLogLevel=Error")
  "Command line arguments to pass to `clush'.")

(defvar clush-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `clush'.")

(defvar clush-prompt-regexp "clush> "
  "Prompt for `clush'.")

(defun clush (where &optional user)
  "Run an inferior instance of `clush' inside Emacs.
WHERE should specify the target nodes in a form `clush'
understands e.g. `-w host' or `-g group'. If USER is passed,
`clush' will be called under `sudo'."
  (interactive
   (list (read-string "Where: " nil 'clush-hist-where)
         (if current-prefix-arg (read-string "User: " nil 'clush-hist-user "root") nil)))

  (let* ((clush-program (if user
                            "sudo"
                          clush-program-name))
         (no-root-args (cons where clush-arguments))
         (args (if user
                   (-flatten (list "-u" user clush-program-name no-root-args))
                 no-root-args))
         (clush-buffer-name (concat "*clush " where "*"))
         (buffer (comint-check-proc clush-buffer-name))
         (clush-buffer
          (if (or buffer (not (derived-mode-p 'clush-mode))
                  (comint-check-proc (current-buffer)))
              (get-buffer-create (or buffer clush-buffer-name))
            (current-buffer))))
    (message (s-join " " args))
    ;; pop to the "*clush...*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window clush-buffer)
    ;; create the comint process if there is no buffer.
    (apply 'make-comint-in-buffer "" clush-buffer
           clush-program nil args)
      (clush-mode)))

(defun clush--initialize ()
  "Helper function to initialize Clush."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode clush-mode comint-mode "Clush"
  "Major mode for `clush'.

\\<clush-mode-map>"
  nil "Clush"
  
  (setq comint-prompt-regexp clush-prompt-regexp)
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-start) clush-prompt-regexp)
  ;; From shell mode
  (shell-completion-vars)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(shell-font-lock-keywords t)))


(add-hook 'clush-mode-hook 'clush--initialize)

(provide 'clush-mode)

;;; clush-mode.el ends here
