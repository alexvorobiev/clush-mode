;;; clush-mode.el -- Emacs mode for interactive clush sessions. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Alex Vorobiev

;; Author: Alex Vorobiev <alexander.vorobiev@gmail.com>
;; URL: https://github.com/alexvorobiev/clush-mode
;; Package-Requires: ((emacs "24.1") (s "1.10.0") (dash "2.11.0"))
;; Version: 1.0.0
;; Keywords: comint clush clustershell ssh shell

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
;; See documentation on https://github.com/alexvorobiev/clush-mode

;;; Code:

(require 'dash)
(require 's)

(defvar clush-file-path "/usr/bin/clush"
  "Path to the program used by `clush'")

;; This works for clush 1.16
(defvar clush-arguments
  ;; Let emacs do the shell syntax highlighting
  '("--color=never"
    ;; No ssh noise
    "--quiet"
    ;; Merge identical output from multiple servers
    "-b"
    ;; Suppress the ssh banner
    "--options=-oLogLevel=Error")
  "Command line arguments to pass to `clush'")

;; todo: clush 1.17 has different options 
;(defvar clush-arguments '("--option=\"color=never\"" " --quiet" " -b")


(defvar clush-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `clush'")

(defvar clush-prompt-regexp "clush> "
  "Prompt for `clush'.")

(defun clush (prefix where)
  "Run an inferior instance of `clush' inside Emacs."
  (interactive "P\nsWhere: ")
  (let* ((clush-program (if prefix
                            "sudo"
                          clush-file-path))
         (no-root-args (cons where clush-arguments))
         (args (if prefix
                   (-flatten (list "-u" "root" clush-file-path no-root-args))
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
  "Helper function to initialize Clush"
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
