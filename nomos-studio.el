;;; nomos-studio.el --- Emacs integration for nomos-studio  -*- lexical-binding: t; -*-
; SPDX-FileCopyrightText: 2025-2026 nomos-studio contributors
; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2026 Thomas Rodgers

;; Author: Thomas Rodgers <rodgert@twrodgers.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cider "1.8"))
;; Keywords: music, live-coding, midi
;; URL: https://github.com/nomos-studio/nomos-studio.el

;;; Commentary:

;; Primary Emacs surface for nomos-studio.
;;
;; Entry points:
;;
;;   M-x nomos-studio-jack-in      Connect CIDER to the nous nREPL.
;;   M-x nomos-studio-ctrl-tree    Open the live ctrl-tree inspector buffer.
;;   M-x nomos-studio-read-path    Read a ctrl-tree path interactively.
;;   M-x nomos-studio-write-path   Write a value to a ctrl-tree path.
;;
;; Quick start:
;;
;;   1. Start the nous nREPL (from src/nous):
;;        lein repl :headless :host localhost :port 7888
;;
;;   2. In Emacs:
;;        M-x nomos-studio-jack-in
;;        M-x nomos-studio-ctrl-tree
;;
;;   3. Evaluate from a CIDER buffer:
;;        (ctrl/set! [:some :path] :value)
;;      The inspector buffer updates on the next poll.
;;
;; The Emacs surface and the browser surface (Phoenix LiveView) are both
;; clients of the same ctrl-tree.  If Emacs can write to the tree, it can
;; drive anything the browser can drive.

;;; Code:

(require 'cider)

;;; ── Customisation ────────────────────────────────────────────────────────

(defgroup nomos-studio nil
  "Emacs integration for nomos-studio."
  :group 'tools
  :prefix "nomos-studio-")

(defcustom nomos-studio-repl-host "localhost"
  "Host where the nous nREPL is running."
  :type 'string
  :group 'nomos-studio)

(defcustom nomos-studio-repl-port 7888
  "Port for the nous nREPL.
Must match the port passed to `lein repl :headless' or `nous.core/start!'."
  :type 'integer
  :group 'nomos-studio)

(defcustom nomos-studio-ctrl-tree-poll-interval 2
  "Seconds between automatic ctrl-tree inspector refreshes.
Set to nil to disable automatic refresh."
  :type '(choice (number :tag "Seconds") (const :tag "Disabled" nil))
  :group 'nomos-studio)

;;; ── CIDER connection ─────────────────────────────────────────────────────

;;;###autoload
(defun nomos-studio-jack-in ()
  "Connect CIDER to the running nous nREPL.

Ensures JAVA_HOME is set before connecting.  The nREPL must already be
running; start it with:

  cd src/nous && lein repl :headless :host localhost :port 7888

Connection details are controlled by `nomos-studio-repl-host' and
`nomos-studio-repl-port'."
  (interactive)
  (unless (getenv "JAVA_HOME")
    (setenv "JAVA_HOME"
            "/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home"))
  (message "nomos-studio: connecting to nous at %s:%d ..."
           nomos-studio-repl-host nomos-studio-repl-port)
  (cider-connect-clj `(:host ,nomos-studio-repl-host
                        :port ,nomos-studio-repl-port)))

;;; ── nREPL eval helper ────────────────────────────────────────────────────

(defun nomos-studio--eval-sync (code)
  "Evaluate CODE via the active CIDER connection.
Returns the value string (as Clojure pr-str) or nil if disconnected or
if the eval produced an error."
  (unless (cider-connected-p)
    (user-error "nomos-studio: not connected — M-x nomos-studio-jack-in"))
  (let* ((response (cider-nrepl-sync-request:eval code))
         (value    (nrepl-dict-get response "value"))
         (err      (nrepl-dict-get response "err")))
    (when err
      (message "nomos-studio eval error: %s" (string-trim err)))
    value))

;;; ── Ctrl-tree inspector ──────────────────────────────────────────────────

(defconst nomos-studio--ctrl-tree-buffer "*nomos-studio:ctrl-tree*")

(defvar-local nomos-studio--poll-timer nil
  "Periodic refresh timer, local to the inspector buffer.")

;; Returns a Clojure vector of [path-str value-str type-name] triples,
;; sorted by path.  Using pr-str on the Clojure side keeps formatting
;; exact; the Elisp side reads the result as an Elisp vector of vectors.
(defconst nomos-studio--all-nodes-expr
  "(do (require 'nous.ctrl)
       (vec
         (map (fn [{:keys [path value type]}]
                [(pr-str path) (pr-str value) (name type)])
              (sort-by #(pr-str (:path %))
                       (nous.ctrl/all-nodes)))))"
  "Clojure expression returning ctrl-tree nodes as [[path value type] ...].")

(defun nomos-studio--render-nodes (nodes-vec)
  "Format NODES-VEC (Elisp vector of [path val type] vectors) into a string."
  (if (= 0 (length nodes-vec))
      ";; ctrl-tree is empty — try (ctrl/set! [:some :path] :value)\n"
    (concat
     (mapconcat
      (lambda (node)
        (format "%-45s  %-25s  %s"
                (aref node 0)
                (aref node 1)
                (aref node 2)))
      nodes-vec
      "\n")
     "\n")))

(defun nomos-studio--refresh-ctrl-tree ()
  "Refresh the ctrl-tree inspector buffer."
  (let ((buf (get-buffer nomos-studio--ctrl-tree-buffer)))
    (when (and buf (buffer-live-p buf))
      (condition-case err
          (let* ((result (nomos-studio--eval-sync nomos-studio--all-nodes-expr))
                 (nodes  (when result (read result)))
                 (text   (if nodes
                             (nomos-studio--render-nodes nodes)
                           ";; ctrl-tree read failed\n")))
            (with-current-buffer buf
              (let ((inhibit-read-only t)
                    (saved-point (point)))
                (erase-buffer)
                (insert text)
                (goto-char (min saved-point (point-max))))))
        (user-error
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert ";; not connected — M-x nomos-studio-jack-in\n"))))))))

;;; ── Inspector mode ───────────────────────────────────────────────────────

(defun nomos-studio-ctrl-tree-refresh ()
  "Manually refresh the ctrl-tree inspector buffer."
  (interactive)
  (nomos-studio--refresh-ctrl-tree))

(defun nomos-studio-ctrl-tree-write-at-point ()
  "Write a new value to the ctrl-tree path on the current line.
Prompts for the new value in the minibuffer."
  (interactive)
  (let* ((line  (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))
         (parts (split-string line "[[:space:]][[:space:]]+" t))
         (path  (car parts)))
    (if (or (null path) (not (string-match-p "^\\[" path)))
        (message "nomos-studio: no ctrl-tree path on this line")
      (let ((value (read-string (format "Set %s to: " path))))
        (nomos-studio-write-path path value)
        (nomos-studio--refresh-ctrl-tree)))))

(defvar nomos-studio-ctrl-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'nomos-studio-ctrl-tree-refresh)
    (define-key map (kbd "RET") #'nomos-studio-ctrl-tree-write-at-point)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `nomos-studio-ctrl-tree-mode'.")

(define-derived-mode nomos-studio-ctrl-tree-mode special-mode "nomos:ctrl-tree"
  "Major mode for the nomos-studio ctrl-tree inspector.

Displays all ctrl-tree nodes with their current values, auto-refreshing
every `nomos-studio-ctrl-tree-poll-interval' seconds.

Key bindings:
  g      Refresh the tree now.
  RET    Write a new value to the path on the current line.
  q      Close the inspector buffer."
  (setq-local buffer-read-only t
              truncate-lines    t)
  (when nomos-studio-ctrl-tree-poll-interval
    (when nomos-studio--poll-timer
      (cancel-timer nomos-studio--poll-timer))
    (setq nomos-studio--poll-timer
          (run-with-timer nomos-studio-ctrl-tree-poll-interval
                          nomos-studio-ctrl-tree-poll-interval
                          #'nomos-studio--refresh-ctrl-tree))))

;;;###autoload
(defun nomos-studio-ctrl-tree ()
  "Open the nomos-studio ctrl-tree inspector buffer.

Shows all ctrl-tree nodes as PATH  VALUE  TYPE, refreshing every
`nomos-studio-ctrl-tree-poll-interval' seconds.

Requires an active CIDER connection; call `nomos-studio-jack-in' first."
  (interactive)
  (let ((buf (get-buffer-create nomos-studio--ctrl-tree-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'nomos-studio-ctrl-tree-mode)
        (nomos-studio-ctrl-tree-mode)))
    (pop-to-buffer buf)
    (nomos-studio--refresh-ctrl-tree)))

;;; ── Path read / write ────────────────────────────────────────────────────

;;;###autoload
(defun nomos-studio-read-path (path)
  "Read and display the current value of ctrl-tree PATH.

PATH is a Clojure vector string, e.g. \"[:filter/cutoff]\".
Returns the value as an Elisp string (Clojure pr-str representation)."
  (interactive "sCtrl-tree path: ")
  (let ((result (nomos-studio--eval-sync
                 (format "(do (require 'nous.ctrl) (pr-str (nous.ctrl/get %s)))"
                         path))))
    (when result
      (let ((val (read result)))
        (message "ctrl-tree %s => %s" path val)
        val))))

;;;###autoload
(defun nomos-studio-write-path (path value)
  "Write VALUE to ctrl-tree PATH.

PATH is a Clojure vector string, e.g. \"[:filter/cutoff]\".
VALUE is a Clojure value string, e.g. \"0.7\" or \":C-major\"."
  (interactive "sCtrl-tree path: \nsValue: ")
  (nomos-studio--eval-sync
   (format "(do (require 'nous.ctrl) (nous.ctrl/set! %s %s))" path value))
  (message "ctrl-tree %s <= %s" path value))

;;; ── Footer ───────────────────────────────────────────────────────────────

(provide 'nomos-studio)

;;; nomos-studio.el ends here
