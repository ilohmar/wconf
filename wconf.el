;;; wconf.el --- Minimal window layout manager   -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Ingo Lohmar <i.lohmar@gmail.com>
;; URL: https://github.com/ilohmar/wconf
;; Version: 0.1
;; Keywords: windows, frames, layout
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the file README.org

;;; Code:

(defgroup wconf nil
  "Easily use several window configurations."
  :group 'convenience)

(defcustom wc-change-config-function #'wc-change-config-default
  "Function called with current config whenever it is set."
  :group 'wconf)

(defcustom wc-file (expand-file-name "window-configs.el" user-emacs-directory)
  "File used to save and load window configurations."
  :group 'wconf)

(defcustom wc-fallback-buffer-name "*scratch*"
  "Name of the buffer to substitute for buffers which are not available."
  :group 'wconf)

(defcustom wc-no-configs-string "-----"
  "String to use if there are no configurations at all."
  :group 'wconf)

(defcustom wc-no-config-name "---"
  "String to use for the empty window configuration."
  :group 'wconf)

;; internal variables and helper functions

(defvar wc--configs nil
  "List of configurations; each item a list (active stored name).")

(defvar wc--index nil
  "Index of currently shown configuration.  After clean and load
this can be nil although wc--configs is not empty.")

(defvar wc-string nil
  "String representing information on the current configuration.")

(require 'cl-lib)

(defsubst wc--ensure-configs (&optional current)
  (unless wc--configs
    (error "wc: No window configurations"))
  (when (and current (not wc--index))
    (error "wc: No window configuration is currently used")))

(defsubst wc--ensure-index (&optional index)
  (unless (<= 0 index (1- (length wc--configs)))
    (error "wc: No window configuration index %s" index)))

(defun wc--current-config ()
  (window-state-get (frame-root-window (selected-frame))
                    'writable))

(defun wc- (index)
  (nth index wc--configs))

(defun wc--to-string (index)
  (if index
      (format "%s:%s"
              (number-to-string index)
              (cl-caddr (wc- index)))
    (concat "-:" wc-no-config-name)))

(defun wc--update-info ()
  (when (functionp wc-change-config-function)
    (funcall wc-change-config-function
             ;; both will be nil if no list
             wc--index
             (and wc--index
                  (car (wc- wc--index))))))

(defun wc--update-active-config ()
  (when wc--index
    (setf (car (wc- wc--index)) (wc--current-config))))

(defun wc--use-config (index)
  (setq wc--index index)
  (window-state-put (car (wc- wc--index))
                    (frame-root-window (selected-frame))
                    'safe)
  (wc--update-info))

(defun wc--reset ()
  "Remove all configurations."
  (setq wc--configs nil)
  (setq wc--index nil)
  (wc--update-info))

(defun wc--copy (wc)
  "Return a deep copy of WC, using `copy-tree'."
  (copy-tree wc t))

;; global stuff

(defun wc-change-config-default (index config)
  "Update `wc-string' to represent configuration CONFIG at
position INDEX."
  (setq wc-string (if wc--configs
                      (wc--to-string index)
                    wc-no-configs-string))
  (force-mode-line-update))

(defun wc-save (&optional filename)
  "Save stored configurations in FILENAME, defaults to `wc-file'."
  (interactive "F")
  (let ((filename (or filename wc-file)))
    (with-temp-file filename
      (prin1 (mapcar #'cdr wc--configs) ;-> (wc name)
             (current-buffer)))
    (message "wc: Save stored configurations in %s" filename)))

(defun wc--sanitize-buffer (b)
  (unless (get-buffer (cadr b))
    (setf (cadr b) wc-fallback-buffer-name
          (cdr (assoc 'start b)) 1
          (cdr (assoc 'point b)) 1
          (cdr (assoc 'dedicated b)) nil)))

(defun wc--sanitize-window-tree (node)
  (let ((buf (assoc 'buffer node)))
    (if buf                             ;in a leaf already
        (wc--sanitize-buffer buf)
      (mapc (lambda (x)
              (when (and (consp x)
                         (memq (car x) '(leaf vc hc)))
                (wc--sanitize-window-tree (cdr x))))
            node))))

;;;###autoload
(defun wc-load (&optional filename)
  "Load stored configurations from FILENAME, defaults to `wc-file'."
  (interactive "f")
  (let ((filename (or filename wc-file)))
    (unless (file-readable-p filename)
      (error "wc: Cannot read file %s" filename))
    (wc--reset)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (setq wc--configs
            (mapcar
             (lambda (f)                ;(wc name)
               (wc--sanitize-window-tree (car f))
               (cons (wc--copy (car f)) f))
             (read (current-buffer)))))
    (message "wc: Load stored configurations from %s" filename))
  (wc--update-info))

;; these functions affect the whole list of configs

;;;###autoload
(defun wc-create (&optional new)
  "Clone the current configuration or create a new \"empty\"
one.  The new configuration is appended to the list and becomes active.

With optional prefix argument NEW, or if there are no
configurations yet, create a new configuration from the current
window config."
  (interactive "P")
  (wc--update-active-config)
  (setq wc--configs
        (append wc--configs
                (list
                 (if (or new (not wc--configs))
                     (progn
                       (message "wc: Created new configuration %s"
                                (length wc--configs))
                       (list (wc--current-config)
                             (wc--current-config)
                             "new"))
                   (wc--ensure-configs 'current)
                   (let ((wc (wc- wc--index)))
                     (message "wc: Cloned configuration %s"
                              (wc--to-string wc--index))
                     (list (wc--copy (car wc))
                           (wc--copy (cadr wc))
                           (cl-caddr wc)))))))
  (wc--use-config (1- (length wc--configs))))

(defun wc-kill ()
  "Kill current configuration."
  (interactive)
  (wc--ensure-configs 'current)
  (let ((old-string (wc--to-string wc--index)))
    (setq wc--configs
          (append (butlast wc--configs (- (length wc--configs) wc--index))
                  (last wc--configs (- (length wc--configs) wc--index 1))))
    (if wc--configs
        (wc--use-config (if (< (1- (length wc--configs)) wc--index)
                            (1- wc--index)
                          wc--index))
      (wc--reset)
      (wc--update-info))
    (message "wc: Killed configuration %s" old-string)))

(defun wc-swap (i j)
  "Swap configurations at positions I and J."
  (interactive
   (progn
     (wc--ensure-configs 'current)      ;for interactive, we want current config
     (list
      wc--index
      (read-number "Swap current config with index: "))))
  (wc--ensure-configs)
  (wc--ensure-index i)
  (wc--ensure-index j)
  (wc--update-active-config)
  (let ((wc (wc- i)))
    (setf (nth i wc--configs) (wc- j))
    (setf (nth j wc--configs) wc))
  (when (memq wc--index (list i j))
    (wc--use-config wc--index))
  (message "wc: Swapped configurations %s and %s"
           (number-to-string i) (number-to-string j)))

;; manipulate single config

(defun wc-rename (name)
  "Rename current configuration to NAME."
  (interactive
   (progn
     (wc--ensure-configs 'current)
     (list
      (read-string "New window configuration name: "
                   (cl-caddr (wc- wc--index))))))
  (wc--ensure-configs 'current)
  (setf (cl-caddr (wc- wc--index)) name)
  (message "wc: Renamed configuration to \"%s\"" name)
  (wc--update-info))

;; interaction b/w stored and active configs

;; these commands only make sense when there are wc--configs, and after
;; wc--index has become non-nil

(defsubst wc--store (wc)
  (setf (cadr wc) (wc--copy (car wc))))

(defsubst wc--restore (wc)
  (setf (car wc) (wc--copy (cadr wc))))

(defun wc-store ()
  "Store currently active configuration."
  (interactive)
  (wc--ensure-configs 'current)
  (wc--update-active-config)
  (wc--store (wc- wc--index))
  (message "wc: Stored configuration %s" (wc--to-string wc--index)))

(defun wc-store-all ()
  "Store all active configurations."
  (interactive)
  (wc--ensure-configs 'current)
  (wc--update-active-config)
  (mapc #'wc--store wc--configs)
  (message "wc: Stored all configurations"))

(defun wc-restore ()
  "Restore stored configuration."
  (interactive)
  (wc--ensure-configs 'current)
  (wc--restore (wc- wc--index))
  (wc--use-config wc--index)
  (message "wc: Restored configuration %s" (wc--to-string wc--index)))

(defun wc-restore-all ()
  "Restore all stored configurations."
  (interactive)
  (wc--ensure-configs 'current)
  (mapc #'wc--restore wc--configs)
  (wc--use-config wc--index)
  (message "wc: Restored all configurations"))

;; change config

(defun wc-switch-to-config (index &optional force)
  "Change to current config INDEX."
  (interactive "P")
  (wc--ensure-configs)
  (let ((index (or index
                   (read-number "Switch to config number: "))))
    (wc--ensure-index index)
    ;; remember active config (w/o name etc)
    (wc--update-active-config)
    ;; maybe use new configuration
    (if (and (eq wc--index index)
             (not force))
        (message "wc: Nothing to do")
      (wc--use-config index)
      (message "wc: Switched to configuration %s" (wc--to-string index)))))

(defun wc-use-previous ()
  "Switch to previous window configuration."
  (interactive)
  (wc--ensure-configs)
  (wc-switch-to-config (mod (1- (or wc--index 1)) (length wc--configs))))

(defun wc-use-next ()
  "Switch to next window configuration."
  (interactive)
  (wc--ensure-configs)
  (wc-switch-to-config (mod (1+ (or wc--index -1)) (length wc--configs))))

(provide 'wconf)
;;; wconf.el ends here
