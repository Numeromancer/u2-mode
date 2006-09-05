;; u2-dired.el -- Copyright (C) 2005 Timothy M. Schaeffer <tschaef@sbcglobal.net>
;;
;; Major mode for working with UniBasic files in EMACS.
;;
;; Author: Tim Schaeffer <numeromancer@users.sourceforge.net>
;; Version: 1.10   Time-stamp: <2005-12-03 14:18:36 timothy>
;; Maintainer: Timothy M. Schaeffer <tschaef@sbcglobal.net>
;; Keywords: languages database telnet
;;
;; ----------------------------------------------------------------------
;;
;; Copyright (C) 2005 Timothy M. Schaeffer <tschaef@sbcglobal.net>
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program  is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file `Copying'.  If not, write to the Free 
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; ----------------------------------------------------------------------
;;
;;; COMMENTARY:
;;  -----------
;;
;;  Functions for displaying a list of tables from a U2 VOC file. Meant
;;  to be similar to dired, but with functions for manipulating U2
;;  tables.
;; 
;;  Very immature.
;;
;;; KNOWN BUGS:
;;  -----------
;;  After logging in and getting to Unidata, we still see the "wait"
;;  cursor until I press Enter.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'unidata)
(require 'cookie)

(defconst u2-dired
  "@(#)$Id$"
  "RCS version info for `unidata-mode'.")

(defmacro setmax (max maybe)
  `(if (> ,maybe ,max)
       (setq ,max ,maybe)
     ,max))

(defun chomp (str)
  (or (and (string-match "\\(.*?\\)\n" str)
           (match-string 1 str))
      str))


(eval-when-compile 
  (defmacro def-local-var (varname value &optional doc)
    `(progn
       (defvar ,varname ,value ,doc)
       (make-variable-buffer-local ',varname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unidata interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unidata-parse-file-list (buf)
  (let (f-list p)
    (save-excursion
      (set-buffer buf)
      (beginning-of-buffer)
      (setq p (point))
      (if (looking-at unidata-list-command)
          (progn
            (forward-line)
            (delete-region p (point))))
      (re-search-forward "\\s-*:\\s-*\\'")
      (replace-match "\n")
      (setq f-list (unidata-parse-listf-output buf)))))


(defun unidata-parse-listf-output (buffer)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let (out-list entry)
      (while (setq entry (unidata-get-next-entry buffer))
        (setq out-list (cons entry out-list)))
      (nreverse out-list))))

(defun unidata-get-next-entry (buffer)
  "Reads following lines in a buffer and makes a list for browsing mode.
When done, point is ready for the next call to this function."
  (let (field-list)
    (beginning-of-line)
    (while (and (looking-at "[ \t]*$") (not (looking-at "\\s-*\\'")))
      (forward-line))
    (while (not (looking-at "[ \t]*$"))
      (setq field-list (cons (chomp (thing-at-point 'line)) field-list))
      (forward-line))
    (nreverse field-list)))


(defcustom unidata-filelist-done-regexp "^[0-9]+ records? listed.*"
  "Regular expression which tells unidata's file list mode that the file list command
is finished."
  :group 'unidata
  :type 'regexp)

(defcustom unidata-list-command "LIST VOC F1 F2 F3 WITH F1 LIKE 'F...' OR F1 LIKE 'f...' OR F1 LIKE 'DIR...' OR F1 LIKE 'dir...' OR F1 LIKE 'LF...' OR F1 LIKE 'lf...' OR F1 LIKE 'LD...' OR F1 LIKE 'ld...' BY F1 BY @ID HDR.SUPP COL.HDR.SUPP VERTICAL"
  "Command used by the Unidata File listing redirector.  If you change
this, you may also need to change `unidata-filelist-done-regexp'"
  :group 'unidata
  :type 'string)

(defun unidata-redirect-listf ()
  (let ((buf-name (concat "*Unidata-list:" unidata-account-path "*")))
    (unidata-redirect
     unidata-list-command
     buf-name
     unidata-process unidata-filelist-done-regexp)
;;    (unidata-parse-file-list (get-buffer buf-name))
))


(defun unidata-send-output-to-buf (proc cmd buf-name)
  (let ((obuf (process-buffer proc))
        (ofilt (process-filter proc))
        (buf (get-buffer-create buf-name)))
    (save-excursion
      (unwind-protect
          (progn
            (set-process-filter proc nil)
            (set-process-buffer proc buf)
            (unidata-send proc cmd)
            (accept-process-output proc))
        (progn
          (set-process-filter proc ofilt)
          (set-process-buffer proc obuf))))))
  

(defvar u2-dired-list nil)
(defvar u2-dired-cookies nil)


;; TODO: use create a search for unidata buffers by account name.
(defun u2-dired-get-unidata-buffer ()
  (nth 0 unidata-buffer-list))

;; TODO: unidata-account-path is now buffer local.  Set it as a buffer
;; local variable and copy it from the unidata bufffer in the dired
;; buffer when that buffer is created.
(defun u2-dired-create-display-buffer (list-buf)
  (let ((buf-name (concat "*U2-list(" unidata-account-path ")*")))
    (setq u2-dired-list (unidata-parse-file-list list-buf))
    (setq u2-dired-cookies (collection-create buf-name 'u2-dired-cookie-display-function))
    (collection-append-cookies u2-dired-cookies u2-dired-list)
    (set-window-buffer (get-buffer-window list-buf) buf-name)))
                             

(defun u2-dired-pad-string (str len)
  "Pad STR to LEN with spaces if necessary.  If STR is longer thatn
LEN, truncate it, and add `...' at the end, so that its length becomes
exactly LEN.  This is for display in the u2-dired buffer. STR is not
modified---the resulting string is returned."
  (if (> (length str) len)
      (concat (substring str 0 (- len 3)) "...")
    (concat str (make-string (- len (length str)) ? ))))

;; A collection of VOC entries representing files are read from LIST
;; output into a list of entries, which are then put into a cookie
;; list (from elib) and displayed in a buffer. Each entry is of the
;; form (type voc-name file-name dict-file-name).

(defun u2-dired-cookie-display-function (cookie)
  (let ((lengths '(20 5 30 30))
        (result "") (full-str ""))
    (dotimes (i 4)
      (progn
        (setq result (concat result
                             (u2-dired-pad-string
                              (nth i cookie) (nth i lengths))
                             "  "))
        (setq full-str (concat full-str (nth i cookie) "\n"))))
    ;; add help-echo property
    (setq result (propertize result 'help-echo full-str))
    (insert result)))

;;; ------------------------------------------------------------
;;; u2-dired Mode Setup
;;; ------------------------------------------------------------

(defvar u2-dired-mode-syntax-table nil
  "Syntax table for Unidata TCL mode.")
(if unidata-mode-syntax-table
    ()
  (setq unidata-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?. "_   " unidata-mode-syntax-table)
  (modify-syntax-entry ?/ "w   " unidata-mode-syntax-table)
  (modify-syntax-entry ?\  "    " unidata-mode-syntax-table))


(defvar u2-dired-mode-abbrev-table nil
  "Abbrev table used while in text mode.")

(define-abbrev-table 'u2-dired-mode-abbrev-table ())

(defvar u2-dired-mode-map nil
  "Keymap for Unidata TCL mode.")

(defun u2-dired-toggle-auto-upcase ()
  (interactive)
  (setq u2-dired-auto-upcase-commands (not u2-dired-auto-upcase-commands)))

(defvar u2-dired-menu (make-sparse-keymap "Unidata"))

(if u2-dired-mode-map
    ()
  (progn
    (setq u2-dired-mode-map (copy-keymap telnet-mode-map))
    (setq u2-dired-menu-map (make-sparse-keymap "Unidata"))
    (define-key u2-dired-mode-map [menu-bar insert]
      (cons "Unidata" u2-dired-menu-map))
    (define-key u2-dired-menu-map [u2-dired-auto-upcase]
      '(menu-item "Auto-Upcase commands"  u2-dired-toggle-auto-upcase
                  :enabled t
                  :visible t
                  :button (:toggle 
                           . (and (boundp u2-dired-auto-upcase-commands)
                                  u2-dired-auto-upcase-commands))))
    (define-key u2-dired-menu-map [u2-dired-view-in-dired]
      '(menu-item "View Account in Dired" u2-dired-view-account-in-dired))
    ))


(define-derived-mode u2-dired-mode telnet-mode "Unidata"
  "Major mode for Unidata TCL command line.
\\{u2-dired-mode-map}"
  (if u2-dired-account-path
      (setq mode-name (concat mode-name "(" unidata-account-path ")"))))

;; TODO: save the user name for constructing remote names for 
;; retrieving files
(defun u2-dired (&optional host)
  "Open a TCL command line in Unidata."
  (interactive) ;;  "sOpen connection to host:")
  (let (path)
    (and host (setq unidata-host host))
    (setq path (unidata-get-account-path host))
    (if (unidata-remote-path-p path)
        (let ((pthlist (unidata-split-host-path path)))
          (setq unidata-account-path (nth 2 pthlist))
          (setq unidata-host (nth 0 pthlist))
          (setq host (nth 0 pthlist))
          (unidata-set-user host (nth 1 pthlist))
          (unidata-set-account-path host unidata-account-path)))
    (if (not (nilstring host))
        (unidata-open-remote-connection host)
      (unidata-open-local-connection))
    (erase-buffer)
    (unidata-mode)))
        
(defun unidata-view-account-in-dired (&optional host)
  (interactive)
  (setq host (or host unidata-host))
  (if (nilstring host)
      (find-file-other-window unidata-account-path)
    (find-file-other-window (unidata-build-host-path host))))


(provide 'unidata)


;; Local variables:
;;   mode: emacs-lisp
;;   auto-save-interval: 1000
;;   indent-tabs-mode: nil
;; End:
