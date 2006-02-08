;; u2-compile.el -- Copyright (C) 2005 Timothy M. Schaeffer <tschaef@sbcglobal.net>
;;
;; Functions for compiling UniBasic source files from EMACS.
;;
;; Author: Timothy M. Schaeffer <tschaef@sbcglobal.net>
;; Version: 1.0  
;; Time-stamp: <2005-11-16 19:21:48 timothy>
;; Maintainer: Timothy M. Schaeffer <tschaef@sbcglobal.net>
;; Keywords: languages
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
;;  Provides commands for compiling UniBasic programs through a
;;  UniData or universe session.  Adapted from Pat Thoyt's Unibasic
;;  mode.
;;
;;; INSTALLATION
;;  ------------
;;
;;
;;; SEE ALSO:
;;  --------
;;  proc-mode - a major mode for editing Pick style PROC buffers.
;;
;;; KNOWN BUGS:
;;  -----------
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'unidata)
(require 'unibasic)

(defcustom unibasic-basic-cmd-string "BASIC"
  "Unidata command used to compile UniBasic source.
If you have written another program or paragraph or proc to handle
compiling Unibasic programs (while doing other things, like running the
source through `m4' or some other macro processor first), you can put
the name of that command here."
  :type 'string
  :group 'unidata)


(defvar unibasic-compile-options-alist
  '(("-D" "Generate X-Reference for debugger")
    ("-G" "Enable profiling")
    ("-LIST" "Generate a list of the program")
    ("-XREF" "Generate a cross reference table")
    ("-Z2" "Enable debugging")
    ("-I" "Keywords are case insensitive")))


(defcustom unibasic-basic-arg-list nil
  "A list of strings of command line arguments for compiling.
The list will be appended as a space separated list of arguments to the end of
the compile command line in ECL.  Usually for specifying debugging arguments to
the compiler, since Unibasic provides little else."
  :group 'unidata
  :type '(repeat (string)))


;; TODO: get rid of this.
(defcustom unibasic-default-source-table "BP"
  "Unidata table to send to the compile command if there is no other."
  :type 'string
  :group 'unidata)
(defcustom unibasic-catalog-command "CATALOG"
  "Unidata command verb used to catalog programs.
If you have written some other programming or paragraph to catalog
Unibasic programs and subroutines, you can put the name of that
command here, and it will be used instead of the default `CATALOG'"
  :type 'string
  :group 'unidata)

(defcustom unibasic-run-command "RUN"
  "Unidata command verb used to catalog programs.
If you have written some other programming or paragraph to run
Unibasic programs and subroutines, you can put the name of that
command here, and it will be used instead of the default `RUN'"
  :type 'string
  :group 'unidata)

;; version checking (from XEmacs sample .emacs file).
(if (and (not (boundp 'emacs-major-version))
         (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
          (string-to-int (substring emacs-version
                                    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
         (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
          (string-to-int (substring emacs-version
                                    (match-beginning 1) (match-end 1)))))

(defun unibasic-emacs-version-or-newer (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
           (>= emacs-minor-version minor))))

(when unibasic-mode-map
    (define-key unibasic-mode-map "\C-cc"    'unibasic-compile)
    (define-key unibasic-mode-map "\C-ct"    'unibasic-catalog)
    (define-key unibasic-mode-map "\C-cr"    'unibasic-run)
    (define-key-after unibasic-menu-map [ub-catlg]
      '("Catalog this program" . unibasic-catalog) t)
    (define-key-after unibasic-menu-map [ub-cmp]
      '("Compile this file" . unibasic-compile) t)
    (define-key-after unibasic-menu-map [ub-run]
      '("Run this program" . unibasic-run) t)

    (setq unibasic-catalog-menu-map (make-sparse-keymap "Unibasic Cataloging"))
    (define-key unibasic-menu-map [catalog-menu]
      (cons "Unibasic Cataloging" unibasic-catalog-menu-map))

    (define-key unibasic-catalog-menu-map [unidata-catalog-global]
      '(menu-item "Catalog Globally" (lambda () (setq unibasic-catalog-method 'global))))
    (define-key unibasic-catalog-menu-map [unidata-catalog-local]
      '(menu-item "Catalog Locally" (lambda () (setq unibasic-catalog-method 'local))))
    (define-key unibasic-catalog-menu-map [unidata-catalog-direct]
      '(menu-item "Catalog Directly" (lambda () (setq unibasic-catalog-method 'direct))))
    
    )



;;; Compilation and catalog commands
(defvar unibasic-basic-cmd-string "BASIC")

(defun unibasic-default-source-table (file)
  "BP")

(defvar unibasic-source-table nil)

(defun unibasic-get-source-table (file-name)
  (cond ((string-match ".*/\\([^/]+\\)/[^/]*\\'" file-name)
         (match-string 1 file-name))
        (unibasic-source-table
         unibasic-source-table)
        (t unibasic-default-source-table)))

(defun unibasic-compile-command (file)
  (mapconcat 'identity
             (append (list unibasic-basic-cmd-string
                           (unibasic-get-source-table file)
                           (file-name-nondirectory file))
                     unibasic-basic-arg-list)
             " "))


(defun unibasic-make-run-command (file)
  (mapconcat 'identity
             (list unibasic-run-command
                   (unibasic-get-source-table file)
                   (file-name-nondirectory file)
                   "\n")
             " "))

(defun unibasic-run-file (ud-proc file &optional options-string)
  (unidata-send-command
   ud-proc
   unibasic-run-command
   (unibasic-get-source-table file)
   (file-name-nondirectory file)
   options-string))

(defun unibasic-compile-file (ud-proc file &optional catalog-p)
  (unidata-send-command
   ud-proc
   (unibasic-compile-command file)))

(defcustom unibasic-catalog-method 'local
  "Method of cataloguing when cataloguing a UniBasic program.
This variable can be the symbols `local', `direct', or `global', with
global being the default used when the variable not one of these
three.  Setting this will cause the appropriate option to be sent to
all Catalog commands."
  :type '(choice (const local) (const global) (const direct))
  :group 'unibasic)

(defun unibasic-catalog-file (ud-proc file)
  "Sends (with FORCE) command to catalog the program associated with FILE
on the Unidata server."
  (unidata-send-command
   ud-proc
   unibasic-catalog-command
   (unibasic-get-source-table file)
   (file-name-nondirectory file)
   (cond ((equal unibasic-catalog-method 'local) "LOCAL")
         ((equal unibasic-catalog-method 'direct) "DIRECT")
         (t ""))
 "FORCE"
   ))


;; TODO: Create a function which will get the directory, or parent
;; directory of the unibasic source file and find a unidata process
;; with that directory as it's current account.  This is necessary
;; because there could be more than one unidata process running,
;; and/or unidata accounts may have performed a LOGTO.  unidata.el
;; should contain a function to search for a unidata process by
;; /current/ account path.  If no such process can be found, signal an
;; error. All references to unidata-process should be eliminated; this variable
;; is now buffer-local to the buffer running the unidata command line.


;; TODO: Change to search through the list, comparing the path of
;; FILE-NAME with each unidata account path, picking that buffer which
;; has the unidata account in which FILE-NAME is found.
(defun get-unidata-process (file-name)
  "Get the unidata process best associated with FILE-NAME."
  (unless (and (boundp 'cached-unidata-process)
               (processp cached-unidata-process))
    (make-local-variable 'cached-unidata-process)
    (setq cached-unidata-process
          (get-buffer-process (car unidata-buffer-list))))
  cached-unidata-process)


(defun unibasic-catalog ()
  (interactive)
  (let ((fn (buffer-file-name (current-buffer))))
    (unibasic-catalog-file (get-unidata-process fn) fn)))

(defun unibasic-compile ()
  (interactive)
  (let* ((fn (buffer-file-name (current-buffer))))
    (unibasic-compile-file (get-unidata-process fn) fn)))


;; I'm getting deja-vu.  I think there is an underlying abstraction here.
;; TODO: prompt form arguments
(defun unibasic-run ()
  (interactive)
  (let* ((fn (buffer-file-name (current-buffer))))
    (unibasic-run-file (get-unidata-process fn) fn)))


(defun unibasic-compile-and-catalog ()
  (interactive)
  (let* ((fn-buf (current-buffer))
         (fn (buffer-file-name fn-buf))
         (u2-process (get-unidata-process fn))
         (u2-buffer (process-buffer u2-process)))
    (save-excursion
      (unibasic-compile-file u2-process fn)
      (with-current-buffer u2-buffer
        ;; TODO: Check status of compile first
        (unibasic-catalog-file u2-process fn)))))

(defun host-is-this-host (host)
  nil)


(eval-when-compile 
  (defmacro def-local-var (varname value &optional doc)
    `(progn
       (defvar ,varname ,value ,doc)
       (make-variable-buffer-local ',varname))))

(provide 'unibasic-compile)


;; Local variables:
;;   mode: emacs-lisp
;;   auto-save-interval: 1000
;;   indent-tabs-mode: nil
;; End:
