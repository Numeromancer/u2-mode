;; u2-compile.el -- 
;; Copyright (C) 2005 Timothy M. Schaeffer <tschaef@sbcglobal.net>
;;
;; Functions for compiling UniBasic source files from EMACS.
;;
;; Author: Tim Schaeffer <numeromancer@users.sourceforge.net>
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
  "Unidata command used to compile UniBasic source.  If you have
written another program or paragraph or proc to handle compiling
Unibasic programs (while doing other things, like running the source
through `m4' or some other macro processor first), you can put the
name of that command here."
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
  "A list of strings of command line arguments for compiling.  The
list will be appended as a space separated list of arguments to the
end of the compile command line in ECL.  Usually for specifying
debugging arguments to the compiler, since Unibasic provides little
else."
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

    (setq unibasic-catalog-menu-map
          (make-sparse-keymap "Unibasic Cataloging"))
    (define-key unibasic-menu-map [catalog-menu]
      (cons "Unibasic Cataloging" unibasic-catalog-menu-map))

    (define-key unibasic-catalog-menu-map [unidata-catalog-global]
      '(menu-item "Catalog Globally" 
                  (lambda () (setq unibasic-catalog-method 'global))))
    (define-key unibasic-catalog-menu-map [unidata-catalog-local]
      '(menu-item "Catalog Locally" 
                  (lambda () (setq unibasic-catalog-method 'local))))
    (define-key unibasic-catalog-menu-map [unidata-catalog-direct]
      '(menu-item "Catalog Directly" 
                  (lambda () (setq unibasic-catalog-method 'direct))))
    
    )



;;; Compilation and catalog commands
(defvar unibasic-basic-cmd-string "BASIC")

(defun unibasic-default-source-table (file)
  "BP")

(defvar unibasic-source-table nil)

(defun %u2-stringize (x)
  (typecase x
      (character (char-to-string x))
      (t (format "%s" x))))

(defun %u2-join (list &optional separator)
  (let ((separator (%u2-stringize (or separator " "))))
    (mapconcat (lambda (x) (format "%s" x)) list separator)))

(defun* %u2-alist-get-string (string alist)
  (save-match-data
    (dolist (a alist)
      (if (string-match (car a) string)
          (let ((c (cdr a)))
            (etypecase c
              (string (return-from %u2-alist-get-string (replace-match c nil nil string)))
              (function (return-from %u2-alist-get-string (apply  c string))))))))
  nil)
  

;; TODO: Find a way to populate this programmatically from the VOC.
(defvar unibasic-dir-to-table-alist ()
  "Map directory names onto Unidata tables.
The keys are regular expressions against which directory names will be
matched.  If they match, `unibasic-get-source-table' will return the
cdr as the table name.")

(defun unibasic-get-source-table (file-name)
  "Find the U2 table associated with FILE-NAME.
FILE-NAME must be a file which maps onto a U2 table in the
VOC. Mappings from directories to table names can be stored in
`unibasic-dir-to-table-alist' (especially helpful for LD type
tables).  If it is not found there, the last part of the directory
name is used as the table name, which will work for DIR type files
mapped to subdirectories of the current U2 account. If FILE-NAME has
no directory part, `unibasic-default-source-table' is used as a last
resort."
  (let ((dir-name (file-name-directory file-name)))
    (cond ((%u2-alist-get-string dir-name unibasic-dir-to-table-alist))
          ((string-match ".*/\\([^/]+\\)/[^/]*\\'" file-name)
           (match-string 1 file-name))
          (unibasic-source-table unibasic-source-table)
          (t (unibasic-default-source-table file-name)))))



(defvar unibasic-table-to-object-table-alist ()
  "Map source table names to object tables.  The keys are regular
expressions against which source table names will be matched.  If they
match, `unibasic-get-object-table' will return the cdr as the table
name, after doing a regex replace. So the entry \(\"TABLE\\(.*\\)\"
. \"TABLE\\1OBJ\"\) would match \"TABLEXYZ\" to \"TABLEXYZOBJ\".  The
cdr can also be a function, %u2-alist-get-string will call with its
argument when it matches.")


(defun unibasic-get-object-table (table)
  "Get the object table associated with UniBasic source table TABLE.
If table is matched in `unibasic-table-to-object-table-alist', that is
used. Otherwise `unibasic-table-to-object-table-format' is used with
format, if non-nil.  TABLE itself is returned as a last resort."
  (cond 
   ((%u2-alist-get-string table unibasic-table-to-object-table-alist))
   (unibasic-table-to-object-table-format
    (format unibasic-table-to-object-table-format  table))
   (t table)))


(defvar unibasic-table-to-object-table-format nil)



(defcustom unibasic-catalog-method 'local
  "Method of cataloguing when cataloguing a UniBasic program.
This variable can be the symbols `local', `direct', or `global', with
global being the default used when the variable not one of these
three.  Setting this will cause the appropriate option to be sent to
all Catalog commands."
  :type '(choice (const local) (const global) (const direct))
  :group 'unibasic)



;; Commands which define actions on unidata records
             
(defvar unidata-record-commands ())

(defun unidata-command-on-record (command ud-proc table recid &rest opt)
  "Perform a command on a Unidata Record with RECID in TABLE.  COMMAND
can be a string, it is used as a format string called with TABLE and
RECID, in that order, as arguments to `format'. If it is a function,
that function is called with TABLE and RECID as arguments to get the
command string sent to the Unidata process.  If it is a symbol, it is
used as a key to `unidata-record-commands', and the associated value
is used as if it were passed as COMMAND; if COMMAND is not there
found, and the symbol has a function value, the function value of
command is used as if it were passed as COMMAND; otherwise, it's name
is converted to uppercase and a the string \"COMMAND TABLE RECID\" is
used as the command string. As an example of this last, if the symbol
'do-stuff is passed, is not found in `undata-record-commands' and has
no function value, the command \"DO-STUFF TABLE RECID\" will be sent
to Unidata."
  (let (cmd-string)
    (while (null cmd-string)
      (etypecase command
        (string (setf cmd-string (format command table recid)))
        (symbol (let ((a (assoc command unidata-record-commands)))
                  (cond (a (setf command (cdr a)))
                        ((functionp command) (setf command (symbol-function command)))
                        (t (setf command (concat (upcase (symbol-name command)) " %s %s"))))))
        (function (setf cmd-string (apply command (list* table recid opt))))
        (t (signal 'wrong-type-argument (list command)))))
    (setf cmd-string cmd-string)
    (unidata-send-command ud-proc cmd-string)))


(defun unidata-get-buffer-record-path (buffer)
  (with-current-buffer buffer
    (or (and (local-variable-p 'unidata-record-path)
             unidata-record-path)
        (let ((file-name (buffer-file-name buffer)))
          (list (unibasic-get-source-table file-name)
                (file-name-nondirectory file-name)
                nil)))))



(defun unidata-command-on-buffer (command buffer &rest rest)
  "Run COMMAND as a U2 command on BUFFER.  The record path for buffer
is found by calling `unidata-get-buffer-record-path' on it, and this
is sent to `unidata-command-on-record' with COMMAND.  OPT is passed to
the command after the table-name and record id."
  (let* ((buffer (or buffer (current-buffer)))
         (recpath (unidata-get-buffer-record-path buffer))
         (udproc (unidata-get-process buffer)))
    (with-current-buffer buffer
      (apply 'unidata-command-on-record
             command
             udproc
             (car recpath)
             (cadr recpath) rest)) ) )


(defun unidata-command-on-file (command ud-proc file-name &optional opt)
  "Find a unidata record for FILE-NAME, and run COMMAND on it.
`unibasic-get-source-table' will be used to find the table name
associated with FILE-NAME, and the file name sans directory will be
used as the record id.  COMMAND should be one defined with
`define-unidata-record-action.'" 
  (unidata-command-on-record
   command
   ud-proc
   (unibasic-get-source-table file)
   (file-name-nondirectory file) opt))



(defmacro define-unidata-record-action (name args &rest body)
  "Define an action which can be acted upon a UniData Record.  If this
is defined just a string, it will be used as a format string, and
format will be applied to the table, record-id and the &rest of the
args, in that order.  Otherwise, it should be a named lambda form,
which takes the table, record-id, any optional arguments it needs, and
returns a string which will be passed to the Unidata process as a
command."
  (setf unidata-record-commands 
        (cons (cons name
                    (cond ((and (null body) (stringp args)) args)
                          (t `(lambda ,args ,@body))))
              unidata-record-commands))
  `',name)

;; Unidata commands are very inconsistent with quotes on arguments.
;; Some commands will require it for some arguments, will accept them
;; but not require them for others, and reject them for others, even
;; if they are the same kind of item, like a record id.  Ugly. 

(define-unidata-record-action compile (table recid  &rest rest)
  (let ((objtable (unibasic-get-object-table table)))
    (%u2-join (list* "BASIC"  table "TO"  objtable  recid rest))))


(define-unidata-record-action catalog (table recid &optional objtable scope forcep)
  (let ((scope (if scope (upcase (format "%s" scope)) ""))
        (force (if t "FORCE" ""))
        (objtable (if objtable objtable (unibasic-get-object-table table)))
        (_ " "))
    (%u2-join (list unibasic-catalog-command objtable recid scope force))))


(define-unidata-record-action run (table recid &rest args)
  (let ((objtable (unibasic-get-object-table table)))
    (%u2-join (list* "RUN" objtable recid  args))))

(defun unibasic-compile (&rest extra-options)
  (interactive (if current-prefix-arg
                   (list (read-from-minibuffer
                          "Options:"
                          (%u2-join unibasic-basic-arg-list)))
                 unibasic-basic-arg-list))
  (let ((buffer (current-buffer)))
    (apply 'unidata-command-on-buffer
           'compile buffer extra-options)))


(defun unibasic-catalog ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unidata-command-on-buffer 'catalog buffer)))

(defun unibasic-run (&rest extra-options)
  (interactive (if current-prefix-arg
                   (list (read-from-minibuffer "Arguments:"))
                 ()))
  (let ((buffer (current-buffer)))
    (apply 'unidata-command-on-buffer 'run buffer extra-options)))

(defun unibasic-compile-and-catalog ()
  "Compile and catalog the current UniBasic buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (save-excursion
      (unidata-command-on-buffer 'compile buffer)
      ;; TODO: Check status of compile first
      (unidata-command-on-buffer 'catalog buffer))))

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
