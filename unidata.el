;; unidata.el -- Copyright (C) 2005 Timothy M. Schaeffer <tschaef@sbcglobal.net>
;;
;; Major mode for working with UniBasic files in EMACS.
;;
;; Author: Tim Schaeffer <numeromancer@users.sourceforge.net>
;; Version: 1.10   Time-stamp: <2005-11-22 23:24:03 timothy>
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
;;   Provides an interface to the IBM Unidata database client, both for
;;   local and remote connections.
;;
;;; INSTALLATION
;;  ------------
;;
;;
;;; KNOWN BUGS:
;;  -----------
;;  After logging in and getting to Unidata, we still see the "wait"
;;  cursor until I press Enter.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'comint)
(require 'telnet)
(require 'u2-cache)
(eval-when-compile (require 'cl))

(defconst u2-mode-cvs-version
  "@(#)$Id$"
  "CVS version info for `unidata-mode'.")

(defconst u2-mode-version
  (if (string-match "\\<[0-9]+\\.[0-9]+\\>" u2-mode-cvs-version)
      (substring u2-mode-cvs-version (match-beginning 0) (match-end 0))
    "0.0")
  "The current version of `u2-mode' in use.
The current version was set up to conform to reserved words specified for
UniData 3.3. For other versions you may need to adjust the lists of special
words at the end of unidata.el and then regenerate the font-lock regular
expressions.")

;; We need to compile our lists of words into regular expressions.
;; In order to do this we need either regexp-opt which is provided with
;; recent emacsen, or we must provide make-regexp.el. Once the library
;; has been byte compiled it will be a lot quicker to load as we won't
;; need to run this for the compiled code.
(eval-when-compile
  (if (condition-case () (require 'regexp-opt) (error nil))
      (defun make-regexp (strings &optional paren lax) nil
        (regexp-opt strings paren))
    (if (not (condition-case () (fboundp 'make-regexp) (error nil)))
        (if (not (load-library "make-regexp"))
            (error "Failed to load make-regexp.el")))))


(defgroup unidata nil
  "Major mode for interacting with IBM Unidata database clients. 
Use the command `unidata' to connect to a unidata server.  You can
connect to a local or remote server, the latter being handled through
a telnet session."
  :prefix "unidata-"
  :group 'comint)
(defcustom unidata-local-shell "/bin/sh"
  "Holds the name of the shell to use when running Unidata locally.
It should hold the name of a Bourne-like shell.  If you use some other, 
you may get it to work by modifying some of the other custom variables,
namely `unidata-setenv-format-string'."
  :group 'unidata
  :type '(string :tag "Shell Name"))
(defcustom unidata-shell-prompt-regexp "^.*[>#%$][ \t]*"
  "Regular expression matching the initial prompt of the shell on the host."
  :type 'regexp
  :group 'unidata)
(defcustom unidata-prompt "^.*[:>][ \t]*$"
  "Unidata prompt when waiting for input"
  :type 'regexp
  :group 'unidata)
(defcustom unidata-environment-alist
  '(("PS1" . "shell>")
    ("UDTHOME" . "/opt/ud61")
    ("UDTBIN" . "${UDTHOME}/bin")
    ("TERM" . "dumb"))
  "Alist of environment variables/values to be set before entering unidata."
  :type '(alist :key-type (string :tag   "Variable")
                :value-type (string :tag "    Value"))
  :group 'unidata)
(defcustom unidata-setenv-format-string "export %s=\"%s\""
  "String used to format the command to set the environment on the
host.  It should have two `%s' specifications; the first will be
replaced by the variable name, the second by the value to which to set
it. The value will be double quoted automatically."
  :type 'string
  :group 'unidata)
(defcustom unidata-new-line "\n"
  "Send to end commands in Unidata."
  :type 'string
  :group 'unidata)
(defcustom unidata-count 0
  "What is this for?"
  :type 'integer
  :group 'unidata)
;; TODO: Add some code to allow tramp to complete this, and still get the
;; right path for the initial `cd', and then `LOGTO.'
(defcustom unidata-account-path ""
  "String which holds the path to the Unidata account on the host.
Unidata TCL Mode will automatically log into this account when starting."
  :type 'directory
  :group 'unidata)
(defcustom unidata-application-path "/opt/ud71"
  "Path to the base Unidata application directory.
This should be the usually be the same as the UDTHOME environment variable."
  :group 'unidata
  :type 'string)
(defcustom unidata-udt-command "${UDTBIN}/udt"
  "Command to call from the shell to start Unidata."
  :group 'unidata
  :type 'string)
(defcustom unidata-initial-count -40
  "Number of characters to expect after login to a remote shell."
  :type 'integer
  :group 'unidata)
(defcustom unidata-maximum-count 4
  "Number of times to try login before giving up."
  :type 'integer
  :group 'unidata)
(defcustom unidata-auto-upcase-commands nil
  "*When non-nil, automatically upcases all TCL commands and their arguments.
Commands and arguments in quotes, and those resembling pathnames (i.e. those having
a `/' in them somewhere) will not be affected."
  :type 'boolean
  :group 'unidata)
(defcustom unidata-command-word-regexp
  "\\b\\(?:[/._,]\\|\\w\\)+\\b\\|\"[^\"]*\""
  "Regular expression matching a word in a TCL command line.
This effects only movement commands on the command line."
  :type 'regexp
  :group 'unidata)
;; TODO: Make the hooks lists of function and change value-type to 'hook
(defcustom unidata-command-hooks
  '(("\\`LOGTO\\'" .  unidata-logto-hook)
    ("\\`HELP\\'" . unidata-help-hook)
    ("\\`BYE\\'" . unidata-bye-hook)
    ("\\`LO\\'" . unidata-bye-hook)
    ("\\`EDIT\\'" . unidata-edit-command-hook))
  "Commands to run to filter commands entered into the Unidata prompt.
The car of each element contains a regular expression matching the 
command name.  The second is a symbol, the name of the function to
be called when that command is issued.  It should take one argument,
a list containing the words from the command line.  It should return
a list containing the words which will actually be sent to the Unidata
server.  The return list can be empty, in which case no command is actually
sent after the function returns (although the function itself can send
commands with `unidata-send').

These are used, e.g., to display help in another buffer when HELP ...
is entered, and to automatically logout of the shell after the user logs
out of Unidata. The defaults should not usually be changed, or their useful
behavior will be removed :). "
  :type '(alist :key-type (regexp :tag "Command Regexp")
                :value-type (function :tag "Hook"))
  :group 'unidata)
;; TODO: make this a list joined at run-time
(defcustom unidata-prevent-interaction-suffix
  "NO.PAGE HDR.SUPP COL.HDR.SUPP COUNT.SUP"
  "Appended to all display commands to prevent paging, headers, etc."
  :type 'string
  :group 'unidata)
(defcustom unidata-help-command-format
  "LIST HELP.FILE WITH @ID LIKE \"...*%s\" TEXT NO.PAGE HDR.SUPP"
  "*String which is used to create the command sent to the host when
the user issues HELP.  Format will be send one arument, the argument
of the help command, when formatting it for sending to Unidata.
DO NOT ADD COUNT.SUP, as we use the count to tell when help is finished."
  :type 'string
  :group 'unidata)
(defcustom unidata-help-done-regexp
  "^\\([0-9]+\\|No\\)[ \t]*records? listed\.?"
  "Regular expression which tells the help redirector that
the help text is finished.  Since we get help with ``LIST HELP.FILE ...'',
we do not suppress the record count, but us it as the EOF marker."
  :type 'regexp
  :group 'unidata)


;; TODO: there is probably a variable like this in comint 
;; already. Use that.
(defvar telnet-login-prompt-regexp "ogin[ \t]*:[ \t]*\\'")


(defvar unidata-process nil
  "Process running the Unidata shell.  This could be done through 
telnet to a remote host.")

(defvar unidata-host nil
  "String representing the Unidata host.")

(defvar host-info nil
  "Local variable holding cache information for logging into a host.")

(defvar unidata-buffer-list nil
  "A list of all buffers running a unidata process.  This will almost
always have only one member.  We use this, for example from unibasic
programs to find a process which we can use for compiling and
cataloging.")

(eval-and-compile
  (defvar unidata-ecl-commands
    '("ACCT_RESTORE" "ACCT.SAVE" "AE" "ANALYZE.FILE" "AVAIL" "BASIC"
      "BASICTYPE" "BLIST" "BLOCK.PRINT" "BLOCK.TERM" "BUILD.INDEX"
      "BYE" "CATALOG" "CENTURY.PIVOT" "CHECKOVER" "CLEAR.ACCOUNT"
      "CLEAR.FILE" "CLEAR.LOCKS" "CLEAR.ONABORT" "CLEAR.ONBREAK"
      "CLEARDATA" "CLEARPROMPTS" "CLR" "CNAME" "COMO" "COMPILE.DICT"
      "CONFIGURE.FILE" "CONNECT" "CONTROLCHARS" "CONVERT.SQL" "COPY"
      "CREATE.FILE" "CREATE.INDEX" "CREATE.TRIGGER" "DATE"
      "DATE.FORMAT" "DEBUG.FLAG" "DEBUGLINE.ATT" "DEBUGLINE.DET"
      "DEFAULT.LOCKED.ACTION" "DELETE" "DELETECOMMON" "DELETE.CATALOG"
      "DELETE.FILE" "DELETE.INDEX" "DELETE.TRIGGER" "DISABLE.INDEX"
      "DISABLE.USERSTATS" "DTX" "DUP.STATUS" "ECLTYPE" "ED"
      "ENABLE.INDEX" "ENABLE.USERSTATS" "FILE.STAT" "FILELIMIT"
      "FILEVER" "FLOAT.PRECISION" "GETUSER" "GROUP.STAT" "HASH.TEST"
      "HELP" "HUSH" "HUSHBASIC" "ISTAT" "LIMIT" "LINE.ATT" "LINE.DET"
      "LINE.STATUS" "LIST.CONNECT" "LIST.INDEX" "LIST.LANGGRP"
      "LIST.LOCKS" "LIST.PAUSED" "LIST.QUEUE" "LIST.READU"
      "LIST.TRIGGER" "LIST.USERSTATS" "LISTPEQS" "LISTPTR" "LISTUSER"
      "LO" "LOCK" "LOGTO" "LS" "LSL" "MAG_RESTORE" "MAKE.MAP.FILE"
      "MAP" "MAX.USER" "MENUS" "MESSAGE" "MIN.MEMORY" "MYSELF"
      "NEWPCODE" "NFAUSER" "NODIRCONVERT" "ON.ABORT" "ON.BREAK" "PAGE"
      "PATHSUB" "PAUSE" "PHANTOM" "PORT.STATUS" "PRIMENUMBER"
      "PRINT.ORDER" "PROTOCOL" "PTERM" "PTRDISABLE" "PTRENABLE"
      "QUIT" "READDICT.DICT" "REBUILD.FILE" "RECORD" "RELEASE"
      "RELEASE.ITEMS" "RESIZE" "REUSE.ROW" "RUN" "SET.DEC" "SET.LANG"
      "SET.MONEY" "SET.THOUS" "SET.TIME" "SET.WIDEZERO" "SETDEBUGLINE"
      "SETFILE" "SETLINE" "SETOSPRINTER" "SETPTR" "SETTAPE" "SORT"
      "SORT.TYPE" "SP.ASSIGN" "SP.CLOSE" "SP.EDIT" "SP.KILL" "SP-LISTQ"
      "SP.STATUS" "SPOOL" "SQL" "STACKCOMMON" "STARTPTR" "STATUS"
      "STOPPTR" "SUPERCLEAR.LOCKS" "SUPERRELEASE" "T.ATT" "T.BAK"
      "T.CHK" "T.DET" "T.DUMP" "T.EOD" "T.FWD" "T.LOAD" "T.RDLBL"
      "T.READ" "T.REW" "T.SPACE" "T.STATUS" "T.UNLOAD" "T.WEOF" "TERM"
      "TIMEOUT" "UDT.OPTIONS" "UNIENTRY" "UNSETDEBUGLINE" "UNSETLINE"
      "UPDATE.INDEX" "USHOW" "UV_RESTORE" "VCATALOG" "VERSION" "VI"
      "WAKE" "WHAT" "WHERE" "WHO" "XTD")))

(defvar unidata-ecl-commands-regexp
  (eval-when-compile
    (make-regexp unidata-ecl-commands)))


(defvar unidata-font-lock-keywords
  (list 
   (cons unidata-ecl-commands-regexp 'font-lock-function-name-face)))

;; TODO: Change to search through the list, comparing the path of
;; FILE-NAME with each unidata account path, picking that buffer which
;; has the unidata account in which FILE-NAME is found.
(defun get-unidata-process (file-name-or-buffer)
  "Get the unidata process best associated with FILE-NAME-OR-BUFFER."
  (let (buffer file-name)
    (when (stringp file-name-or-buffer)
      (setq buffer (get-buffer file-name-or-buffer))
      (and buffer
          (setq file-name-or-buffer buffer)))
    (when (bufferp file-name-or-buffer)
      (setq buffer file-name-or-buffer)
      (setq file-name (buffer-file-name buffer)))
    (or buffer (setq file-name file-name-or-buffer))
    (unless (and (boundp 'cached-unidata-process)
                 (processp cached-unidata-process))
      (make-local-variable 'cached-unidata-process)
      (setq cached-unidata-process
            (get-buffer-process (car unidata-buffer-list))))
    (unless (member (process-status cached-unidata-process) '(run))
      (setq cached-unidata-process
            (get-buffer-process (car unidata-buffer-list))))
    cached-unidata-process))

(defalias 'unidata-get-process (symbol-function 'get-unidata-process))


(defun unidata-check-process (proc) t)

(defun unidata-send-command (proc cmd &rest arg-list)
  (and (unidata-check-process proc)
       (process-send-string
        proc
        (concat (mapconcat 'identity (cons cmd arg-list) " ")
                unidata-new-line))))

(defconst unidata-extract-source-table-regexp
  ".*/\\([^/]+\\)/[^/]*\\'")



(defun unidata-process-initial-commands (p user host commands)
  "Send initial commands to the host, in order."
  (let (cmd)
    (while commands
      (setq cmd (pop commands))
      (message "Sending command to remote shell: %s" cmd)
      (ub-tcl-send-command p user host cmd))))

;; Don't upcase args with slashes or commas, as these are 
;; case-sensitive file names (the comma is for Unidata LD 
;; file types.  It is also used to separate id's in copy
;; commands, which will also be case sensitive. For convenience, we
;; don't upcase double-quoted strings, but we do upcase single quoted
;; strings. Quotes are unconsistently accepted by unidata commands.
(defun unidata-upcase-command-words (string)
  (let ((mk 0) ms)
    (while (string-match  unidata-command-word-regexp string mk)
      (setq mk (match-end 0))
      (setq ms (match-string 0 string))
      (unless (save-match-data
                (or (char-equal (aref ms 0) ?\")
                    (string-match "/" ms)
                    (string-match "," ms)))
        (setq string (replace-match (upcase ms) 1 1 string)))))
  string)



;; TODO: is this run outsid the unidata buffer?
(defun unidata-set-mode-name ()
    (setq mode-name (concat "Unidata(" unidata-account-path ")")))

(defun unidata-logto-hook (cmd-list)
  (save-excursion
    ;; TODO: is this necessary?
    (setq unidata-account-path (car (cdr cmd-list)))
    (unidata-set-mode-name)
    cmd-list))

(defun unidata-edit-command-hook (cmd-list)
  (if (< (length cmd-list) 3)
      (error "Invalid edit syntax : %s" cmd-list))
  (let* ((dict (string= (nth 1 cmd-list) "DICT"))
         (table-name (nth (if dict 2 1) cmd-list))
         (rec-id (nth (if dict '3 '2) cmd-list)))
    (unidata-edit-record table-name rec-id  dict)
    nil))

(defun unidata-redirect (cmd buf-name proc done-regexp)
  (let (buf)
    (comint-redirect-setup
     buf-name (current-buffer)
     done-regexp nil)
    (setq comint-redirect-original-filter-function
          'unidata-filter)
    (set-process-filter proc 'comint-redirect-filter)
    (setq buf (get-buffer-create buf-name))
    (save-excursion (set-buffer buf) (erase-buffer))
    (process-send-string
     (current-buffer)
     (concat cmd "\n"))
    ;; Ensure that the unidata buffer filter gets set back to 
    ;; the interactive filter unidata-post-ud-filter, even if something
    ;; goes wrong, and the comint filter cannot set it back
    (run-at-time "5 sec" nil
                 (lambda (buffer)
                   (message "Resetting process filter for %s." buffer)
                   (unidata-fix-redirect buffer))
                 (current-buffer))
   ;;;;;;;;;;;;;;;;;;;;;
   ;;; Show the output
   ;;;;;;;;;;;;;;;;;;;;;
    (display-buffer buf)))


(defun unidata-fix-redirect (&optional buffer)
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (comint-redirect-cleanup)
      (set-process-filter unidata-process 'unidata-filter))))

(defun unidata-help-hook (cmd-list)
  "Redirect help on a command to a seperate buffer."
  (let* ((oproc unidata-process)
         (obuf (process-buffer oproc))
         (ofilt (process-filter oproc))
         (ocirx comint-prompt-regexp))
    (unwind-protect
        (let (cmd)
          (setq cmd (format unidata-help-command-format (cadr cmd-list)))
          (unidata-redirect cmd "*Unidata-Help*" unidata-process unidata-help-done-regexp)
          (list ""))
      (progn
        (setq comint-prompt-regexp ocirx)))))

(defun unidata-bye-hook(cmd-list)
  (comint-send-string proc "BYE")
  (comint-send-string proc unidata-new-line)
  (comint-send-string proc "exit")
  (comint-send-string proc unidata-new-line))

(defun unidata-check-command-list (cl)
  (if (null cl)
      (error "Command list is null.")))

;;; TODO: write func to split words using quotes, &c,
;;; and pass those around to filters and hooks
(defun unidata-filter-command (string)
  (let* ((str string)
         (cmd-list (split-string str "[ \t]+"))
         (word cmd-list))
    (unidata-check-command-list cmd-list)
    (if unidata-auto-upcase-commands
        (setq str (unidata-upcase-command-words str)))
    (setq cmd-list (split-string str "[ \t]+"))
    (if cmd-list
        (dolist (hook unidata-command-hooks)
          (save-match-data
            (if (and (stringp (car cmd-list))(cdr hook) (functionp (cdr hook))
                     (string-match (car hook) (car cmd-list)))
                (setq cmd-list (funcall (cdr hook) cmd-list))))))
    (and cmd-list (mapconcat 'identity cmd-list " "))))

(defun unidata-send (proc string)
  (let ((cmd  (unidata-filter-command string)))
    (unless (or (not cmd) (and nil (string= cmd "")) )
      (comint-send-string proc cmd)
      (comint-send-string proc unidata-new-line))))



(defun unidata-insert-prompt ()
  (save-excursion
    (set-buffer (process-buffer unidata-process))
    (insert-before-markers "\n:")))

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
  

(defun unidata-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let* ((last-insertion (marker-position (process-mark proc)))
	   (delta (- (point) last-insertion))
	   (ie (and comint-last-input-end
		    (marker-position comint-last-input-end)))
	   (w (get-buffer-window (current-buffer)))
	   (ws (and w (window-start w))))
      (setq string (unidata-strip-extra-lines string))
      (goto-char last-insertion)
      (insert-before-markers string)
      (set-marker comint-last-output-start last-insertion)
      (set-marker (process-mark proc) (point))
      (if ws (set-window-start w ws t))
      (if ie (set-marker comint-last-input-end ie))
      (while (progn (skip-chars-backward "^\C-m" last-insertion)
		    (> (point) last-insertion))
	(delete-region (1- (point)) (point)))
      (goto-char (process-mark proc))
      ;; If point is after the insertion place, move it
      ;; along with the text.
      (if (> delta 0)
	  (goto-char (+ (process-mark proc) delta))))))

(defun unidata-setup-environment (proc)
  (condition-case nil
      (dolist (ev unidata-environment-alist)     
        (unidata-send-command
         proc
         (format unidata-setenv-format-string (car ev) (cdr ev)))
        (accept-process-output proc 10))
    (error (error "Error : make sure the variable unidata-environment-list is a proper alist."))))
  

(defun unidata-get-to-ud (proc)
  (unidata-setup-environment proc)
  (unidata-send-command
   proc "cd"
   (if (unidata-remote-path-p unidata-account-path)
       (unidata-break-account-path unidata-account-path)
     unidata-account-path))
  (unidata-send-command proc unidata-udt-command)
  (setq comint-prompt-regexp "^.*[:>][ \t]*\\'")
  (setq comint-input-sender 'unidata-send)
  ;; Turn off paging, set terminal to really wide
  (accept-process-output proc))



(defun unidata-strip-extra-lines (string)
  (let ((str string))
    (if (string-match "\\(\n[ \t]*\n\\)+" str)
        (setq str (replace-match "\n" nil nil str)))
    str))


(defun unidata-post-ud-filter (proc string)
  (cond ((string-match unidata-prompt string)
         (set-process-filter proc 'unidata-filter)
         (unidata-send-command proc "TERM 200,0")))
  (unidata-filter proc string))

(defun unidata-pre-ud-filter (proc string)
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((case-fold-search t))
      (cond ((string-match unidata-shell-prompt-regexp string)
             (set-process-filter proc 'unidata-post-ud-filter)
             (unidata-get-to-ud proc))))))

(defun unidata-remote-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((case-fold-search t))
      (cond ((string-match "No such host" string)
	     (kill-buffer (process-buffer proc))
	     (error "No such host"))
            ((string-match telnet-login-prompt-regexp string)
	     (setq telnet-count 0)
             (setq unidata-user (unidata-get-user unidata-host))
	     (send-string proc (concat unidata-user
				       unidata-new-line)))
	    ((string-match "passw" string)
	     (setq telnet-count 0)
	     (send-string proc (concat (unidata-get-passwd unidata-host unidata-user)
				       unidata-new-line))
	     (clear-this-command-keys)
             (set-process-filter proc 'unidata-pre-ud-filter))
	    (t (telnet-check-software-type-initialize string)
	       (cond ((> telnet-count telnet-maximum-count)
                      (set-process-filter proc 'unidata-pre-ud-filter))
		     (t (setq telnet-count (1+ telnet-count)))))))))



(defun unidata-get-last-output ()
  (let ((proc (get-buffer-process (current-buffer)))
        output)
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
			  (forward-line 0)
			  (point-marker))))
	(buffer-substring comint-last-input-end pmark)))))









;;; ------------------------------------------------------------
;;; Functions for editing records
;;; ------------------------------------------------------------

(defcustom unidata-temp-record-dir "_HOLD_"
  "Directory in which to write temporary records while editing.
This must be a DIR type file in the VOC.  Records will be written into
temporary files here with specially formatted name so that it can be
copied back into the original file when finished editing."
  :type 'string
  :group 'unidata)


;; I originally quoted the table (file) names in the command too, but
;; that gave a syntax error in Unidata; apparently you can only quote
;; the record ids. 
(defun unidata-make-edit-record-command (table-name record-id tmp-file &optional dict)
  (let ((cmd-string
         (format "COPY FROM %s %s TO %s \"%s\",\"%s\" OVERWRITING"
                 (if dict "DICT" "") table-name unidata-temp-record-dir
                 record-id  tmp-file)))
    cmd-string))
         
;; (defun unidata-make-save-record-command (tmp-file)
;;   (let* ((rec-info-list (unidata-split-temp-record-name tmp-file))
;;          (table-name (nth 0 rec-info-list))
;;          (rec-id (nth 1 rec-info-list))
;;          (dict (nth 2 rec-info-list)))
;;     (format "COPY FROM %s TO %s %s \"%s\",\"%s\" OVERWRITING"
;;             unidata-temp-record-dir (if dict "DICT" "") table-name
;;             tmp-file rec-id)))

(defun unidata-make-save-record-command2 (rec-info-path)
  (let* ((table-name (nth 0 rec-info-path))
         (rec-id (nth 1 rec-info-path))
         (dict (nth 2 rec-info-path))
         (tmp-rec-id (file-name-nondirectory buffer-file-name)))
    (format "COPY FROM %s TO %s %s \"%s\",\"%s\" OVERWRITING"
            unidata-temp-record-dir (if dict "DICT" "") table-name
            tmp-rec-id rec-id)))


(defvar unidata-tmp-record-file-extension "u2r")
(defvar unidata-tmp-file-cnt 0)


;; Unidata has the perfectly reasonable limit of 32 chars in an ID
;; :-), so we limit the temp file name to 32 bytes. 
(defun unidata-make-tmpfile-name (table-name record-id &optional dict)
  (flet ((cutstr (s) (substring s 0 (min 7 (length s))))
         (clean (s) (while (string-match "[^-a-zA-Z0-9_.]" s)
                      (setq s (replace-match "_" t t s)))
                s)
         (inc-count () (setq  unidata-tmp-file-cnt
                              (% (1+ unidata-tmp-file-cnt) 100000))))
    (format  "%s%s~%s-%05d.%s"
             (if dict "dict~" "")
             (cutstr table-name)
             (clean (cutstr record-id))
             (inc-count)
             unidata-tmp-record-file-extension)))


;; TODO:Use the 3-digit numbers to make sure the this works even if
;; ~[0-9]{3} occurs in the record id or the table name (however
;; unlikely that may be.
;; (defun unidata-split-temp-record-name (tmp-name)
;;   (save-match-data
;;     (let ((temp-rec-regexp (concat "u2"
;;                                    "\\(\\(?:~dict\\)?\\)"
;;                                    "~\\([0-9]\\{3\\}\\)"
;;                                    "\\(.*\\)"
;;                                    "~\\([0-9]\\{3\\}\\)"
;;                                    "\\(.*\\)"
;;                                    "\." unidata-tmp-record-file-extension)))
;;       (string-match temp-rec-regexp tmp-name)
;;       (list (match-string 3 tmp-name)
;;             (match-string 5 tmp-name)
;;             (not (nilstring (match-string 1 tmp-name)))))))


(defcustom unidata-keep-temps-on-killing-buffer nil
  "Toggle deleting record temp files when the edit buffer is killed.
This should normally be nil, but if you want to keep the temp files
around after killing the edit buffer you can set it to non-nil."
  :type 'boolean
  :group 'unidata)


;; TODO: (Maybe) Change the interactive spec to generate a list of
;; valid table names instead of all the files in the account directory
;; (which will include dictionary files).  Maybe allow dictionary
;; names, but turn it into and edition of the dictionary record
;; (probably the better path).

(defun unidata-edit-record (table-name rec-id &optional dict u2-buffer)
  "Modify and save a record of a Unidata data file in an Emacs buffer.
We copy the record into a temporary file and open that file in an
emacs buffer.  A temporary file is created in the value of
`unidata-temp-record-dir' for editing; this directory must be the name
of a DIR type file in the current Unidata account directory. Saving
the buffer also writes the modified record into the database. This,
then, is an great alternative to the Unidata editor commands.  The
temporary file is deleted when the buffer is killed unless
`unidata-keep-temps-on-killing-buffer' is non-nil."
  (interactive "sTable Name:\nsRecord-Id:")
  (let* ((buffer (or u2-buffer (current-buffer)))
         (u2proc (get-buffer-process buffer))
         (tmp-file (unidata-make-tmpfile-name table-name rec-id dict))
         (edit-cmd (unidata-make-edit-record-command table-name rec-id tmp-file dict))
         (keep-trying t))
    (unidata-send-command u2proc edit-cmd)
    (while keep-trying
      (setq keep-trying
            (accept-process-output u2proc 5))  ;; give up after 10 seconds
      (if (string-match "records copied" (unidata-get-last-output))
          (setq keep-trying nil)))
    (if (string-match "records copied" (unidata-get-last-output))
        (save-excursion
          (find-file-other-window (concat unidata-full-path "/"
                                          unidata-temp-record-dir "/"
                                          tmp-file))
          ;; Now in the record buffer. Setup local variable to point
          ;; to unidata process, for saving later.
          (make-local-variable 'unidata-process)
          (make-local-variable 'unidata-record-path)
          (make-local-variable 'u2-buffer)
          (setq unidata-process u2proc)
          (setq unidata-record-path (list table-name rec-id dict))
          (setq u2-buffer buffer)
          (rename-buffer (concat table-name ":" rec-id))
          (add-hook 'after-save-hook 'unidata-save-record nil t)
          (unless unidata-keep-temps-on-killing-buffer
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (delete-file (buffer-file-name)))
                        nil t)))
      (error "Copy failed: %s,%s" table-name rec-id)) )
  nil)

(put 'u2-buffer 'permanent-local t)
(put 'unidata-process 'permanent-local t)
(put 'unidata-record-path 'permanent-local t)

(defun unidata-save-record (&optional buffer)
  "Saves the record opened with `unidata-edit-record'.
This should be run from the buffer in which the record is displayed.
When the buffer was created, `unidata-process' was setup as a
buffer-local variable which points to the unidata process from which
this record was opened.  We use this to send the command to save the
record." 
  (let* ((buffer (or buffer (current-buffer)))
         (full-file-name (buffer-file-name buffer))
         (file-name (file-name-nondirectory full-file-name))
         (process (or (if (member (process-status unidata-process) '(run))
                          unidata-process)
                      (get-unidata-process full-file-name))))
    (with-current-buffer buffer
      (unidata-send-command
       process
       (unidata-make-save-record-command2 unidata-record-path))
      (accept-process-output unidata-process))))











;;; ------------------------------------------------------------
;;; Unidata Mode Setup
;;; ------------------------------------------------------------

(defvar unidata-mode-syntax-table nil
  "Syntax table for Unidata TCL mode.")
(if unidata-mode-syntax-table
    ()
  (setq unidata-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?. "_   " unidata-mode-syntax-table)
  (modify-syntax-entry ?/ "w   " unidata-mode-syntax-table)
  (modify-syntax-entry ?\  "    " unidata-mode-syntax-table))


(defvar unidata-mode-abbrev-table nil
  "Abbrev table used while in text mode.")

(define-abbrev-table 'unidata-mode-abbrev-table ())

(defvar unidata-mode-map nil
  "Keymap for Unidata TCL mode.")

(defun unidata-toggle-auto-upcase ()
  (interactive)
  (setq unidata-auto-upcase-commands (not unidata-auto-upcase-commands)))

(defvar unidata-menu (make-sparse-keymap "Unidata"))

(if unidata-mode-map
    ()
  (progn
    (setq unidata-mode-map (copy-keymap telnet-mode-map))
    (define-key unidata-mode-map "\C-cd" 'unibasic-view-account-in-dired)
    (setq unidata-menu-map (make-sparse-keymap "Unidata"))
    (define-key unidata-mode-map [menu-bar insert]
      (cons "Unidata" unidata-menu-map))
    (define-key unidata-menu-map [unidata-auto-upcase]
      '(menu-item "Auto-Upcase commands"  unidata-toggle-auto-upcase
                  :enabled t
                  :visible t
                  :button (:toggle 
                           . (and (boundp unidata-auto-upcase-commands)
                                  unidata-auto-upcase-commands))))
    (define-key unidata-menu-map [unidata-dedicated-window]
      '(menu-item "Dedicated Window"  unidata-toggle-window-dedicated
                  :enabled t
                  :visible t
                  :button (:toggle . (window-dedicated-p (selected-window)))))
    (define-key unidata-menu-map [unidata-view-in-dired]
      '(menu-item "View Account in Dired" unidata-view-account-in-dired))
    ))


(defun unidata-toggle-window-dedicated ()
  (interactive)
  (let ((win (selected-window)))
    (set-window-dedicated-p win (not (window-dedicated-p win)))))

(define-derived-mode unidata-mode telnet-mode "Unidata"
  "Major mode for Unidata TCL command line.
\\{unidata-mode-map}"
  (if unidata-account-path
      (setq mode-name (concat mode-name "(" unidata-account-path ")"))))


(put 'unidata-account-path 'permanent-local t)
(put 'unidata-host 'permanent-local t)
(put 'unidata-process 'permanent-local t)
(put 'unidata-user 'permanent-local t)
(put 'unidata-full-path 'permanent-local t)


(defvar u2-connection-method-defaults
  '((u2-connection-function u2-open-connection-rsh)
   (u2-login-program "telnet")
   (u2-remote-sh "/bin/sh")
   (u2-login-args nil)
   (u2-file-prefix "")
   (u2-password-end-of-line "\n")))


(defvar u2-connection-method-alist
  '(("local" 
     (u2-connection-function u2-open-local-connection)
     (u2-login-program nil)
     (u2-remote-sh "/bin/sh")
     (u2-login-args nil)
     (u2-file-prefix nil)
     (u2-password-end-of-line nil))
    ("telnet" 
     (u2-connection-function u2-open-remote-connection)
     (u2-login-program "telnet")
     (u2-remote-sh "/bin/sh")
     (u2-login-args nil)
     (u2-file-prefix nil)
     (u2-password-end-of-line nil))
    ("u2-telnet" 
     (u2-connection-function u2-open-remote-connection)
     (u2-login-program "telnet")
     (u2-remote-sh nil)
     (u2-login-args nil)
     (u2-file-prefix nil)
     (u2-password-end-of-line nil))
    ("ssh"
     (u2-connection-function u2-open-remote-connection)
     (u2-login-program "ssh")
     (u2-remote-sh "/bin/sh")
     (u2-login-args nil)
     (u2-file-prefix nil)
     (u2-password-end-of-line nil))
    ("rlogin"
     (u2-connection-function u2-open-remote-connection)
     (u2-login-program "ssh")
     (u2-remote-sh "/bin/sh")
     (u2-login-args nil)
     (u2-file-prefix nil)
     (u2-password-end-of-line nil))))

(defun u2-get-connection-setting (method setting-key)
  (let* ((ms (assoc method u2-connection-method-alist))
         (sttng (and ms (assoc setting-key ms))))
    (if sttng
        (cadr sttng)
      (let ((ds (assoc setting-key u2-connection-method-defaults)))
        (and ds (cadr ds))))))
      

(defun unidata-setup-connection (full-path user host path process)
    (make-local-variable 'unidata-host)
    (make-local-variable 'unidata-account-path)
    (make-local-variable 'unidata-process)
    (make-local-variable 'unidata-user)
    (make-local-variable 'unidata-full-path)
    (setq unidata-full-path full-path)
    (setq unidata-host host)
    (setq unidata-account-path path)
    (setq unidata-process process)
    (setq unidata-user user)
    (unidata-set-user host user)
    (unidata-set-account-path host path))
  

(defcustom u2-host-user-connection-settings
  '((".*" ".*"
     ((u2-connection-method . "telnet")
      (u2-environment
       ("PS1" . "shell>")
       ("UDTHOME" . "/usr/ud")
       ("UDTBIN" . "${UDTHOME}/bin")
       ("TERM" . "dumb"))
      (u2-shell-prompt-regexp . "shell>")
      (u2-prompt-regexp . "^.*[:>][ 	]*$")
      (u2-set-environment-format-string . "export %s=\"%s\"")
      (u2-shell-command-name . "/bin/sh")
      (u2-newline-string . "\n")
      (u2-home-path . "/usr/ud")
      (u2-command . "${UDTBIN}/udt"))))
  "*List of connection information for host/user pairs.
This list consists of a host regexp as its first element, and a user
regexp as its second element.  These are use to match a host user
pair.  The third element is an alist whose keys are symbols used as
configuration tags. The values of the alist are the values of the
configuration settings.  The following configuration settings are
supported:

u2-connection-method      
    A string naming one of the connection methods in
    `u2-connection-method-alist'; this will used as the method for 
    connecting with the host/user pair when the method is not
    explicitely given in the path

u2-remote-user-name
    If the user is not given, and this setting is present, its  value
    will be used as the user name for connecting to the given host. If
    this is not found, `user-login-name' is used.

u2-environment
    A list of cons cells whose elemtents are strings, the car is the
    environment variable set on the host before the U2 program is run.
    This does not work for connections like `u2-telnet' where the
    program is not run from the shell, such as to a U2 Telnet server,
    which goes directly into Unidata or Universe after loggin in.

u2-shell-prompt-regexp
    The shell prompt which will be used to tell when we are logged in
    and have entered the shell on the host.  Useless for `u2-telnet'
    and the like.

u2-prompt-regexp
    Prompt used to know when the U2 program has begun.

u2-set-environment-format-string
    Format string used to create the commands to set environment
    variables in `u2-environment' on the host.  The first parameter to 
    `format' will be the variable name, the second its value.

u2-shell-command-name
    Name of the command to run the shell on the host.  Usually
    \"/bin/sh\".  Not used for `u2-telnet' and the like.

u2-newline-string
    String sent to make a newline on the host after a command.

u2-home-path
    Path to the unidata system. Used to find the global catalog.

u2-command
    The command string used to invoke the U2 system on the host.  Set
    this if you invoke it through a shell script, for example."
  :group 'unidata
  :type '(repeat (list :tag "Host/User Connection settings"
                  (regexp :tag "Host regexp")
                  (regexp  :tag "User regexp")
                  (set
                   :tag "Settings"
                   (cons :tag "Connection Method"
                         (const u2-connection-method)
                         (string :tag "Connection method"))
                   (cons :tag "Remote User Name"
                         (const u2-remote-user-name) 
                         (string :tag "Remote user name"))
                   (cons :tag "Environment Variables"
                         (const u2-environment)
                         (repeat :tag "Environment"
                                 (cons 
                                  :tag "Environment variable to be set by shell before running U2" 
                                  (string :tag "Environment variable name")
                                  (string :tag "Environment variable value"))))
                   (cons :tag "Shell Prompt Regexp"
                         (const u2-shell-prompt-regexp)
                         (string :tag "Shell prompt regexp"))
                   (cons :tag "U2 Prompt Regexp"
                         (const u2-prompt-regexp)
                         (string :tag "Prompt regexp"))
                   (cons :tag "Set Environment Format String"
                         (const u2-set-environment-format-string)
                         (string :tag "Set enviroment command format string"))
                   (cons :tag "Shell Command"
                         (const u2-shell-command-name)
                         (string :tag "Shell"))
                   (cons :tag "Newline String"
                         (const u2-newline-string)
                         (string :tag "Newline string"))
                   (cons :tag "Path to Uni(Verse|Data)"
                         (const  u2-home-path)
                         (string :tag "U2 home path"))
                   (cons :tag "Uni(data|verse) Command"
                         (const u2-command)
                         (string :tag "U2 command name"))
                   ;; (cons (const :tag "Key" u2-type) (string :tag "U2 type"))
                   ))))

(defun* u2-get-host-user-setting (host user setting)
  (dolist (l u2-host-user-connection-settings)
    (if (and (string-match (car l) host)
             (or (null user) (string-match (cadr l) user)))
        (let* ((settings-alist (caddr l))
               (sttng (assoc setting settings-alist)))
          (and  sttng (return-from u2-get-host-user-setting (cdr sttng)))))))
      
(put 'u2-error
     'error-condition
     '(error u2-error))

(put 'u2-error 'error-message "U2-mode error")

(put 'u2-connection-error
     'error-conditions
     '(error 'u2-error 'u2-connection-error))
(put 'u2-error 'error-message "U2-mode connection error")

(defun unidata-open-remote-connection (path)
  (destructuring-bind (host user upath method)
      (unidata-split-host-path path)
    (or user
        (setq user (u2-get-host-user-setting host nil 'u2-remote-user-name))
        (setq user user-login-name)
        (signal 'u2-connection-error "Could not find a user for host %s" host))
    (or method
        (setq method
              (u2-get-host-user-setting
               host user 'u2-connection-method))
        "ssh")
    (let* ((comint-delimiter-argument-list '(?\  ?\t))
           (login-program (u2-get-connection-setting method 'u2-login-program))
           (name (concat "Unidata(" login-program ")-"  host))
           (buffer (get-buffer (concat "*" name "*")))
           (login-options (u2-get-connection-setting method 'u2-login-args))
           process)
      ;; Hey! I think now I'll remember what nconc does!
      (unless (member "-l" login-options)
        (nconc login-options (list "-l" user)))
      (nconc login-options host)
      (if (and buffer (get-buffer-process buffer))
          (pop-to-buffer (concat "*" name "*"))
        (pop-to-buffer
         (apply 'make-comint name login-program nil login-options)))
      (setq process (get-buffer-process (current-buffer)))
      (set-process-filter process 'unidata-remote-initial-filter)
      (setq comint-input-sender 'unidata-send)
      ;; Don't send the `open' cmd till telnet is ready for it.
      (with-timeout
          (60 (accept-process-output process)
              (error "Unidata timed out waiting for telnet server " host)))
      (unidata-setup-connection path user host upath process)
      (setq telnet-count unidata-initial-count))))

(put 'unidata-process 'permanent-local t)
(put 'unidata-record-path 'permanent-local t)

(defun unidata-open-local-connection (path)
  (let* ((comint-delimiter-argument-list '(?\  ?\t))
         (name "Unidata")
	 (buffer (get-buffer (concat "*" name "(" path ")*")))
	 process)
    (if (and buffer (get-buffer-process buffer))
	(pop-to-buffer (concat "*" name "*"))
      (pop-to-buffer 
       (apply 'make-comint name unidata-local-shell nil))
      (setq process (get-buffer-process (current-buffer)))
      (unidata-setup-connection path "" "" path process)
      (set-process-filter process 'unidata-pre-ud-filter)
      (setq comint-input-sender 'unidata-send)
      ;; Don't send the `open' cmd till telnet is ready for it.
      (with-timeout
          (60 (accept-process-output process)
              (error "Unidata timed out waiting for local shell `%s' "
                     unidata-local-shell))))))



;; TODO: save the user name for constructing remote names for 
;; retrieving files
(defun unidata (&optional path)
  "Open a TCL command line in Unidata."
  (interactive "DUnidata Path:")
  (unless path (error "No path specified."))
  (destructuring-bind (host user upath method)
      (unidata-split-host-path path)
    ;;    (setq path (unidata-get-account-path host))
    (if (unidata-remote-path-p path)
        (unidata-open-remote-connection path)
      (unidata-open-local-connection path))
    (push (current-buffer) unidata-buffer-list)
    (erase-buffer)
    (unidata-mode)))
        
(defun unidata-view-account-in-dired (&optional host)
  (interactive)
  (setq host (or host unidata-host))
  (if (nilstring host)
      (find-file-other-window unidata-account-path)
    (find-file-other-window unidata-full-path)))


(provide 'unidata)


;;
;; $Log$
;; Revision 1.14  2006/09/26 14:36:48  numeromancer
;; In the process of modifying the parameters for connecting, so that
;; I can have two connections in the same emacs sessions, and connection
;; paramters can be given separately for each host.  It probably does not
;; work; I'm committing it to get a current copy on my laptop.
;;
;; Revision 1.13  2006/09/11 15:23:56  numeromancer
;; Changed the way arguments are passed in unidata action functions.
;; Changed the formatting of commands by creating function %u2-join to
;;   join command words.
;; Changed the redirect fixup after the help command so it works.
;; Added menu item for PLZ command for piclan.
;;
;; Revision 1.12  2006/09/05 17:39:17  numeromancer
;; Fixed a bug from left-over local q function, and with different arg specs
;; for extra args for u2-compile actions.  Changed email address to my
;; sourceforge address.
;;
;; Revision 1.11  2006/09/01 14:44:14  numeromancer
;; Added permanant local variable u2-buffer.
;;
;; Revision 1.10  2006/08/28 19:26:08  numeromancer
;; Fixed conflict; same change made in two sandboxes.
;;
;; Revision 1.9  2006/08/28 19:23:01  numeromancer
;; I am unifying the relation between buffers which are U2 records and the
;; U2 process, to make it easier to create new commands to work on such
;; buffers.  For now, this is just for the compile/catalog/run commands.
;;
;; Revision 1.8  2006/08/17 14:09:38  numeromancer
;; Changed command word regexp so commas are considered part of a command
;; word.  This allows LD paths to be considered command words.
;;
;; Revision 1.7  2006/08/17 12:54:58  numeromancer
;; Changed unidata to not upcase ECL words with a comma in them, since some
;; tables have these in them (LD types).  Also, added some stuff to help
;; move to compiling and such with information saved with a buffer when files
;; are opened with EDIT ....
;;
;; Revision 1.6  2006/08/09 19:18:23  numeromancer
;; BASIC alignment additions, and corrections to record editing.
;;
;; Revision 1.5  2006/03/06 03:33:18  numeromancer
;; The record editing code was failing with temp file names longer than
;; 32 characters because of Unidata's limit on IDs.  I changed the code to
;; create a buffer-local list with the table & id names, the the dict flag,
;; instead of parsing it from the file names.  I then changed the format of
;; the temp files to ensure that they are <= 32 characters, and to clean out
;; characters which might confuse the shell (in my case, especially '*').  I
;; made it more robust about saving the file when the unidata process has been
;; restarted.
;;
;; Revision 1.4  2006/03/03 03:37:00  numeromancer
;; *** empty log message ***
;;
;;


;; Local variables:
;;   mode: emacs-lisp
;;   auto-save-interval: 1000
;;   indent-tabs-mode: nil
;; End:
