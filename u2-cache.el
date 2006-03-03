;;; -*- mode: Emacs-Lisp; coding: iso-2022-7bit; byte-compile-dynamic: t; -*-
;;; u2-cache.el --- file information caching for U2

;; Copyright (C) 2000, 2005 by Free Software Foundation, Inc.

;; Author: Daniel Pittman <daniel@inanna.danann.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; An implementation of file information caching for remote files.

;; Each connection (method, user, host) has a unique cache.
;; Each file has a unique set of properties associated with it.
;; Although method doesn't seem to be necessary, it allows a simpler
;; implementation.  And it doesn't matter, because it is rather rare a
;; remote host will be accessed using different methods in parallel.

;;; Code:

;; Maybe the code could be rearranged to avoid completely (method, user, host).
;; This would require that all functions are called from the connection buffer
;; only; something which could be forgotten.  And I have no idea whether
;; this is really a performance boost.

;; Another performance speedup could be the use of 'eq instead of 'equal for
;; the hash test function.  But this case the key, being a string, would need
;; to be transformed into a unique symbol, which would eat all saved time.

;; ;; Pacify byte-compiler.
;; (autoload 'u2-get-buffer "u2")
;; (autoload 'u2-message "u2")
;; (autoload 'u2-u2-file-p "u2")
;; (autoload 'with-parsed-u2-file-name "u2")

;; (defvar u2-cache-data nil
;;   "Hash table for remote files properties.
;; This variable is automatically made buffer-local to each process buffer
;; upon opening the connection.")

;; (defun u2-cache-setup (method user host)
;;   "Initialise the cache system for a new U2 connection."
;;   (with-current-buffer (u2-get-buffer method user host)
;;     (set (make-local-variable 'u2-cache-data)
;; 	 (make-hash-table :test 'equal))))

;; (defun u2-cache-get-file-property (method user host file key default)
;;   "Get the property KEY of FILE from the cache context of the
;; user USER on the remote machine HOST.  Return DEFAULT if not set."
;;   (with-current-buffer (u2-get-buffer method user host)
;;     (unless (hash-table-p u2-cache-data)
;;       (u2-cache-setup method user host))
;;     (let* ((file (directory-file-name file))
;; 	   (hash (gethash file u2-cache-data))
;; 	   (prop (if (hash-table-p hash) (gethash key hash default) default)))
;;       (u2-message 6 "%s %s %s" file key prop)
;;       prop)))

;; (defun u2-cache-set-file-property (method user host file key value)
;;   "Set the property KEY of FILE to VALUE, in the cache context of the
;; user USER on the remote machine HOST.  Returns VALUE."
;;   (with-current-buffer (u2-get-buffer method user host)
;;     (unless (hash-table-p u2-cache-data)
;;       (u2-cache-setup method user host))
;;     (let ((file (directory-file-name file)))
;;       (puthash key value
;; 	       (or (gethash file u2-cache-data)
;; 		   (puthash file (make-hash-table :test 'equal)
;; 			    u2-cache-data)))
;;       (u2-message 6 "%s %s %s" file key value)
;;       value)))

;; (defun u2-cache-flush-file (method user host file)
;;   "Remove all properties of FILE in the cache context of USER on HOST."
;;   (with-current-buffer (u2-get-buffer method user host)
;; ;    (u2-message 6 "%s" (u2-cache-print u2-cache-data))
;;     (let ((file (directory-file-name file)))
;;       (remhash file u2-cache-data))))

;; (defun u2-cache-flush-directory (method user host directory)
;;   "Remove all properties of DIRECTORY in the cache context of USER on HOST.
;; Remove also properties of all files in subdirectories"
;;   (with-current-buffer (u2-get-buffer method user host)
;; ;    (u2-message 6 "%s" (u2-cache-print u2-cache-data))
;;     (let ((directory (directory-file-name directory)))
;;       (maphash
;;        '(lambda (key value)
;; 	  (when (string-match directory key)
;; 	    (remhash key u2-cache-data)))
;;        u2-cache-data))))

;; (defun u2-cache-flush (method user host)
;;   "Remove all information from the cache context of USER on HOST."
;;   (with-current-buffer (u2-get-buffer method user host)
;; ;    (u2-message 6 "%s" (u2-cache-print u2-cache-data))
;;     (clrhash u2-cache-data)))

;; (defmacro with-cache-data (method user host file key &rest body)
;;   "Check in U2 cache for KEY, otherwise execute BODY and set cache.
;; The cache will be set for absolute FILE names only; otherwise it is
;; not unique."
;;   `(if (file-name-absolute-p ,file)
;;        (let ((value (u2-cache-get-file-property
;; 		     ,method ,user ,host ,file ,key 'undef)))
;; 	 (when (eq value 'undef)
;; 	   ;; We cannot pass ,@body as parameter to
;; 	   ;; `u2-cache-set-file-property' because it mangles our
;; 	   ;; debug messages.
;; 	   (setq value (progn ,@body))
;; 	   (u2-cache-set-file-property
;; 	    ,method ,user ,host ,file ,key value))
;; 	 value)
;;      ,@body))

;; (put 'with-cache-data 'lisp-indent-function 5)
;; (put 'with-cache-data 'edebug-form-spec t)

;; (defun u2-cache-print (table)
;;   "Prints hash table TABLE."
;;   (let (result tmp)
;;     (maphash
;;      '(lambda (key value)
;; 	(setq tmp (format "(%s %s)" key
;; 			  (if (hash-table-p value)
;; 			      (u2-cache-print value)
;; 			    value))
;; 	      result (if result (concat result " " tmp) tmp)))
;;      table)
;;     result))

;; ;; Reverting a buffer should also flush file properties.  They could
;; ;; have been changed outside U2.

;; (defun u2-cache-before-revert-function ()
;;   "Flush all U2 cache properties from buffer-file-name."
;;   (let ((bfn (buffer-file-name))
;; 	;; Pacify byte-compiler.
;; 	method user host localname)
;;     (when (and (stringp bfn)
;; 	       (u2-u2-file-p bfn))
;;       (with-parsed-u2-file-name bfn nil
;; 	(u2-cache-flush-file method user host localname)))))

;; (add-hook 'before-revert-hook 'u2-cache-before-revert-function)


;; (provide 'u2-cache)

;; ;;; u2-cache.el ends here



(defun nilstring (string)
  (or (null string)
      (string= string "")))
(defun non-nilstring (string)
  (and (not (nilstring string)) string))
;;;; ------------------------------------------------------------
;;;; User / Host mapping and caching support.
;;;; ------------------------------------------------------------
;;;; Host information is used in enough different places that it
;;;; should be in a separate library, perhaps. It would be nice
;;;; to unify the ange-ftp/efs, tramp, and others which do this.

(defvar unidata-user-hashtable (make-hash-table :test 'equal))
(defvar unidata-passwd-hashtable (make-hash-table :test 'equal))
(defvar unidata-account-path-hashtable (make-hash-table :test 'equal))
(defvar unidata-hosts nil)

(defun unidata-prompt-for-host ()
  (let ((host)
	(prompt (if unidata-hosts
		    (format "Host(%s):" (car unidata-hosts))
		  "Host:")))
    (prog1
	(setq host
	      (let ((enable-recursive-minibuffers t))
		(read-string prompt nil unidata-hosts)))
      (and (not (string-equal host ""))
	   (progn
	     (delete host unidata-hosts)
	     (push host unidata-hosts))))))

(defun unidata-set-user (host user)
  "For a given HOST, set or change the default USER."
  (interactive "sHost: \nsUser: ")
  (puthash host user  unidata-user-hashtable))

(defun unidata-get-user (host)
  "Given a HOST, return the default USER."
  (let ((user (gethash host unidata-user-hashtable)))
    (or user
	(prog1
	    (setq user
		  (let ((enable-recursive-minibuffers t))
		    (read-string (format "User for %s: " host)
				 (user-login-name))))
	  (unidata-set-user host user)))))



(defun unidata-build-host-path (&optional host)
  (let* ((host (or host unidata-host))
	 (user (unidata-get-user host))
	 (account-path (or unidata-account-path
                           (unidata-get-account-path host))))
    (format "/%s@%s:%s" user host account-path)))

(defun unidata-remote-path-p (path)
  (and path (string-match "/[^@]+@[^:]+:\\(.*\\)" path)))

(defvar unidata-path-splitting-regexp
 "/\\(?:\\(?:\\([^:]+\\):\\)?\\)\\([^@]+\\)@\\([^:]+\\):\\(.*\\)"
"String used to split remote paths for unidata.  When matched against
a string, the following will be the sub-matches:
	1: the connection type, if any (for tramp)
	2: the user name
	3: the remote host
	4: the directory on the remote host.
This string is used by `unidata-split-host-path' to create a list of
these parts, so you should usually call that, rather than using this
directly.")

(defun unidata-split-host-path (path)
  "Split a remote path name into its consituent parts.
These are the host, the user, the directory, and the connection method
(for using with tramp). All but the method name /must/ be in the
string or the extraction will fail.  The path-string should look like
this: `/<method>:<user>@<host>:<remote path>'. The resulting list
looks like this : (<host> <user> <remote directory> <method>)."
  (save-match-data
    (if (string-match  unidata-path-splitting-regexp  path)
        (list 
         (match-string 3 path)
         (match-string 2 path)
         (match-string 4 path)
         (match-string 1 path))
      (list nil nil path))))

(defun unidata-break-account-path (path)
  (nth 2 (unidata-split-host-path path)))

(defun unidata-set-account-path (host account-path)
  "For a given HOST, set or change the default USER."
  (interactive "sHost: \nsAccount Path: ")
  (puthash host account-path  unidata-account-path-hashtable))

(defun unidata-get-account-path (&optional host)
  (interactive)
;;  (or host (setq host unidata-host))
  (or (non-nilstring (gethash host unidata-account-path-hashtable nil))
      (unidata-prompt-for-account-path host)))

(defun unidata-prompt-for-account-path (&optional host)
  "Given a HOST, return the default USER."
  (let* ((account-path (gethash host unidata-account-path-hashtable nil))
	 (default (cond ((non-nilstring account-path)
			 (format "(%s)" account-path))
			(t "")))
	 (prompt (format "Account Path for %s%s: " host default)))
    (prog1
	(setq account-path
	      (let ((enable-recursive-minibuffers t))
		(read-file-name prompt nil account-path t account-path)))
      (if (unidata-remote-path-p account-path)
          (let* ((pl (unidata-split-host-path account-path))
                 (phost (nth 0 pl))
                 (puser (nth 1 pl))
                 (real-path (nth 2 pl)))
            (unidata-set-user phost puser)
            (setq host phost)))
      (unidata-set-account-path host account-path))))

;;;; ------------------------------------------------------------
;;;; Password support.
;;;; ------------------------------------------------------------


(defun unidata-set-passwd (host user passwd)
  "For a given HOST and USER, set or change the associated PASSWORD."
  (interactive (list (read-string "Host: ")
		     (read-string "User: ")
		     (read-passwd "Password: ")))
  (puthash (list host user)
	   passwd
	   unidata-passwd-hashtable))

(defun unidata-get-host-with-passwd (user)
  "Given a USER, return a host we know the password for."
  (catch 'found-one
    (maphash
     (function (lambda (host val)
		 (if (gethash (list host user) unidata-passwd-hashtable)
		     (throw 'found-one host))))
     unidata-user-hashtable)
    (save-match-data
      (maphash
       (function
	(lambda (key value)
	  (if (string-match "^[^/]*\\(/\\).*$" (car key))
	      (let ((host (substring (car key) 0 (match-beginning 1))))
		(if (and (string-equal user (substring (car key) (match-end 1)))
			 value)
		    (throw 'found-one host)))
            (car key))))
       unidata-passwd-hashtable))
    nil))

(defun unidata-get-passwd (host user)
  "Return the password for specified HOST and USER, asking user if necessary."
  ;; look up password in the hash table first; user might have overridden the
  ;; defaults.
  (cond ((gethash (list host user) unidata-passwd-hashtable))
	;; see if same user has logged in to other hosts; if so then prompt
	;; with the password that was used there.
	(t
	 (let* ((other (unidata-get-host-with-passwd user))
		(passwd (if other
                            
			    ;; found another machine with the same user.
			    ;; Try that account.
			    (read-passwd
			     (format "passwd for %s@%s (default same as %s@%s): "
				     user host user other)
			     nil
			     (gethash (list other user) unidata-passwd-hashtable))
                          
			  ;; I give up.  Ask the user for the password.
                          ;; Ask tramp for it if tramp is available.
                          (let ((prompt (format "Password for %s@%s: " user host)))
                            (if nil ;;(functionp 'tramp-read-passwd)
                                (tramp-read-passwd)
                              (read-passwd prompt)))
                          )))
	   (unidata-set-passwd host user passwd)
           passwd))))


(provide 'u2-cache)