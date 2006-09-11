;;; u2-piclan.el --- Functions specific to using the Piclan system under U2.

;; Copyright (C) 2006  Free Software Foundation, Inc.

;; Author: Tim Schaeffer <numeromancer@users.sourceforge.net>
;; Keywords: processes, terminals, comm
;; Time-stamp: <2006-09-01 14:39:41 timothy>
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:
(require 'unibasic-compile)

(define-unidata-record-action plz "PLZ %s %s")

(defun u2-plz ()
  (interactive)
  (unidata-command-on-buffer 'plz (current-buffer)))


(when unibasic-mode-map
    (define-key unibasic-mode-map "\C-cz"    'u2-plz))


(provide 'u2-piclan)
;;; u2-piclan.el ends here

;; Local variables:
;;   mode: emacs-lisp
;;   auto-save-interval: 1000
;;   indent-tabs-mode: nil
;; End:
