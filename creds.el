;;; creds.el --- a simple parser credential file lib

;; Copyright (C) 2013
;;   Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t@gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.4
;; Package-Requires: ((dash "2.5.0"))
;; Keywords: credentials
;; URL: https://github.com/ardumont/emacs-creds

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; A small library (non optimal) to deal with more entries than just credentials
;; (The search is linear so not optimal)

;; Here is an example of .authinfo
;; machine machine0 port http login nouser password nopass
;; machine machine1 login some-login password some-pwd port 993
;; machine machine2 login some-login port 587 password some-pwd
;; machine jabber         login some-login password some-pwd
;; machine description    name "my name is" blog some-blog mail some-mail

(require 'dash)

(defun creds/read-lines (filepath)
  "Return a list of lines from a file."
  (with-temp-buffer
    (insert-file-contents filepath)
    (--map (split-string it "[ ]+") (split-string (buffer-string) "\n" t))))

(defun creds/get-with (data key-value-pairs)
  "Return the data list for the list key-value-pairs of associations."
  (when data
    (let ((entry-line-as-alist (-partition 2 (car data))))
      (if (->> key-value-pairs
            (mapcar (lambda (key-value)
                      (let ((key   (car key-value))
                            (value (cdr key-value)))
                        (string= value (car (assoc-default key entry-line-as-alist))))))
            (--all? (equal t it)))
          (--mapcat it entry-line-as-alist)
        (creds/get-with (cdr data) key-value-pairs)))))

(defun creds/get (data entry-name)
  "Return the data list for the line entry-name"
  (creds/get-with data `(("machine" . ,entry-name))))

(defun creds/get-entry (data entry-key)
  "Given a data list, return the entry in that list"
  (when data
    (let ((data-as-alist (-partition 2 data)))
      (-when-let (value (assoc-default entry-key data-as-alist))
        (car value)))))

(provide 'creds)

;;; creds.el ends here
