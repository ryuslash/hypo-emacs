;;; hypo.el --- Client functions for hypo            -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: tools
;; Version: 0.1.0

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

;; Some functions to interact with hypo
;; (http://projects.ryuslash.org/hypo/).

;;; Code:

(autoload 'beginning-of-sexp "thingatpt")
(autoload 'end-of-sexp "thingatpt")

(defun hypo--collect-url ()
  "Collect the returned url."
  (let* ((start (search-forward "\n\n"))
         (end (1- (search-forward "\n"))))
    (copy-region-as-kill start end)))

;;;### autoload
(defun hypo-region (start end filename)
  "Send the region START up to END to hypo as FILENAME."
  (interactive
   (list (region-beginning) (region-end)
         (let ((fname (buffer-file-name)))
           (if fname
               (concat
                "region-" (file-name-nondirectory (buffer-file-name)))
             (read-string "Filename: " (buffer-name))))))
  (let ((url-request-data (buffer-substring-no-properties start end))
        (url-request-method "PUT"))
    (url-retrieve (concat "https://ryuslash.org/hypo/" filename)
                  (lambda (status) (hypo--collect-url) (kill-buffer)))))

;;;###autoload
(defun hypo-buffer (buffer filename)
  "Send BUFFER to hypo as FILENAME."
  (interactive
   (list (current-buffer)
         (let ((fname (buffer-file-name)))
           (if fname
               (concat (file-name-nondirectory (buffer-file-name)))
             (read-string "Filename: " (buffer-name))))))
  (with-current-buffer buffer
    (hypo-region (point-min) (point-max) filename)))

;;;###autoload
(defun hypo-defun (filename)
  "Send the function at point to hypo as FILENAME."
  (interactive (list (or (concat (add-log-current-defun)
                                 (file-name-extension
                                  (or (buffer-file-name) "") t))
                         (read-string "Filename: " (buffer-name)))))
  (save-excursion
   (hypo-region (progn (beginning-of-defun) (point))
                (progn (end-of-defun) (point))
                filename)))

;;;###autoload
(defun hypo-last-sexp (filename)
  "Send the sexp at point to hypo as FILENAME."
  (interactive (list (read-string
                      "Filename: "
                      (cons (file-name-extension
                             (or (buffer-file-name) "") t) 0))))
  (save-excursion
    (hypo-region (progn (beginning-of-sexp) (point))
                 (progn (end-of-sexp) (point))
                 filename)))

(provide 'hypo)
;;; hypo.el ends here
