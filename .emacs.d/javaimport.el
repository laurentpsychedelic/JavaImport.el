;;; javaimport.el --- This module allows inserting imports in Java and Groovy source code files, based on the current word. The classes to be  mported are scanned in the current project file tree, the standard Java (and Groovy) SDK APIs, and in the libs (JAR files) used in the project.

;; Copyright (C) 2013  Laurent FABRE

;; Author: Laurent FABRE <laurentdev@laurentdev-CF-S10CYBDR>
;; Keywords: 

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

;; 

;;; Code:

(setq javaimport-valid-object-name "[a-zA-Z_][a-zA-Z_0-9]*")
(setq javaimport-package-regexp (concat "package[[:space:]]+\\(" javaimport-valid-object-name "\\([.]" javaimport-valid-object-name "\\)*\\)"))
(setq javaimport-class-regexp (concat "class[[:space:]]+\\(" javaimport-valid-object-name "\\)"))

(defun javaimport-compute-brace-differential (text)
  "Compute the differential of right - left curved braces in given text"
  (let (n-left n-right)
    (setq n-left (javaimport-count-occurences-of "{" text))
    (setq n-right (javaimport-count-occurences-of "}" text))
    (- n-left n-right)))

(defun javaimport-count-occurences-of (pattern text)
  "Count occurence of given pattern (pure text) in given text"
  (with-temp-buffer
    (let ((n 0))
      (setq case-fold-search nil)
      (insert text)
      (goto-char 1)
      (while (re-search-forward (regexp-quote pattern) nil t)
        (setq n (1+ n)))
      n)))

(defun javaimport-combine-sub-class-with-parent-class (parent-class sub-class offset)
  "Combine class names depending on their inclusion relation defined by offset.
 If sub-class is an internal class directly defined within parent-class offset is equal to 1
 (parent class name +1) and 'parent-class.sub-class' is returned (ex: from 'InternalClass'
 defined within 'ParentClass' class, 'ParentClass.InternalClass'). 
If the offset is 0, their is no parent/internal class relation (they are at the same level), 
thus sub-class is returned as it. For negative offsets, sub-class is actually at a higher
 level than parent-class. The according number of level is deleted from parent-class and 
sub-class is added at the end (ex: with 'ParentClass.InternalClass' and 'AnOtherClass' with 
offset=-1, 'AnOtherClass' is returned"
  (if parent-class
      (let (parent-eles npop)
        (setq parent-eles (split-string parent-class "[.]")) ; elements of the parent class name
        (setq npop (1+ (- offset))) ; number of elements to pop from parent class before appending sub-class elements
        (mapc (lambda (ele) (pop parent-eles)) (make-list npop 1))
        (setq sub-class (mapconcat 'identity (append parent-eles (cons sub-class '())) "."))
        ))
  sub-class)

(defun javaimport-scan-package-in-source (source-code)
  "Scan source and return the package declared inside"
  (string-match javaimport-package-regexp source-code)
  (match-string 1 source-code))

(defun javaimport-test-scan-defined-classes-in-source ()
  "Test..."
  (interactive)
  (javaimport-scan-defined-classes-in-source (buffer-string)))
(defun javaimport-scan-defined-classes-in-source (source-code)
  "Scan source code and return a list of the classes defined within it"
  (with-temp-buffer
    (let ((package "") (class-list ()) (curr-class nil) (last-class nil) (class-offset 0) (curr-point 1) (last-point 1) (interval-text ""))
      (insert source-code)
      (goto-char 0)
      (setq case-fold-search nil)
      (setq package (javaimport-scan-package-in-source source-code))
      (while (setq curr-point (re-search-forward javaimport-class-regexp nil t))
        (setq curr-class (match-string 1))
        (setq interval-text (substring source-code last-point curr-point))
        (setq class-offset (javaimport-compute-brace-differential interval-text))
        (setq curr-class (javaimport-combine-sub-class-with-parent-class last-class curr-class class-offset))
        (setq class-list (append (cons curr-class '()) class-list))
        (setq last-class curr-class)
        (setq last-point curr-point))
      (setq class-list (mapcar (lambda (ele) (if package (concat package "." ele) ele)) class-list))
      class-list)))


(provide 'javaimport)
;;; javaimport.el ends here

