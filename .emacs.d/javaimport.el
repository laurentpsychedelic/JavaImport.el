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

(setq javaimport-valid-object-name-regexp "[a-zA-Z_][a-zA-Z_0-9]*")
(setq javaimport-package-regexp (concat "package[[:space:]]+\\(" javaimport-valid-object-name-regexp "\\([.]" javaimport-valid-object-name-regexp "\\)*\\)"))
(setq javaimport-class-regexp (concat "\\(\\(public\\)[[:space:]]+\\)?\\(\\(class\\)\\|\\(interface\\)\\)[[:space:]]+\\(" javaimport-valid-object-name-regexp "\\)"))
(setq javaimport-class-html-path-regexp (concat "a href=\"\\(" javaimport-valid-object-name-regexp "\\([/]"  javaimport-valid-object-name-regexp "\\)*[.]html\\)\""))
(setq javaimport-class-regexp-class-name-index 6)
(setq javaimport-class-regexp-class-access-modifier-index 2)
(setq javaimport-class-html-path-regexp-fqn-index 1)
;; This is the list of functions providing a list a classes by different means
(setq javaimport-class-providers (list 'javaimport-get-all-classes-defined-in-dir-sources 'javaimport-get-all-classes-defined-in-dir-jars 'javaimport-get-all-classes-defined-in-html-files))

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
      (beginning-of-buffer)
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
        (setq sub-class (mapconcat 'identity (append parent-eles (cons sub-class '())) "."))))
  sub-class)

(defun javaimport-scan-package-in-source (source-code)
  "Scan source and return the package declared inside"
  (string-match javaimport-package-regexp source-code)
  (match-string 1 source-code))

(defun javaimport-remove-all-comments-in-buffer ()
  "Remove all comments in current buffer"
  (interactive)
  (beginning-of-buffer)
  (while ; // comments
      (re-search-forward (regexp-quote "//") nil t)
    (goto-char (match-beginning 0))
    (kill-line))
  (beginning-of-buffer)
  (while ; /* ... */ comments
      (re-search-forward (regexp-quote "/*") nil t)
    (goto-char (match-beginning 0))
    (push-mark-command nil)
    (re-search-forward (regexp-quote "*/"))
    (kill-region (point) (mark))))

(defun javaimport-remove-all-string-litterals-in-buffer ()
  "Remove all comments in current buffer"
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward (regexp-quote "[^\\][\"]") nil t)
    (goto-char (match-beginning 0))
    (push-mark-command nil)
    (forward-char)
    (re-search-forward (regexp-quote "\""))
    (kill-region (point) (mark))))

(defun javaimport-get-all-files-with-matching-extension (extension dir)
  "Get all files with matching extension in specified directory"
  (split-string (shell-command-to-string (concat "find " dir " -iname \\*." extension))))
; (javaimport-get-all-files-with-matching-extension "groovy" "/home/laurentdev/dev/SE-View_101.git/")

; (message (format "All detected classes: %s" (javaimport-get-all-classes-defined-in-dir-sources "/home/laurentdev/dev/SE-View_101.git/" "LineAnalysisChart")))
(defun javaimport-get-all-classes-defined-in-dir-sources (dir &optional token)
  "Get the list of all classes defined in the source files in the given directory"
  (let ((class-list ()) (file-list ()))
    (mapc (lambda (extension) (setq file-list (append (javaimport-get-all-files-with-matching-extension extension dir) file-list)))
          (list "java" "groovy"))
    (mapc (lambda (filepath) (setq class-list (append (javaimport-scan-defined-classes-in-source (javaimport-get-file-contents filepath) token) class-list)))
          file-list)
    class-list))

(require 'arc-mode)

; (javaimport-get-all-classes-defined-in-dir-jars "/home/laurentdev/dev/SE-View_101.git" "Copyable")
(defun javaimport-get-all-classes-defined-in-dir-jars (dir &optional token)
  "Get the list of all classes defined in the JAR files in the given directory"
  (let ((class-list ()) (file-list ()))
    (mapc (lambda (extension) (setq file-list (append (javaimport-get-all-files-with-matching-extension extension dir) file-list)))
          (list "jar"))
    (setq file-list (remove-duplicates file-list :test (lambda (a b) (string= (file-name-nondirectory a) (file-name-nondirectory b)))))
    (mapc (lambda (filepath) (setq class-list (append (javaimport-scan-defined-classes-in-jarfile filepath token) class-list)))
          file-list)
    class-list))

;(message (format "Classes in JAR: %s" (javaimport-scan-defined-classes-in-jarfile "/home/laurentdev/dev/SE-View_101.git/backend/dist/SE-View_101_backend.jar")))
(defun javaimport-scan-defined-classes-in-jarfile (jarfile-path &optional token)
  "Scan and return all the classes defined in JAR file"
  (with-temp-buffer
    (let ((classes ()) (archive-files ()))
      (insert (javaimport-get-file-contents jarfile-path))
      (setq archive-files (funcall 'archive-zip-summarize))
      (setq archive-files (mapcar (lambda (ele) (elt ele 0)) archive-files)) 
      (mapc (lambda (ele)
              (when (and ele 
                         (not (string-match "\\([$][0-9]+\\)+" ele))
                         (not (string-match "META-INF" ele)))
                (setq ele (replace-regexp-in-string "[.]class$" "" ele))
                (setq ele (replace-regexp-in-string "\\([/]\\|[$]\\)" "." ele))
                (if (or (not token) (javaimport-token-matches-class-fqn ele token))
                    (add-to-list 'classes (list ele nil)))))
            archive-files)
      classes)))

(defun javaimport-get-file-contents (filepath)
  "Get file contents as a string"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

; (message (format "Classes: %s" (javaimport-scan-defined-classes-in-source (buffer-string)))))
(defun javaimport-scan-defined-classes-in-source (source-code &optional token)
  "Scan source code and return a list of the classes defined within it"
  (with-temp-buffer
    (let ((package "") (class-list ()) (access-modifier) (curr-class nil) (last-class nil) (class-offset 0) (curr-point 1) (last-point 1) (interval-text ""))
      (insert source-code)
      (javaimport-remove-all-comments-in-buffer)
      (javaimport-remove-all-string-litterals-in-buffer)
      (beginning-of-buffer)
      (setq case-fold-search nil)
      (setq package (javaimport-scan-package-in-source source-code))
      (while (setq curr-point (re-search-forward javaimport-class-regexp nil t))
        (setq curr-class (match-string javaimport-class-regexp-class-name-index))
        (setq access-modifier (match-string javaimport-class-regexp-class-access-modifier-index))
        (setq interval-text (substring (buffer-string) last-point curr-point))
        (setq class-offset (javaimport-compute-brace-differential interval-text))
        (setq curr-class (javaimport-combine-sub-class-with-parent-class last-class curr-class class-offset))
        (if (or (not token) (javaimport-token-matches-class-fqn curr-class token))
            (add-to-list 'class-list (list curr-class (if access-modifier access-modifier "package-private"))))
        (setq last-class curr-class)
        (setq last-point curr-point))
      (setq class-list (mapcar (lambda (ele) (if package (list (concat package "." (car ele)) (car (nreverse ele))) ele)) class-list))
      class-list)))

(setq javaimport-class-html-provider-files (list "~/.emacs.d/java-doc/allclasses-noframe.html"))
; (message (format "Classes in HTML docs: %s" (javaimport-get-all-classes-defined-in-html-files "ArrayList")))
(defun javaimport-get-all-classes-defined-in-html-files (dir &optional token)
  "Scan and return the list of all classes defined in the HTML documentation files (the dir argument is ignored!"
  (let ((class-list ()))
    (mapc (lambda (file) (setq class-list (append (javaimport-scan-defined-classes-in-html (javaimport-get-file-contents file) token) class-list))) javaimport-class-html-provider-files)
    class-list))
          
; (message (format "Classes in HTML: %s" (javaimport-scan-defined-classes-in-html (javaimport-get-file-contents "~/settings/.emacs.d/java-doc/allclasses-noframe.html"))))
(defun javaimport-scan-defined-classes-in-html (html-source &optional token)
  "Scan HTML source and return a list of classes linked inside (ex. JDK7 allclasses-noframe.html)"
  (with-temp-buffer
    (let ((class-list ()) (class ""))
      (setq case-fold-search t)
      (insert html-source)
      (beginning-of-buffer)
      (while (re-search-forward javaimport-class-html-path-regexp nil t)
        (setq class (replace-regexp-in-string "[.]html$" "" (replace-regexp-in-string "[/]" "." (match-string javaimport-class-html-path-regexp-fqn-index))))
        (if (or (not token) (javaimport-token-matches-class-fqn class token))
            (add-to-list 'class-list (list class nil))))
      class-list)))

; (if (javaimport-token-matches-class-fqn "i.am.a.very.cool.java.library.VeryHandySupportClass" "VeryHandySupportClass") (message "Matched!") (message "Nope..."))
(defun javaimport-token-matches-class-fqn (class-name token)
  "Return whether the token passed as arguments matches the class name (fully qualified name) passed as argument"
  (or (string-match (concat "^" token "$") class-name) (string-match (concat "[.]" token "$") class-name)))

; (message (format "All detected classes: %s" (javaimport-get-all-classes-defined-in-dir "/home/laurentdev/dev/SE-View_101.git/")))
(defun javaimport-get-all-classes-defined-in-dir (dir &optional token)
  "Get the list of all classes defined in the given directory from various sources (source file, JARs, ...)"
    (let ((class-list ()))
      (mapc (lambda (provider) (setq class-list (append (funcall provider dir token) class-list))) javaimport-class-providers)
      class-list))

(defun javaimport-get-all-classes-define-in-parent-project (token)
  "Get the list of all classes defined in current project directory tree from various sources (source file, JARs, ...)"
  (javaimport-get-all-classes-defined-in-dir (javaimport-get-project-root) token))

(setq javaimport-build-file-default-patterns (list "[Bb]uild[.]xml" ".+[.]pom" ".+[.]gradle" "src"))
(setq javaimport-file-extension-to-build-file-patterns-association-table
     (list 
      (append '("java") javaimport-build-file-default-patterns)
      (append '("groovy") javaimport-build-file-default-patterns)))

; (javaimport-get-project-root)
(defun javaimport-get-project-root ()
  "Get project root"
  (let ((directory nil)
        (start-dir (file-name-directory (buffer-file-name)))
        (extension (file-name-extension (file-name-nondirectory (buffer-file-name))))
        (patterns ()))
    (setq patterns (cdr (assoc extension javaimport-file-extension-to-build-file-patterns-association-table)))
    (if (not patterns)
        (setq patterns javaimport-build-file-default-patterns))
    (catch 'break
      (mapc 
       (lambda (pattern) 
         (setq directory (javaimport-locate-parent-directory-with-given-file-pattern start-dir pattern))
         (if directory (throw 'break directory)))
       patterns))
    directory))

(defun javaimport-locate-parent-directory-with-given-file-pattern (start-dir pattern)
  "Locate a directory in parent tree containing a file with given pattern and return its path, or nil if not found"
  (locate-dominating-file start-dir (lambda (dir) (directory-files dir nil pattern))))


(provide 'javaimport)
;;; javaimport.el ends here
