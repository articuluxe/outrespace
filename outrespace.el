;;; outrespace.el --- c++ namespace utility functions
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Wednesday, June  1, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-06-16 07:52:53 dharms>
;; Modified by: Dan Harms
;; Keywords: c++ namespace

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

(make-variable-buffer-local
 (defvar outre-list
   '()
   "List of namespaces in the current buffer."))

(defgroup outrespace-mode nil
  "Namespacer mode."
  :group 'outrespace)

(defface outre-highlight-face
  '((t (:foreground "wheat" :background "cadetblue4" :bold t)))
  "Font lock mode face used to highlight namespace names."
  :group 'outrespace-mode)

(defun outre-in-comment-or-string ()
  "Return t if point is within a comment or string."
  (or (nth 3 (syntax-ppss))
      (nth 4 (syntax-ppss))))

(defun outre-not-in-comment-or-string ()
  "Return t if point is not within a comment or string."
  (not (outre-in-comment-or-string)))

(defun outre--move-point-to-ns (ns)
  "Move point to namespace NS."
  (goto-char (car (outre--get-ns-delimiter-pos ns))))

(defun outre--on-namespace-selected (ns)
  "Select a namespace."
  (let ((name (outre--get-ns-name-pos ns))
        beg end ov)
    (outre--move-point-to-ns ns)
    (if name
        (progn
          (setq beg (car name))
          (setq end (cadr name))
          (setq ov (make-overlay beg end))
          (overlay-put ov 'face 'outre-highlight-face)
          (sit-for 1)
          (delete-overlay ov)))
    ))

(defun outre--scan-all-ns ()
  "Scan current buffer for all namespaces."
  (save-excursion
    (widen)
    (let ((lst '()) beg curr cont parent)
      (goto-char (point-min))
      (while (setq beg (outre--find-ns-next))
        (setq cont
              (if (and
                   beg
                   (setq
                    parent
                    (catch 'found
                      (seq-doseq
                          (elt lst)
                        (if (outre--get-distance-from-begin beg elt)
                            (throw 'found elt) nil)))))
                  (cadr (outre--get-ns-names parent))
                nil))
        (setq curr (outre-parse-namespace beg cont))
        (if (outre--namespace-nested-p curr)
            (let* ((name-pos (outre--get-ns-name-pos curr))
                   (posb (car name-pos))
                   (pose (cadr name-pos)))
              (mapc
               (lambda (elt)
                 (setq lst
                       (cons
                        (list
                         (list (car elt)
                               (setq cont
                                     (if cont
                                         (concat cont "::" (car elt))
                                       (car elt))))
                         (outre--get-ns-tag-pos curr)
                         (list
                          (+ posb (cdr elt))
                          (+ posb (cdr elt) (length (car elt))))
                         (outre--get-ns-delimiter-pos curr))
                        lst)))
               (outre--flatten-nested-names
                (buffer-substring-no-properties posb pose))))
          (setq lst (cons curr lst))))
      lst)))

(defun outre-scan-buffer ()
  "Scan current buffer for all namespaces.
Store in result `outre-list'."
  (interactive)
  (let ((start (current-time)))
    (setq outre-list (outre--scan-all-ns))
    (message "It took %.3f to scan buffer's namespaces"
             (float-time (time-subtract (current-time) start)))))

;;;###autoload
(defun outre-goto-namespace-next ()
  "Move point to the next start of a valid namespace."
  (interactive)
  (let (ns)
    (when (catch 'found
            (save-excursion
              (let ((beg (outre--find-ns-next)))
                (setq ns (and beg (outre-parse-namespace beg)))
                (if ns
                    (throw 'found t)
                  (message "no namespace following point")
                  nil))))
      (outre--on-namespace-selected ns))))
        ;; (progn
        ;;   (setq ns
        ;;   (goto-char (car (nth 2 ns)))
        ;;   (set-mark-command nil)
        ;;   (goto-char (cadr (nth 2 ns)))
        ;;   (setq deactivate-mark nil)
        ;;   )

;;;###autoload
(defun outre-goto-namespace-previous ()
  "Move point to the prior start of a valid namespace."
  (interactive)
  (let ((pt (point))
        ns)
    (when (catch 'found
            (save-excursion
              (let ((beg (outre--find-ns-previous))
                    delim)
                (setq ns (and beg (outre-parse-namespace beg)))
                (setq delim (and ns (outre--get-ns-delimiter-pos ns)))
                ;; if between delimiters, choose current
                ;; if outside (before) delimiters, search for previous
                ;; this seems to work intuitively; but relies on point
                ;; being before the beginning delimiter when a namespace
                ;; is selected
                (when delim
                  (unless (< (car delim) pt)
                    (setq beg (outre--find-ns-previous))
                    (setq ns (and beg (outre-parse-namespace beg)))))
                (if ns
                    (throw 'found t)
                  (message "no namespace preceding point")
                  nil))))
      (outre--on-namespace-selected ns))))

(defun outre--find-ns-next ()
  "Return location of beginning of next valid namespace."
  (catch 'found
    (while (search-forward-regexp "\\_<namespace\\_>" nil t)
      (and (outre-not-in-comment-or-string)
           (throw 'found (match-beginning 0))))))

(defun outre--find-ns-previous ()
  "Return location of beginning of previous valid namespace."
  (catch 'found
    (while (search-backward-regexp "\\_<namespace\\_>" nil t)
      (and (outre-not-in-comment-or-string)
           (throw 'found (match-beginning 0))))))

(defun outre--at-ns-begin-p (loc)
  "Evaluate whether point is at the beginning of a namespace."
  (looking-at "namespace"))

(defun outre--namespace-nested-p (ns)
  "Return whether the namespace NS is a nested namespace (C++17)."
  (string-match-p "::" (car (outre--get-ns-names ns))))

(defun outre--flatten-nested-names (tags)
  "Flatten the input TAGS into a list of nested tags.
Useful for C++17's nested namespaces.  The result is a list of
cons cells, each of which is of the form: `(tag . pos)', where
tag is the individual tag, and pos is its position in the input.
The resultant list may have only one element."
  (let ((pos 0)
        (res '())
        next)
    (while (setq next (string-match-p "::" tags pos))
      (setq res (cons
                 (cons (substring tags pos next) pos)
                 res))
      (setq pos (+ 2 next)))
    ;; add on the final tag
    (when (substring tags pos)
      (setq res (cons
                 (cons
                  (substring tags pos) pos) res)))
    (nreverse res)))

(defun outre--get-ns-names (ns)
  "Return the list '(name full-name) of NS."
  (nth 0 ns))

(defun outre--get-ns-tag-pos (ns)
  "Return the list '(beg end) of the `namespace' tag of NS."
  (nth 1 ns))

(defun outre--get-ns-name-pos (ns)
  "Return the list '(beg end) of the name, if any, of NS."
  (nth 2 ns))

(defun outre--get-ns-delimiter-pos (ns)
  "Return the list '(beg end) of the scope {} of NS."
  (nth 3 ns))

(defun outre-parse-namespace (loc &optional parent)
  "Parse the namespace starting at LOC.
PARENT contains any enclosing namespaces."
  (save-excursion
    (let (tag-pos name-pos delimiter-pos title beg end)
      (goto-char loc)
      (unless (outre--at-ns-begin-p loc)
        (error "not looking at valid namespace"))
      (setq beg (point))
      (forward-sexp)
      (setq end (point))
      (setq tag-pos (list beg end))
      (unless (search-forward-regexp "\\s-+\\(.+?\\)\\s-+\\({\\)" nil t)
        (error "error parsing namespace"))
      (setq title (match-string-no-properties 1))
      (when title
        (setq name-pos (list (match-beginning 1) (match-end 1))))
      (unless (match-string 2)
        (error "error parsing namespace \"{\""))
      (goto-char (match-beginning 2))
      (setq beg (point))
      (forward-list)
      (setq end (point))
      (setq delimiter-pos (list beg end))
      (list (list title (if parent
                            (concat parent "::" title)
                          title))
            tag-pos name-pos delimiter-pos))))

(defun outre--get-full-extant (ns)
  "Return the bounds (beg . end) of the full namespace extent in NS."
  (cons (car (outre--get-ns-tag-pos ns))
        (cadr (outre--get-ns-delimiter-pos ns))))

(defun outre--get-distance-from-begin (pos ns)
  "Return the distance of a point POS from start of namespace in NS.
If POS is before or after the namespace bounds, return nil."
  (let ((extant (outre--get-full-extant ns)))
    (if (and (<= (car extant) pos) (>= (cdr extant) pos))
        (- pos (car extant))
      nil)))

(defun outre--collect-namespaces-around-pos (pos lst)
  "Return a list of all namespaces in LST which surround POS."
  (seq-filter (lambda (ns)
                (outre--get-distance-from-begin pos ns))
              lst))

;; (defun outre--sort-namespaces-by-distance (pos lst)
;;   "Sort a list of namespaces in LST by the distance to POS."
;;   (sort lst
;;         (lambda (lhs rhs)
;;           ;; todo
;;           )))

(defun outre--find-enclosing-ns ()
  "Return the namespace around point, if any."
  (save-excursion
    (let* ((pt (point))
           (beg (outre--find-ns-previous))
           (ns (and beg (outre-parse-namespace beg))))
      (and ns (outre--get-distance-from-begin pt ns) ns))))

(defun outre-change-enclosing-ns-name ()
  "Change the name of the enclosing namespace, if one exists."
  (interactive)
  (let ((ns (outre--find-enclosing-ns))
        start end name)
    (if ns
        (progn
          (setq name
                (read-string
                 (concat "Change namespace "
                         (car (outre--get-ns-names ns))
                         " to: ")))
          (setq start (car (outre--get-ns-name-pos ns)))
          (setq end (cadr (outre--get-ns-name-pos ns)))
          (delete-region start end)
          (goto-char start)
          (insert name)
          ;; todo: change comment after ending delimiter
          )
      (message "No enclosing namespace"))))

(defun outre-jump-to-ns (ns)
  "Jump to the beginning of namespace NAME."
  (interactive)
  (when ns
    (outre--on-namespace-selected ns)))

(defun outre--find-ns-by-name (name)
  "Return the namespace matching NAME."
  (seq-find (lambda(elt)
              (string-equal name (cadr (outre--get-ns-names elt))))
            outre-list))

(defun outre-ivy-jump-to-ns ()
  "Jump to a namespace in current buffer, using ivy to select."
  (interactive)
  (outre-scan-buffer)
  (let ((lst (mapcar
              (lambda(elt)
                (cadr (outre--get-ns-names elt)))
              outre-list))
        name)
    (setq name
          (ivy-read "Namespace: " (nreverse lst)
                    :re-builder #'ivy--regex
                    :sort nil
                    :initial-input nil))
    (when name
      (outre-jump-to-ns (outre--find-ns-by-name name))
      (message "Jumped to namespace %s" name)
      )))

;; namespace
(defvar c-basic-offset)
(defun outre-wrap-namespace-region (start end name)
  "Insert enclosing namespace brackets around a region."
  (interactive "r\nsEnter the namespace name (leave blank for anonymous): ")
    (save-excursion
      (goto-char end) (insert "\n}")
      (insert-char ?\s c-basic-offset)
      (insert "// end ")
      (if (zerop (length name))
          (insert "anonymous "))
      (insert "namespace " name "\n")
      (goto-char start)
      (insert "namespace ")
      (if (not (zerop (length name)))
          (insert name " "))
      (insert "{\n\n")))

(defvar outrespace-mode-map (make-sparse-keymap)
  "Keymap for `outre-mode'.")

(defun outre-define-keys ()
  "Define keys for `outre-mode'."
  (define-key outrespace-mode-map "\M-p" 'outre-goto-namespace-previous)
  (define-key outrespace-mode-map "\M-n" 'outre-goto-namespace-next)
  )

(define-minor-mode outrespace-mode
  "Helper for c++ namespaces."
  :init-value nil
  :lighter " NS"
  :keymap outrespace-mode-map
  (if outrespace-mode
      (outre-define-keys)
    t))

(provide 'outrespace)
;;; outrespace.el ends here
