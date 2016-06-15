;;; outrespace.el --- c++ namespace utility functions
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Wednesday, June  1, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-06-15 07:55:03 dharms>
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

(defun outre--on-namespace-selected (props)
  "Select a namespace."
  (let ((name (nth 2 props))
        beg end ov)
    (goto-char (car (nth 3 props)))
    (if name
        (progn
          (setq beg (car name))
          (setq end (cadr name))
          (setq ov (make-overlay beg end))
          (overlay-put ov 'face 'outre-highlight-face)
          (sit-for 2)
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
                (cadr (nth 0 parent))
              nil))
      (setq curr (outre-parse-namespace beg cont))
      (setq lst (cons curr lst)))
    lst)))

(defun outre-scan-buffer ()
  "Scan current buffer for all namespaces.
Store in result `outre-list'."
  (interactive)
  (setq outre-list (outre--scan-all-ns)))

;;;###autoload
(defun outre-find-namespace-next ()
  "Find the next start of a valid namespace."
  (interactive)
  (let* ((beg (outre--find-ns-next))
         (lst (and beg (outre-parse-namespace beg))))
    (if lst
        (outre--on-namespace-selected lst)
        ;; (progn
        ;;   (setq lst
        ;;   (goto-char (car (nth 2 lst)))
        ;;   (set-mark-command nil)
        ;;   (goto-char (cadr (nth 2 lst)))
        ;;   (setq deactivate-mark nil)
        ;;   )
      (message "no namespace following point")
      )))

;;;###autoload
(defun outre-find-namespace-previous ()
  "Find the prior start of a valid namespace."
  (interactive)
  ;; todo: must traverse behind current namespace
  (let* ((beg (outre--find-ns-previous)) lst)
    (if beg
        (progn
          (setq lst (outre-parse-namespace beg))
          (goto-char (car (nth 2 lst)))
          (set-mark-command nil)
          (goto-char (cadr (nth 2 lst)))
          (setq deactivate-mark nil)
          )
      (message "no namespace following point")
      )))

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

;;;###autoload
(defun outre-parse-namespace (loc &optional parent)
  "Parse the namespace starting at LOC."
  (interactive)
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

(defun outre--get-full-extant (props)
  "Return the bounds (beg . end) of the full namespace extent in PROPS."
  (cons (car (nth 1 props)) (cadr (nth 3 props))))

(defun outre--get-distance-from-begin (pos props)
  "Return the distance of a point POS from start of namespace in PROPS.
If POS is before or after the namespace bounds, return nil."
  (let ((extant (outre--get-full-extant props)))
    (if (and (<= (car extant) pos) (>= (cdr extant) pos))
        (- pos (car extant))
      nil)))

(defun outre--collect-namespaces-around-pos (pos lst)
  "Return a list of all namespaces in LST which surround POS."
  (seq-filter (lambda (props)
                (outre--get-distance-from-begin pos props))
              lst))

(defun outre--sort-namespaces-by-distance (pos lst)
  "Sort a list of namespaces in LST by the distance to POS."
  (sort lst
        (lambda (lhs rhs)
          ;; todo

          )))

(defvar outrespace-mode-map (make-sparse-keymap)
  "Keymap for `outre-mode'.")

(defun outre-define-keys ()
  "Define keys for `outre-mode'."
  (define-key outrespace-mode-map "\M-p" 'outre-find-namespace-previous)
  (define-key outrespace-mode-map "\M-n" 'outre-find-namespace-next)
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
