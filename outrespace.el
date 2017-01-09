;;; outrespace.el --- c++ namespace utility functions
;; Copyright (C) 2016-2017  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Wednesday, June  1, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-01-09 08:03:42 dharms>
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

(defcustom outre-prefix (kbd "C-c n")
  "Prefix key for outrespace-mode."
  :group 'outrespace
  :type 'vector
  )

(defface outre-highlight-face
  '((t :inherit highlight :bold t))
;  '((t (:foreground "wheat" :background "cadetblue4" :bold t)))
  "Font lock mode face used to highlight namespace names."
  :group 'outrespace-mode)

(defvar outre-anon-name "<anon>"
  "A display name for anonymous namespaces.")

(defvar outre-debug nil
  "Whether to print debugging statistics to the `*Messages*' buffer.")

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
        beg end str ov)
    (outre--move-point-to-ns ns)
    (setq beg (car name))
    (setq end (cadr name))
    (setq str (buffer-substring-no-properties beg end))
    (when str
      (setq ov (make-overlay beg end))
      (overlay-put ov 'face 'outre-highlight-face)
      (sit-for 1)
      (delete-overlay ov))
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
    (when outre-debug
      (message "It took %.3f sec. to scan buffer's namespaces"
               (float-time (time-subtract (current-time) start))))))

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
                ;; If between delimiters, choose current.
                ;; If outside (before) delimiters, search for previous.
                ;; This seems to work intuitively; but relies on point
                ;; being before the beginning delimiter when a namespace
                ;; is selected.
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
  "Evaluate whether location LOC is at the beginning of a namespace."
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
    (let (tag-pos name-pos delimiter-pos title beg end title-trimmed)
      (goto-char loc)
      (unless (outre--at-ns-begin-p loc)
        (error "not looking at valid namespace"))
      ;; get bounds of namespace tag
      (setq beg (point))
      (forward-sexp)
      (setq end (point))
      (setq tag-pos (list beg end))
      (unless
          (search-forward-regexp
           (concat
            ;; whitespace (including newline)
            "\\(?:\\s-\\|\n\\)+"
            ;; optional namespace name followed by whitespace
            "\\([A-Za-z0-9:_]+\\(\\s-\\|\n\\)*\\)?"
            ;; opening brace (excluding surrounding whitespace)
            "\\(?:\\s-*\\|\n\\)*\\({\\)")
           nil t)
        (error "Error parsing namespace"))
      ;; get bounds of opening delimiter `{'
      (goto-char (match-beginning 2))
      (setq beg (point))
      (forward-list)
      (setq end (point))
      (setq delimiter-pos (list beg end))
      ;; get bounds of name, if any exists
      (setq title (match-string-no-properties 1)) ;may have whitespace
      (setq beg (match-beginning 1))
      (setq end (match-end 1))
      ;; note string-trim alters match-data
      (if (and title (setq title-trimmed (string-trim title)))
          (setq name-pos (list beg (+ beg (length title-trimmed))))
        (setq name-pos (list (1+ (cadr tag-pos)) (1+ (cadr tag-pos))))
        (setq title-trimmed outre-anon-name))
      (list (list title-trimmed (if parent
                            (concat parent "::" title-trimmed)
                          title-trimmed))
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

(defun outre--sort-namespaces-by-distance (pos lst)
  "Sort a list of namespaces in LST by the distance to POS."
  (sort lst
        (lambda (lhs rhs)
          (< (outre--get-distance-from-begin pos lhs)
             (outre--get-distance-from-begin pos rhs))
          )))

(defun outre--find-enclosing-ns-manual ()
  "Return the namespace around point, if any.
This scans the buffer ad-hoc, not using the results already
stored in `outre-list', if any."
  (save-excursion
    (catch 'found
      (let* ((pt (point))
             (beg (outre--find-ns-previous))
             (ns (and beg (outre-parse-namespace beg))))
        (while ns
          (when (outre--get-distance-from-begin pt ns)
            (throw 'found ns))
          (setq beg (outre--find-ns-previous))
          (setq ns (and beg (outre-parse-namespace beg))))))))

(defun outre--find-enclosing-ns ()
  "Return the namespace around point, if any.
This uses the results, if any, of a previous buffer scan,
stored in `outre-list'."
  (let ((pt (point))
        (lst outre-list)
        srt)
    (setq srt (outre--sort-namespaces-by-distance
               pt
               (outre--collect-namespaces-around-pos pt lst)))
    (car srt)))

(defun outre-change-enclosing-ns-name ()
  "Change the name of the enclosing namespace, if one exists."
  (interactive)
  (outre-scan-buffer)
  (let ((ns (outre--find-enclosing-ns)))
    (if ns
        (outre--change-ns-name ns)
      (message "No enclosing namespace"))))

(defun outre--change-ns-name (ns)
  "Change the name of namespace denoted by NS to NEW."
  (let ((start (car (outre--get-ns-name-pos ns)))
        (end (cadr (outre--get-ns-name-pos ns)))
        (old (car (outre--get-ns-names ns)))
        new)
    (setq new (read-string
               (concat "Change namespace " old
                       " to (leave blank for anonymous): ")))
    (when (string-equal old outre-anon-name)
      (setq old "anonymous"))
    (save-excursion
      ;; change any comment with old name at ns end
      ;; (only look on same line as last delimiter)
      (goto-char (cadr (outre--get-ns-delimiter-pos ns)))
      (when (and
             (search-forward old (line-end-position) t)
             (nth 4 (syntax-ppss)))
        (if (string-blank-p new)
            (replace-match "anonymous")
          (replace-match new))))
    ;; change the namespace tag
    (delete-region start end)
    (goto-char start)
    (insert new)
    (just-one-space)))

(defun outre-delete-enclosing-ns ()
  "Delete the enclosing namespace, if one exists.
This removes the tags and delimiters, not the content."
  (interactive)
  (outre-scan-buffer)
  (let ((ns (outre--find-enclosing-ns)))
    (if ns
        (outre--delete-ns ns)
      (message "No enclosing namespace"))))

(defun outre--delete-ns (ns)
  "Delete the namespace denoted by NS (though not its content)."
  (when ns
    (save-excursion
      (let ((start (car (outre--get-ns-tag-pos ns)))
            (end (car (outre--get-ns-delimiter-pos ns)))
            (coda (cadr (outre--get-ns-delimiter-pos ns))))
        (goto-char coda)
        (delete-char -1)
        (comment-kill 1)
        (outre--clean-up-ws-around-point)
        (delete-region start (1+ end))
        (goto-char start)
        (outre--clean-up-ws-around-point)))))

(defun outre--highlight-ns (ns)
  "Highlight the namespace denoted by NS."
  (when ns
    (let ((start (car (outre--get-ns-delimiter-pos ns)))
          (end (cadr (outre--get-ns-delimiter-pos ns))))
      (goto-char start)
      (set-mark-command nil)
      (goto-char end)
      (setq deactivate-mark nil))))

(defun outre--clean-up-ws-around-point ()
  "Clean up whitespace around point."
  (just-one-space)
  (delete-blank-lines))

(defun outre--jump-to-ns (ns)
  "Jump to the beginning of namespace NAME."
  (when ns
    (outre--on-namespace-selected ns)))

(defun outre--get-ns-by-name (name)
  "Return the namespace matching NAME."
  (seq-find (lambda(elt)
              (string-equal name (cadr (outre--get-ns-names elt))))
            outre-list))

(defun outre-ivy-jump-to-ns ()
  "Jump to a namespace in current buffer, using ivy to select."
  (interactive)
  (let ((name (outre--choose-ns-name-with-ivy "Namespace to jump to: ")))
    (when name
      (outre--jump-to-ns (outre--get-ns-by-name name)))))

(defun outre-change-ns-name ()
  "Select a namespace, then change its name."
  (interactive)
  (outre-scan-buffer)
  (let ((name (outre--choose-ns-name-with-ivy "Namespace to change: ")))
    (when name
      (outre--change-ns-name (outre--get-ns-by-name name)))))

(defun outre-delete-ns-by-name ()
  "Select a namespace, then delete it (though not its content)."
  (interactive)
  (outre-scan-buffer)
  (let ((name (outre--choose-ns-name-with-ivy "Namespace to delete: ")))
    (when name
      (outre--delete-ns (outre--get-ns-by-name name)))))

(defun outre-highlight-ns-by-name ()
  "Select a namespace, then highlight it."
  (interactive)
  (outre-scan-buffer)
  (let ((name (outre--choose-ns-name-with-ivy "Namespace to highlight: ")))
    (when name
      (outre--highlight-ns (outre--get-ns-by-name name)))))

(defun outre-print-enclosing-ns-name ()
  "Print the closest namespace surrounding point, if any."
  (interactive)
  (outre-scan-buffer)
  (let ((ns (outre--find-enclosing-ns)))
    (when ns
      (message "Namespace: %s" (cadr (outre--get-ns-names ns))))))

(defun outre--choose-ns-name-with-ivy (&optional prompt)
  "Use ivy to select a namespace in the current buffer."
  (outre-scan-buffer)
  (let ((lst (mapcar
              (lambda(elt)
                (cadr (outre--get-ns-names elt)))
              outre-list))
        name)
    (unless prompt (setq prompt "Namespace: "))
    (ivy-read prompt (nreverse lst)
              :re-builder #'ivy--regex
              :sort nil
              :initial-input nil)))

;; namespace
(defvar c-basic-offset)
(defun outre-wrap-namespace-region (start end name)
  "Surround the region (START, END) with a namespace NAME."
  (interactive "r\nsEnter the namespace name (leave blank for anonymous): ")
    (save-excursion
      (goto-char end) (insert "\n}")
      (insert-char ?\s c-basic-offset)
      (insert "// end namespace "
              (if (zerop (length name))
                  "anonymous" name)
              "\n")
      (goto-char start)
      (insert "namespace ")
      (unless (zerop (length name))
          (insert name " "))
      (insert "{\n\n")))

(defun outrespace-define-keys (map)
  "Define in MAP key bindings for `outrespace-mode'."
  (define-key map "\M-p" 'outre-goto-namespace-previous)
  (define-key map "\M-n" 'outre-goto-namespace-next)
  (define-key map "p" 'outre-print-enclosing-ns-name)
  (define-key map "n" 'outre-wrap-namespace-region)
  (define-key map "j" 'outre-ivy-jump-to-ns)
  (define-key map "c" 'outre-change-ns-name)
  (define-key map "C" 'outre-change-enclosing-ns-name)
  (define-key map "d" 'outre-delete-ns-by-name)
  (define-key map "D" 'outre-delete-enclosing-ns)
  (define-key map "h" 'outre-highlight-ns-by-name)
  (define-key map [t] 'outre-turn-off)
  )

(defvar outrespace-mode-map
  (let ((map (make-sparse-keymap)))
    (outrespace-define-keys map)
    map)
  "Keymap for `outre-mode'.")

(defcustom outrespace-prefix-key
  "\C-cn"
  "Prefix key for `outrespace-mode'.")

(defun outrespace-define-prefix (map)
  "Define a prefix keymap MAP for `outrespace-mode'."
  (define-key map outrespace-prefix-key outrespace-mode-map))

(defvar outrespace-prefix-map
  (let ((map (make-sparse-keymap)))
    (outrespace-define-prefix map)
    map)
  "Prefix keymap for `outrespace-mode'.")

(defun outre-turn-off ()
  (interactive)
  (outrespace-mode -1))

(define-minor-mode outrespace-mode
  "Helper for c++ namespaces."
  :init-value nil
  :lighter " NS"
  :keymap outrespace-prefix-map
  (if outrespace-mode
      (outre-define-keys)
    t))

(provide 'outrespace)
;;; outrespace.el ends here
