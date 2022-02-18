#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_outrespace-select-ns.el --- test selecting namespaces
;; Copyright (C) 2017-2018, 2022  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, August 17, 2017
;; Version: 1.0
;; Modified Time-stamp: <2022-02-18 13:33:34 dharms>
;; Modified by: Dan Harms
;; Keywords: outrespace namespace

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
(load-file "test/outrespace-test-common.el")
(require 'outrespace)

(ert-deftest ert-outre-test-select-ns-std ()
  (let ((completing-read-function 'completing-read-default)
        (outrespace--select-ns-func 'outrespace--select-ns-standard)
        ns read-index)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &optional _3 _4 _5 _6 _7 _8) (interactive)
                 (car (seq-elt collection read-index)))))
      (with-temp-buffer
        (c++-mode)
        (insert "
#include <myfile>

namespace first {
}
namespace second {}
namespace first
{}
namespace third
{
}
namespace first   { }
")
        (setq read-index 0)
        (setq ns (outrespace--choose-ns-by-name))
        (should (string-equal "first"
                              (car (outrespace--get-ns-names ns))))
        (setq read-index 1)
        (setq ns (outrespace--choose-ns-by-name))
        (should (string-equal "second"
                              (car (outrespace--get-ns-names ns))))
        (setq read-index 2)
        (setq ns (outrespace--choose-ns-by-name))
        (should (string-equal "first"
                              (car (outrespace--get-ns-names ns))))
        (setq read-index 3)
        (setq ns (outrespace--choose-ns-by-name))
        (should (string-equal "third"
                              (car (outrespace--get-ns-names ns))))
        (setq read-index 0)
        (setq ns (outrespace--choose-ns-by-name))
        (should (string-equal "first"
                              (car (outrespace--get-ns-names ns))))
        ))))

;;; test_outrespace-select-ns.el ends here
