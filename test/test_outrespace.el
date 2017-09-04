#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_outrespace.el --- test outrespace.el
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 23, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-09-04 07:02:44 dharms>
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
;; Test various outrespace namespace utility functions.
;;

;;; Code:

(load-file "test/outrespace-test-common.el")
(require 'outrespace)

(ert-deftest ert-outre-test-in-comment-or-string ()
  (with-temp-buffer
    (c++-mode)
    (insert "int decl; // this is a comment")
    (goto-char (point-min))
    (should (outrespace-not-in-comment-or-string))
    (search-forward-regexp "//")
    (should (outrespace-in-comment-or-string))
    ))

(ert-deftest ert-outre-test-parse-ns ()
  (with-temp-buffer
    (c++-mode)
    (insert"
#include <file>

using namespace ignore;

namespace my_namespace {

int decl;

}

")
    (outrespace-scan-buffer)
    (should (eq (seq-length outrespace-list) 1))
    (setq ns (pop outrespace-list))
    (should (equal (outrespace--get-ns-names ns)
                   '("my_namespace" "my_namespace")))
    ))

(ert-deftest ert-outre-test-parse-ns-no-using ()
  (with-temp-buffer
    (c++-mode)
    (insert "
#include <file>

using  namespace ignore;

namespace my_namespace
{
// comment
}

")
    (outrespace-scan-buffer)
    (should (eq (seq-length outrespace-list) 1))
    (setq ns (pop outrespace-list))
    (should (equal (outrespace--get-ns-names ns)
                   '("my_namespace" "my_namespace")))
    ))

(ert-run-tests-batch-and-exit (car argv))

;;; test_outrespace.el ends here
