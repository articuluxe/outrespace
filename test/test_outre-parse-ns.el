#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_outre-parse-ns.el --- outrespace namespace test
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, June 22, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-06-27 08:04:36 dharms>
;; Modified by: Dan Harms
;; Keywords: test outrespace

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

(defun outre-test-parse-ns-regexp-helper (str)
  "Test the namespace regexp against STR."
  (with-temp-buffer
    (c++-mode)
    (insert str)
    (goto-char (point-min))
    (let ((loc (outre--find-ns-next)))
      (should loc)
      (goto-char loc)
      (should (outre--at-ns-begin-p loc))
      (should (string= (match-string 0) "namespace"))
      (forward-sexp)
      (should (search-forward-regexp (outre--namespace-regexp) nil t))
      (should (string= (save-match-data (string-trim (match-string 1))) "name"))
      (should (string= (match-string 2) "{"))
      )))

(ert-deftest outre-test-parse-ns-regexp ()
  "Test parsing many namespaces."
  (outre-test-parse-ns-regexp-helper "namespace name { }")
  (outre-test-parse-ns-regexp-helper " namespace name { }")
  (outre-test-parse-ns-regexp-helper "namespace  name { }")
  (outre-test-parse-ns-regexp-helper "  namespace  name { }")
  (outre-test-parse-ns-regexp-helper " namespace  name    { }")
  (outre-test-parse-ns-regexp-helper " namespace name {  }")
  (outre-test-parse-ns-regexp-helper " namespace  name{} ")
  (outre-test-parse-ns-regexp-helper "namespace
name{ }")
  (outre-test-parse-ns-regexp-helper "namespace \nname{ }")
  (outre-test-parse-ns-regexp-helper "namespace name \n{ } ")
  (outre-test-parse-ns-regexp-helper "namespace name \n {  }  ")
  (outre-test-parse-ns-regexp-helper "namespace\n  name\n { }")
  (outre-test-parse-ns-regexp-helper "namespace \n  name \n   { }")
  (outre-test-parse-ns-regexp-helper " namespace
  name
 { }")
  (outre-test-parse-ns-regexp-helper "  namespace \n \n \n name  \n \n { }")
  )

(defun outre-test-parse-ns-helper (str)
  "Test the namespace parsing against STR."
  (let (ns)
    (with-temp-buffer
      (c++-mode)
      (insert str)
      (outre-scan-buffer)
      (should (eq (seq-length outre-list) 1))
      (setq ns (car outre-list))
      (should (equal (outre--get-ns-names ns)
                     '("ns" "ns")))
      )))

(ert-deftest outre-test-parse-ns ()
  (outre-test-parse-ns-helper "namespace ns { /* useless comments */ }")
  (outre-test-parse-ns-helper " namespace ns { /* useless comments */ }")
  (outre-test-parse-ns-helper "  namespace ns{ /* useless comments */ } ")
  (outre-test-parse-ns-helper " namespace ns{/* useless comments */ }")
  (outre-test-parse-ns-helper "   namespace ns {/* useless comments */ }")
  (outre-test-parse-ns-helper "  namespace ns{/* useless comments */ }")
  (outre-test-parse-ns-helper " namespace ns {
/* useless comments */ }")
  (outre-test-parse-ns-helper " namespace ns
{ /* useless comments */ }")
  (outre-test-parse-ns-helper " namespace
  ns
 { /* useless comments */ }")
  (outre-test-parse-ns-helper " namespace ns {
/* useless comments */ }")
  (outre-test-parse-ns-helper " namespace     ns
 { /* useless comments */ }")
  (outre-test-parse-ns-helper " namespace    ns \n{ /* useless comments */ }")
  (outre-test-parse-ns-helper " namespace    ns \n {
/* useless comments */ }")
  (outre-test-parse-ns-helper " namespace \n\n  \nns { /* useless comments */ }")
  (outre-test-parse-ns-helper " namespace ns \n\n\n  { \n /* useless comments */ }")
  )

(defun outre-test-parse-anon-ns-helper (str)
  "Test the namespace parsing against STR."
  (let (ns)
    (with-temp-buffer
      (c++-mode)
      (insert str)
      (outre-scan-buffer)
      (should (eq (seq-length outre-list) 1))
      (setq ns (car outre-list))
      (should (equal (outre--get-ns-names ns)
                     '("<anon>" "<anon>")))
      )))

(ert-deftest outre-test-parse-anon-ns ()
  (outre-test-parse-anon-ns-helper "namespace { /* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace { /* useless comments */ }")
  (outre-test-parse-anon-ns-helper "  namespace{ /* useless comments */ } ")
  (outre-test-parse-anon-ns-helper " namespace {/* useless comments */ }")
  (outre-test-parse-anon-ns-helper "   namespace {/* useless comments */ }")
  (outre-test-parse-anon-ns-helper "  namespace{/* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace {
/* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace
{ /* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace

 { /* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace{
/* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace
 { /* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace    \n{ /* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace    \n {
/* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace \n\n  \n { /* useless comments */ }")
  (outre-test-parse-anon-ns-helper " namespace \n\n\n  { \n /* useless comments */ }")
  )

(ert-run-tests-batch-and-exit (car argv))

;;; test_outre-parse-ns.el ends here
