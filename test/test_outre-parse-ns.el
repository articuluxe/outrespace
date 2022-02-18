#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_outre-parse-ns.el --- outrespace namespace test
;; Copyright (C) 2017-2018, 2022  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, June 22, 2017
;; Version: 1.0
;; Modified Time-stamp: <2022-02-18 13:33:34 dharms>
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
    (let ((loc (outrespace--find-ns-next)))
      (should loc)
      (goto-char loc)
      (should (outrespace--at-ns-begin-p))
      (should (string= (match-string 0) "namespace"))
      (forward-sexp)
      (should (search-forward-regexp (outrespace--namespace-regexp) nil t))
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
      (outrespace-scan-buffer)
      (should (eq (seq-length outrespace-list) 1))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
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
      (outrespace-scan-buffer)
      (should (eq (seq-length outrespace-list) 1))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
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

(defun outre-test-parse-nested-ns-helper (str)
  "Test the namespace parsing against STR."
  (let (ns)
    (with-temp-buffer
      (c++-mode)
      (insert str)
      (outrespace-scan-buffer)
      (should (eq (seq-length outrespace-list) 2))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("nested" "name::nested")))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("name" "name")))
      )))

(ert-deftest outre-test-parse-nested-ns ()
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { /* useless comments */ }}")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { /* useless comments */} }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested {/* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested{ /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name {namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name{ namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "  namespace   name   {  namespace   nested   {   /* useless comments */ }   } ")
  (outre-test-parse-nested-ns-helper "\nnamespace name { namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace\n name { namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace \nname { namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name\n { namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name \n{ namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name {\n namespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { \nnamespace nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace\n nested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace \nnested { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested\n { /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested \n{ /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested {\n /* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { \n/* useless comments */ } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { /* useless comments */\n } }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { /* useless comments */ \n} }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { /* useless comments */ }\n }")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { /* useless comments */ } \n}")
  (outre-test-parse-nested-ns-helper "namespace name { namespace nested { /* useless comments */ } }\n")
  )

(defun outre-test-parse-nested-anon-ns-helper (str)
  "Test the namespace parsing against STR."
  (let (ns)
    (with-temp-buffer
      (c++-mode)
      (insert str)
      (outrespace-scan-buffer)
      (should (eq (seq-length outrespace-list) 2))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("<anon>" "name::<anon>")))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("name" "name")))
      )))

(ert-deftest outre-test-parse-nested-anon-ns ()
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace { /* useless comments */ }}")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace{ /* useless comments */} }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace {/* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name {namespace{ /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name{ namespace { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "  namespace   name   {  namespace      {   /* useless comments */ }   } ")
  (outre-test-parse-nested-anon-ns-helper "\nnamespace name { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace\n name { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace \nname { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name\n { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name \n{ namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name {\n namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { \nnamespace  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace\n  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace \n  { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace \n { /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  \n{ /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  {\n /* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  { \n/* useless comments */ } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  { /* useless comments */\n } }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  { /* useless comments */ \n} }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  { /* useless comments */ }\n }")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  { /* useless comments */ } \n}")
  (outre-test-parse-nested-anon-ns-helper "namespace name { namespace  { /* useless comments */ } }\n")
  )

(defun outre-test-parse-anon-nested-ns-helper (str)
  "Test the namespace parsing against STR."
  (let (ns)
    (with-temp-buffer
      (c++-mode)
      (insert str)
      (outrespace-scan-buffer)
      (should (eq (seq-length outrespace-list) 2))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("nested" "<anon>::nested")))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("<anon>" "<anon>")))
      )))

(ert-deftest outre-test-parse-anon-nested-ns ()
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace { namespace nested { /* useless comments */ }}")
  (outre-test-parse-anon-nested-ns-helper "namespace { namespace nested { /* useless comments */} }")
  (outre-test-parse-anon-nested-ns-helper "namespace{ namespace nested {/* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace{ namespace nested{ /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace {namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace{ namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "  namespace      {  namespace   nested   {   /* useless comments */ }   } ")
  (outre-test-parse-anon-nested-ns-helper "\nnamespace { namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace\n  { namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace \n { namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace \n { namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  \n{ namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  {\n namespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { \nnamespace nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace\n nested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace \nnested { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested\n { /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested \n{ /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested {\n /* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested { \n/* useless comments */ } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested { /* useless comments */\n } }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested { /* useless comments */ \n} }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested { /* useless comments */ }\n }")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested { /* useless comments */ } \n}")
  (outre-test-parse-anon-nested-ns-helper "namespace  { namespace nested { /* useless comments */ } }\n")
  )

(defun outre-test-parse-nested-and-anon-ns-helper (str)
  "Test the namespace parsing against STR."
  (let (ns)
    (with-temp-buffer
      (c++-mode)
      (insert str)
      (outrespace-scan-buffer)
      (should (eq (seq-length outrespace-list) 2))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("<anon>" "<anon>::<anon>")))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("<anon>" "<anon>")))
      )))

(ert-deftest outre-test-parse-anon-and-nested-ns ()
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace { namespace  { /* useless comments */ }}")
  (outre-test-parse-nested-and-anon-ns-helper "namespace { namespace  { /* useless comments */} }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace{ namespace  {/* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace{ namespace { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace {namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace{ namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "  namespace      {  namespace      {   /* useless comments */ }   } ")
  (outre-test-parse-nested-and-anon-ns-helper "\nnamespace { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace\n  { namespace { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace \n { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace \n { namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  \n{ namespace { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  {\n namespace  { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { \nnamespace { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace\n  { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace \n { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace \n { /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace \n{ /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace  {\n /* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace  { \n/* useless comments */ } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace  { /* useless comments */\n } }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace { /* useless comments */ \n} }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace  { /* useless comments */ }\n }")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace  { /* useless comments */ } \n}")
  (outre-test-parse-nested-and-anon-ns-helper "namespace  { namespace { /* useless comments */ } }\n")
  )

(defun outre-test-parse-nested-ns-c++17-helper (str)
  "Test the namespace parsing against STR."
  (let (ns)
    (with-temp-buffer
      (c++-mode)
      (insert str)
      (outrespace-scan-buffer)
      (should (eq (seq-length outrespace-list) 2))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("nested" "name::nested")))
      (setq ns (pop outrespace-list))
      (should (equal (outrespace--get-ns-names ns)
                     '("name" "name")))
      )))

(ert-deftest outre-test-parse-nested-ns-c++17 ()
  (outre-test-parse-nested-ns-c++17-helper "namespace name::nested { }")
  (outre-test-parse-nested-ns-c++17-helper " namespace name::nested { }")
  (outre-test-parse-nested-ns-c++17-helper "namespace  name::nested { }")
  (outre-test-parse-nested-ns-c++17-helper "namespace name::nested  { }")
  (outre-test-parse-nested-ns-c++17-helper "namespace name::nested {  }")
  (outre-test-parse-nested-ns-c++17-helper "namespace name::nested { } ")
  (outre-test-parse-nested-ns-c++17-helper " namespace  name::nested { }")
  (outre-test-parse-nested-ns-c++17-helper " namespace name::nested  { }")
  (outre-test-parse-nested-ns-c++17-helper " namespace  name::nested {  }")
  (outre-test-parse-nested-ns-c++17-helper " namespace   name::nested { }  ")
  (outre-test-parse-nested-ns-c++17-helper "namespace   name::nested   {   }")
  (outre-test-parse-nested-ns-c++17-helper " namespace     name::nested    {    }   ")
  )


;;; test_outre-parse-ns.el ends here
