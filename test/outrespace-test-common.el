;;; outrespace-test-common.el --- common test utilities for outrespaace
;; Copyright (C) 2017, 2022  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, June 21, 2017
;; Version: 1.0
;; Modified Time-stamp: <2022-02-18 13:17:49 dharms>
;; Modified by: Dan Harms
;; Keywords: outrespace test

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
(require 'ert)
(defvar base-test-dir
  (file-name-as-directory
   (file-name-directory load-file-name)))
(defvar absolute-root-dir
  (cond ((eq system-type 'windows-nt)
         (expand-file-name "c:\\Users"))
        (t "/home"))
  "An absolute path name near the root of the current host.")
(setq debug-on-error t)

;; project-specific code begins here

;;; outrespace-test-common.el ends here
