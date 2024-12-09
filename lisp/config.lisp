;; config.lisp --- Package definition for CLOG-C3

;; File: package.lisp
;; Description: Package definition for CLOG-C3
;; Author: li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Maintainer: li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Copyright (c) 2024, li-yiyang, all rights reserved
;; Created: 2024-10-19 23:34
;; Version: 0.1.0
;; Last-Updated: 2024-10-20 00:37
;;           By: li-yiyang
;; URL: https://www.github.com/li-yiyang/clog-c3
;; Keywords: CLOG, C3, D3, chart
;; Compatibility: CLOG, C3 v7, D3 v5
;;
;; This file is part of CLOG-C3.
;;
;; License
;;
;; CLOG-C3 is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; CLOG-C3 is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with CLOG-C3. If not, see <https://www.gnu.org/licenses/>.

(in-package :clog-c3)

;; Path or URL (CDN) for C3 and D3
;; Recommanded to replace them with your stastic root path.

(defparameter *clog-d3-js-path*
  "https://cdnjs.cloudflare.com/ajax/libs/d3/5.0.0/d3.min.js"
  "Path or URL to D3 JS. ")

(defparameter *clog-c3-js-path*
  "https://cdnjs.cloudflare.com/ajax/libs/c3/0.7.20/c3.min.js"
  "Path or URL to C3 JS. ")

(defparameter *clog-c3-css-path*
  "https://cdnjs.cloudflare.com/ajax/libs/c3/0.7.20/c3.css"
  "Path or URL to C3 CSS. ")

(defun c3-data-type-list ()
  "Return a list of currently supported C3 type. "
  (list :line :step :scatter :bar))
