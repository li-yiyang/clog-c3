;; package.lisp --- Package definition for CLOG-C3

;; File: package.lisp
;; Description: Package definition for CLOG-C3
;; Author: li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Maintainer: li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Copyright (c) 2024, li-yiyang, all rights reserved
;; Created: 2024-10-19 23:34
;; Version: 0.1.0
;; Last-Updated: 2024-10-19 23:34
;;           By: li-yiyang
;; URL: https://github.com/li-yiyang/clog-c3
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

(defpackage #:clog-c3
  (:use :cl :clog)
  (:documentation
   "A (not complete) C3 wrapper for CLOG.

Config:
   Path or URL (CDN) for C3 and D3.
   Recommanded to replace them with your stastic root path.
 * `*clog-d3-js-path*'   d3.min.js 5.0.0
 * `*clog-c3-js-path*'   c3.min.js 0.7.20
 * `*clog-c3-css-path*'  c3.css    0.7.20

Usage:
 * `create-clog-c3-plot'
   Create a C3 plot div container with given dataset.

Class:
 * `clog-c3'

Method:
 * `c3-load':   load/update dataset
 * `c3-unload': unload dataset
 * `c3-domain': get/set C3 plot zoom domain

   Simple C3 API (see https://c3js.org/reference.html for API section)
 * `c3-focus', `c3-defocus', `c3-revert'
 * `c3-show', `c3-hide', `c3-toggle-hiding'
 * `c3-show-legend', `c3-hide-legend'
 * `c3-unzoom'
 * `c3-flush'

Develop Note:
 * `c3-chart-id' should be used to refer the C3 chart
   object, use it like the `clog:script-id' method.
 * `c3-data-id' and `c3-form' should be used for plotting
   abstract data object
 * main function is `create-clog-c3-plot', but not
   recommand to add other implementation, change how
   it is implemented directly
 ")
  (:export
   ;;; config
   #:*clog-d3-js-path*
   #:*clog-c3-js-path*
   #:*clog-c3-css-path*

   ;;; wrapper
   ;; for Dev usage:
   #:clog-c3
   #:c3-chart-id
   #:c3-data-id
   #:c3-form

   ;; for Norm usage:
   #:create-clog-c3-plot
   #:c3-load
   #:c3-unload
   #:c3-domain

   ;; simple C3 method
   #:c3-focus         #:c3-focus-all
   #:c3-defocus       #:c3-defocus-all
   #:c3-revert        #:c3-revert-all
   #:c3-show          #:c3-show-all
   #:c3-hide          #:c3-hide-all
   #:c3-toggle-hiding #:c3-toggle-hiding-all
   #:c3-show-legend   #:c3-show-legend-all
   #:c3-hide-legend   #:c3-hide-legend-all
   #:c3-unzoom
   #:c3-flush))
