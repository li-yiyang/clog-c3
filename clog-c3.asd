;;; clog-c3.asd --- ASDF definition for CLOG-C3

;; File: clog-c3.asd
;; Description: ASDF definition for CLOG-C3.
;; Author: li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Maintainer: li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Copyright (c) 2024, li-yiyang, all rights reserved
;; Created: 2024-10-19 23:45
;; Version: 0.1.0
;; Last-Updated: 2024-10-20 00:19
;;           By: li-yiyang
;; URL: https://www.github.com/li-yiyang/clog-c3
;; Keywords: CLOG, C3, D3, chart
;; Compatibility: CLOG, C3 v7, D3 v5
;;
;; This file is part of CLOG-C3.
;;
;;

;;;; License
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

(asdf:defsystem #:clog-c3
  :author ("凉凉")
  :maintainer ("凉凉")
  :url "http://github.com/li-yiyang/clog-c3"
  :version "0.1.0"
  :license "GPLv3"
  :description "This is C3 wrapper for CLOG. "
  :depends-on (clog str trivial-indent)
  :serial t
  :pathname "lisp"
  :components
  ((:file "package")

   ;; Config
   (:file "config")

   ;; Package Develop Note:
   ;;  * use those js macros and functions defined in utils.lisp
   (:file "utils")

   (:file "wrapper-documentation")
   (:file "wrapper")))

;;; clog-c3.asd ends here
