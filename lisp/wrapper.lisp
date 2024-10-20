;;; wrapper.lisp --- Implementations for CLOG-C3 wrapper function

;; File:        wrapper.lisp
;; Description: Implementations for CLOG-C3 wrapper function
;; Author:      li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Maintainer:  li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Copyright (c) 2024, li-yiyang, all rights reserved
;; Created: 2024-10-19 23:39
;; Version: 0.1.0
;; Last-Updated: 2024-10-21 00:02
;;           By: li-yiyang
;; URL: https://github.com/li-yiyang/clog-c3
;; Keywords: CLOG, C3, D3, chart
;; Compatibility: CLOG, C3 v7, D3 v5
;;
;;

;;; License
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

;; clog-c3

;; call JS C3 resize API after `clog:set-geometry'
(defmethod set-geometry :after ((c3 clog-c3)
                                &key left top right bottom width height units)
  (declare (ignore left top right bottom units))
  (js-execute c3 (format nil "~A.resize({width:~A,height:~A})"
                         (c3-chart-id c3)
                         (or width  (width c3))
                         (or height (height c3)))))

;; call JS C3 destroy API before calling `clog:destroy'
(defmethod destroy :before ((c3 clog-c3))
  (js-execute c3 (format nil "~A = ~A.destory()"
                         (c3-chart-id c3) (c3-chart-id c3))))

;; ensure-clog-c3-env

(defmethod ensure-clog-c3-env ((c3 clog-c3))
  (let ((clog-document (html-document (connection-data-item c3 "clog-body"))))
    (load-css    clog-document *clog-c3-css-path*)
    (load-script clog-document *clog-d3-js-path*)
    (load-script clog-document *clog-c3-js-path*)))

;; c3-data-id

;; by default `c3-data-id' will raise error for no implementation
(defmethod c3-data-id (dataset)
  (restart-case
      (error (format nil "`c3-data-id' is not implemented for `~A'"
                     (class-name (class-of dataset))))
    (set-id (new-id)
      :report "Set id with given input"
      :interactive (lambda () (list (js-form (read))))
      new-id)))

;; if given `data' is string, treat it as data id, return itself
(defmethod c3-data-id ((str string)) str)

;; by default `c3-form' will use (js-form data :array)
(defmethod c3-form (data)
  (values (js-form data :no-brackets-array) nil))

;; create-clog-c3-plot
;; Develop Note:
;;  this is hard to debug... please refer to
;;    https://c3js.org/reference.html
;;  and the JS code comments

(defmethod create-clog-c3-plot ((obj clog-obj) dataset
                                &key
                                  (id (c3-data-id dataset))
                                  color
                                  (type :line)
                                  xs-dataset
                                  xs-id
                                  x-label
                                  y-label
                                  hide-tooltip
                                  hide-legend
                                  (x-label-position :outer-right)
                                  (y-label-position :outer-top)
                                  (width  800)
                                  (height 600)
                                  html-id class style
                                &allow-other-keys)
  (let ((c3    (create-div obj :html-id html-id :class class :style style))
        (xs-p  nil)
        (xs-id xs-id))
    (change-class c3 'clog-c3)
    (ensure-clog-c3-env c3)

    ;; don't blame on me for such long code
    ;; it is painful when you deal with UI things (or should be)
    (js-execute-format c3
        "~A=c3.generate(~A)"
      (c3-chart-id c3)
      (js{
        (:bindto  (format nil "\"#~A\"" (html-id c3)))
        (:data
         (js{
           (:type    (js-form type :string))
           (:columns (multiple-value-bind (data-form xs-form)
                         (c3-form dataset)
                       (js[
                        (js[ (js-form id :string) data-form)
                        (cond (xs-dataset
                               (setf xs-p  t
                                     xs-id (or xs-id
                                               (when xs-dataset
                                                 (c3-data-id xs-dataset))
                                               (format nil "~A-x" id)))
                               (js[ (js-form xs-id :string)
                                    (c3-form xs-dataset)))
                              (xs-form
                               (setf xs-p  t
                                     xs-id (or xs-id
                                               (when xs-dataset
                                                 (c3-data-id xs-dataset))
                                               (format nil "~A-x" id)))
                               (js[ (js-form xs-id :string)
                                    xs-form))
                              (t "")))))
           ((:color color) (format nil "{\"~A\":\"~A\"}" (js-form id) color))
           ((:xs    xs-p)  (format nil "{\"~A\":\"~A\"}" (js-form id) xs-id))))
        ((:tooltip hide-tooltip) "{show:false}")
        ((:legend  hide-legend)  "{show:false}")
        ((:axis (or x-label y-label))
         (js{
                      (:y (js{
                 (:label
                  (js-form (or y-label "y") :string))
                 ((:position x-label-position)
                  (js-form y-label-position :string))))
           (:x (js{
                 (:label
                  (js-form (or x-label "x") :string))
                 ((:position y-label-position)
                  (js-form x-label-position :string))))))))

    (set-geometry c3 :width width :height height)
    c3))

(defmethod c3-load ((c3 clog-c3) dataset
                    &key (id (c3-data-id dataset))
                      xs-dataset xs-id color type (done-handler nil done-handler-p)
                    &allow-other-keys)
  (declare (ignore done-handler))
  ;; TODO:
  (when done-handler-p
    (warn "`c3-load' functionality `done-handler' is not implemented yet"))

  (js-execute-format c3
      "~A.load(~A)"
    (c3-chart-id c3)
    (multiple-value-bind (data-form xs-form)
        (c3-form dataset)
      (js{
        (:columns (js[ (js[ (js-form id :string) data-form)
                       (when* (or xs-form xs-dataset)
                         (js[ (js-form (or xs-id
                                           (when xs-dataset
                                             (c3-data-id xs-dataset))
                                           (format nil "~A-x" id))
                                       :string)
                              (or xs-form (c3-form xs-dataset))))))
        ((:type  type)  (js-form type  :string))
        ((:color color) (js-form color :string))))))

(defmethod c3-unload ((c3 clog-c3) &rest targets)
  (js-execute-format c3 "~A.unload({ids:[~{~A~^,~}]})"
    (c3-chart-id c3)
    (mapcar (lambda (target) (js-form (c3-data-id target) :string)) targets)))

(defmethod c3-domain ((c3 clog-c3))
  (js-query c3 (format nil "~A.zoom()" (c3-chart-id c3))))

(defmethod (setf c3-domain) (domain (c3 clog-c3))
  (assert (= 2 (length domain)))
  (js-execute-format c3 "~A.zoom([~{~F,~F~}])" (c3-data-id c3) domain))

;;; wrapper.lisp ends here
