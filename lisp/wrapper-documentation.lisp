;;; wrapper-documentation.lisp --- Documentations for CLOG-C3 wrapper functions

;; File:        wrapper-documentation.lisp
;; Description: Documentations for CLOG-C3 wrapper functions
;; Author:      li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Maintainer:  li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Copyright (c) 2024, li-yiyang, all rights reserved
;; Created: 2024-10-19 23:37
;; Version: 0.1.0
;; Last-Updated: 2024-10-19 23:37
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

(defclass clog-c3 (clog-element)
  ((chart-id :initform (format nil "clog['C3-~A']" (clog-connection:generate-id))
             :reader   c3-chart-id))
  (:documentation
   "CLOG wrapper for C3 chart.

Usage:
 * `create-clog-c3-plot' create and attach dataset on clog object

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
 * call JS C3 resize API after `clog:set-geometry'
 * call JS C3 destroy API before calling `clog:destroy'
"))

;;; CLOG flavor binding

(defgeneric c3-chart-id (c3)
  (:documentation
   "Return `clog-c3' object JS script name.

Develop Note:
this is much like `script-id' and should be used in
JS script calling. "))

(defgeneric ensure-clog-c3-env (c3)
  (:documentation
   "Load D3, C3, C3 CSS to CLOG only once. "))

(defgeneric create-clog-c3-plot (clog-obj dataset
                                 &key
                                   id color type
                                   hide-tooltip
                                   hide-legend
                                   xs-dataset xs-id
                                   x-max-ticks
                                   x-label y-label
                                   x-label-position
                                   y-label-position
                                   width height
                                   html-id class style
                                 &allow-other-keys)
  (:documentation
   "Create a CLOG C3 div container on `clog-obj' and trun it to C3 chart.
Return the `clog-c3' object created.

Para:
 * `dataset'
 * `type': data plotted type
   (refer to `c3-data-type-list' function results)
 * `id': if `dataset' does not support `c3-data-id',
   then `id' should be manually given
 * `color': set color for the dataset
 * `xs-id': if not provided but given `xs-dataset',
   the `xs-id' is `id' with \"-x\" postfixed
 * `xs-dataset': if `dataset' should be rendered
   as 2D data point and need additional xs infomation

   (if `dataset' supports `c3-form' and returns
    rendered xs infomation, but note that `xs-dataset'
    will overwrites xs infomation from `c3-form')
 * `x-label', `y-label', `x-label-position', `y-label-position'
   related with axis label
 * `x-max-ticks': set max x-ticks
 * `hide-tooltip': non-nil for hiding tooltip (default nil)
 * `hide-legend': non-nil for hiding legend (default nil)
 * `width', `height': the div width and height
 * `html-id', `class', `style': CLOG stuff
   (not recommanded to set mannually)
"))

;;; Lisp to C3 JS bridge

(defgeneric c3-data-id (data)
  (:documentation
   "Return a string (if possible) for `data' ID in C3 chart.

Develop Note:
 * by default `c3-data-id' will raise error for no implementation,
   so call it like:
       (or id (c3-data-id data))
 * if given `data' is string, treat it as data id, return itself
 * define the implementation for abstract data class"))

(defgeneric c3-plot-type (data)
  (:documentation
   "Return a keyword or string (if possible) for `data' type in C3 chart.

Develop Note:
 * by default `c3-plot-type' will raise error for no implementation,
   so call it like:

       (or type (c3-plot-type data))
 * if given `data' is string or keyword, treat it as data type,
   return itself
 * define the implementation for abstract data class"))

(defgeneric c3-form (data)
  (:documentation
   "Render `data' as C3 column data form.
Return values are rendered C3 data form string and
rendered C3 x form string if needed (nil if not).

Develop Note:
 * by default `c3-form' will use (js-form data :array)
 * to support prefix with column id, render it like
       ~{~A~^,~}
   without wrapping around []
 * wrap [] in `c3-load' and other calling situation
 * for those scatter plot like 2D data, render
   should return x form string. "))

;;; C3 Method Wrapper

;; load/unload data to the chart

(defgeneric c3-load (c3 dataset
                     &key id xs-dataset xs-id color type done-handler
                     &allow-other-keys)
  (:documentation
   "Load data to C3 chart.

Para:
 * `dataset': data to be plotted
   (define `c3-form' for custom dataset)
 * `id': if `dataset' does not support `c3-data-id',
   then `id' should be manually given
 * `xs-dataset': if `dataset' should be rendered
   as 2D data point and need additional xs infomation

   (if `dataset' supports `c3-form' and returns
    rendered xs infomation, but note that `xs-dataset'
    will overwrites xs infomation from `c3-form')
 * `xs-id': if not provided but given `xs-dataset',
   the `xs-id' is `id' with \"-x\" postfixed
 * `color': set color for the dataset
 * `type': data plotted type
   (refer to `c3-data-type-list' function results)
 * `done-handler': function to be called when loaded

Example:
    (c3-load c3 dataset
             :id \"data1\"
             :color :red    ;; predined color symbol or \"#ff0000\" like
             :type  :line
             :done-handler (lambda () (format t \"loaded~%\")))

Develop Note:
 * the `dataset' should be transfered to columns
 * not recommanded to define other implementation,
   define `c3-form' and `c3-data-id' on specific
   data class instead

see: https://c3js.org/reference.html#api-load"))

(defgeneric c3-unload (c3 &rest targets)
  (:documentation
   "Unload data from C3 chart.

Para:
 * `targets': list of targets, target could be:
   + data id: \"data1\"
   + dataset object with `c3-data-id' support

see: https://c3js.org/reference.html#api-unload"))

(defgeneric c3-domain (c3)
  (:documentation
   "Get/Set C3 current zoomed x domain.
Return value is x-min and x-max list.

Example:
    (c3-domain c3)                           ;; => (x-min x-max)
    (setf (c3-domain c3) (list x-min x-max)) ;; => update

see: https://c3js.org/reference.html#api-zoom"))

;; Simple C3 Methods

(define-c3-method (c3-focus "focus") (target-ids)
  "This API highlights specified targets and fade out the others.

Para:
 * `target-ids' list of or single target ids string to be highlighted

Example:
   (c3-focus c3 \"data1\")
   (c3-focus c3 '(\"data1\" \"data2\"))

see: `c3-focus-all', `c3-defocus', `c3-revert'")

(define-c3-method (c3-defocus "defocus") (target-ids)
  "This API fades out specified targets and reverts the others.

Para:
 * `target-ids' list of or single target ids string to be highlighted

Example:
    (c3-defocus c3 \"data1\")

see: `c3-defocus-all', `c3-focus', `c3-revert'")

(define-c3-method (c3-revert "revert") (target-ids)
  "This API reverts specified targets.

Para:
 * `target-ids' list of or single target ids string to be highlighted

Example:
    (c3-revert c3 \"data1\")

see: `c3-revert-all', `c3-focus', `c3-defocus'")

(define-c3-method (c3-show "show") (target-ids &key with-legend)
  "This API shows specified targets.

Para:
 * `target-ids' list of or single target ids string to be highlighted
 * `with-legend' if set to be `t', legend will be shown together
   with the specified data

see: `c3-hide'")

(define-c3-method (c3-hide "hide") (target-ids &key with-legend)
  "This API hides specified targets.

Para:
 * `target-ids' list of or single target ids string to be highlighted
 * `with-legend' if set to be `t', legend will be hide together
   with the specified data

see: `c3-show', `c3-hide-all', `c3-toggle-hiding'")

(define-c3-method (c3-toggle-hiding "toggle") (target-ids &key with-legend)
  "This API toggles (shows or hides) specified targets.

Para:
 * `target-ids' list of or single target ids string to be highlighted
 * `with-legend' if set to be `t', legend will be toggled together
   with the specified data

see: `c3-show', `c3-hide', `c3-toggle-hiding-all'")

(define-c3-method (c3-show-legend "legend.show") (target-ids)
  "Hide legend for each target.

Para:
 * `target-ids' array or string for legend ids

see `c3-hide-legend', `c3-show-legend-all'")

(define-c3-method (c3-hide-legend "legend.hide") (target-ids)
  "Hide legend for each target.

Para:
 * `target-ids' array or string for legend ids

see `c3-show-legend', `c3-hide-legend-all'")

(define-c3-method (c3-unzoom "unzoom") ()
  "Unzoom to the original domain.

see: `c3-zoom'")

(define-c3-method (c3-flush "flush") ()
  "Force to redraw

see: `c3-resize'")

;;; wrapper-documentation.lisp ends here
