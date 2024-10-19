;;; utils.lisp --- Helpful macros for dev time usage.

;; File:        utils.lisp
;; Description: Helpful macros for dev time usage.
;; Author:      li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Maintainer:  li-yiyang (also recognized as 凉凉, ryo) <thebigbigwordl@qq.com>
;; Copyright (c) 2024, li-yiyang, all rights reserved
;; Created: 2024-10-19 23:34
;; Version: 0.1.0
;; Last-Updated: 2024-10-19 23:34
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-c3-lambda-list (c3-lambda-list)
    "Parse `c3-lambda-list'.
see Usage in `define-c3-method'. "
    (loop with key-p = nil
          for sym in c3-lambda-list
          if (eq sym '&key)
            do (setf key-p t)
          if key-p
            collect sym into keys
          else
            collect sym into vars
          finally (return (values vars keys)))))

(defmacro define-c3-method ((method-name js-name) c3-lambda-list &optional docstring)
  "Define simple C3 API method.

Usage:
 * `c3-lambda-list' only support normal arg and `&key'
   with no default key value.

     mapping rule: (see `parse-c3-lambda-list')
   + lisp lambda list -> js funcall string
   + (a b)            -> (a,b)
   + (a b &key key)   -> (a,b,{key:val})
                      -> (a,b) if no key is provided
 * `target-ids' is preserved, if first variable in `c3-lambda-list'
   is `target-ids', define a method with name <method-name>-all with
   only c3 object but no other argument passing in for no arg js call

Example:
  see usage in wrapper-documentation.lisp

Develop Note:
 * see https://c3js.org/reference.html for API section for
   C3 API method to be ported
 * write docstring in wrapper-documentation.lisp for API usage
"
  (multiple-value-bind (vars keys)
      (parse-c3-lambda-list c3-lambda-list)
    (let ((lambda-list `(,@vars ,@(when keys `(&rest options ,@keys))))
          (js-call     (with-output-to-string (js)
                         (format js "~~A.~A(" js-name)
                         (when (> (length vars) 0)        (format js "~~A"))  ; first
                         (dotimes (i (1- (length vars)))  (format js ",~~A")) ; rest
                         (when keys                       (format js ",~~A")) ; options
                         (format js ");"))))
      `(progn
         (defgeneric ,method-name (c3 ,@lambda-list)
           (:documentation
            ,(format nil "~A~&see https://c3js.org/reference.html#api-~A"
                     (or docstring "")
                     (str:replace-all "." "-" js-name))))
         (defmethod ,method-name ((c3 clog-c3) ,@lambda-list)
           ,(when keys `(declare (ignorable ,@(rest keys))))
           (js-execute c3 (format nil ,js-call
                                  (c3-chart-id c3)
                                  ,@(mapcar (lambda (var) `(js-form ,var)) vars)
                                  ,@(when keys '((js-form options :plist))))))

         ;; define <method-name>-all for non input
         ,@(when (eq (first vars) 'target-ids)
             (let ((method-all-name (intern (format nil "~@:(~A~)-ALL" method-name)))
                   (js-call         (format nil "~~A.~A()" js-name)))
               `((defgeneric ,method-all-name (c3)
                   (:documentation
                    ,(format nil "Applies `~A' on all targets~%see: `~A'"
                             method-name method-name)))
                 (defmethod ,method-all-name ((c3 clog-c3))
                   (js-execute c3 (format nil ,js-call (c3-chart-id c3)))))))))))

;; SLY/SLIME support
(trivial-indent:define-indentation define-c3-method  (4 &lambda &body))

;; JS DSL
;; not exported

(defgeneric js-form (obj &optional hints)
  (:documentation
   "Output `obj' as JS form with possible `hints'.

Para:
 * `hints': add flexiable JS form output possiblities
     for `symbol'
   + `:snake-case' (default)
   + `:camel-case'
   + `:param-case'
   + `:string' will trun into a \"~(~A~)\" like string
   + `t' and `nil' will be `true' and `false'

     for `list'
   + `:array' (default) turn list as JS array
   + `:no-brackets-array' turn list as array but no brackets
   + `:plist', `:alist' will turn plist/alist to a JS hash
   + `:nested-plist', `:nested-alist' will trun recursively

     for `string'
   + `:string' (default) will turn string as JS string
   + `:js-form' will turn string as JS code

     for `number'
   + `:int', `:float' (default) will format as specific form
   + `(:round 0.1)' will truncate number to second bit
"))

(defmethod js-form (obj &optional hints)
  (declare (ignore hints))
  (format nil "~A" obj))

(defmethod js-form ((sym symbol) &optional hints)
  (cond ((eq sym t)   "true")
        ((eq sym nil)
         (case hints
           ((:snake-case :camel-case :param-case) "false")
           (otherwise "{}")))
        (t (case hints
             (:camel-case (str:camel-case (symbol-name sym)))
             (:param-case (str:param-case (symbol-name sym)))
             (:string     (format nil "\"~(~A~)\"" sym))
             (otherwise   (str:snake-case (symbol-name sym)))))))

(defmethod js-form ((list list) &optional (hints :array))
  (case hints
    (:no-brackets-array
     (format nil "~{~A~^,~}" (mapcar #'js-form list)))
    (:plist
     (with-output-to-string (js)
       (format js "{")
       (loop for (key val) on list by #'cddr
             do (format js "~A:~A," (js-form key) (js-form val)))
       (format js "}")))
    (:nested-plist
     (with-output-to-string (js)
       (format js "{")
       (loop for (key val) on list by #'cddr
             do (format js "~A:~A," (js-form key) (js-form val :nested-plist)))
       (format js "}")))
    (:alist
     (with-output-to-string (js)
       (format js "{")
       (loop for (key . val) in list
             do (format js "~A:~A," (js-form key) (js-form val)))
       (format js "}")))
    (:nested-alist
     (with-output-to-string (js)
       (format js "{")
       (loop for (key . val) in list
             do (format js "~A:~A," (js-form key) (js-form val :nested-alist)))
       (format js "}")))
    (otherwise (format nil "[~{~A~^,~}]" (mapcar #'js-form list)))))

(defmethod js-form ((str string) &optional (hints :js-form))
  (case hints
    (:string  (format nil "~S" str))
    (otherwise str)))

(defmethod js-form ((num number) &optional (hints :float))
  (case hints
    (:int   (format nil "~D" (truncate num)))
    (:float (format nil "~F" (float    num)))
    (otherwise (if (and (listp hints) (eq (first hints) :round))
                   (format nil "~F" (fround num (second hints)))
                   (format nil "~A" num)))))

;; used in JS macro
(defvar *js-stream*)

(defmacro js-execute-format
    (obj fmt-str &body fmt-args)
  "Executate JS code as `*js-stream*' output on `obj'. "
  `(js-execute ,obj (format nil ,fmt-str ,@fmt-args)))

(defmacro js{ (&body key-body)
  "Format `key-body' as JS hash to `*js-stream*'.
Return js-form string.

Note: `key' can be:
 * atom `key'        -> to hash key
 * list `(key cond)' -> add hash slot if cond

Example:
    (js{
      (:data (js{
               (:type type))))
"
  (flet ((fmt-str (key-body &optional (fmt-str ",~A:~~A"))
           (let* ((key     (first key-body))
                  (body    (rest  key-body))
                  (cond-p  (listp key))
                  (cond    (when cond-p (second key)))
                  (key     (if cond-p   (first key) key))
                  (fmt-str (format nil fmt-str (js-form key))))
             (if cond-p
                 `(when ,cond (format *js-stream* ,fmt-str (progn ,@body)))
                 `(format *js-stream* ,fmt-str (progn ,@body))))))
    `(with-output-to-string (*js-stream*)
       (format *js-stream* "{")
       ,(when (first key-body) (fmt-str (first key-body) "~A:~~A"))
       ,@(mapcar #'fmt-str (rest key-body))
       (format *js-stream* "}"))))

(defmacro js[ (&rest args)
  "Format `args' as JS array to `*js-stream*'
Return \"\" empty string.

Example:
    (js[ 1 2 3)
"
  `(with-output-to-string (*js-stream*)
     (format *js-stream* "[")
     ,(when (first args)
        `(format *js-stream* "~A" (js-form ,(first args))))
     ,@(loop for arg in (rest args)
             collect `(format *js-stream* ",~A" (js-form ,arg)))
     (format *js-stream* "]")))

(defmacro when* (condition &body body)
  "Like `when' but return \"\" if not. "
  `(if ,condition (progn ,@body) ""))

;;; utils.lisp ends here
