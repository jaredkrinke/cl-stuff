;;;; Minimal, local-only HTTP server based on Hunchentoot, without default resources/error templates

(require "hunchentoot")

(defpackage :min-http
  (:use :cl))

(in-package :min-http)

(defclass min-http-server (hunchentoot:acceptor)
  ()
  (:default-initargs
   :document-root nil
   :error-template-directory nil
   :port 8888
   :address "127.0.0.1")
   (:documentation "Minimal, local-only HTTP server, without any resources or error messages"))

;;; Always return "not found" with an empty body
(defmethod hunchentoot:acceptor-dispatch-request ((srv min-http-server) (request hunchentoot:request))
  (setf (hunchentoot:return-code hunchentoot:*reply*) hunchentoot:+http-not-found+)
  "")

;;; Return an empty body for all errors
(defmethod hunchentoot:acceptor-status-message ((acceptor min-http-server) http-status-code &rest args)
  (declare (ignore args))
  "")

