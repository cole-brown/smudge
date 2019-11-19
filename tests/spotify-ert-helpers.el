;;; spotify-ert-helpers.el --- Helpers for tests for spotify.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit test helper functions/macros for spotify.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                      spotify-ert-helpers.el tests                        --
;;------------------------------------------------------------------------------

(require 'spotify-json)

;;------------------------------------------------------------------------------
;; String JSON -> Hash Table JSON
;;------------------------------------------------------------------------------

(defmacro spotify-ert/util/with-json (json-str &rest body)
  "Reads/decodes JSON-STR to JSON-OBJ (in a let binding), then runs BODY forms
in the same scope.
"
  (declare (indent defun))
  `(spotify--json-setup
     (let ((json-obj (json-read-from-string ,json-str)))
       ,@body)))
;; (macroexpand '(spotify-ert/util/with-json "{\"hi\":3}" (message "%S" json-obj)))


;;------------------------------------------------------------------------------
;; Test Data
;;------------------------------------------------------------------------------




;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-ert-helpers)
