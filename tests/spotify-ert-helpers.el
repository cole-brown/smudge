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
;; Vars
;;------------------------------------------------------------------------------

(defvar spotify-ert/util/with-json/munger nil
  "If this is a functionp, it will be supplied with the JSON-OBJ
of `spotify-ert/util/with-json' before that function returns.
Basically, a built-in hook/filter for massaging the data into
shape for a test.

Munger must take and return JSON-OBJ hash table.")


;;------------------------------------------------------------------------------
;; String JSON -> Hash Table JSON
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-01-10]: Move to data?
;;   Or rename this file to be about data helpers?
(defmacro spotify-ert/util/with-json (json-str &rest body)
  "Reads/decodes JSON-STR to JSON-OBJ (in a let binding), then runs BODY forms
in the same scope.
"
  (declare (indent 1))
  `(spotify--json-setup
     (let* ((json-input ,json-str)
            (json-obj (if (null json-input)
                          nil
                        (json-read-from-string json-input)))
            (json-obj (if (not (functionp spotify-ert/util/with-json/munger))
                          json-obj
                        (funcall spotify-ert/util/with-json/munger json-obj))))
       ,@body)))
;; (macroexpand '(spotify-ert/util/with-json "{\"hi\":3}" (message "%S" json-obj)))
;; (spotify-ert/util/with-json "{\"hi\":3}" (message "%S" json-obj))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-ert-helpers)
