;;; spotify-json-ert.el --- Tests for spotify-json.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-json.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                          spotify-json.el tests                           --
;;------------------------------------------------------------------------------

(require 'spotify-json)

;; §-TODO-§ [2019-11-15]: Put in tests/ folder?

;; §-TODO-§ [2019-11-15]: Have a main func for requiring all test files so ert
;; learns about all the spotify tests only if we want to run them?

;; §-TODO-§ [2019-11-15]: Have a main func that invokes that one, and then runs
;; all the spotify tests?


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Test: spotify--json-setup
;;------------------------------------------------------------------------------

;; (defmacro spotify--json-setup (&rest body) ...
(ert-deftest spotify-ert/spotify--json-setup ()
  "Test that calling the macro does set the bindings up for json.el."
  (let ((json-object-type 'test-symbol-0)
        (json-array-type  'test-symbol-1)
        (json-key-type    'test-symbol-2))
    ;; Force them to known not-what-we-use values so I can test...
    (should-not (eq json-object-type spotify--json-object-type))
    (should-not (eq json-array-type spotify--json-array-type))
    (should-not (eq json-key-type spotify--json-key-type))
    ;; and json-obj shouldn't exist.
    (should-error (not (null json-obj)))

    ;; Now let's test that our macro sets things up good.
    (spotify--json-setup
      ;; JSON should be set up like we want...
      (should (eq json-object-type spotify--json-object-type))
      (should (eq json-array-type spotify--json-array-type))
      (should (eq json-key-type spotify--json-key-type))
      ;; and json-obj should exist as proper type.
      (should (not (null json-obj)))
      (should (hash-table-p json-obj)))))


;;------------------------------------------------------------------------------
;; Test: spotify--json-bool-encode
;;------------------------------------------------------------------------------

;; (defun spotify--json-bool-encode (value &optional false testfn) ...
(ert-deftest spotify-ert/spotify--json-bool-encode ()
  "Test for encoding emacs bools to json bools."
  (should (eq (spotify--json-bool-encode nil) :json-false))
  (should (eq (spotify--json-bool-encode t) t))
  (should (eq (spotify--json-bool-encode 'test-symbol-0) t))

  ;; Slightly more complicated. Use a variable and define false as
  ;; `test-symbol-0'.
  (let ((my-false 'test-symbol-0))
    (should (eq (spotify--json-bool-encode my-false 'test-symbol-0)
                :json-false))))


;;------------------------------------------------------------------------------
;; Test: spotify--json-bool-decode
;;------------------------------------------------------------------------------

;; (defun spotify--json-bool-decode (value &optional false testfn) ...
(ert-deftest spotify-ert/spotify--json-bool-decode ()
  "Test for encoding emacs bools to json bools."
  (should (eq (spotify--json-bool-decode :json-false) nil))
  (should (eq (spotify--json-bool-decode t) t))
  (should (eq (spotify--json-bool-decode 'test-symbol-0) t))
  (should (eq (spotify--json-bool-decode "off") t))
  (should (eq (spotify--json-bool-decode "off" #'string=) t))

  ;; Slightly more complicated. Use a variable and define false as
  ;; `test-symbol-0'.
  (let ((my-false 'test-symbol-0)
        (my-false-str "off"))
    (should (eq (spotify--json-bool-decode my-false 'test-symbol-0)
                nil))
    (should (eq (spotify--json-bool-decode my-false-str "off" #'string=)
                nil))))


;;------------------------------------------------------------------------------
;; Test: spotify--json-encode-internal
;;------------------------------------------------------------------------------

;; (defun spotify--json-encode-internal (artist name
;;                                       duration-ms track-number
;;                                       playing shuffling repeating)
;; ...
(ert-deftest spotify-ert/spotify--json-encode-internal ()
  "Test for encoding data to our json-internal format."

  ;; Build our four hash-table json structures for testing.
  ;; Two via `spotify--json-encode-internal' and two from test data.
  (spotify-ert/data/with-json-internal

   ;;---
   ;; Test "Aesop Rock"
   ;;---
   (should (string= (gethash 'artist json-shrunk)
                    (gethash 'artist json-shrunk-roundtrip)))

   (should (string= (gethash 'name json-shrunk)
                    (gethash 'name json-shrunk-roundtrip)))

   (should (string= (gethash 'player_state json-shrunk)
                    (gethash 'player_state json-shrunk-roundtrip)))

   (should (= (gethash 'duration json-shrunk)
              (gethash 'duration json-shrunk-roundtrip)))

   (should (= (gethash 'track_number json-shrunk)
              (gethash 'track_number json-shrunk-roundtrip)))

   (should (eq (gethash 'player_shuffling json-shrunk)
               (gethash 'player_shuffling json-shrunk-roundtrip)))

   (should (eq (gethash 'player_repeating json-shrunk)
               (gethash 'player_repeating json-shrunk-roundtrip)))

   ;;---
   ;; Test "\"Weird Al\""
   ;;---
   (should (string= (gethash 'artist json-foil)
                    (gethash 'artist json-foil-roundtrip)))
   ;; This was a problem with our hand-crafted json -
   ;; Weird Al's quotation marks.
   ;; So make sure we do not have backslashes and do have quotes.
   (should-not (string-match "\\\\" (gethash 'artist json-foil)))
   (should-not (string-match "\\\\" (gethash 'artist json-foil-roundtrip)))
   (should (string-match "\"" (gethash 'artist json-foil)))
   (should (string-match "\"" (gethash 'artist json-foil-roundtrip)))
   (should (string-match "\"Weird Al\"" (gethash 'artist json-foil)))
   (should (string-match "\"Weird Al\"" (gethash 'artist json-foil-roundtrip)))

   (should (string= (gethash 'name json-foil)
                    (gethash 'name json-foil-roundtrip)))

   (should (string= (gethash 'player_state json-foil)
                    (gethash 'player_state json-foil-roundtrip)))

   (should (= (gethash 'duration json-foil)
              (gethash 'duration json-foil-roundtrip)))

   (should (= (gethash 'track_number json-foil)
              (gethash 'track_number json-foil-roundtrip)))

   (should (eq (gethash 'player_shuffling json-foil)
               (gethash 'player_shuffling json-foil-roundtrip)))

   (should (eq (gethash 'player_repeating json-foil)
               (gethash 'player_repeating json-foil-roundtrip)))))


;;------------------------------------------------------------------------------
;; Test: spotify--json-internal-status-field
;;------------------------------------------------------------------------------

;; (defun spotify--json-internal-status-field (status field) ...
(ert-deftest spotify-ert/spotify--json-internal-status-field ()
  "Test for encoding data to our json-internal format."

  ;; Build four hash-table json structures for testing.
  ;; Two via `spotify--json-encode-internal' and two from test data.
  (spotify-ert/data/with-json-internal

   ;;---
   ;; Aesop Rock - Shrunk
   ;;---

   ;; compare by hand
   (should (string= "Aesop Rock"
                    (spotify--json-internal-status-field json-shrunk :artist)))
   (should (string= "Shrunk"
                    (spotify--json-internal-status-field json-shrunk :track)))
   (should (= 9
              (spotify--json-internal-status-field json-shrunk :track-number)))
   (should (= 265333
              (spotify--json-internal-status-field json-shrunk
                                                   :duration-millisecond)))
   (should (eq t (spotify--json-internal-status-field json-shrunk
                                                      :playing-bool)))
   (should (eq t (spotify--json-internal-status-field json-shrunk
                                                      :shuffling-bool)))
   (should (eq nil (spotify--json-internal-status-field json-shrunk
                                                        :repeating-bool)))

   ;; compare to each other
   (should (string= (spotify--json-internal-status-field json-shrunk-roundtrip :artist)
                    (spotify--json-internal-status-field json-shrunk :artist)))
   (should (string= (spotify--json-internal-status-field json-shrunk-roundtrip :track)
                    (spotify--json-internal-status-field json-shrunk :track)))
   (should (= (spotify--json-internal-status-field json-shrunk-roundtrip :track-number)
              (spotify--json-internal-status-field json-shrunk :track-number)))
   (should (= (spotify--json-internal-status-field json-shrunk-roundtrip
                                                   :duration-millisecond)
              (spotify--json-internal-status-field json-shrunk
                                                   :duration-millisecond)))
   (should (eq (spotify--json-internal-status-field json-shrunk-roundtrip
                                                    :playing-bool)
               (spotify--json-internal-status-field json-shrunk
                                                    :playing-bool)))
   (should (eq (spotify--json-internal-status-field json-shrunk-roundtrip
                                                    :shuffling-bool)
               (spotify--json-internal-status-field json-shrunk
                                                    :shuffling-bool)))
   (should (eq (spotify--json-internal-status-field json-shrunk-roundtrip
                                                    :repeating-bool)
               (spotify--json-internal-status-field json-shrunk
                                                    :repeating-bool)))

   ;;---
   ;; "Weird Al" Yankovic - Foil
   ;;---

   ;; compare by hand
   (should (string= "\"Weird Al\" Yankovic"
                    (spotify--json-internal-status-field json-foil :artist)))
   (should (string= "Foil"
                    (spotify--json-internal-status-field json-foil :track)))
   (should (= 99
              (spotify--json-internal-status-field json-foil :track-number)))
   (should (= 11111
              (spotify--json-internal-status-field json-foil
                                                   :duration-millisecond)))
   (should (eq nil (spotify--json-internal-status-field json-foil
                                                        :playing-bool)))
   (should (eq nil (spotify--json-internal-status-field json-foil
                                                        :shuffling-bool)))
   (should (eq t (spotify--json-internal-status-field json-foil
                                                      :repeating-bool)))

   ;; compare to each other
   (should (string= (spotify--json-internal-status-field json-foil-roundtrip :artist)
                    (spotify--json-internal-status-field json-foil :artist)))
   (should (string= (spotify--json-internal-status-field json-foil-roundtrip :track)
                    (spotify--json-internal-status-field json-foil :track)))
   (should (= (spotify--json-internal-status-field json-foil-roundtrip :track-number)
              (spotify--json-internal-status-field json-foil :track-number)))
   (should (= (spotify--json-internal-status-field json-foil-roundtrip
                                                   :duration-millisecond)
              (spotify--json-internal-status-field json-foil
                                                   :duration-millisecond)))
   (should (eq (spotify--json-internal-status-field json-foil-roundtrip
                                                    :playing-bool)
               (spotify--json-internal-status-field json-foil
                                                    :playing-bool)))
   (should (eq (spotify--json-internal-status-field json-foil-roundtrip
                                                    :shuffling-bool)
               (spotify--json-internal-status-field json-foil
                                                    :shuffling-bool)))
   (should (eq (spotify--json-internal-status-field json-foil-roundtrip
                                                    :repeating-bool)
               (spotify--json-internal-status-field json-foil
                                                    :repeating-bool)))
   ))

;;------------------------------------------------------------------------------
;; Test: spotify--json-api-to-internal
;;------------------------------------------------------------------------------

;; (defun spotify--json-api-to-internal (player-status) ...

;; §-TODO-§ [2019-11-18]: Impl or skip due to "deprecate-this-plz" TODO?


;;   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------
;;                             END OF UNIT TESTS!
;;------------------------------------------------------------------------------
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


;;------------------------------------------------------------------------------
;; Test Data Helpers
;;------------------------------------------------------------------------------

(defun spotify-ert/aggressive-trim (string)
  "Trims a lot of whitespace. All of it."
  (replace-regexp-in-string (rx whitespace) "" string))


(defmacro spotify-ert/data/with-json-internal (&rest body)
  "Creates 4 JSON hash table objects - 2 each for the
player-statuses in `spotify-ert/data-string/json-internal' with a let binding,
then executes BODY forms in a progn.

JSON variables:
  - Aesop Rock
    - `json-shrunk':
      - directly from `spotify-ert/data-string/json-internal'
    - `json-shrunk-roundtrip':
      - encoded with `spotify--json-encode-internal', then decoded.
  - \"Weird Al\" Yankovic
    - `json-foil':
      - directly from `spotify-ert/data-string/json-internal'
    - `json-foil-roundtrip':
      - encoded with `spotify--json-encode-internal', then decoded.
"
  `(let ((str-shrunk (spotify--json-encode-internal
                      "Aesop Rock" "Shrunk"
                      265333 9 t t nil))
         (json-shrunk nil)
         (json-shrunk-roundtrip nil)
         (str-foil (spotify--json-encode-internal
                    "\"Weird Al\" Yankovic" "Foil"
                    11111 99 nil nil t))
         (json-foil nil)
         (json-foil-roundtrip nil))

     ;; Convert back to json so we can compare better...
     ;; JSON doesn't have an ordering, so no guarentee...
     (spotify--json-setup
       ;; back to json for these
       (setq json-shrunk-roundtrip (json-read-from-string str-shrunk))
       (setq json-foil-roundtrip (json-read-from-string str-foil))
       ;; test data string to json for these, compare aganist our round trips.
       (setq json-shrunk (json-read-from-string
                          (nth 0 spotify-ert/data-string/json-internal)))
       (setq json-foil (json-read-from-string
                        (nth 1 spotify-ert/data-string/json-internal))))

     (progn
       ,@body)))


;;------------------------------------------------------------------------------
;; Test Data
;;------------------------------------------------------------------------------

(defconst spotify-ert/data-string/json-internal
  '("{
    \"artist\": \"Aesop Rock\",
    \"duration\": 265333,
    \"track_number\": 9,
    \"name\":  \"Shrunk\",
    \"player_state\": \"playing\",
    \"player_shuffling\": true,
    \"player_repeating\": false
    }"

    "{
    \"artist\": \"\\\"Weird Al\\\" Yankovic\",
    \"duration\": 11111,
    \"track_number\": 99,
    \"name\":  \"Foil\",
    \"player_state\": \"paused\",
    \"player_shuffling\": false,
    \"player_repeating\": true
    }")
  "Some strings representing our internal/simplified JSON data
structure of Spotify player status.")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-json)
