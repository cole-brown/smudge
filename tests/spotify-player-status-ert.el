;;; spotify-player-status-ert.el --- Tests for spotify-player-status.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-player-status.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                     spotify-player-status.el tests                       --
;;------------------------------------------------------------------------------

(require 'spotify-player-status)


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Test: spotify--cache-player-status-enabled-set
;;------------------------------------------------------------------------------

;; (defun spotify--cache-player-status-enabled-set (option-name value) ...
(ert-deftest spotify-ert/spotify--json-setup ()
  "Test that calling the macro does set the bindings up for json.el."
  (spotify-ert/util/with-json-internal
   ))


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-caching-callback
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-caching-callback (callback status)
;;   "Receive updated player status, cache it, and return to
;; CALLBACK unmodified."


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-caching-closure
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-caching-closure (callback)


;;------------------------------------------------------------------------------
;; Test: spotify--cache-set-status
;;------------------------------------------------------------------------------

;;(defun spotify--cache-set-status (status)
;;   "Sets `spotify--cache-player-status' to STATUS with current
;; time as timestamp as long as its fresher than the currently held status."


;;------------------------------------------------------------------------------
;; Test: spotify--cache-get-status-if
;;------------------------------------------------------------------------------

;;(defun spotify--cache-get-status-if (status)
;;  "Returns `spotify--cache-player-status' if STATUS is nil."


;;------------------------------------------------------------------------------
;; Test: spotify--cache-get-timestamp-if
;;------------------------------------------------------------------------------

;;(defun spotify--cache-get-timestamp-if (status)
;;  "Returns `spotify--cache-player-status' if STATUS is nil."


;;------------------------------------------------------------------------------
;; Test: spotify--normalized-status-type
;;------------------------------------------------------------------------------

;;(defun spotify--normalized-status-type (status)
;;  "Takes in a few kinds of STATUSes and returns a valid normalized STATUS.


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-field-raw
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-field-raw (status-n field-true translators)
;;   "STATUS-N must be normalized - i.e. STATUS-N should be passed
;; through `spotify--normalized-status-type' first.


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-translate
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-translate (field field-true value dictionary)


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-field
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-field (status field dictionary)
;;  "Returns value of FIELD in STATUS, or nil.


;;------------------------------------------------------------------------------
;; Test: spotify-player-status-field
;;------------------------------------------------------------------------------

;; (defun spotify-player-status-field (field &optional dictionary) ...
;;  "Returns value of FIELD in cached status, or nil.


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-format-field
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-format-field (input fmt-spec field
;;                                                  status dictionary)
;;  "Returns INPUT string with FMT-SPEC replaced by FIELD's value
;; from STATUS.


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-format
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-format (fmt-str &optional
;;                                               status dictionary)
;;   "Returns a formatted string based on FMT-STR and STATUS.


;;------------------------------------------------------------------------------
;; Test: spotify-player-status-get
;;------------------------------------------------------------------------------

;;(defun spotify-player-status-get (status)
;;  "Returns a formatted string of player status based on STATUS and formatted by


































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
  (spotify-ert/util/with-json-internal

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
  (spotify-ert/util/with-json-internal

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


;;------------------------------------------------------------------------------
;; Test Data
;;------------------------------------------------------------------------------

(defconst spotify-api-json-ert/data/player-status-in-full
  "{
    \"device\" : {
      \"id\" : \"1234567890\",
      \"is_active\" : true,
      \"is_private_session\" : false,
      \"is_restricted\" : false,
      \"name\" : \"Emacs AR Glasses 5001+ Pro#\",
      \"type\" : \"Computer\",
      \"volume_percent\" : 42
    },
    \"shuffle_state\" : false,
    \"repeat_state\" : \"off\",
    \"timestamp\" : 3,
    \"context\" : {
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
      },
      \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
      \"type\" : \"album\",
      \"uri\" : \"spotify:album:1234567890\"
    },
    \"progress_ms\" : 15611,
    \"item\" : {
      \"album\" : {
        \"album_type\" : \"album\",
        \"artists\" : [ {
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
          },
          \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
          \"id\" : \"1234567890\",
          \"name\" : \"\\\"Weird Al\\\" Yankovic\",
          \"type\" : \"artist\",
          \"uri\" : \"spotify:artist:1234567890\"
        } ],
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
        },
        \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
        \"id\" : \"1234567890\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
          \"width\" : 640
        }, {
          \"height\" : 300,
          \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
          \"width\" : 300
        }, {
          \"height\" : 64,
          \"url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
          \"width\" : 64
        } ],
        \"name\" : \"Mandatory Fun\",
        \"release_date\" : \"2014-07-15\",
        \"release_date_precision\" : \"day\",
        \"total_tracks\" : 12,
        \"type\" : \"album\",
        \"uri\" : \"spotify:album:1234567890\"
      },
      \"artists\" : [ {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
        },
        \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
        \"id\" : \"0123456789\",
        \"name\" : \"\\\"Weird Al\\\" Yankovic\",
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:1234567890\"
      } ],
      \"disc_number\" : 1,
      \"duration_ms\" : 142946,
      \"explicit\" : false,
      \"external_ids\" : {
        \"isrc\" : \"USRC11401404\"
      },
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
      },
      \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
      \"id\" : \"1234567890\",
      \"is_local\" : false,
      \"is_playable\" : true,
      \"name\" : \"Foil\",
      \"popularity\" : 49,
      \"preview_url\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
      \"track_number\" : 3,
      \"type\" : \"track\",
      \"uri\" : \"spotify:track:1234567890\"
    },
    \"currently_playing_type\" : \"track\",
    \"actions\" : {
      \"disallows\" : {
        \"resuming\" : true
      }
    },
    \"is_playing\" : true
   }"
"A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/me/player' endpoint.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
")

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-player-status-ert)
