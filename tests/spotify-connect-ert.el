;;; spotify-connect-ert.el --- Tests for spotify-connect.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-connect.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                        spotify-connect.el tests                          --
;;------------------------------------------------------------------------------

(require 'spotify-connect)


;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Test: spotify-when-device-active
;;------------------------------------------------------------------------------

;;(defun spotify-when-device-active (body)
(ert-deftest spotify-ert/spotify-when-device-active ()
  "Test that this macro correctly keys off of an active device.
"
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


(defmacro spotify--when-device->with-status (&rest body)


(defun spotify-connect-player-status ()

(defun spotify-connect-player-play-track (uri &optional context)

(defun spotify-connect-player-pause ()

(defun spotify-connect-player-play ()

(defun spotify-connect-player-toggle-play ()

(defun spotify-connect-player-next-track ()

(defun spotify-connect-player-previous-track ()

(defun spotify-connect-volume-up (amount)

(defun spotify-connect-volume-down (amount)

(defun spotify-connect-volume-mute-unmute (unmute-volume)

(defun spotify-connect-toggle-repeat ()

(defun spotify-connect-toggle-shuffle ()
























;;------------------------------------------------------------------------------
;; Test: spotify--cache-player-status-enabled-set
;;------------------------------------------------------------------------------

;; (defun spotify--cache-player-status-enabled-set (option-name value) ...
(ert-deftest spotify-ert/spotify--cache-player-status-enabled-set ()
  "Test that enabling caching via Customize enables caching."

  ;; Clear out whatever and set up for test.
  (spotify-ert/helper/reset-cache-enabled)

  ;; test with setq
  (setq spotify-cache-player-status-enabled t)
  ;; - no redirect
  (should-not (null spotify-cache-player-status-enabled))
  (should (null spotify--player-status-redirect))

  ;; reset
  (spotify-ert/helper/reset-cache-enabled)

  ;; test with customize-set-variable
  (customize-set-variable 'spotify-cache-player-status-enabled t)
  ;; - yes redirect
  (should-not (null spotify-cache-player-status-enabled))
  (should-not (null spotify--player-status-redirect))
  (should (eq spotify--player-status-redirect
              #'spotify--player-status-caching-closure))

  ;; reset
  (spotify-ert/helper/reset-cache-enabled)

  ;; test with instructions for doing manually
  (setq spotify-cache-player-status-enabled t)
  (setq spotify--player-status-redirect #'spotify--player-status-caching-closure)
  ;; - yes redirect
  (should-not (null spotify-cache-player-status-enabled))
  (should-not (null spotify--player-status-redirect))
  (should (eq spotify--player-status-redirect
              #'spotify--player-status-caching-closure))

  ;; reset
  (spotify-ert/helper/reset-cache-enabled)

  ;; test with test helper
  (spotify-ert/helper/enable-cache)
  ;; - yes redirect
  (should-not (null spotify-cache-player-status-enabled))
  (should-not (null spotify--player-status-redirect))
  (should (eq spotify--player-status-redirect
              #'spotify--player-status-caching-closure)))


;;------------------------------------------------------------------------------
;; Test: spotify--cache-set-status
;;------------------------------------------------------------------------------

;;(defun spotify--cache-set-status (status)
(ert-deftest spotify-ert/spotify--cache-set-status ()
  "Test that status is set correctly when this is called, based on caching
settings.
"
  (spotify-ert/helper/reset-cache-enabled)
  (setq spotify-ert/callback/status nil)
  (setq spotify--cache-player-status nil)

  (should (null spotify--cache-player-status))

  ;; No caching. Don't care - should still have saved status.
  (spotify--cache-set-status 'test-symbol-0)
  (should-not (null spotify--cache-player-status))
  (should (listp spotify--cache-player-status))
  (should (= (length spotify--cache-player-status) 2))
  (should (or (equal (nth 0 spotify--cache-player-status) (current-time))
              (time-less-p (nth 0 spotify--cache-player-status) (current-time))))
  (should (eq (nth 1 spotify--cache-player-status) 'test-symbol-0))

  ;; clear
  (setq spotify--cache-player-status nil)
  (should (null spotify--cache-player-status))

  ;; Enable caching. Don't care - should still have saved status.
  (spotify-ert/helper/enable-cache)
  (spotify--cache-set-status 'test-symbol-0)
  (should-not (null spotify--cache-player-status))
  (should (listp spotify--cache-player-status))
  (should (= (length spotify--cache-player-status) 2))
  (should (or (equal (nth 0 spotify--cache-player-status) (current-time))
              (time-less-p (nth 0 spotify--cache-player-status) (current-time))))
  (should (eq (nth 1 spotify--cache-player-status) 'test-symbol-0)))


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-caching-callback
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-caching-callback (callback status)
(ert-deftest spotify-ert/spotify--player-status-caching-callback ()
  "Test that `spotify--player-status-caching-callback' does the
proper caching, callbacks, etc.
"
  ;; caching doesn't (currently) matter - this function is after the decision to
  ;; cache or not.

  ;; no cache enabled first
  (setq spotify-ert/callback/status nil)
  (spotify-ert/helper/reset-cache-enabled)
  (should (null spotify-ert/callback/status))
  (spotify--player-status-caching-callback #'spotify-ert/helper/callback
                                           'test-symbol-0)
  (should (eq spotify-ert/callback/status 'test-symbol-0))

  ;; enable cache - no change
  (setq spotify-ert/callback/status nil)
  (spotify-ert/helper/enable-cache)
  (should (null spotify-ert/callback/status))
  (spotify--player-status-caching-callback #'spotify-ert/helper/callback
                                           'test-symbol-0)
  (should (eq spotify-ert/callback/status 'test-symbol-0)))


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-caching-closure
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-caching-closure (callback)
(ert-deftest spotify-ert/spotify--player-status-caching-closure ()
  "Test that closure generator function generates closures correctly.
"
  (spotify-ert/helper/reset-all)

  (add-hook 'spotify--cache-player-status-hook
            #'spotify-ert/helper/player-status-hook)

  (let ((func (spotify--player-status-caching-closure
               #'spotify-ert/helper/callback)))
    (should (null spotify-ert/hook/player-status))

    ;; 1. func should be a closure with one arg for status.
    (should-not (null func))
    (should (functionp func))

    ;; 2. func should call our spotify-ert/helper/callback when it is called.
    (should (null spotify-ert/callback/status))
    (should (null spotify-ert/hook/player-status))
    (funcall func 'test-symbol-0)
    (should-not (null spotify-ert/callback/status))
    (should (eq spotify-ert/callback/status 'test-symbol-0))

    ;; 3. func should call any hooks.
    (should-not (null spotify-ert/hook/player-status))
    (should (eq spotify-ert/hook/player-status 0)))) ;; called once


;;------------------------------------------------------------------------------
;; Test: spotify--cache-get-status-if
;;------------------------------------------------------------------------------

;;(defun spotify--cache-get-status-if (status)
(ert-deftest spotify-ert/spotify--cache-get-status-if ()
  "Test that cache status getter works.
"
  (spotify-ert/helper/reset-all)

  ;; nothing cached, nothing supplied - nil
  (should (null (spotify--cache-get-status-if nil)))

  ;; nothing cached, something supplied - get back supplied value
  (should (eq (spotify--cache-get-status-if 'test-symbol-0)
              'test-symbol-0))

  ;; something cached, nothing supplied - cache returned
  (spotify--cache-set-status 'test-symbol-1)
  (should (eq (spotify--cache-get-status-if nil)
              'test-symbol-1))

  ;; something cached, something supplied - get back supplied value
  (should (eq (spotify--cache-get-status-if 'test-symbol-2)
              'test-symbol-2)))


;;------------------------------------------------------------------------------
;; Test: spotify--cache-get-timestamp-if
;;------------------------------------------------------------------------------

;;(defun spotify--cache-get-timestamp-if (status)
(ert-deftest spotify-ert/spotify--cache-get-timestamp-if ()
  "Test that cache timestamp getter works.
"
  (spotify-ert/helper/reset-all)

  ;; nothing cached, nothing supplied - nil
  (should (null (spotify--cache-get-timestamp-if nil)))

  ;; nothing cached, something supplied - get back t
  (should (eq (spotify--cache-get-timestamp-if 'test-symbol-0)
              t))

  ;; something cached, nothing supplied - cache returned foo
  (spotify--cache-set-status 'test-symbol-1)
  (let ((time (spotify--cache-get-timestamp-if nil)))
    (should-not (null time))
    (should (or (equal time (current-time))
                (time-less-p time (current-time)))))

  ;; something cached, something supplied - get back t
  (should (eq (spotify--cache-get-timestamp-if 'test-symbol-2)
              t)))


;;------------------------------------------------------------------------------
;; Test: spotify--normalized-status-type
;;------------------------------------------------------------------------------

;;(defun spotify--normalized-status-type (status)
(ert-deftest spotify-ert/spotify--normalized-status-type ()
  "Test that status is normalized into nil/hash-table.
"
  (spotify-ert/helper/reset-all)

  ;; null should return null
  (should (null (spotify--normalized-status-type nil)))

  ;; string should return json hash-table
  (let ((status (spotify--normalized-status-type
                 spotify-player-status-ert/data/player-status-in-full)))
    (should-not (null status))
    (should (hash-table-p status)))

  ;; hash-table should just be returned.
  (spotify-ert/util/with-json spotify-player-status-ert/data/player-status-in-full
    (should-not (null json-obj))
    (should (hash-table-p json-obj))))


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-field-raw
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-field-raw (status-n field-true translators)
(ert-deftest spotify-ert/spotify--player-status-field-raw ()
  "Test that player status fields can be retrieved from json status.
"
  (spotify-ert/helper/reset-all)

  (spotify-ert/util/with-json spotify-player-status-ert/data/player-status-in-full

    (should (string= "\"Weird Al\" Yankovic"
                     (spotify--player-status-field-raw
                      json-obj
                      :artist
                      spotify--player-status-translators)))
    (should (string= "Foil"
                     (spotify--player-status-field-raw
                      json-obj
                      :track
                      spotify--player-status-translators)))))


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-translate
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-translate (field field-true value dictionary)
(ert-deftest spotify-ert/spotify--player-status-translate ()
  "Test that fields from json status are mutated correctly by
dictionaries/translators.
"
  (spotify-ert/helper/reset-all)

  ;; bool -> string
  (should (string= spotify-player-status-shuffling-text
                   (spotify--player-status-translate
                    :shuffling :shuffling-bool
                    t
                    spotify--player-status-translators)))

  (should (string= spotify-player-status-not-shuffling-text
                   (spotify--player-status-translate
                    :shuffling :shuffling-bool
                    nil
                    spotify--player-status-translators)))

  ;; string -> truncated string
  (let* ((long-str "Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
         (short-str (truncate-string-to-width long-str 15 0 nil "...")))
    (should (string= short-str
                   (spotify--player-status-translate
                    :artist :artist
                    long-str
                    spotify--player-status-translators)))))


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-field
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-field (status field dictionary)
(ert-deftest spotify-ert/spotify--player-status-field ()
  "Test that reading fields from status works and uses dictionaries/translators.
"
  (spotify-ert/helper/reset-all)

  (spotify-ert/util/with-json spotify-player-status-ert/data/player-status-in-full

    (should (string= "\"Weird Al\" Y..."
                     (spotify--player-status-field
                      json-obj
                      :artist
                      spotify--player-status-translators)))

    (should (string= "Foil"
                     (spotify--player-status-field
                      json-obj
                      :track
                      spotify--player-status-translators)))
    (should (= 3
               (spotify--player-status-field
                json-obj
                :track-number
                spotify--player-status-translators)))))


;;------------------------------------------------------------------------------
;; Test: spotify-player-status-field
;;------------------------------------------------------------------------------

;; (defun spotify-player-status-field (field &optional dictionary) ...
(ert-deftest spotify-ert/spotify-player-status-field ()
  "Test that reading fields from /cached/ status works and uses
dictionaries/translators.
"
  (spotify-ert/helper/reset-all)

  ;; no cached status - no fields
  (should (string= ""
                   (spotify-player-status-field
                    :artist
                    spotify--player-status-translators)))
  (should (string= ""
                   (spotify-player-status-field
                    :artist)))
  (should (eq nil
             (spotify-player-status-field
              :track-number)))

  ;; Cache this status.
  (spotify-ert/util/with-json spotify-player-status-ert/data/player-status-in-full
    (spotify--cache-set-status json-obj))

  ;; Now we should have fields.
  (should (string= "\"Weird Al\" Y..."
                   (spotify-player-status-field
                    :artist
                    spotify--player-status-translators)))
  (should (string= "\"Weird Al\" Y..."
                   (spotify-player-status-field
                    :artist)))

  (should (string= "Foil"
                   (spotify-player-status-field
                    :track
                    spotify--player-status-translators)))
  (should (string= "Foil"
                   (spotify-player-status-field
                    :track)))
  (should (= 3
             (spotify-player-status-field
              :track-number
              spotify--player-status-translators)))
  (should (= 3
             (spotify-player-status-field
              :track-number))))


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-format-field
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-format-field (input fmt-spec field
;;                                                  status dictionary)
(ert-deftest spotify-ert/spotify--player-status-format-field ()
  "Test that a single field is formatted correctly in string based on status.
"
  (spotify-ert/helper/reset-all)

  ;; player status object
  (spotify-ert/util/with-json spotify-player-status-ert/data/player-status-in-full

    (should (string= "\"Weird Al\" Y..."
                     (spotify--player-status-format-field
                      "%a"
                      "%a"
                      :artist
                      json-obj
                      spotify--player-status-translators)))

    (should (string= "Hello, \"Weird Al\" Y..."
                     (spotify--player-status-format-field
                      "Hello, %a"
                      "%a"
                      :artist
                      json-obj
                      spotify--player-status-translators)))

    (should (string= "This is track 3."
                     (spotify--player-status-format-field
                      "This is track %n."
                      "%n"
                      :track-number
                      json-obj
                      spotify--player-status-translators)))))


;;------------------------------------------------------------------------------
;; Test: spotify--player-status-format
;;------------------------------------------------------------------------------

;;(defun spotify--player-status-format (fmt-str &optional
;;                                               status dictionary)
(ert-deftest spotify-ert/spotify--player-status-format ()
  "Test that whole string is formatted correctly based on status.
"
  (spotify-ert/helper/reset-all)

  (spotify-ert/util/with-json spotify-player-status-ert/data/player-status-in-full

    (should (string= "\"Weird Al\" Y..."
                     (spotify--player-status-format
                      "%a"
                      json-obj
                      spotify--player-status-translators)))

    ;; not cached yet...
    (should (string= ""
                     (spotify--player-status-format
                      "%a")))

    ;; cache and try again
    (spotify--cache-set-status json-obj)
    (should (string= "\"Weird Al\" Y..."
                     (spotify--player-status-format
                      "%a")))

    (should (string= "\"Weird Al\" Y... - Foil (#3)"
                     (spotify--player-status-format
                      "%a - %t (#%n)")))

    (should (string= "-"
                     (spotify--player-status-format
                      "%m")))
    (should (string= "42"
                     (spotify--player-status-format
                      "%v")))))


;;------------------------------------------------------------------------------
;; Test: spotify-player-status-get
;;------------------------------------------------------------------------------

;; (defun spotify-player-status-get (status)
(ert-deftest spotify-ert/spotify-player-status-get ()
  "Test that string returned follows `spotify-player-status-format'.
"
  (spotify-ert/helper/reset-all)

  ;; something simple to start with...
  (setq spotify-player-status-format "%a - %t")

  ;; no status give and none cached - no string
  (should (string= ""
              (spotify-player-status-get nil)))

  (spotify-ert/util/with-json spotify-player-status-ert/data/player-status-in-full

    ;; still no.
    (should (string= ""
                     (spotify-player-status-get nil)))

    (should (string= "\"Weird Al\" Y... - Foil"
                     (spotify-player-status-get json-obj)))

    ;; cache and try again
    (spotify--cache-set-status json-obj)
    (should (string= "\"Weird Al\" Y... - Foil"
                     (spotify-player-status-get json-obj)))
    (should (string= "\"Weird Al\" Y... - Foil"
                     (spotify-player-status-get nil)))

    ;; more complex format now
    (setq spotify-player-status-format "%a - %t (#%n, %l): %p, %s, %r, %v, %m")
    (should (string= "\"Weird Al\" Y... - Foil (#3, 2:22): Playing, -, -, 42, -"
                     (spotify-player-status-get nil)))))


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

;; §-TODO-§ [2019-12-03]: move this to a central location, let all tests use it
;; from there.
(defconst spotify-player-status-ert/data/player-status-in-full
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
