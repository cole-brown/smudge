;;; spotify-api-json-ert.el --- Tests for spotify-api-json.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-api-json.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                        spotify-api-json.el tests                         --
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-11-18]: put this directory into the load path somehow/where.

(require 'spotify-api-json)

(require 'spotify-ert-helpers)

;; §-TODO-§ [2019-11-15]: Have a main func for requiring all test files so ert
;; learns about all the spotify tests only if we want to run them?

;; §-TODO-§ [2019-11-15]: Have a main func that invokes that one, and then runs
;; all the spotify tests?


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Test: spotify--api-json-get-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-json-get-field (type json field) ...
(ert-deftest spotify-ert/spotify--api-json-get-field ()
  "Test that `spotify--api-json-get-field' can verify a field
belongs to a Spotify Connect API JSON object type, and can get
the field from the JSON object."
  (spotify-ert/util/with-json spotify-api-json-ert/data/player-status
    (should (spotify--api-json-get-field spotify--api-player-status
                                         json-obj
                                         'device))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api-player-status
                           json-obj
                           'device)))

    (should (string= "off"
                     (spotify--api-json-get-field spotify--api-player-status
                                                  json-obj
                                                  'repeat_state)))

    (should (eq :json-false
                (spotify--api-json-get-field spotify--api-player-status
                                             json-obj
                                             'shuffle_state)))

    ;; is_playing
    ;; item

    ;;---
    ;; Don't care about/use these currently, but they exist so test?
    ;;---
    (should (spotify--api-json-get-field spotify--api-player-status
                                         json-obj
                                         'context))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api-player-status
                           json-obj
                           'context)))

    (should (= 1490252122574
               (spotify--api-json-get-field spotify--api-player-status
                                            json-obj
                                            'timestamp)))

    (should (string= "44272"
               (spotify--api-json-get-field spotify--api-player-status
                                            json-obj
                                            'progress_ms)))

    (should (string= "track"
               (spotify--api-json-get-field spotify--api-player-status
                                            json-obj
                                            'currently_playing_type)))

    (should (spotify--api-json-get-field spotify--api-player-status
                                         json-obj
                                         'actions))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api-player-status
                           json-obj
                           'actions)))))
;; §-TODO-§ [2019-11-18]: Test devices or some other json object?

;;------------------------------------------------------------------------------
;; Test: spotify--api-player-status-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-player-status-field (status keyword) ...
(ert-deftest spotify-ert/spotify--api-player-status-field ()
  "§-TODO-§ [2019-11-18] THIS"
)



;;------------------------------------------------------------------------------
;; Test: spotify--api-track-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-track-field (track keyword) ...
(ert-deftest spotify-ert/spotify--api-track-field ()
  "§-TODO-§ [2019-11-18] THIS"
)



;;------------------------------------------------------------------------------
;; Test: spotify--api-artist-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-artist-field (artist keyword) ...
(ert-deftest spotify-ert/spotify--api-artist-field ()
  "§-TODO-§ [2019-11-18] THIS"
)



;;------------------------------------------------------------------------------
;; Test: spotify--api-device-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-device-field (device keyword) ...
(ert-deftest spotify-ert/spotify--api-device-field ()
  "§-TODO-§ [2019-11-18] THIS"
)


;;------------------------------------------------------------------------------
;; Test: spotify--api-object-get-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-object-get-field (object key type) ...
(ert-deftest spotify-ert/spotify--api-object-get-field ()
  "§-TODO-§ [2019-11-18] THIS"
)


;;------------------------------------------------------------------------------
;; Test: spotify--api-devices
;;------------------------------------------------------------------------------

;; (defun spotify--api-devices (json) ...
(ert-deftest spotify-ert/spotify--api-devices ()
  "Pretty simple test right now... Grabs devices list from API return."
  (spotify-ert/util/with-json spotify-api-json-ert/data/devices-list
    (let ((devices (spotify--api-devices json-obj))
          (bools '(t :json-false)))
      ;; We got something vaguely right, right?
      (should-not (null devices))
      (should (listp devices))
      (should (= 3 (length devices)))

      ;; The fields are the right types and exist, right?
      (dolist (device devices)
        (should (stringp (gethash 'id device)))
        (should (member (gethash 'is_active device) bools))
        (should (member (gethash 'is_private_session device) bools))
        (should (member (gethash 'is_restricted device) bools))
        (should (stringp (gethash 'name device)))
        (should (stringp (gethash 'type device)))
        (should (integerp (gethash 'volume_percent device)))
        ;; nil for field that doesn't exist
        (should (null (gethash 'field-dne device)))))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-player-status
;;------------------------------------------------------------------------------

;; (defun spotify--api-player-status (json keyword) ...
(ert-deftest spotify-ert/spotify--api-player-status ()
  "Test that the function can find all the expected keywords in the
player-status return from \"/v1/me/player\" endpoint of Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
"
  ;; (spotify-ert/util/with-json spotify-api-json-ert/data/player-status
  ;;   (spotify--api-player-status json-obj )

  ;;   (let ((status )
  ;;         (bools '(t :json-false)))
  ;;     ;; We got something vaguely right, right?
  ;;     (should-not (null devices))
  ;;     (should (listp devices))
  ;;     (should (= 3 (length devices)))

  ;;     ;; The fields are the right types and exist, right?
  ;;     (dolist (device devices)
  ;;       (should (stringp (gethash 'id device)))
  ;;       (should (member (gethash 'is_active device) bools))
  ;;       (should (member (gethash 'is_private_session device) bools))
  ;;       (should (member (gethash 'is_restricted device) bools))
  ;;       (should (stringp (gethash 'name device)))
  ;;       (should (stringp (gethash 'type device)))
  ;;       (should (integerp (gethash 'volume_percent device)))
  ;;       ;; nil for field that doesn't exist
  ;;       (should (null (gethash 'field-dne device))))))
  )


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

(defconst spotify-api-json-ert/data/devices-list
  "{
      \"devices\": [
         {
            \"id\": \"b46689a4cd5\",
            \"is_active\": true,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Your MacBook\",
            \"type\": \"Computer\",
            \"volume_percent\": 70
         },
         {
            \"id\": \"0d184899bc8\",
            \"is_active\": false,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Living Room\",
            \"type\": \"TV\",
            \"volume_percent\": 25
         },
         {
            \"id\": \"2f3c360198ede6\",
            \"is_active\": false,
            \"is_private_session\": false,
            \"is_restricted\": false,
            \"name\": \"Office Speaker\",
            \"type\": \"Unknown\",
            \"volume_percent\": 82
         }
      ]
   }"
  "A sample return value from Spotify Connect API
'/v1/me/player/devices' endpoint.

https://developer.spotify.com/documentation/web-api/guides/using-connect-web-api/
")


(defconst spotify-api-json-ert/data/player-status
  "{
    \"timestamp\": 1490252122574,
    \"device\": {
      \"id\": \"3f228e06c8562e2f439e22932da6c3231715ed53\",
      \"is_active\": false,
      \"is_restricted\": false,
      \"name\": \"Emacs Phone 3000\",
      \"type\": \"Smartphone\",
      \"volume_percent\": 54
    },
    \"progress_ms\": \"44272\",
    \"is_playing\": true,
    \"currently_playing_type\": \"track\",
    \"actions\": {
      \"disallows\": {
        \"resuming\": true
      }
    },
    \"item\": {},
    \"shuffle_state\": false,
    \"repeat_state\": \"off\",
    \"context\": {
      \"external_urls\" : {
        \"spotify\" : \"http://open.spotify.com/user/spotify/playlist/49znshcYJROspEqBoHg3Sv\"
      },
      \"href\" : \"https://api.spotify.com/v1/users/spotify/playlists/49znshcYJROspEqBoHg3Sv\",
      \"type\" : \"playlist\",
      \"uri\" : \"spotify:user:spotify:playlist:49znshcYJROspEqBoHg3Sv\"
    }
  }"
  "A sample return value from Spotify Connect API
'/v1/me/player' endpoint.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-api-json-ert)
