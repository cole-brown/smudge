;;; spotify-api-json-ert.el --- Tests for spotify-api-json.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-api-json.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                        spotify-api-json.el tests                         --
;;------------------------------------------------------------------------------

(require 'spotify-api-json)


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
  (spotify-ert/util/with-json spotify-api-json-ert/data/player-status-truncated
    (should (spotify--api-json-get-field spotify--api/data/player-status
                                         json-obj
                                         'device))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api/data/player-status
                           json-obj
                           'device)))

    (should (string= "off"
                     (spotify--api-json-get-field spotify--api/data/player-status
                                                  json-obj
                                                  'repeat_state)))

    (should (eq :json-false
                (spotify--api-json-get-field spotify--api/data/player-status
                                             json-obj
                                             'shuffle_state)))
    (should (eq t
                (spotify--api-json-get-field spotify--api/data/player-status
                                             json-obj
                                             'is_playing)))

    (should (spotify--api-json-get-field spotify--api/data/player-status
                                         json-obj
                                         'item))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api/data/player-status
                           json-obj
                           'item)))

    ;;---
    ;; Don't care about/use these currently, but they exist so test?
    ;;---
    (should (spotify--api-json-get-field spotify--api/data/player-status
                                         json-obj
                                         'context))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api/data/player-status
                           json-obj
                           'context)))

    (should (= 1490252122574
               (spotify--api-json-get-field spotify--api/data/player-status
                                            json-obj
                                            'timestamp)))

    (should (string= "44272"
                     (spotify--api-json-get-field spotify--api/data/player-status
                                                  json-obj
                                                  'progress_ms)))

    (should (string= "track"
                     (spotify--api-json-get-field spotify--api/data/player-status
                                                  json-obj
                                                  'currently_playing_type)))

    (should (spotify--api-json-get-field spotify--api/data/player-status
                                         json-obj
                                         'actions))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api/data/player-status
                           json-obj
                           'actions)))))
;; §-TODO-§ [2019-11-18]: Test devices or some other json object?


;;------------------------------------------------------------------------------
;; Test: spotify--api-object-get-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-object-get-field (object key type) ...

(ert-deftest spotify-ert/spotify--api-object-get-field/player-status ()
  "Tests the functionality of `spotify--api-object-get-field' on a player-status
object.
"
  ;; player status object
  (spotify-ert/util/with-json spotify-api-json-ert/data/player-status-in-full

    (should (eq nil
                (spotify--api-object-get-field
                 json-obj
                 (assoc :shuffling-bool spotify--keyword->api-field)
                 spotify--api/data/player-status)))

    (should (eq nil
                (spotify--api-object-get-field
                 json-obj
                 (assoc :repeating-bool spotify--keyword->api-field)
                 spotify--api/data/player-status)))

    (should (eq t
                (spotify--api-object-get-field
                 json-obj
                 (assoc :playing-bool spotify--keyword->api-field)
                 spotify--api/data/player-status)))))


(ert-deftest spotify-ert/spotify--api-object-get-field/artist ()
  "Tests the functionality of `spotify--api-object-get-field' on an artist
object.
"
  ;; artist object
  (spotify-ert/util/with-json spotify-api-json-ert/data/artist

    (should (string= "\"Weird Al\" Yankovic"
                     (spotify--api-object-get-field
                      json-obj
                      (assoc :artist spotify--keyword->api-field)
                      spotify--api/data/artist-simple)))))


(ert-deftest spotify-ert/spotify--api-object-get-field/track ()
  "Tests the functionality of `spotify--api-object-get-field' on a track
object.
"
  ;; track object
  (spotify-ert/util/with-json spotify-api-json-ert/data/track

    (should (string= "Foil"
                     (spotify--api-object-get-field
                      json-obj
                      (assoc :track spotify--keyword->api-field)
                      spotify--api/data/track-full)))

    (should (= 3
               (spotify--api-object-get-field
                json-obj
                (assoc :track-number spotify--keyword->api-field)
                spotify--api/data/track-full)))

    (should (= 142946
               (spotify--api-object-get-field
                json-obj
                (assoc :duration-millisecond spotify--keyword->api-field)
                spotify--api/data/track-full)))))


(ert-deftest spotify-ert/spotify--api-object-get-field/device ()
  "Tests the functionality of `spotify--api-object-get-field' on a device
object.
"
  ;; device object
  (spotify-ert/util/with-json spotify-api-json-ert/data/device

    (should (string= "1234567890"
                     (spotify--api-object-get-field
                      json-obj
                      (assoc :device-active-id spotify--keyword->api-field)
                      spotify--api/data/device-full)))

    (should (string= "Emacs AR Glasses 5001+ Pro#"
                     (spotify--api-object-get-field
                      json-obj
                      (assoc :device-active-name spotify--keyword->api-field)
                      spotify--api/data/device-full)))

    (should (eq t
                (spotify--api-object-get-field
                 json-obj
                 (assoc :device-active-state spotify--keyword->api-field)
                 spotify--api/data/device-full)))

    (should (= 42
               (spotify--api-object-get-field
                json-obj
                (assoc :volume spotify--keyword->api-field)
                spotify--api/data/device-full)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-device-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-device-field (device keyword) ...
(ert-deftest spotify-ert/spotify--api-device-field ()
  "Test whether `spotify--api-device-field' returns expected values
for keywords."
  ;; device object
  (spotify-ert/util/with-json spotify-api-json-ert/data/device

    (should (string= "1234567890"
                     (spotify--api-device-field
                      json-obj
                      :device-active-id)))

    (should (string= "Emacs AR Glasses 5001+ Pro#"
                     (spotify--api-device-field
                      json-obj
                      :device-active-name)))

    (should (eq t
                (spotify--api-device-field
                 json-obj
                 :device-active-state)))

    (should (= 42
               (spotify--api-device-field
                json-obj
                :volume)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-artist-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-artist-field (artist keyword) ...
(ert-deftest spotify-ert/spotify--api-artist-field ()
  "Test whether `spotify--api-artist-field' returns expected values
for keywords."
  ;; artist object
  (spotify-ert/util/with-json spotify-api-json-ert/data/artist

    (should (string= "\"Weird Al\" Yankovic"
                     (spotify--api-artist-field
                      json-obj
                      :artist)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-track-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-track-field (track keyword) ...
(ert-deftest spotify-ert/spotify--api-track-field ()
  "Test whether `spotify--api-track-field' returns expected values
for keywords."
  ;; track object
  (spotify-ert/util/with-json spotify-api-json-ert/data/track

    (should (string= "Foil"
                     (spotify--api-track-field
                      json-obj
                      :track)))

    (should (= 3
               (spotify--api-track-field
                json-obj
                :track-number)))

    (should (= 142946
               (spotify--api-track-field
                json-obj
                :duration-millisecond)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-player-status-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-player-status-field (status keyword) ...
(ert-deftest spotify-ert/spotify--api-player-status-field ()
  "Tests whether `spotify--api-player-status-field' returns expected values
for keywords."

  ;; player status object
  (spotify-ert/util/with-json spotify-api-json-ert/data/player-status-in-full

    (should (eq nil
                (spotify--api-player-status-field
                 json-obj
                 :shuffling-bool)))

    (should (eq nil
                (spotify--api-player-status-field
                 json-obj
                 :repeating-bool)))

    (should (eq t
                (spotify--api-player-status-field
                 json-obj
                 :playing-bool)))))


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
  (spotify-ert/util/with-json spotify-api-json-ert/data/player-status-in-full
    (setq debug-on-error t)
    (should (eq nil
                (spotify--api-player-status
                 json-obj
                 :shuffling-bool)))

    (should (eq nil
                (spotify--api-player-status
                 json-obj
                 :repeating-bool)))

    (should (eq t
                (spotify--api-player-status
                 json-obj
                 :playing-bool)))

    (should (string= "\"Weird Al\" Yankovic"
                     (spotify--api-player-status
                      json-obj
                      :artist)))
    (should (string= "Foil"
                     (spotify--api-player-status
                      json-obj
                      :track)))

    (should (= 3
               (spotify--api-player-status
                json-obj
                :track-number)))

    (should (= 142946
               (spotify--api-player-status
                json-obj
                :duration-millisecond)))

    (should (string= "1234567890"
                     (spotify--api-player-status
                      json-obj
                      :device-active-id)))

    (should (string= "Emacs AR Glasses 5001+ Pro#"
                     (spotify--api-player-status
                      json-obj
                      :device-active-name)))

    (should (eq t
                (spotify--api-player-status
                 json-obj
                 :device-active-state)))

    (should (= 42
               (spotify--api-player-status
                json-obj
                :volume)))))


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


(defconst spotify-api-json-ert/data/player-status-truncated
  "{
    \"timestamp\": 1490252122574,
    \"device\": {
      \"id\": \"0123456789\",
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
        \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/49znshcYJROspEqBoHg3Sv\"
      },
      \"href\" : \"https://api.spotify.com/v1/users/spotify/playlists/49znshcYJROspEqBoHg3Sv\",
      \"type\" : \"playlist\",
      \"uri\" : \"spotify:user:spotify:playlist:49znshcYJROspEqBoHg3Sv\"
    }
  }"
  "A sample return value from Spotify Connect API '/v1/me/player'
endpoint. Sample is very minimal; see
`spotify-api-json-ert/data/player-status-in-full' for much
longer, fuller one.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
")


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


(defconst spotify-api-json-ert/data/artist
  "{
     \"external_urls\" : {
       \"spotify\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\"
     },
     \"href\" : \"https://open.spotify.com/user/spotify/playlist/1234567890\",
     \"id\" : \"1234567890\",
     \"name\" : \"\\\"Weird Al\\\" Yankovic\",
     \"type\" : \"artist\",
     \"uri\" : \"spotify:artist:1234567890\"
   }"
"A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/me/player' endpoint, cut down to just an artist object.

https://developer.spotify.com/documentation/web-api/reference/object-model/#artist-object-simplified
")


(defconst spotify-api-json-ert/data/track
  "{
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
   }"
"A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/me/player' endpoint, cut down to just a track object.

https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full
")


(defconst spotify-api-json-ert/data/device
  "{
     \"id\" : \"1234567890\",
     \"is_active\" : true,
     \"is_private_session\" : false,
     \"is_restricted\" : false,
     \"name\" : \"Emacs AR Glasses 5001+ Pro#\",
     \"type\" : \"Computer\",
     \"volume_percent\" : 42
   }"
"A sample (actual (-ish, editted/sanitized it)) return value from Spotify Connect API
'/v1/me/player' endpoint, cut down to just a device object.

https://developer.spotify.com/documentation/web-api/reference/player/get-a-users-available-devices/
")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-api-json-ert)
