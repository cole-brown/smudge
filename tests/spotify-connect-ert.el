;;; spotify-connect-ert.el --- Tests for spotify-connect.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-connect.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                        spotify-connect.el tests                          --
;;------------------------------------------------------------------------------

(require 'cl) ;; cl-flet*
(require 'spotify-connect)


;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-01-08]: move to a common place?
(defvar spotify-ert/mock/called nil
  "List of symbols or nil.")


(defvar spotify-ert/mock/spotify-api-device-list/is-active t
  "Non-nil for letting spotify-when-device-active execute, nil for blocking.")


;; §-TODO-§ [2020-01-08]: move other vars here


;;------------------------------------------------------------------------------
;; spotify-api: setup stubs
;;------------------------------------------------------------------------------

(defun spotify-ert/mock/spotify-oauth2-token ()
  "Mock function."
  ;; returns *spotify-oauth2-token*
  nil)


(defun spotify-ert/mock/spotify-api-call-async
    (method uri &optional data callback is-retry)
  "Mock Function."
  ;; returns value of oauth2-url-retrieve - json object?
  nil)


(defun spotify-ert/mock/spotify-current-user (callback)
  "Mock Function."
  ;; ask spotify for use, set *spotify-user*, return user to callback.
  )


(defun spotify-ert/mock/spotify-get-items (json)
  "Mock Function."
  ;; Just returns 'items from json...
  )


(defun spotify-ert/mock/spotify-get-search-track-items (json)
  "Mock Function."
  ;; Just returns 'tracks from json..
  )


(defun spotify-ert/mock/spotify-get-search-playlist-items (json)
  "Mock Function."
  ;; Just returns 'playlists from json.
  )


(defun spotify-ert/mock/spotify-get-message (json)
  "Mock Function."
  ;; Just returns 'message from json.
  )


(defun spotify-ert/mock/spotify-get-playlist-tracks (json)
  "Mock Function."
  )


(defun spotify-ert/mock/spotify-get-search-playlist-items (json)
  "Mock Function."
  ;; Just gets 'plalists from json.
  )


(defun spotify-ert/mock/spotify-get-track-album (json)
  "Mock Function."
  ;; Just gets 'album from json.
  )


(defun spotify-ert/mock/spotify-get-track-number (json)
  "Mock Function."
  ;; Just gets 'track_number from json.
  )


(defun spotify-ert/mock/spotify-get-disc-number (json)
  "Mock Function."
  ;; Just gets 'disc_number from json.
  )


(defun spotify-ert/mock/spotify-get-track-duration (json)
  "Mock Function."
  ;; Just gets 'duration_ms from json.
  )


(defun spotify-ert/mock/spotify-get-track-duration-formatted (json)
  "Mock Function."
  ;; Just gets formatted track duration from json.
  )


(defun spotify-ert/mock/spotify-get-track-album-name (json)
  "Mock Function."
  ;; Just gets 'name from track...
  )


(defun spotify-ert/mock/spotify-get-track-artist (json)
  "Mock Function."
  ;; Just gets first of track artists from track...
  )


(defun spotify-ert/mock/spotify-get-track-artist-name (json)
  "Mock Function."
  ;; Just gets first of track artists from track...
  )


(defun spotify-ert/mock/spotify-get-track-popularity (json)
  "Mock Function."
  ;; Just gets 'popularity from track...
  )


(defun spotify-ert/mock/spotify-is-track-playable (json)
  "Mock Function."
  ;; Gets playable flag, converts to emacs bool...
  )


(defun spotify-ert/mock/spotify-get-item-name (json)
  "Mock Function."
  ;; Just gets 'name from json...
  )


(defun spotify-ert/mock/spotify-get-item-id (json)
  "Mock Function."
  ;; Just gets 'id from json...
  )


(defun spotify-ert/mock/spotify-get-item-uri (json)
  "Mock Function."
  ;; Just gets 'uri from json...
  )


(defun spotify-ert/mock/spotify-get-playlist-track-count (json)
  "Mock Function."
  ;; Just gets 'total from 'tracks from json...
  )


(defun spotify-ert/mock/spotify-get-playlist-owner-id (json)
  "Mock Function."
  ;; Just gets 'owner from 'tracks from json...
  )


(defun spotify-ert/mock/spotify-api-search (type query page callback)
  "Mock Function."
  ;; Calls "/search" Spotify Connect API endpoint with a query.
  )


(defun spotify-ert/mock/spotify-api-featured-playlists (page callback)
  "Mock Function."
  ;; Calls 'featured-playlist' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-user-playlists (user-id page callback)
  "Mock Function."
  ;; Calls 'users/<user>/playlists' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-playlist-create (user-id name
                                                     is-public callback)
  "Mock Function."
  ;; Calls 'users/<user>/playlists' Spotify Connect API endpoint to create a
  ;; playlist.
  )


(defun spotify-ert/mock/spotify-api-playlist-add-track
    (user-id playlist-id track-id callback)
  "Mock Function."
  ;; Calls spotify-api-playlist-add-tracks for this one track.
  )


(defun spotify-ert/mock/spotify-format-id (type id)
  "Mock Function."
  ;; turns args into a... 'spotify id'?
  )


(defun spotify-ert/mock/spotify-api-playlist-add-tracks
    (user-id playlist-id track-ids callback)
  "Mock Function."
  ;; Calls 'users/<user>/playlists/<playlist>/tracks" Spotify Connect API
  ;; endpoint to add the list of track ids to the playlist.
  )


(defun spotify-ert/mock/spotify-api-playlist-follow (playlist callback)
  "Mock Function."
  ;; Calls '/users/<user>/playlists/<playlist>/followers' Spotify Connect API
  ;; endpoint. To... follow playlist?
  )


(defun spotify-ert/mock/spotify-api-playlist-unfollow (playlist callback)
  "Mock Function."
  ;; Calls '/users/<user>/playlists/<playlist>/followers' Spotify Connect API
  ;; endpoint. To... unfollow playlist?
  )


(defun spotify-ert/mock/spotify-api-playlist-tracks (playlist page callback)
  "Mock Function."
  ;; Calls '/users/<user>/playlists/<playlist>/followers' Spotify Connect API
  ;; endpoint. To... unfollow playlist?
  )


(defun spotify-ert/mock/spotify-api-album-tracks (album page callback)
  "Mock Function."
  ;; Calls '/albums/%s/tracks' Spotify Connect API
  ;; endpoint.
  )


(defun spotify-ert/mock/spotify-popularity-bar (popularity)
  "Mock Function."
  ;; Converts popularity int into... progress bar kinda thing.
  )


(defun spotify-ert/mock/spotify-api-recently-played (page callback)
  "Mock Function."
  ;; Calls '/me/player/recently-played' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-device-list (callback)
  "Mock Function."
  ;; Calls '/me/player/devices' Spotify Connect API endpoint.

  (spotify-ert/util/with-json
      ;; Choose device list with active device, or no active device, depending
      ;; on setup.
      (if spotify-ert/mock/spotify-api-device-list/is-active
          spotify-connect-ert/data/devices-list/active
        spotify-connect-ert/data/devices-list/inactive)
    ;; And just give back device list data to async callback.
    (when callback (funcall callback json-obj))))


(defun spotify-ert/mock/spotify-api-transfer-player (device-id &optional callback)
  "Mock Function."
  ;; Calls '/me/player' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-set-volume (device-id percentage &optional callback)
  "Mock Function."
  ;; Calls '/me/player/volume' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-get-player-status (callback)
  "Mock Function."
  ;; Calls '/me/player' Spotify Connect API endpoint.


  ;; Get our full status data...
  (spotify-ert/util/with-json
      spotify-player-status-ert/data/player-status-in-full
    ;; ...and just give it to the callback.
    (when callback (funcall callback json-obj))))


(defun spotify-ert/mock/spotify-api-play (&optional callback uri context)
  "Mock Function."
  ;; Calls '/me/player/play' Spotify Connect API endpoint.
  (push 'spotify-api-play spotify-ert/mock/called))


(defun spotify-ert/mock/spotify-api-pause (&optional callback)
  "Mock Function."
  ;; Calls '/me/player/pause' Spotify Connect API endpoint.
  (push 'spotify-api-pause spotify-ert/mock/called))


(defun spotify-ert/mock/spotify-api-next (&optional callback)
  "Mock Function."
  ;; Calls '/me/player/next' Spotify Connect API endpoint.
  (push 'spotify-api-next spotify-ert/mock/called))


(defun spotify-ert/mock/spotify-api-previous (&optional callback)
  "Mock Function."
  ;; Calls '/me/player/previous' Spotify Connect API endpoint.
  (push 'spotify-api-previous spotify-ert/mock/called))


(defun spotify-ert/mock/spotify-api-repeat (state &optional callback)
  "Mock Function."
  ;; Calls '/me/player/repeat' Spotify Connect API endpoint.
  (push 'spotify-api-repeat spotify-ert/mock/called))


(defun spotify-ert/mock/spotify-api-shuffle (state &optional callback)
  "Mock Function."
  ;; Calls '/me/player/shuffle' Spotify Connect API endpoint.
  (push 'spotify-api-shuffle spotify-ert/mock/called))


;;---
;; Setup Functions
;;---


(defun spotify-ert/spotify-connect/setup ()
  "Per-test setup/reset."
  (setq spotify-ert/mock/called nil)
  (setq spotify-ert/mock/spotify-api-device-list/is-active t)
  (setq spotify-player-status nil))


;; With a bit of help from:
;; https://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
(defmacro spotify-ert/mock (func &optional mock &rest body)
  "Sets up these tests to not actually call Spotify Connect
API... Just calls fake handlers and then we can inspect status
and return what we want.

Could do a different, system-level test or something if full
emacs to spotify api test is desired.

FUNC should be the function symbol to be replaced (e.g. message).
MOCK should be the function symbol to replace it.

Executes BODY forms if successful setting up mock functions.
"
  (declare (indent defun))
  `(let* ((func-sym ,func)
          (mock-sym (or ,mock
                        (intern (concat "spotify-ert/mock/"
                                        (symbol-name func-sym))))))
     (cl-letf (((symbol-function func-sym) mock-sym))
       ,@body)))
;; (defun xx (fmt &rest args) (message "xx: %S" (format fmt args)))
;; (defun yy (fmt &rest args) (message "yy: %S" (format fmt args)))
;; (defun spotify-ert/mock/xx (f &rest a) (message "zz: %S" (format f a)))
;; (macroexpand '(spotify-ert/mock 'xx 'yy (xx "hello?")))
;; (spotify-ert/mock 'xx 'yy (xx "hello?"))
;; (spotify-ert/mock 'xx nil (xx "hello?"))



;;------------------------------------------------------------------------------
;; Test: spotify-when-device-active
;;------------------------------------------------------------------------------

;;(defun spotify-when-device-active (body)
(ert-deftest spotify-ert/spotify-when-device-active ()
  "Test that this macro only executes body when there is an active device.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil

    ;; run tests
    (let ((entered-body nil))
      (setq spotify-ert/mock/spotify-api-device-list/is-active t)
      (spotify-when-device-active
       (should (eq t t))
       (setq entered-body 'test-symbol-0))
      (should (eq entered-body 'test-symbol-0)))

    (let ((entered-body nil))
      (setq spotify-ert/mock/spotify-api-device-list/is-active nil)
      (spotify-when-device-active
       (should (eq nil t))
       (setq entered-body 'test-symbol-0))
      (should (eq entered-body nil)))))


;;------------------------------------------------------------------------------
;; Test: spotify--when-device->with-status
;;------------------------------------------------------------------------------

;;(defun spotify--when-device->with-status (&rest body)
(ert-deftest spotify-ert/spotify--when-device->with-status ()
  "Test that macro only executes body when device is active and provides status
obj.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (spotify-ert/mock 'spotify-api-get-player-status nil

    ;; run tests
    (let ((entered-body nil))
      (setq spotify-ert/mock/spotify-api-device-list/is-active t)
      (spotify--when-device->with-status
        (should (eq t
                    (spotify--api-player-status-field
                     status
                     :playing-bool)))
        (setq entered-body 'test-symbol-0))
      (should (eq entered-body 'test-symbol-0)))

    (let ((entered-body nil))
      (setq spotify-ert/mock/spotify-api-device-list/is-active nil)
      (spotify--when-device->with-status
        (should (eq nil t))
        (setq entered-body 'test-symbol-0))
      (should (eq entered-body nil))))))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-status
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-status ()
(ert-deftest spotify-ert/spotify-connect-player-status ()
  "Test that this gets player status from Spotify Connect API.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)

  (spotify-ert/mock 'spotify-api-get-player-status nil
    (should (string= "[Playing: \"Weird Al\" Y... - Foil ◷ 2:22 --]"
                     (spotify-connect-player-status)))

    (setq spotify-player-status nil)
    (setq spotify-ert/mock/spotify-api-device-list/is-active nil)
    (should (eq nil
                (spotify-connect-player-status)))))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-play-track
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-play-track (uri &optional context)
(ert-deftest spotify-ert/spotify-connect-player-play-track ()
  "Test that this requests a track be played to Spotify Connect API.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-play nil
    ;; This function does nothing. Just a passthrough to spotify-api.el.
    ;; So we can't really test anything.
    (should (eq t t))))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-pause
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-pause ()
(ert-deftest spotify-ert/spotify-connect-player-pause ()
  "Test that this requests device pauses play.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-pause nil
    ;; This function does nothing. Just a passthrough to spotify-api.el.
    ;; So we can't really test anything.
    (should (eq t t))))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-play
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-play ()
(ert-deftest spotify-ert/spotify-connect-player-play ()
  "Test that this requests device plays.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-play nil
    ;; This function does nothing. Just a passthrough to spotify-api.el.
    ;; So we can't really test anything.
    (should (eq t t))))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-toggle-play
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-toggle-play ()
(ert-deftest spotify-ert/spotify-connect-player-toggle-play ()
  "Test that this request play/pause status toggle.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-play nil
    (spotify-ert/mock 'spotify-api-pause nil
      ;; This function doesn't do much, but we can at least make sure it calls
      ;; play or pause as appropriate.

      ;; §-TODO-§ [2020-01-07]: <--now-->

      (should (eq t t)))))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-next-track
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-next-track ()
(ert-deftest spotify-ert/spotify-connect-player-next-track ()
  "Test that this requests skip to next track.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (ignore)
    )

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-previous-track
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-previous-track ()
(ert-deftest spotify-ert/spotify-connect-player-previous-track ()
  "Test that this requests skip to previous track.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (ignore)
    )

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-volume-up
;;------------------------------------------------------------------------------

;;(defun spotify-connect-volume-up (amount)
(ert-deftest spotify-ert/spotify-connect-volume-up ()
  "Test that this request a volume increase of a certain amount.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (ignore)
    )

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-volume-down
;;------------------------------------------------------------------------------

;;(defun spotify-connect-volume-down (amount)
(ert-deftest spotify-ert/spotify-connect-volume-down ()
  "Test that this request a volume decrease of a certain amount.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (ignore)
    )

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-volume-mute-unmute
;;------------------------------------------------------------------------------

;;(defun spotify-connect-volume-mute-unmute (unmute-volume)
(ert-deftest spotify-ert/spotify-connect-volume-mute-unmute ()
  "Test that this requests mute to 0 volume, or unmute to unmute-volume.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (ignore)
    )

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-toggle-repeat
;;------------------------------------------------------------------------------

;;(defun spotify-connect-toggle-repeat ()
(ert-deftest spotify-ert/spotify-connect-toggle-repeat ()
  "Test that this requests repeat flag toggle.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (ignore)
    )

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-toggle-shuffle
;;------------------------------------------------------------------------------

;;(defun spotify-connect-toggle-shuffle ()
(ert-deftest spotify-ert/spotify-connect-toggle-shuffle ()
  "Test that this requests shuffle flag toggle.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (ignore)
    )

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


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


;; §-TODO-§ [2019-12-03]: move this to a central location, let all tests use it
;; from there.
(defconst spotify-connect-ert/data/devices-list/active
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


(defconst spotify-connect-ert/data/devices-list/inactive
  "{
      \"devices\": [
         {
            \"id\": \"b46689a4cd5\",
            \"is_active\": false,
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


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-player-status-ert)
