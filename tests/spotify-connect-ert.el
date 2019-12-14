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
;; spotify-api: setup stubs
;;------------------------------------------------------------------------------

(defun spotify-ert/mock/spotify-oauth2-token ()
  "Mock function."
  ;; returns *spotify-oauth2-token*
  nil)


(defun spotify-ert/mock/spotify-api-call-async (method uri &optional data callback is-retry)
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
  ).


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


;; §-TODO-§ [2019-12-11]: you are here, mucking about with stubs...
(defun spotify-ert/mock/spotify-api-playlist-add-track (user-id playlist-id track-id callback)
  "Mock Function."
  (spotify-api-playlist-add-tracks user-id playlist-id (list track-id) callback))


(defun spotify-ert/mock/spotify-format-id (type id)
  "Mock Function."
   (if (string-match-p "spotify" id) (format "\"%s\"" id) (format "\"spotify:%s:%s\"" type id)))


(defun spotify-ert/mock/spotify-api-playlist-add-tracks (user-id playlist-id track-ids callback)
  "Mock Function."
  (let ((tracks (format "%s" (mapconcat (lambda (x) (spotify-format-id "track" x)) track-ids ","))))
    (spotify-api-call-async
     "POST"
     (format "/users/%s/playlists/%s/tracks"
             (url-hexify-string user-id) (url-hexify-string playlist-id))
     (format "{\"uris\": [ %s ]}" tracks)
     callback)))


(defun spotify-ert/mock/spotify-api-playlist-follow (playlist callback)
  "Mock Function."
  (let ((owner (spotify-get-playlist-owner-id playlist))
        (id (spotify-get-item-id playlist)))
    (spotify-api-call-async
     "PUT"
     (format "/users/%s/playlists/%s/followers"
             (url-hexify-string owner)
             (url-hexify-string id))
     nil
     callback)))


(defun spotify-ert/mock/spotify-api-playlist-unfollow (playlist callback)
  "Mock Function."
  (let ((owner (spotify-get-playlist-owner-id playlist))
        (id (spotify-get-item-id playlist)))
    (spotify-api-call-async
     "DELETE"
     (format "/users/%s/playlists/%s/followers"
             (url-hexify-string owner)
             (url-hexify-string id))
     nil
     callback)))


(defun spotify-ert/mock/spotify-api-playlist-tracks (playlist page callback)
  "Mock Function."
  (let ((owner (spotify-get-playlist-owner-id playlist))
        (id (spotify-get-item-id playlist))
        (offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat (format "/users/%s/playlists/%s/tracks?"
                     (url-hexify-string owner)
                     (url-hexify-string id))
             (url-build-query-string `((limit  ,spotify-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))


(defun spotify-ert/mock/spotify-api-album-tracks (album page callback)
  "Mock Function."
  (let ((album-id (spotify-get-item-id album))
        (offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat (format "/albums/%s/tracks?"
                     (url-hexify-string album-id))
             (url-build-query-string `((limit ,spotify-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))


(defun spotify-ert/mock/spotify-popularity-bar (popularity)
  "Mock Function."
  (let ((num-bars (truncate (/ popularity 10))))
    (concat (make-string num-bars ?X)
            (make-string (- 10 num-bars) ?-))))


(defun spotify-ert/mock/spotify-api-recently-played (page callback)
  "Mock Function."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat "/me/player/recently-played?"
             (url-build-query-string `((limit  ,spotify-api-search-limit)
                                       (offset ,offset))
                                     nil t))
     nil
     callback)))


(defun spotify-ert/mock/spotify-api-device-list (callback)
  "Mock Function."
  (let ((callback callback))
    (spotify-api-call-async
     "GET"
     "/me/player/devices"
     nil
     callback)))


(defun spotify-ert/mock/spotify-api-transfer-player (device-id &optional callback)
  "Mock Function."
  (spotify-api-call-async
   "PUT"
   "/me/player"
   (format "{\"device_ids\":[\"%s\"]}" device-id)
   callback))


(defun spotify-ert/mock/spotify-api-set-volume (device-id percentage &optional callback)
  "Mock Function."
  (spotify-api-call-async
   "PUT"
   (concat "/me/player/volume?"
           (url-build-query-string `((volume_percent ,percentage)
                                     (device_id      ,device-id))
                                   nil t))
   nil
   callback))


(defun spotify-ert/mock/spotify-api-get-player-status (callback)
  "Mock Function."
  (let ((callback (if (functionp spotify--player-status-redirect)
                      (funcall spotify--player-status-redirect callback)
                    callback)))
    (spotify-api-call-async
     "GET"
     "/me/player"
     nil
     callback)))


(defun spotify-ert/mock/spotify-api-play (&optional callback uri context)
  "Mock Function."
  (spotify-api-call-async
   "PUT"
   "/me/player/play"
   (concat " { "
           (cond ((and uri context) (format "\"context_uri\": \"%s\", \"offset\": {\"uri\": \"%s\"}" context uri))
                 (context           (format "\"context_uri\": \"%s\"" context))
                 (uri               (format "\"uris\": [ \"%s\" ]" uri))
                 (t                  ""))
           " } ")
   callback))


(defun spotify-ert/mock/spotify-api-pause (&optional callback)
  "Mock Function."
  (spotify-api-call-async
   "PUT"
   "/me/player/pause"
   nil
   callback))


(defun spotify-ert/mock/spotify-api-next (&optional callback)
  "Mock Function."
  (spotify-api-call-async
   "POST"
   "/me/player/next"
   nil
   callback))


(defun spotify-ert/mock/spotify-api-previous (&optional callback)
  "Mock Function."
  (spotify-api-call-async
   "POST"
   "/me/player/previous"
   nil
   callback))


(defun spotify-ert/mock/spotify-api-repeat (state &optional callback)
  "Mock Function."
  (spotify-api-call-async
   "PUT"
   (concat "/me/player/repeat?"
           (url-build-query-string `((state ,state))
                                   nil t))
   nil
   callback))


(defun spotify-ert/mock/spotify-api-shuffle (state &optional callback)
  "Mock Function."
  (spotify-api-call-async
   "PUT"
   (concat "/me/player/shuffle?"
           (url-build-query-string `((state ,state))
                                   nil t))
   nil
   callback))

;;---
;; §-TODO-§ [2019-12-11]: Setup for mocs?
;;---

(defun spotify-ert/json-api-setup ()
  "Sets up these tests to not actually call Spotify Connect
API... Just calls fake handlers and then we can inspect status
and return what we want.
p
Could do a different, system-level test or something if full
emacs to spotify api test is desired.
"
  ;; Think we need to stub out... all of spotify-api.el?
  )


;;------------------------------------------------------------------------------
;; Test: spotify-when-device-active
;;------------------------------------------------------------------------------

;;(defun spotify-when-device-active (body)
(ert-deftest spotify-ert/spotify-when-device-active ()
  "Test that this macro only executes body when there is an active device.
"
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify--when-device->with-status
;;------------------------------------------------------------------------------

;;(defun spotify--when-device->with-status (&rest body)
(ert-deftest spotify-ert/spotify--when-device->with-status ()
  "Test that macro only executes body when device is active and provides status
obj.
"
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))

;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-status
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-status ()
(ert-deftest spotify-ert/spotify-connect-player-status ()
  "Test that this gets player status from Spotify Connect API.
"
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-play-track
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-play-track (uri &optional context)
(ert-deftest spotify-ert/spotify-connect-player-play-track ()
  "Test that this requests a track be played to Spotify Connect API.
"
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-pause
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-pause ()
(ert-deftest spotify-ert/spotify-connect-player-pause ()
  "Test that this requests device pauses play.
"
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-play
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-play ()
(ert-deftest spotify-ert/spotify-connect-player-play ()
  "Test that this requests device plays.
"
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-toggle-play
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-toggle-play ()
(ert-deftest spotify-ert/spotify-connect-player-toggle-play ()
  "Test that this request play/pause status toggle.
"
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-next-track
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-next-track ()
(ert-deftest spotify-ert/spotify-connect-player-next-track ()
  "Test that this requests skip to next track.
"
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
  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <tests here>


    ))

















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
