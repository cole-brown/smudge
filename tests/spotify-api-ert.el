;;; spotify-api-ert.el --- Tests for spotify-api.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-api.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                          spotify-api.el tests                            --
;;------------------------------------------------------------------------------

;; Test Helpers
(require 'spotify-ert-mock-stub)
(require 'spotify-ert-data-functions)
(require 'spotify-ert-cache)
(require 'spotify-ert-setup)

;; Test Data
(require 'spotify-ert-data-connect-api)

;; Spotify.el Requirements
(require 'spotify-api)




;; §-TODO-§ [2020-01-21]: make better docstrings for some of theses...




;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------

(defvar spotify-ert/callback/caller-args nil
  "Args list passed into a callback.")


(defun spotify-ert/callback/args-1 (arg)
  "One argument callback helper function."
  (push arg spotify-ert/callback/caller-args))


;;------------------------------------------------------------------------------
;; Spotify-API.el: Mocks & Stubs
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-01-14]: delete?

;; (defun spotify-ert/mock/spotify-api-device-list (callback)
;;   "Mock Function."
;;   ;; Calls '/me/player/devices' Spotify Connect API endpoint.
;;   (spotify-ert/util/with-json
;;       ;; Choose device list with active device, or no active device, depending
;;       ;; on setup.
;;       (if spotify-ert/mock/spotify-api-device-list/is-active
;;           spotify-ert/data/connect-api/devices-list/active
;;         spotify-ert/data/connect-api/devices-list/inactive)
;;     ;; And just give back device list data to async callback.
;;     (when callback (funcall callback json-obj))))


;; (defun spotify-ert/mock/spotify-api-set-volume (device-id percentage &optional callback)
;;   "Mock Function."
;;   ;; Calls '/me/player/volume' Spotify Connect API endpoint.
;;   (setq spotify-ert/mock/volume-amount    percentage)
;;   (setq spotify-ert/mock/volume-device-id device-id)

;;   (push 'spotify-api-set-volume spotify-ert/stub/called))


;;---
;; Setup Functions
;;---

(defun spotify-ert/spotify-api/reset ()
  "Reset vars & stuff. Can be called in the middle of a test to
clear up for next item on test agenda."

  ;; General reset
  (spotify-ert/setup/reset)

  ;; Our stuff
  (setq spotify-ert/callback/caller-args nil))


(defun spotify-ert/spotify-api/setup ()
  "Per-test setup."

  ;; General
  (spotify-ert/spotify-api/reset)
  (spotify-ert/setup/setup))


(defun spotify-ert/spotify-api/teardown ()
  "Per-test teardown."

  ;; General
  (spotify-ert/setup/teardown))


;; NOTE: Not sure about testing this right now. I think this would be more of a
;; higher level/system test instead of a simple unit test.
;;
;; ;;---------------------------------------------------------------------------
;; ;; Test: spotify-oauth2-token
;; ;;---------------------------------------------------------------------------
;;
;; ;; (defun spotify-oauth2-token ()
;; ;;   "Retrieve the Oauth2 access token that must be used to interact with the
;; ;; Spotify API."
;;
;; (ert-deftest spotify-ert/spotify-oauth2-token ()
;;   "Test that `spotify-oauth2-token' works correctly.
;; "
;;   ;; Only test if we have valid credentials for this test, as it goes out to
;;   ;; actually ask Spotify Connect API for the token.
;;   (when (and (not (null spotify-oauth2-auth-url))
;;              (not (null spotify-oauth2-token-url))
;;              (not (null spotify-oauth2-client-id))
;;              (not (null spotify-oauth2-client-secret))
;;              (not (null spotify-oauth2-scopes))
;;              (not (null spotify-oauth2-callback)))
;;     (spotify-ert/spotify-api/setup)
;;
;;     (setq *spotify-oauth2-token* nil)
;;     (should (eq *spotify-oauth2-token* nil))
;;
;;     ;; Test that a oauth2 token is gotten when we don't have one yet?
;;     ;; TODO?
;;
;;     ;; Test that a new oauth2 token is gotten when old is expired?
;;     ;; TODO?
;;
;;     (spotify-ert/spotify-api/teardown)
;;     ))


;;---------------------------------------------------------------------------
;; Test: spotify-oauth2-token
;;---------------------------------------------------------------------------

;; (defun spotify-oauth2-token ()
;;   "Retrieve the Oauth2 access token that must be used to interact with the
;; Spotify API."

(ert-deftest spotify-ert/spotify-oauth2-token ()
  "This is merely a test to make sure we're not running real
spotify-oauth2-token code. That is, this is basically just a test of our test
setup and especially `spotify-ert/setup/error-out-functions'.
"

  (spotify-ert/spotify-api/setup)

  (setq *spotify-oauth2-token* nil)
  (should (eq *spotify-oauth2-token* nil))

  ;; Should error because we have replaced actual function with one that just
  ;; raises `error'.
  (should-error (spotify-oauth2-token))

  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-call-async
;;------------------------------------------------------------------------------

;; (defun spotify-api-call-async (method uri &optional data callback is-retry)
;;   "Make a request to the given Spotify service endpoint and calls CALLBACK
;; with the parsed JSON response."

(ert-deftest spotify-ert/spotify-api-call-async ()
  "This is merely a test to make sure we're not running real
spotify-oauth2-token code. That is, this is basically just a test of our test
setup and especially `spotify-ert/setup/error-out-functions'.
"
  (spotify-ert/spotify-api/setup)

  ;; Should error because we have replaced actual function with one that just
  ;; raises error.
  (should-error (spotify-api-call-async "GET" "/does-not-exist"))

  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-current-user
;;------------------------------------------------------------------------------

;; (defun spotify-current-user (callback)

(ert-deftest spotify-ert/spotify-current-user ()
  "Test what we can of `spotify-current-user'.
"
  (spotify-ert/spotify-api/setup)

  ;; Make sure `*spotify-user*' is nil.
  (setq *spotify-user* nil)
  (should (eq *spotify-user* nil))

  ;; Call. Should error calling `spotify-api-call-async' because we've stubbed
  ;; that out to just raise an error.
  (should-error (spotify-current-user #'spotify-ert/callback/args-1))

  ;; Set `*spotify-user*' to non-nil. This is not a valid "user object" as the
  ;; real one would get from the Spotify Connect API. Just a symbol to test
  ;; caching logic.
  (setq *spotify-user* 'test-user)
  ;; Call. Should return `*spotify-user*' as set.
  (spotify-current-user #'spotify-ert/callback/args-1)
  (should (memq 'test-user spotify-ert/callback/caller-args))

  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-items
;;------------------------------------------------------------------------------

;; (defun spotify-get-items (json)
;;   "Return the list of items from the given JSON object."

(ert-deftest spotify-ert/spotify-get-items ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)

  ;; §-TODO-§ [2020-01-15]: This should probably not die.
  ;; (should (eq (spotify-get-items nil) nil))

  (spotify-ert/util/with-json
      ;; Choose either valid data or nil, depending on setup.
      (if spotify-ert/mock/spotify-api-device-list/is-active
          spotify-ert/data/connect-api/search-artists
        nil)

    ;; Get spotify-api using spotify-api-json?
    (let* ((artists (gethash 'artists json-obj))
           (items (spotify-get-items artists)))
      ;; We have 'items' in our json-obj, and we have the right number...
      ;; so ok whatever.
      (should-not (null items))
      (should (= (length items) 5))))

  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-search-track-items
;;------------------------------------------------------------------------------

;; (defun spotify-get-search-track-items (json)
;;   "Return track items from the given search results JSON object."

(ert-deftest spotify-ert/spotify-get-search-track-items ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)

  ;; §-TODO-§ [2020-01-15]: This should probably not die.
  ;; (should (eq (spotify-get-search-track-items nil) nil))

  (spotify-ert/util/with-json
      ;; Choose either valid data or nil, depending on setup.
      (if spotify-ert/mock/spotify-api-device-list/is-active
          spotify-ert/data/connect-api/search-tracks
        nil)

    ;; Get spotify-api using spotify-api-json?
    (let* ((tracks (spotify-get-search-track-items json-obj)))
      ;; We have 'tracks' in our json-obj, and we have the right number...
      ;; so ok whatever.
      (should-not (null tracks))
      (should (= (length tracks) 5))))

  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-search-playlist-items
;;------------------------------------------------------------------------------

;; (defun spotify-get-search-playlist-items (json)
;;   "Return playlist items from the given search results JSON object."

(ert-deftest spotify-ert/spotify-get-search-playlist-items ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)

  (spotify-ert/util/with-json
      ;; Choose either valid data or nil, depending on setup.
      (if spotify-ert/mock/spotify-api-device-list/is-active
          spotify-ert/data/connect-api/search-playlists
        nil)

    ;; Get spotify-api using spotify-api-json?
    (let* ((playlists (spotify-get-search-playlist-items json-obj)))
      ;; We have 'playlists' in our json-obj, and we have the right number...
      ;; so ok whatever.
      (should-not (null playlists))
      (should (= (length playlists) 5))))

  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-message
;;------------------------------------------------------------------------------

;; (defun spotify-get-message (json)
;;   "Return the message from the featured playlists JSON object."

(ert-deftest spotify-ert/spotify-get-message ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)

  (spotify-ert/util/with-json
      ;; Choose either valid data or nil, depending on setup.
      (if spotify-ert/mock/spotify-api-device-list/is-active
          spotify-ert/data/connect-api/featured-playlists
        nil)

    (should (string= (spotify-get-message json-obj)
                     "This is the test featured playlist data!")))

  ;; §-TODO-§ [2020-01-21]: This should probably not die.
  ;; (setq spotify-ert/mock/spotify-api-device-list/is-active nil)
  ;; (spotify-ert/util/with-json
  ;;     ;; Choose either valid data or nil, depending on setup.
  ;;     (if spotify-ert/mock/spotify-api-device-list/is-active
  ;;         spotify-ert/data/connect-api/featured-playlists
  ;;       nil)
  ;;
  ;;   (should (eq (spotify-get-message json-obj)
  ;;               nil)))

  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-playlist-tracks
;;------------------------------------------------------------------------------

;; (defun spotify-get-playlist-tracks (json)
;;   "Return the list of tracks from the given playlist JSON object."

(ert-deftest spotify-ert/spotify-get-playlist-tracks ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)

  (spotify-ert/util/with-json
      ;; Choose either valid data or nil, depending on setup.
      (if spotify-ert/mock/spotify-api-device-list/is-active
          spotify-ert/data/connect-api/playlist-tracks
        nil)

    (let ((tracks (spotify-get-playlist-tracks json-obj)))
      (should (= (length tracks) 2))
      (should (string= (spotify-get-track-artist-name (nth 0 tracks))
                       "test-artist-0"))

      ;; It's the example data from the docs, and the 2nd item is pretty
      ;; sparse... But should it be null now?
      ;; That's the existing functionality, so we'll go with it for now.
      (should (null (nth 1 tracks)))))

  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-track-album
;;------------------------------------------------------------------------------

;; (defun spotify-get-track-album (json)
;;   "Return the simplified album object from the given track JSON object."

(ert-deftest spotify-ert/spotify-get-track-album ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-track-number
;;------------------------------------------------------------------------------

;; (defun spotify-get-track-number (json)
;;   "Return the track number from the given track JSON object."

(ert-deftest spotify-ert/spotify-get-track-number ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-disc-number
;;------------------------------------------------------------------------------

;; (defun spotify-get-disc-number (json)
;;   "Return the disc number from the given track JSON object."

(ert-deftest spotify-ert/spotify-get-disc-number ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-track-duration
;;------------------------------------------------------------------------------

;; (defun spotify-get-track-duration (json)
;;   "Return the track duration, in milliseconds, from the given track JSON object."

(ert-deftest spotify-ert/spotify-get-track-duration ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-track-duration-formatted
;;------------------------------------------------------------------------------

;; (defun spotify-get-track-duration-formatted (json)
;;   "Return the formatted track duration from the given track JSON object."

(ert-deftest spotify-ert/spotify-get-track-duration-formatted ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-track-album-name
;;------------------------------------------------------------------------------

;; (defun spotify-get-track-album-name (json)
;;   "Return the album name from the given track JSON object."

(ert-deftest spotify-ert/spotify-get-track-album-name ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-track-artist
;;------------------------------------------------------------------------------

;; (defun spotify-get-track-artist (json)
;;   "Return the first simplified artist object from the given track JSON object."

(ert-deftest spotify-ert/spotify-get-track-artist ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-track-artist-name
;;------------------------------------------------------------------------------

;; (defun spotify-get-track-artist-name (json)
;;   "Return the first artist name from the given track JSON object."

(ert-deftest spotify-ert/spotify-get-track-artist-name ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-track-popularity
;;------------------------------------------------------------------------------

;; (defun spotify-get-track-popularity (json)
;;   "Return the popularity from the given track/album/artist JSON object."

(ert-deftest spotify-ert/spotify-get-track-popularity ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-is-track-playable
;;------------------------------------------------------------------------------

;; (defun spotify-is-track-playable (json)
;;   "Return whether the given track JSON object represents a playable track by
;; the current user."

(ert-deftest spotify-ert/spotify-is-track-playable ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-item-name
;;------------------------------------------------------------------------------

;; (defun spotify-get-item-name (json)
;;   "Return the name from the given track/album/artist JSON object."

(ert-deftest spotify-ert/spotify-get-item-name ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-item-id
;;------------------------------------------------------------------------------

;; (defun spotify-get-item-id (json)
;;   "Return the id from the given JSON object."

(ert-deftest spotify-ert/spotify-get-item-id ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-item-uri
;;------------------------------------------------------------------------------

;; (defun spotify-get-item-uri (json)
;;   "Return the uri from the given track/album/artist JSON object."

(ert-deftest spotify-ert/spotify-get-item-uri ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-playlist-track-count
;;------------------------------------------------------------------------------

;; (defun spotify-get-playlist-track-count (json)
;;   "Return the number of tracks of the given playlist JSON object."

(ert-deftest spotify-ert/spotify-get-playlist-track-count ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-get-playlist-owner-id
;;------------------------------------------------------------------------------

;; (defun spotify-get-playlist-owner-id (json)
;;   "Return the owner id of the given playlist JSON object."

(ert-deftest spotify-ert/spotify-get-playlist-owner-id ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-search
;;------------------------------------------------------------------------------

;; (defun spotify-api-search (type query page callback)
;;   "Search artists, albums, tracks or playlists that match a keyword string,
;; depending on the `type' argument."

(ert-deftest spotify-ert/spotify-api-search ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-featured-playlists
;;------------------------------------------------------------------------------

;; (defun spotify-api-featured-playlists (page callback)
;;   "Return the given page of Spotify's featured playlists."

(ert-deftest spotify-ert/spotify-api-featured-playlists ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-user-playlists
;;------------------------------------------------------------------------------

;; (defun spotify-api-user-playlists (user-id page callback)
;;   "Return the playlists for the given user."

(ert-deftest spotify-ert/spotify-api-user-playlists ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-playlist-create
;;------------------------------------------------------------------------------

;; (defun spotify-api-playlist-create (user-id name is-public callback)
;;   "Create a new playlist with the given name for the given user."

(ert-deftest spotify-ert/spotify-api-playlist-create ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-playlist-add-track
;;------------------------------------------------------------------------------

;; (defun spotify-api-playlist-add-track (user-id playlist-id track-id callback)
;;   "Add single track to playlist."

(ert-deftest spotify-ert/spotify-api-playlist-add-track ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-format-id
;;------------------------------------------------------------------------------

;; (defun spotify-format-id (type id)
;;   "Wrap raw id to type if necessary."

(ert-deftest spotify-ert/spotify-format-id ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-playlist-add-tracks
;;------------------------------------------------------------------------------

;; (defun spotify-api-playlist-add-tracks (user-id playlist-id track-ids callback)
;;   "Add tracks in list track-ids in playlist."

(ert-deftest spotify-ert/spotify-api-playlist-add-tracks ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-playlist-follow
;;------------------------------------------------------------------------------

;; (defun spotify-api-playlist-follow (playlist callback)
;;   "Add the current user as a follower of a playlist."

(ert-deftest spotify-ert/spotify-api-playlist-follow ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-playlist-unfollow
;;------------------------------------------------------------------------------

;; (defun spotify-api-playlist-unfollow (playlist callback)
;;   "Remove the current user as a follower of a playlist."

(ert-deftest spotify-ert/spotify-api-playlist-unfollow ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-playlist-tracks
;;------------------------------------------------------------------------------

;; (defun spotify-api-playlist-tracks (playlist page callback)
;;   "Return the tracks of the given user's playlist."

(ert-deftest spotify-ert/spotify-api-playlist-tracks ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-album-tracks
;;------------------------------------------------------------------------------

;; (defun spotify-api-album-tracks (album page callback)
;;   "Return the tracks for the given album."

(ert-deftest spotify-ert/spotify-api-album-tracks ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-popularity-bar
;;------------------------------------------------------------------------------

;; (defun spotify-popularity-bar (popularity)
;;   "Return the popularity indicator bar proportional to the given parameter,
;; which must be a number between 0 and 100."

(ert-deftest spotify-ert/spotify-popularity-bar ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-recently-played
;;------------------------------------------------------------------------------

;; (defun spotify-api-recently-played (page callback)
;;   "Retrieve the list of recently played tracks."

(ert-deftest spotify-ert/spotify-api-recently-played ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-device-list
;;------------------------------------------------------------------------------

;; (defun spotify-api-device-list (callback)
;;   "Call CALLBACK with the list of devices available for use with Spotify Connect."

(ert-deftest spotify-ert/spotify-api-device-list ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-transfer-player
;;------------------------------------------------------------------------------

;; (defun spotify-api-transfer-player (device-id &optional callback)
;;   "Transfer playback to DEVICE-ID and determine if it should start playing."

(ert-deftest spotify-ert/spotify-api-transfer-player ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-set-volume
;;------------------------------------------------------------------------------

;; (defun spotify-api-set-volume (device-id percentage &optional callback)
;;   "Set the volume level to PERCENTAGE of max for DEVICE-ID."

(ert-deftest spotify-ert/spotify-api-set-volume ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-get-player-status
;;------------------------------------------------------------------------------

;; (defun spotify-api-get-player-status (callback)
;;   "Get the Spotify Connect status of the currently active player."

(ert-deftest spotify-ert/spotify-api-get-player-status ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-play
;;------------------------------------------------------------------------------

;; (defun spotify-api-play (&optional callback uri context)
;;   "Play a track. If no args, resume playing current track. Otherwise, play URI in CONTEXT."

(ert-deftest spotify-ert/spotify-api-play ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-pause
;;------------------------------------------------------------------------------

;; (defun spotify-api-pause (&optional callback)
;;   "Pause the currently playing track."

(ert-deftest spotify-ert/spotify-api-pause ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-next
;;------------------------------------------------------------------------------

;; (defun spotify-api-next (&optional callback)
;;   "Skip to the next track."

(ert-deftest spotify-ert/spotify-api-next ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-previous
;;------------------------------------------------------------------------------

;; (defun spotify-api-previous (&optional callback)
;;   "Skip to the previous track."

(ert-deftest spotify-ert/spotify-api-previous ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-repeat
;;------------------------------------------------------------------------------

;; (defun spotify-api-repeat (state &optional callback)
;;   "Set repeat of current track to STATE."

(ert-deftest spotify-ert/spotify-api-repeat ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-api-shuffle
;;------------------------------------------------------------------------------

;; (defun spotify-api-shuffle (state &optional callback)
;;   "Set repeat of current track to STATE."

(ert-deftest spotify-ert/spotify-api-shuffle ()
  "Test that this is a thing and something happens maybe.
"
  (spotify-ert/spotify-api/setup)



  (spotify-ert/spotify-api/teardown))






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


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-api-ert)
