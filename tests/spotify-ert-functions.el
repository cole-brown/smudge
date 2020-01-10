;;; spotify-ert-functions.el --- Helpers for spotify.el unit tests. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit test helpers for spotify-connect.el code using ERT.

;; Macro for making stub/mocks. Setup functions.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                    Mocks & Stubs & Helpers, oh my.                       --
;;------------------------------------------------------------------------------

(require 'cl) ;; cl-flet*
(require 'spotify-ert-helpers)

;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------

(defvar spotify-ert/stub/called nil
  "List of symbols or nil.")


(defvar spotify-ert/mock/spotify-api-device-list/is-active t
  "Non-nil for letting spotify-when-device-active execute, nil for blocking.")


(defvar spotify-ert/setup/error-out-functions/orig nil
  "Data saved about original function definitons.")


;;------------------------------------------------------------------------------
;; Mock / Stub Macros
;;------------------------------------------------------------------------------

;; With a bit of help from:
;; https://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
(defmacro spotify-ert/mock (func &optional mock &rest body)
  "Replaces FUNC with MOCK for duration of BODY (`cl-letf' binding).

If MOCK is nil, FUNC will be replaced with a function with symbol name:
  `spotify-ert/mock/FUNC'.

Sets up BODY to not actually call Spotify Connect
API... Just calls fake handlers and then we can inspect status
and return what we want.

FUNC should be the function symbol to be replaced (e.g. message).
MOCK should be the function symbol to replace it (e.g. my-message-tester).

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


(defmacro spotify-ert/stub (func &rest body)
  "Replaces FUNC with a stub for duration of BODY (`cl-letf' binding).

The stub will just push the FUNC symbol onto the
`spotify-ert/stub/called' list, so be sure to clear that out as
needed.

Sets up BODY to not actually call Spotify Connect
API... Just calls fake handlers and then we can inspect status
and return what we want.

FUNC should be the function symbol to be replaced (e.g. message).

Executes BODY forms if successful setting up mock functions.
"
  (declare (indent defun))
  `(let* ((func-sym ,func))
     ;; Set FUNC to be a lambda that just pushes FUNC symbol to called list.
     (cl-letf (((symbol-function func-sym)
                (lambda (&rest ignored)
                  (push func-sym spotify-ert/stub/called))))
       ,@body)))
;; (defun xx (fmt &rest args) (message "xx: %S" (format fmt args)))
;; (macroexpand '(spotify-ert/stub 'xx (xx "hello?")))
;; spotify-ert/stub/called
;; (spotify-ert/stub 'xx (xx "hello?"))
;; spotify-ert/stub/called


;;------------------------------------------------------------------------------
;; Setup / Reset
;;------------------------------------------------------------------------------

(defun spotify-ert/setup/reset ()
  "Start-of or during test reset.
"
  ;;---
  ;; Reset Spotify.el
  ;;---
  (spotify-ert/teardown/error-out-functions)

  (setq spotify-player-status-format "[%p: %a - %t ◷ %l %r%s]")
  (setq spotify-player-status   nil)

  ;;---
  ;; Reset Tests
  ;;---

  ;; Our common mocks and stubs.
  (setq spotify-ert/mock/spotify-api-device-list/is-active t)

  (setq spotify-ert/stub/called                    nil)
  (setq spotify-ert/util/with-json/munger          nil)
  (setq spotify-ert/setup/error-out-functions/orig nil))


(defun spotify-ert/setup/setup ()
  "Start-of-test setup steps.
"
  ;; Setup fail functions.
  (spotify-ert/setup/error-out-functions)
  ;; Reset vars & things.
  (spotify-ert/setup/reset))


(defun spotify-ert/setup/teardown ()
  "End-of-test teardown steps.
"
  ;; Revert fail functions.
  (spotify-ert/teardown/error-out-functions))


;;------------------------------------------------------------------------------
;; Error out of these by default
;;------------------------------------------------------------------------------

(defun spotify-ert/setup/error-out (func)
  "Set up FUNC to throw an error. Uses `fset' to replace the function def with
this error throwing one. Will save the old function definition for reverting to.
"
  (push '(func (symbol-function 'func))
        spotify-ert/setup/error-out-functions/orig)

  (fset func
        (lambda (&rest ignored)
          "Error-Out function."
          (error (concat "(error-out) "
                         (symbol-name func)
                         ": Intentionally dying here - find an upstream "
                         "function to terminate the test at.")))))


(defun spotify-ert/setup/error-out-functions ()
  "Set up functions we don't want to get to via tests to throw errors.
"
  (spotify-ert/setup/error-out 'spotify-oauth2-token)
  (spotify-ert/setup/error-out 'spotify-api-call-async))


;; §-TODO-§ [2020-01-10]: don't think this works...
(defun spotify-ert/teardown/error-out-functions ()
  "Reverts all the functions in `spotify-ert/setup/error-out-functions/orig'.
"
  (dolist (cell spotify-ert/setup/error-out-functions/orig)
    (when cell
      (fset (nth 0 cell) (nth 1 cell)))))

;;-
;;-----
;;-------
;;-----------
;;------------------------------------------------------------------------------
;; Ignore these, then delete them.
;;------------------------------------------------------------------------------
;;-----------
;;-------
;;-----
;;-

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

(defun spotify-ert/mock/spotify-api-transfer-player (device-id &optional callback)
  "Mock Function."
  ;; Calls '/me/player' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-play (&optional callback uri context)
  "Mock Function."
  ;; Calls '/me/player/play' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-pause (&optional callback)
  "Mock Function."
  ;; Calls '/me/player/pause' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-next (&optional callback)
  "Mock Function."
  ;; Calls '/me/player/next' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-previous (&optional callback)
  "Mock Function."
  ;; Calls '/me/player/previous' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-repeat (state &optional callback)
  "Mock Function."
  ;; Calls '/me/player/repeat' Spotify Connect API endpoint.
  )


(defun spotify-ert/mock/spotify-api-shuffle (state &optional callback)
  "Mock Function."
  ;; Calls '/me/player/shuffle' Spotify Connect API endpoint.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-ert-functions)
