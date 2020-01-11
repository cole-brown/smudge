;;; spotify-player-status-ert.el --- Helpers for spotify.el unit tests. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit test helpers for spotify.el's status caching.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                           Status Cache Help                              --
;;------------------------------------------------------------------------------


;; Test Helpers
(require 'spotify-ert-data-functions)
(require 'spotify-ert-mock-stub)
;; (require 'spotify-ert-setup)

;; Test Data
;; (require 'spotify-ert-data-connect-api)

;; Spotify.el Requirements
(require 'spotify-player-status)


;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------

(defun spotify-ert/cache/reset-all ()
  "Resets vars/settings associated with these tests.
"
  (spotify-ert/cache/reset-cache-enabled)

  (setq spotify-ert/cache/callback/status nil)
  (setq spotify--cache-player-status nil)

  (setq spotify-ert/cache/hook/player-status nil)
  (setq spotify--cache-player-status-hook nil))


(defun spotify-ert/cache/reset-cache-enabled ()
  "Sets `spotify-cache-player-status-enabled' and
`spotify--player-status-redirect' to nil.
"
  (setq spotify-cache-player-status-enabled nil)
  (setq spotify--player-status-redirect nil)

  (should (null spotify-cache-player-status-enabled))
  (should (null spotify--player-status-redirect)))


(defun spotify-ert/cache/enable ()
  "Sets `spotify-cache-player-status-enabled' and
`spotify--player-status-redirect' to enabled.
"
  (customize-set-variable 'spotify-cache-player-status-enabled t)

  (should-not (null spotify-cache-player-status-enabled))
  (should-not (null spotify--player-status-redirect))
  (should (eq spotify--player-status-redirect
              #'spotify--player-status-caching-closure)))


(defun spotify-ert/cache/callback (status)
  "Callback function for aiding testing."
  (setq spotify-ert/cache/callback/status status))


(defun spotify-ert/cache/player-status-hook ()
  "Do something to mark that a hook call happened.
"
  (setq spotify-ert/cache/hook/player-status
        (if (null spotify-ert/cache/hook/player-status)
            0
          (1+ spotify-ert/cache/hook/player-status))))


(defvar spotify-ert/cache/callback/status nil
  "`spotify-ert/cache/callback' will save its status arg here.")


(defvar spotify-ert/cache/hook/player-status nil
  "`spotify-ert/helper/player-status-hook' will note a hook call here.")
