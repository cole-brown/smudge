;;; spotify-player-status-obsolete.el --- obsoleted Player Status functions -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Spotify functions that have had their functionality superceded by
;; newer, better things.

;;; Code:

;;---------------------------------Spotify.el-----------------------------------
;;--                           Old and rusty code.                            --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Deprecated as of:
;;   version: ""
;;      date: []
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Player Status refactor work for status caching.
;;
;; Deprecated as of:
;;   version: "0.1.0"
;;      date: [2019-10-31]
;;------------------------------------------------------------------------------

(defun spotify-player-status-playing-indicator (str)
  "Return the value of the player state variable.
This value corresponding to the player's current state in STR."
  (cond ((string= "playing" str) spotify-player-status-playing-text)
        ((string= "stopped" str) spotify-player-status-stopped-text)
        ((string= "paused" str) spotify-player-status-paused-text)))

;; obsoleted by: (spotify-player-status-field :playing)
;;               (spotify-player-status-field :paused)
;;               (spotify-player-status-field :stopped)
(make-obsolete 'spotify-player-status-playing-indicator
               'spotify-player-status-field
               "0.1.0")


(defun spotify-player-status-shuffling-indicator (shuffling)
  "Return the value of the shuffling state variable.
This value corresponds to the current SHUFFLING state."
  (if (eq shuffling t)
      spotify-player-status-shuffling-text
    spotify-player-status-not-shuffling-text))

;; obsoleted by: (spotify-player-status-field :shuffling)
(make-obsolete 'spotify-player-status-shuffling-indicator
               'spotify-player-status-field
               "0.1.0")


(defun spotify-player-status-repeating-indicator (repeating)
  "Return the value of the repeating state variable.
This corresponds to the current REPEATING state."
  (if (eq repeating t)
      spotify-player-status-repeating-text
    spotify-player-status-not-repeating-text))

;; obsoleted by: (spotify-player-status-field :repeating)
(make-obsolete 'spotify-player-status-repeating-indicator
               'spotify-player-status-field
               "0.1.0")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-player-status-obsolete)
