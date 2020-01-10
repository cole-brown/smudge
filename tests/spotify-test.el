;;; spotify-test.el --- Test runner for Spotify ERT. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for Spotify.el code using ERT.

;; Do not load/require this file for normal spotify.el usage. Only bother if you
;; want to run the unit tests easily.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                          spotify-test.el tests                           --
;;------------------------------------------------------------------------------

;; Not requiring our files as I don't want to push the test dir into the load
;; path. Use `spotify-ert/load' instead.


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defconst spotify-ert/test-selector-regexp "spotify-ert/"
  "String that will select all our unit tests, as we have \"namespaced\" them
in the standard 'spotify-' \"namespace\" and then under 'ert/'.")


(defconst spotify-ert/test-buffer-name "*Spotify.el ERT Results*"
  "Name of buffer for ERT test run results to be displayed.")


;;------------------------------------------------------------------------------
;; Test Runners
;;------------------------------------------------------------------------------

(defun spotify-ert/load ()
  "Loads all Spotify.el unit test files."
  (interactive)

  ;; §-TODO-§ [2020-01-10]: Move spotify--eat-errors to somewhere better and
  ;; remove this.
  (require 'spotify-hydra)

  ;;---
  ;; Data
  ;;---
  (load-file "data/spotify-ert-data-connect-api.el")

  ;;---
  ;; Helpers
  ;;---
  (load-file "spotify-ert-helpers.el")
  (load-file "spotify-ert-functions.el")

  ;;---
  ;; Tests (in implementation order)
  ;;---
  (load-file "spotify-json-ert.el")
  (load-file "spotify-api-json-ert.el")
  (load-file "spotify-player-status-ert.el")
  (load-file "spotify-connect-ert.el"))


(defun spotify-ert/run ()
  "Runs all Spotify.el unit tests."
  (interactive)
  (ert spotify-ert/test-selector-regexp spotify-ert/test-buffer-name))
