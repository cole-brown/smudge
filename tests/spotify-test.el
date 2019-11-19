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

;; Require all our test files so ERT learns about all the spotify tests now that
;; this has been required/loaded.
(require 'spotify-json-ert)
(require 'spotify-api-json-ert)

;; §-TODO-§ [2019-11-18]: an autoload? That will just autoload the tests?


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defconst spotify-ert/test-selector-regexp "spotify-ert/"
  "String that will select all our unit tests, as we have \"namespaced\" them
in the standard 'spotify-' \"namespace\" and then under 'ert/'.")


(defconst spotify-ert/test-selector-regexp "*Spotify.el ERT Results*"
  "Name of buffer for ERT test run results to be displayed.")


;;------------------------------------------------------------------------------
;; Test Runners
;;------------------------------------------------------------------------------

(defun spotify-ert/run ()
  "Runs all Spotify.el unit tests."
  (interactive)
  (ert spotify-ert/test-selector-regexp ))
