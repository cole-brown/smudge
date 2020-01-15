;;; spotify-ert-setup.el --- Helpers for spotify.el unit tests. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit test helpers for spotify-connect.el code using ERT.

;; Macro for making stub/mocks. Setup functions.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                    Mocks & Stubs & Helpers, oh my.                       --
;;------------------------------------------------------------------------------

(require 'cl) ;; cl-flet*
(require 'spotify-ert-mock-stub)

;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------

(defvar spotify-ert/setup/error-out-functions/orig nil
  "Data saved about original function definitons.")


;;------------------------------------------------------------------------------
;; Setup / Reset
;;------------------------------------------------------------------------------

(defun spotify-ert/setup/reset ()
  "Start-of or during test reset.
"
  ;;---
  ;; Reset Spotify.el
  ;;---
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


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-ert-setup)
