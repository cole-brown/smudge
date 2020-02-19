;;; spotify-controller-ert.el --- Tests for spotify-controller.el. -*- lexical-binding: t -*-

;; Copyright (C) 2020 Cole Brown

;;; Commentary:

;; Unit tests for spotify-controller.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                      spotify-controller.el tests                         --
;;------------------------------------------------------------------------------

;; Test Helpers
(require 'spotify-ert-data-functions)
(require 'spotify-ert-mock-stub)
(require 'spotify-ert-setup)

;; Test Data
(require 'spotify-ert-data-connect-api)

;; Spotify.el Requirements
(require 'spotify-connect)
(require 'spotify-controller)


;;------------------------------------------------------------------------------
;; Settings, Vars, Helpers
;;------------------------------------------------------------------------------

;; (defvar spotify-ert/mock/volume-amount nil
;;   "Volume amount given to a volume mocking function.")


;; (defvar spotify-ert/mock/volume-device-id  nil
;;   "Device ID given to a volume mocking function.")


;;------------------------------------------------------------------------------
;; Spotify-API.el: Mocks & Stubs
;;------------------------------------------------------------------------------

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



;; (defun spotify-ert/mock/spotify-api-get-player-status (callback)
;;   "Mock Function."
;;   ;; Calls '/me/player' Spotify Connect API endpoint.

;;   ;; Get our full status data...
;;   (spotify-ert/util/with-json
;;       ;; Choose either valid data or nil, depending on setup.
;;       (if spotify-ert/mock/spotify-api-device-list/is-active
;;           spotify-ert/data/connect-api/player-status
;;         nil)
;;     ;; ...and just give it to the callback.
;;     (when callback (funcall callback json-obj))))


;;---
;; Setup Functions
;;---

(defun spotify-ert/spotify-controller/reset ()
  "Per-test setup/reset."

  ;; General reset
  (spotify-ert/setup/reset)

  ;; Our vars
  ;; (setq spotify-ert/mock/volume-amount    nil)
  ;; (setq spotify-ert/mock/volume-device-id nil)
  )


(defun spotify-ert/spotify-controller/setup ()
  "Per-test teardown."

  ;; General
  (spotify-ert/spotify-controller/reset)
  (spotify-ert/setup/setup))


(defun spotify-ert/spotify-controller/teardown ()
  "Per-test teardown."

  ;; General
  (spotify-ert/setup/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-apply
;;------------------------------------------------------------------------------

;; (defun spotify-apply (suffix &rest args)
(ert-deftest spotify-ert/spotify-apply ()
  "Test that spotify-apply can dispatch to correct method based on connection.
"
  (spotify-ert/spotify-controller/setup)

  ;; §-TODO-§ [2020-02-18]: --now--

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

                              ;; <maybe tests here>
                              (should (eq t nil))

                              )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-player-status-refresh-string
;;------------------------------------------------------------------------------

;; (defun spotify-player-status-refresh-string (metadata)
(ert-deftest spotify-ert/spotify-player-status-refresh-string ()
  "Test that `spotify-player-status-refresh-string' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-start-player-status-timer
;;------------------------------------------------------------------------------

;; (defun spotify-start-player-status-timer ()
(ert-deftest spotify-ert/spotify-start-player-status-timer ()
  "Test that `spotify-start-player-status-timer' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-stop-player-status-timer
;;------------------------------------------------------------------------------

;; (defun spotify-stop-player-status-timer ()
(ert-deftest spotify-ert/spotify-stop-player-status-timer ()
  "Test that `spotify-stop-player-status-timer' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-player-status
;;------------------------------------------------------------------------------

;; (defun spotify-player-status ()
(ert-deftest spotify-ert/spotify-player-status ()
  "Test that `spotify-player-status' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-play-uri
;;------------------------------------------------------------------------------

;; (defun spotify-play-uri (uri)
(ert-deftest spotify-ert/spotify-play-uri ()
  "Test that `spotify-play-uri' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-play-track
;;------------------------------------------------------------------------------

;; (defun spotify-play-track (track &optional context)
(ert-deftest spotify-ert/spotify-play-track ()
  "Test that `spotify-play-track' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-toggle-play
;;------------------------------------------------------------------------------

;; (defun spotify-toggle-play ()
(ert-deftest spotify-ert/spotify-toggle-play ()
  "Test that `spotify-toggle-play' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-next-track
;;------------------------------------------------------------------------------

;; (defun spotify-next-track ()
(ert-deftest spotify-ert/spotify-next-track ()
  "Test that `spotify-next-track' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-previous-track
;;------------------------------------------------------------------------------

;; (defun spotify-previous-track ()
(ert-deftest spotify-ert/spotify-previous-track ()
  "Test that `spotify-previous-track' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-volume-up
;;------------------------------------------------------------------------------

;; (defun spotify-volume-up ()
(ert-deftest spotify-ert/spotify-volume-up ()
  "Test that `spotify-volume-up' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-volume-down
;;------------------------------------------------------------------------------

;; (defun spotify-volume-down ()
(ert-deftest spotify-ert/spotify-volume-down ()
  "Test that `spotify-volume-down' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-volume-mute-unmute
;;------------------------------------------------------------------------------

;; (defun spotify-volume-mute-unmute ()
(ert-deftest spotify-ert/spotify-volume-mute-unmute ()
  "Test that `spotify-volume-mute-unmute' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-toggle-repeat
;;------------------------------------------------------------------------------

;; (defun spotify-toggle-repeat ()
(ert-deftest spotify-ert/spotify-toggle-repeat ()
  "Test that `spotify-toggle-repeat' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: splotify-toggle-shuffle
;;------------------------------------------------------------------------------

;; (defun splotify-toggle-shuffle ()
(ert-deftest spotify-ert/splotify-toggle-shuffle ()
  "Test that `splotify-toggle-shuffle' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-is-repeating
;;------------------------------------------------------------------------------

;; (defun spotify-is-repeating ()
(ert-deftest spotify-ert/spotify-is-repeating ()
  "Test that `spotify-is-repeating' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-is-shuffling
;;------------------------------------------------------------------------------

;; (defun spotify-is-shuffling ()
(ert-deftest spotify-ert/spotify-is-shuffling ()
  "Test that `spotify-is-shuffling' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify--controller-status-updated
;;------------------------------------------------------------------------------

;; (defun spotify--controller-status-updated ()
(ert-deftest spotify-ert/spotify--controller-status-updated ()
  "Test that `spotify--controller-status-updated' <does something>.
"
  (spotify-ert/spotify-controller/setup)

  ;; <maybe tests here>

  (spotify-ert/util/with-json spotify-connect-ert/data/player-status-in-full

    ;; <maybe tests here>


    )

  (spotify-ert/spotify-controller/teardown))


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
(provide 'spotify-controller-ert)
