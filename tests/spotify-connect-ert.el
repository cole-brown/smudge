;;; spotify-connect-ert.el --- Tests for spotify-connect.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-connect.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                        spotify-connect.el tests                          --
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

(defvar spotify-ert/mock/volume-amount nil
  "Volume amount given to a volume mocking function.")


(defvar spotify-ert/mock/volume-device-id  nil
  "Device ID given to a volume mocking function.")


;;------------------------------------------------------------------------------
;; Spotify-API.el: Mocks & Stubs
;;------------------------------------------------------------------------------

(defun spotify-ert/mock/spotify-api-device-list (callback)
  "Mock Function."
  ;; Calls '/me/player/devices' Spotify Connect API endpoint.
  (spotify-ert/util/with-json
      ;; Choose device list with active device, or no active device, depending
      ;; on setup.
      (if spotify-ert/mock/spotify-api-device-list/is-active
          spotify-ert/data/connect-api/devices-list/active
        spotify-ert/data/connect-api/devices-list/inactive)
    ;; And just give back device list data to async callback.
    (when callback (funcall callback json-obj))))


(defun spotify-ert/mock/spotify-api-set-volume (device-id percentage &optional callback)
  "Mock Function."
  ;; Calls '/me/player/volume' Spotify Connect API endpoint.
  (setq spotify-ert/mock/volume-amount    percentage)
  (setq spotify-ert/mock/volume-device-id device-id)

  (push 'spotify-api-set-volume spotify-ert/stub/called))



(defun spotify-ert/mock/spotify-api-get-player-status (callback)
  "Mock Function."
  ;; Calls '/me/player' Spotify Connect API endpoint.

  ;; Get our full status data...
  (spotify-ert/util/with-json
      ;; Choose either valid data or nil, depending on setup.
      (if spotify-ert/mock/spotify-api-device-list/is-active
          spotify-ert/data/connect-api/player-status
        nil)
    ;; ...and just give it to the callback.
    (when callback (funcall callback json-obj))))


;;---
;; Setup Functions
;;---

(defun spotify-ert/spotify-connect/reset ()
  "Per-test setup/reset."

  ;; General reset
  (spotify-ert/setup/reset)

  ;; Our vars
  (setq spotify-ert/mock/volume-amount    nil)
  (setq spotify-ert/mock/volume-device-id nil))


(defun spotify-ert/spotify-connect/setup ()
  "Per-test teardown."

  ;; General
  (spotify-ert/spotify-connect/reset)
  (spotify-ert/setup/setup))


(defun spotify-ert/spotify-connect/teardown ()
  "Per-test teardown."

  ;; General
  (spotify-ert/setup/teardown))


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
      (should (eq entered-body nil))))

  (spotify-ert/spotify-connect/teardown))


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
      (should (eq entered-body nil)))))

  (spotify-ert/spotify-connect/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-status
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-status ()
(ert-deftest spotify-ert/spotify-connect-player-status ()
  "Test that this gets player status from Spotify Connect API.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)

  (spotify-ert/mock 'spotify-api-device-list nil
    (spotify-ert/mock 'spotify-api-get-player-status nil
      (should (string= "[Playing: \"Weird Al\" Y... - Foil ◷ 2:22 --]"
                       (spotify-connect-player-status)))

      (setq spotify-player-status nil)
      (setq spotify-ert/mock/spotify-api-device-list/is-active nil)

      (should (string= ""
                  (spotify-connect-player-status)))))

  (spotify-ert/spotify-connect/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-play-track
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-play-track (uri &optional context)
(ert-deftest spotify-ert/spotify-connect-player-play-track ()
  "Test that this requests a track be played to Spotify Connect API.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (spotify-ert/stub 'spotify-api-play
      ;; This function does nothing. Just a passthrough to spotify-api.el.
      ;; So we can't really test anything.
      (spotify-connect-player-play-track nil)
      (should-not (null spotify-ert/stub/called))
      (should     (memq 'spotify-api-play spotify-ert/stub/called))))

  (spotify-ert/spotify-connect/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-pause
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-pause ()
(ert-deftest spotify-ert/spotify-connect-player-pause ()
  "Test that this requests device pauses play.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (spotify-ert/stub 'spotify-api-pause nil
      ;; This function does nothing. Just a passthrough to spotify-api.el.
      ;; So we can't really test anything.
      (spotify-connect-player-pause)
      (should-not (null spotify-ert/stub/called))
      (should     (memq 'spotify-api-pause spotify-ert/stub/called))))

  (spotify-ert/spotify-connect/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-play
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-play ()
(ert-deftest spotify-ert/spotify-connect-player-play ()
  "Test that this requests device plays.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)
  (spotify-ert/mock 'spotify-api-device-list nil
    (spotify-ert/stub 'spotify-api-play
      ;; This function does nothing. Just a passthrough to spotify-api.el.
      ;; So we can't really test anything.
      (spotify-connect-player-play)
      (should-not (null spotify-ert/stub/called))
      (should     (memq 'spotify-api-play spotify-ert/stub/called))))

  (spotify-ert/spotify-connect/teardown))


;;------------------------------------------------------------------------------
;; Test: spotify-connect-player-toggle-play
;;------------------------------------------------------------------------------

;;(defun spotify-connect-player-toggle-play ()
(ert-deftest spotify-ert/spotify-connect-player-toggle-play ()
  "Test that this request play/pause status toggle.
"
  ;; setup mocks
  (spotify-ert/spotify-connect/setup)

  ;; spotify--when-device->with-status
  (spotify-ert/mock 'spotify-api-device-list nil
    (spotify-ert/mock 'spotify-api-get-player-status nil
      ;; spotify-connect-player-toggle-play
      (spotify-ert/stub 'spotify-api-play
        (spotify-ert/stub 'spotify-api-pause
          ;; This function doesn't do much, but we can at least make sure it
          ;; calls play or pause as appropriate.

          ;; data has is_playing as true, so test that it calls pause first
          (should (null spotify-ert/stub/called))
          (spotify-connect-player-toggle-play)
          (should-not (null spotify-ert/stub/called))
          (should     (memq 'spotify-api-pause spotify-ert/stub/called))
          (should-not (memq 'spotify-api-play  spotify-ert/stub/called))

          ;; reset for second half
          (setq spotify-ert/stub/called nil)
          (should (null spotify-ert/stub/called))

          ;; Massage data with spotify-ert/util/with-json/munger to change
          ;; is_playing to false.
          (setq spotify-ert/util/with-json/munger
                (lambda (json-obj)
                  (puthash 'is_playing :json-false json-obj)
                  (push 'munger spotify-ert/stub/called)
                  json-obj))
          (spotify-connect-player-toggle-play)
          (should-not (null spotify-ert/stub/called))
          (should     (memq 'munger            spotify-ert/stub/called))
          (should     (memq 'spotify-api-play  spotify-ert/stub/called))
          (should-not (memq 'spotify-api-pause spotify-ert/stub/called))
          ))))

  (spotify-ert/spotify-connect/teardown))


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
    (spotify-ert/stub 'spotify-api-next
      ;; This function does nothing. Just a passthrough to spotify-api.el.
      ;; So we can't really test anything.
      (spotify-connect-player-next-track)
      (should-not (null spotify-ert/stub/called))
      (should     (memq 'spotify-api-next spotify-ert/stub/called))))

  (spotify-ert/spotify-connect/teardown))


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
    (spotify-ert/stub 'spotify-api-previous
      ;; This function does nothing. Just a passthrough to spotify-api.el.
      ;; So we can't really test anything.
      (spotify-connect-player-previous-track)
      (should-not (null spotify-ert/stub/called))
      (should     (memq 'spotify-api-previous spotify-ert/stub/called))))

  (spotify-ert/spotify-connect/teardown))


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
    (spotify-ert/mock 'spotify-api-get-player-status nil
      (spotify-ert/mock 'spotify-api-set-volume nil
        ;; get orig volume
        (let ((vol-orig 42)
              (vol-change 10))
          (should (= vol-orig 42))

          ;; up it
          (spotify-connect-volume-up vol-change)

          (should-not (null spotify-ert/stub/called))
          (should-not (null spotify-ert/mock/volume-amount))
          (should-not (null spotify-ert/mock/volume-device-id))

          (should     (memq 'spotify-api-set-volume spotify-ert/stub/called))
          (should     (= spotify-ert/mock/volume-amount
                         (+ vol-orig vol-change)))
          (should     (string= spotify-ert/mock/volume-device-id
                               "test-device-id"))))))

  (spotify-ert/spotify-connect/teardown))


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
    (spotify-ert/mock 'spotify-api-get-player-status nil
      (spotify-ert/mock 'spotify-api-set-volume nil
        ;; get orig volume
        (let ((vol-orig 42)
              (vol-change 10))
          (should (= vol-orig 42))

          ;; down it
          (spotify-connect-volume-down vol-change)

          (should-not (null spotify-ert/stub/called))
          (should-not (null spotify-ert/mock/volume-amount))
          (should-not (null spotify-ert/mock/volume-device-id))

          (should     (memq 'spotify-api-set-volume spotify-ert/stub/called))
          (should     (= spotify-ert/mock/volume-amount
                         (- vol-orig vol-change)))
          (should     (string= spotify-ert/mock/volume-device-id
                               "test-device-id"))))))

  (spotify-ert/spotify-connect/teardown))


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
    (spotify-ert/mock 'spotify-api-get-player-status nil
      (spotify-ert/mock 'spotify-api-set-volume nil
        ;; get orig volume
        (let* ((vol-orig 42)
              (vol-change 10)
              (vol-muted 0)
              (vol-unmuted (+ vol-orig vol-change)))

          ;; Test that it will currently mute.
          (spotify-connect-volume-mute-unmute vol-orig)

          (should-not (null spotify-ert/stub/called))
          (should-not (null spotify-ert/mock/volume-amount))
          (should-not (null spotify-ert/mock/volume-device-id))

          (should     (memq 'spotify-api-set-volume spotify-ert/stub/called))
          (should     (= spotify-ert/mock/volume-amount vol-muted))
          (should     (string= spotify-ert/mock/volume-device-id
                               "test-device-id"))
          (should-not (memq 'munger spotify-ert/stub/called))

          ;; get ready for another go
          (spotify-ert/spotify-connect/reset)
          (should (null spotify-ert/mock/volume-amount))
          (should (null spotify-ert/mock/volume-device-id))

          ;; Make munger to 0 volume/muted state.
          (setq spotify-ert/util/with-json/munger
                (lambda (json-obj)
                  (push 'munger spotify-ert/stub/called)
                  (let ((devices (gethash 'devices json-obj)))
                    (if (null devices)
                        ;; player status json
                        (puthash 'volume_percent 0
                                 (gethash 'device json-obj))
                      ;; devices list json
                      (dolist (device devices)
                        (puthash 'volume_percent 0 device))))
                  json-obj))

          ;; Test that it will now unmute.
          (spotify-connect-volume-mute-unmute vol-unmuted)
          (should-not (null spotify-ert/stub/called))
          (should-not (null spotify-ert/mock/volume-amount))
          (should-not (null spotify-ert/mock/volume-device-id))

          (should     (memq 'spotify-api-set-volume spotify-ert/stub/called))
          (should     (= spotify-ert/mock/volume-amount vol-unmuted))
          (should     (string= spotify-ert/mock/volume-device-id
                               "test-device-id"))
          (should     (memq 'munger spotify-ert/stub/called))))))

  (spotify-ert/spotify-connect/teardown))


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
    (spotify-ert/mock 'spotify-api-get-player-status nil
      (spotify-ert/stub 'spotify-api-repeat

        ;; This function does nothing. Just a passthrough to spotify-api.el.
        ;; So we can't really test anything.
        (spotify-connect-toggle-repeat)
        (should-not (null spotify-ert/stub/called))
        (should     (memq 'spotify-api-repeat spotify-ert/stub/called)))))

  (spotify-ert/spotify-connect/teardown))


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
    (spotify-ert/mock 'spotify-api-get-player-status nil
      (spotify-ert/stub 'spotify-api-shuffle

        ;; This function does nothing. Just a passthrough to spotify-api.el.
        ;; So we can't really test anything.
        (spotify-connect-toggle-shuffle)
        (should-not (null spotify-ert/stub/called))
        (should     (memq 'spotify-api-shuffle spotify-ert/stub/called)))))

  (spotify-ert/spotify-connect/teardown))


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
(provide 'spotify-connect-ert)
