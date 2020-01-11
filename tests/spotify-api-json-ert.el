;;; spotify-api-json-ert.el --- Tests for spotify-api-json.el. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Unit tests for spotify-api-json.el code using ERT.

;;; Code:

;;-----------------------------Spotify Unit Tests-------------------------------
;;--                        spotify-api-json.el tests                         --
;;------------------------------------------------------------------------------

;; Test Helpers
(require 'spotify-ert-data-functions)
(require 'spotify-ert-mock-stub)
(require 'spotify-ert-setup)

;; Test Data
;; (require 'spotify-ert-data-connect-api)

;; Spotify.el Requirements
(require 'spotify-api-json)


;;------------------------------------------------------------------------------
;; Test: spotify--api-json-get-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-json-get-field (type json field) ...
(ert-deftest spotify-ert/spotify--api-json-get-field ()
  "Test that `spotify--api-json-get-field' can verify a field
belongs to a Spotify Connect API JSON object type, and can get
the field from the JSON object."
  (spotify-ert/util/with-json spotify-ert/data/connect-api/player-status/truncated
    (should (spotify--api-json-get-field spotify--api/data/player-status
                                         json-obj
                                         'device))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api/data/player-status
                           json-obj
                           'device)))

    (should (string= "off"
                     (spotify--api-json-get-field spotify--api/data/player-status
                                                  json-obj
                                                  'repeat_state)))

    (should (eq :json-false
                (spotify--api-json-get-field spotify--api/data/player-status
                                             json-obj
                                             'shuffle_state)))
    (should (eq t
                (spotify--api-json-get-field spotify--api/data/player-status
                                             json-obj
                                             'is_playing)))

    (should (spotify--api-json-get-field spotify--api/data/player-status
                                         json-obj
                                         'item))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api/data/player-status
                           json-obj
                           'item)))

    ;;---
    ;; Don't care about/use these currently, but they exist so test?
    ;;---
    (should (spotify--api-json-get-field spotify--api/data/player-status
                                         json-obj
                                         'context))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api/data/player-status
                           json-obj
                           'context)))

    (should (= 1490252122574
               (spotify--api-json-get-field spotify--api/data/player-status
                                            json-obj
                                            'timestamp)))

    (should (string= "44272"
                     (spotify--api-json-get-field spotify--api/data/player-status
                                                  json-obj
                                                  'progress_ms)))

    (should (string= "track"
                     (spotify--api-json-get-field spotify--api/data/player-status
                                                  json-obj
                                                  'currently_playing_type)))

    (should (spotify--api-json-get-field spotify--api/data/player-status
                                         json-obj
                                         'actions))
    (should (hash-table-p (spotify--api-json-get-field
                           spotify--api/data/player-status
                           json-obj
                           'actions)))))
;; §-TODO-§ [2019-11-18]: Test devices or some other json object?


;;------------------------------------------------------------------------------
;; Test: spotify--api-object-get-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-object-get-field (object key type) ...

(ert-deftest spotify-ert/spotify--api-object-get-field/player-status ()
  "Tests the functionality of `spotify--api-object-get-field' on a player-status
object.
"
  ;; player status object
  (spotify-ert/util/with-json spotify-ert/data/connect-api/player-status

    (should (eq nil
                (spotify--api-object-get-field
                 json-obj
                 (assoc :shuffling-bool spotify--keyword->api-field)
                 spotify--api/data/player-status)))

    (should (eq nil
                (spotify--api-object-get-field
                 json-obj
                 (assoc :repeating-bool spotify--keyword->api-field)
                 spotify--api/data/player-status)))

    (should (eq t
                (spotify--api-object-get-field
                 json-obj
                 (assoc :playing-bool spotify--keyword->api-field)
                 spotify--api/data/player-status)))))


(ert-deftest spotify-ert/spotify--api-object-get-field/artist ()
  "Tests the functionality of `spotify--api-object-get-field' on an artist
object.
"
  ;; artist object
  (spotify-ert/util/with-json spotify-ert/data/connect-api/artist

    (should (string= "\"Weird Al\" Yankovic"
                     (spotify--api-object-get-field
                      json-obj
                      (assoc :artist spotify--keyword->api-field)
                      spotify--api/data/artist-simple)))))


(ert-deftest spotify-ert/spotify--api-object-get-field/track ()
  "Tests the functionality of `spotify--api-object-get-field' on a track
object.
"
  ;; track object
  (spotify-ert/util/with-json spotify-ert/data/connect-api/track

    (should (string= "Foil"
                     (spotify--api-object-get-field
                      json-obj
                      (assoc :track spotify--keyword->api-field)
                      spotify--api/data/track-full)))

    (should (= 3
               (spotify--api-object-get-field
                json-obj
                (assoc :track-number spotify--keyword->api-field)
                spotify--api/data/track-full)))

    (should (= 142946
               (spotify--api-object-get-field
                json-obj
                (assoc :duration-millisecond spotify--keyword->api-field)
                spotify--api/data/track-full)))))


(ert-deftest spotify-ert/spotify--api-object-get-field/device ()
  "Tests the functionality of `spotify--api-object-get-field' on a device
object.
"
  ;; device object
  (spotify-ert/util/with-json spotify-ert/data/connect-api/device

    (should (string= "test-device-id-only"
                     (spotify--api-object-get-field
                      json-obj
                      (assoc :device-active-id spotify--keyword->api-field)
                      spotify--api/data/device-full)))

    (should (string= "Emacs AR Glasses 5001+ Pro#"
                     (spotify--api-object-get-field
                      json-obj
                      (assoc :device-active-name spotify--keyword->api-field)
                      spotify--api/data/device-full)))

    (should (eq t
                (spotify--api-object-get-field
                 json-obj
                 (assoc :device-active-state spotify--keyword->api-field)
                 spotify--api/data/device-full)))

    (should (= 42
               (spotify--api-object-get-field
                json-obj
                (assoc :volume spotify--keyword->api-field)
                spotify--api/data/device-full)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-device-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-device-field (device keyword) ...
(ert-deftest spotify-ert/spotify--api-device-field ()
  "Test whether `spotify--api-device-field' returns expected values
for keywords."
  ;; device object
  (spotify-ert/util/with-json spotify-ert/data/connect-api/device

    (should (string= "test-device-id-only"
                     (spotify--api-device-field
                      json-obj
                      :device-active-id)))

    (should (string= "Emacs AR Glasses 5001+ Pro#"
                     (spotify--api-device-field
                      json-obj
                      :device-active-name)))

    (should (eq t
                (spotify--api-device-field
                 json-obj
                 :device-active-state)))

    (should (= 42
               (spotify--api-device-field
                json-obj
                :volume)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-artist-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-artist-field (artist keyword) ...
(ert-deftest spotify-ert/spotify--api-artist-field ()
  "Test whether `spotify--api-artist-field' returns expected values
for keywords."
  ;; artist object
  (spotify-ert/util/with-json spotify-ert/data/connect-api/artist

    (should (string= "\"Weird Al\" Yankovic"
                     (spotify--api-artist-field
                      json-obj
                      :artist)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-track-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-track-field (track keyword) ...
(ert-deftest spotify-ert/spotify--api-track-field ()
  "Test whether `spotify--api-track-field' returns expected values
for keywords."
  ;; track object
  (spotify-ert/util/with-json spotify-ert/data/connect-api/track

    (should (string= "Foil"
                     (spotify--api-track-field
                      json-obj
                      :track)))

    (should (= 3
               (spotify--api-track-field
                json-obj
                :track-number)))

    (should (= 142946
               (spotify--api-track-field
                json-obj
                :duration-millisecond)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-player-status-field
;;------------------------------------------------------------------------------

;; (defun spotify--api-player-status-field (status keyword) ...
(ert-deftest spotify-ert/spotify--api-player-status-field ()
  "Tests whether `spotify--api-player-status-field' returns expected values
for keywords."

  ;; player status object
  (spotify-ert/util/with-json spotify-ert/data/connect-api/player-status

    (should (eq nil
                (spotify--api-player-status-field
                 json-obj
                 :shuffling-bool)))

    (should (eq nil
                (spotify--api-player-status-field
                 json-obj
                 :repeating-bool)))

    (should (eq t
                (spotify--api-player-status-field
                 json-obj
                 :playing-bool)))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-devices
;;------------------------------------------------------------------------------

;; (defun spotify--api-devices (json) ...
(ert-deftest spotify-ert/spotify--api-devices ()
  "Pretty simple test right now... Grabs devices list from API return."
  (spotify-ert/util/with-json spotify-ert/data/connect-api/devices-list/active
    (let ((devices (spotify--api-devices json-obj))
          (bools '(t :json-false)))
      ;; We got something vaguely right, right?
      (should-not (null devices))
      (should (listp devices))
      (should (= 3 (length devices)))

      ;; The fields are the right types and exist, right?
      (dolist (device devices)
        (should (stringp (gethash 'id device)))
        (should (member (gethash 'is_active device) bools))
        (should (member (gethash 'is_private_session device) bools))
        (should (member (gethash 'is_restricted device) bools))
        (should (stringp (gethash 'name device)))
        (should (stringp (gethash 'type device)))
        (should (integerp (gethash 'volume_percent device)))
        ;; nil for field that doesn't exist
        (should (null (gethash 'field-dne device)))))))


;;------------------------------------------------------------------------------
;; Test: spotify--api-player-status
;;------------------------------------------------------------------------------

;; (defun spotify--api-player-status (json keyword) ...
(ert-deftest spotify-ert/spotify--api-player-status ()
  "Test that the function can find all the expected keywords in the
player-status return from \"/v1/me/player\" endpoint of Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
"
  (spotify-ert/util/with-json spotify-ert/data/connect-api/player-status
    (setq debug-on-error t)
    (should (eq nil
                (spotify--api-player-status
                 json-obj
                 :shuffling-bool)))

    (should (eq nil
                (spotify--api-player-status
                 json-obj
                 :repeating-bool)))

    (should (eq t
                (spotify--api-player-status
                 json-obj
                 :playing-bool)))

    (should (string= "\"Weird Al\" Yankovic"
                     (spotify--api-player-status
                      json-obj
                      :artist)))
    (should (string= "Foil"
                     (spotify--api-player-status
                      json-obj
                      :track)))

    (should (= 3
               (spotify--api-player-status
                json-obj
                :track-number)))

    (should (= 142946
               (spotify--api-player-status
                json-obj
                :duration-millisecond)))

    (should (string= "test-device-id"
                     (spotify--api-player-status
                      json-obj
                      :device-active-id)))

    (should (string= "Emacs AR Glasses 5001+ Pro#"
                     (spotify--api-player-status
                      json-obj
                      :device-active-name)))

    (should (eq t
                (spotify--api-player-status
                 json-obj
                 :device-active-state)))

    (should (= 42
               (spotify--api-player-status
                json-obj
                :volume)))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-api-json-ert)
