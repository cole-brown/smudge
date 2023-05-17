;;; smudge-connect.el --- Control remote and local Spotify instances  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jason Dufair

;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:

;; This library uses the "connect" APIs to control transport functions of remote and local instances
;; of Spotify clients.  It implements a set of multimethod-like functions that are dispatched in
;; smudge-controller.el.

;; smudge-connect.el --- Smudge transport for the Spotify Connect API

;;; Code:

(require 'smudge-api)
(require 'smudge-controller)

(defun smudge-connect-player-status ()
  "Get the player status of the currently playing device, if any.
Returns a JSON string in the format:
{
  \"artist\": \"Aesop Rock\",
  \"duration\": 265333,
  \"track_number\": 9,
  \"name\":  \"Shrunk\",
  \"player_state\": \"playing\",
  \"player_shuffling\": \"t\",
  \"player_repeating\": \"context\"
}"
  (smudge-api-get-player-status
   (lambda (status)
     (if-let* ((status status)
               (track (gethash 'item status))
               (json (concat
                      "{"
                      (format "\"artist\":\"%s\","
                              (gethash 'name (car (gethash 'artists track))))
                      (format "\"duration\": %d,"
                              (gethash 'duration_ms track))
                      (format "\"track_number\":%d,"
                              (gethash 'track_number track))
                      (format "\"name\":\"%s\","
                              (gethash 'name track))
                      (format "\"player_state\":\"%s\","
                              (if (eq (gethash 'is_playing status) :json-false) "paused" "playing"))
                      (format "\"player_shuffling\":%s,"
                              (if (not (eq (gethash 'shuffle_state status) :json-false))"true" "false"))
                      (format "\"player_repeating\":%s"
                              (if (string= (gethash 'repeat_state status) "off") "false" "true"))
                      "}")))
         (progn
           (smudge-cache-update-status status)
           (smudge-controller-update-metadata json))
       (smudge-controller-update-metadata nil)))))

(defmacro smudge-connect-when-device-active (body)
  "Evaluate BODY when there is an active device, otherwise show an error message."
  `(smudge-api-device-list
    (lambda (json)
      (if-let ((json json)
               (devices (gethash 'devices json))
               (active (> (length (seq-filter (lambda (dev) (eq (gethash 'is_active dev) t)) devices)) 0)))
          (progn ,body)
        (message "No active device")))))

(defun smudge-connect-player-play-track (uri &optional context)
  "Play a track URI via Spotify Connect in an optional CONTEXT."
  (smudge-connect-when-device-active
   (smudge-api-play nil uri context)))

(defun smudge-connect-player-pause ()
  "Pause the currently playing track."
  (smudge-connect-when-device-active
   (smudge-api-pause)))

(defun smudge-connect-player-toggle-play ()
  "Toggle playing status of current track."
  (smudge-connect-when-device-active
   (smudge-api-get-player-status
    (lambda (status)
      (if status
          (if (not (eq (gethash 'is_playing status) :json-false))
              (smudge-api-pause)
            (smudge-api-play)))))))

(defun smudge-connect-player-next-track ()
  "Skip to the next track."
  (smudge-connect-when-device-active
   (smudge-api-next)))

(defun smudge-connect-player-previous-track ()
  "Skip to the previous track."
  (smudge-connect-when-device-active
   (smudge-api-previous)))

(defun smudge-connect-volume-get-next (current-volume direction)
  "Return current volume +/- the delta for changing volume, clamped [0, 100].

CURRENT-VOLUME should be the most up-to-date volume of the Spotify device from
e.g. cache (`smudge-cache-get-volume'), status (`smudge-connect-get-volume').
Example:
  (smudge-connect-when-device-active
   (smudge-api-get-player-status
    (lambda (status)
      (message \"'Volume Up' would increase volume: %d%% -> %d%%\"
               (smudge-connect-get-volume status)
               (smudge-connect-volume-get-next
                (smudge-connect-get-volume status)
                +1)))))

DIRECTION should be a positive or negative non-zero integer."
  ;; Clamp between 0 and 100.
  (max (min
        (+ current-volume
           ;; Multiply delta by direction to get the correct +/-.
           (* smudge-volume-percent-delta
              ;; Normalize to +1 or -1.
              (/ direction (abs direction))))
        100)
       0))
;; (smudge-connect-when-device-active
;;  (smudge-api-get-player-status
;;   (lambda (status)
;;     (message "'Volume Up' would increase volume: %d%% -> %d%%"
;;              (smudge-connect-get-volume status)
;;              (smudge-connect-volume-get-next
;;               (smudge-connect-get-volume status)
;;               +1)))))
;; (smudge-connect-when-device-active
;;  (smudge-api-get-player-status
;;   (lambda (status)
;;     (message "'Volume Down' would decrease volume: %d%% -> %d%%"
;;              (smudge-connect-get-volume status)
;;              (smudge-connect-volume-get-next
;;               (smudge-connect-get-volume status)
;;               -11)))))

(defun smudge-connect-volume-up ()
  "Turn up the volume on the actively playing device."
  (smudge-connect-when-device-active
   (smudge-api-get-player-status
    (lambda (status)
      (let ((new-volume (smudge-connect-volume-get-next (smudge-connect-get-volume status) +1)))
        (smudge-api-set-volume
         (smudge-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume increased to %d%%" new-volume))))))))

(defun smudge-connect-volume-down ()
  "Turn down the volume (for what?) on the actively playing device."
  (smudge-connect-when-device-active
   (smudge-api-get-player-status
    (lambda (status)
      (let ((new-volume (smudge-connect-volume-get-next (smudge-connect-get-volume status) -1)))
        (smudge-api-set-volume
         (smudge-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume decreased to %d%%" new-volume))))))))

(defun smudge-connect-volume-mute-unmute ()
  "Mute/unmute the volume on the actively playing device by setting the volume to 0."
  (smudge-connect-when-device-active
   (smudge-api-get-player-status
    (lambda (status)
      (let* ((device-id      (smudge-connect-get-device-id status))
             (current-volume (smudge-connect-get-volume    status))
             (unmute-volume  (smudge-cache-get-volume      :id device-id 100)))
        (if (eq current-volume 0)
            ;; Unmute to cached volume or full blast if nothing cached.
            (smudge-api-set-volume device-id unmute-volume
                                   (lambda (_) (message "Volume unmuted to %d%%" unmute-volume)))
          (smudge-api-set-volume device-id 0
                                 (lambda (_) (message "Volume muted")))))))))

(defun smudge-connect-toggle-repeat ()
  "Toggle repeat for the current track."
  (smudge-connect-when-device-active
   (smudge-api-get-player-status
    (lambda (status)
      (smudge-api-repeat (if (smudge-connect--is-repeating status) "off" "context"))))))

(defun smudge-connect-toggle-shuffle ()
  "Toggle shuffle for the current track."
  (smudge-connect-when-device-active
   (smudge-api-get-player-status
    (lambda (status)
      (smudge-api-shuffle (if (smudge-connect--is-shuffling status) "false" "true"))))))

(defun smudge-connect-get-device-id (player-status)
  "Get the id if from PLAYER-STATUS of the currently playing device, if any."
  (when player-status
    (gethash 'id (gethash 'device player-status))))

(defun smudge-connect-get-volume (player-status)
  "Get the volume from PLAYER-STATUS of the currently playing device, if any."
  (when player-status
    (smudge-cache-update-status player-status)
    (gethash 'volume_percent (gethash 'device player-status))))

(defun smudge-connect--is-shuffling (player-status)
  "Business logic for shuffling state of PLAYER-STATUS."
  (and player-status
         (not (eq (gethash 'shuffle_state player-status) :json-false))))

(defun smudge-connect--is-repeating (player-status)
  "Business logic for repeat state of PLAYER-STATUS."
  (string= (gethash 'repeat_state player-status) "context"))


(provide 'smudge-connect)

;;; smudge-connect.el ends here
