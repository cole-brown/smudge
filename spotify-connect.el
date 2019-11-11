;;; spotify-connect.el --- Spotify.el transport for the Spotify Connect API -*- lexical-binding: t -*-

;;; Commentary:

;; Talks with Spotify Connect via spotify-api.el

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'spotify-api)
(require 'spotify-json)

(defun spotify-connect-player-status ()
  "Get the player status of the currently playing device, if any.
Returns a JSON string in the `spotify--encode-json-simple' format.
"
  (spotify-api-get-player-status
   (lambda (status)
     (spotify-player-status-refresh-string
      (spotify--json-api-to-internal status)))))

(defmacro spotify-when-device-active (body)
  "Evaluate BODY when there is an active device, otherwise shows an error message."
  `(spotify-api-device-list
    (lambda (json)
      (if-let ((devices (spotify--json-api-device-list json))
               (active (> (length (seq-filter (lambda (dev) (eq (gethash 'is_active dev) t)) devices)) 0)))
          (progn ,body)
        (message "No active device")))))

(defun spotify-connect-player-play-track (uri &optional context)
  "Play a track URI via Spotify Connect in an optional CONTEXT."
  (let ((uri uri)
        (context context))
    (spotify-when-device-active
     (spotify-api-play nil uri context))))

(defun spotify-connect-player-pause ()
  "Pause the currently playing track."
  (spotify-when-device-active
   (spotify-api-pause)))

(defun spotify-connect-player-play ()
  "Play something probably."
  (spotify-when-device-active
   (spotify-api-play)))

(defun spotify-connect-player-toggle-play ()
  "Toggle playing status of current track."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (lambda (status)
      (if status
          (if (not (eq (gethash 'is_playing status) :json-false))
              (spotify-api-pause)
            (spotify-api-play)))))))

(defun spotify-connect-player-next-track ()
  "Skip to the next track."
  (spotify-when-device-active
   (spotify-api-next)))

(defun spotify-connect-player-previous-track ()
  "Skip to the previous track."
  (spotify-when-device-active
   (spotify-api-previous)))

(defun spotify-connect-volume-up (amount)
  "Turn up the volume on the actively playing device."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (lambda (status)
      (let ((new-volume (min (+ (spotify-connect-get-volume status) amount) 100)))
        (spotify-api-set-volume
         (spotify-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume increased to %d%%" new-volume))))))))

(defun spotify-connect-volume-down (amount)
  "Turn down the volume (for what?) on the actively playing device."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (lambda (status)
      (let ((new-volume (max (- (spotify-connect-get-volume status) amount) 0)))
        (spotify-api-set-volume
         (spotify-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume decreased to %d%%" new-volume))))))))

(defun spotify-connect-volume-mute-unmute (unmute-volume)
  "Mute/unmute the volume on the actively playing device by setting the volume to 0."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (lambda (status)
      (let ((volume (spotify-connect-get-volume status)))
        (if (eq volume 0)
            (spotify-api-set-volume (spotify-connect-get-device-id status) unmute-volume
                                    (lambda (_) (message "Volume unmuted to %s." unmute-volume)))
          (spotify-api-set-volume (spotify-connect-get-device-id status) 0
                                  (lambda (_) (message "Volume muted.")))))))))

(defun spotify-connect-toggle-repeat ()
  "Toggle repeat for the current track."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (lambda (status)
      (spotify-api-repeat (if (spotify--is-repeating status) "off" "context"))))))

(defun spotify-connect-toggle-shuffle ()
  "Toggle shuffle for the current track."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (lambda (status)
      (spotify-api-shuffle (if (spotify--is-shuffling status) "false" "true"))))))

(defun spotify-connect-get-device-id (player-status)
  "Get the id if from PLAYER-STATUS of the currently playing device, if any."
  (when player-status
    (gethash 'id (gethash 'device player-status))))

(defun spotify-connect-get-volume (player-status)
  "Get the volume from PLAYER-STATUS of the currently playing device, if any."
  (when player-status
    (gethash 'volume_percent (gethash 'device player-status))))

(defun spotify--is-shuffling (player-status)
  "Business logic for shuffling state of PLAYER-STATUS."
  (and player-status
       (not (eq (gethash 'shuffle_state player-status) :json-false))))

(defun spotify--is-repeating (player-status)
  "Business logic for repeat state of PLAYER-STATUS."
  (string= (gethash 'repeat_state player-status) "context"))


(provide 'spotify-connect)

;;; spotify-connect.el ends here
