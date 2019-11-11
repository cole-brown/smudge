;;; spotify-connect.el --- Spotify.el transport for the Spotify Connect API -*- lexical-binding: t -*-

;;; Commentary:

;; Talks with Spotify Connect via spotify-api.el

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'spotify-api)
(require 'spotify-json)
(require 'spotify-feedback)

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
        (spotify--feedback--no-device)))))

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
    (spotify--if-status-closure status
      (if (spotify--json-api-status-field status :playing-bool)
          (spotify-api-pause)
        (spotify-api-play))))))

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
    (spotify--if-status-closure status
      (let* ((cur-volume (spotify--json-api-status-field status :volume))
             (new-volume (min (+ cur-volume amount) 100)))
        (spotify-api-set-volume
         (spotify-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume increased to %d%%" new-volume))))))))

(defun spotify-connect-volume-down (amount)
  "Turn down the volume (for what?) on the actively playing device."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (spotify--if-status-closure status
      (let* ((cur-volume (spotify--json-api-status-field status :volume))
             (new-volume (max (- cur-volume amount) 0)))
        (spotify-api-set-volume
         (spotify-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume decreased to %d%%" new-volume))))))))

(defun spotify-connect-volume-mute-unmute (unmute-volume)
  "Mute/unmute the volume on the actively playing device by setting the volume to 0."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (spotify--if-status-closure status
      (let ((volume (spotify--json-api-status-field status :volume))
            (dev-id (spotify-connect-get-device-id status)))
        (if (eq volume 0)
            (spotify-api-set-volume dev-id unmute-volume
                                    (lambda (_) (message "Volume unmuted to %s." unmute-volume)))
          (spotify-api-set-volume dev-id 0
                                  (lambda (_) (message "Volume muted.")))))))))

;; §-TODO-§ [2019-11-10]: do these really have to give strings?
(defun spotify-connect-toggle-repeat ()
  "Toggle repeat for the current track."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (spotify--if-status-closure status
      (spotify-api-repeat
       (if (spotify--json-api-status-field status :repeating-bool)
           "off"
         "context"))))))

;; §-TODO-§ [2019-11-10]: macro called:
;; spotify--device-active->status-if
;; or something for:
;;  (spotify-when-device-active
;;   (spotify-api-get-player-status
;;    (spotify--if-status-closure status

;; §-TODO-§ [2019-11-10]: do these really have to give strings?
(defun spotify-connect-toggle-shuffle ()
  "Toggle shuffle for the current track."
  (spotify-when-device-active
   (spotify-api-get-player-status
    (spotify--if-status-closure status
      (spotify-api-shuffle
       (if (spotify--json-api-status-field status :repeating-bool)
           "false"
         "true"))))))

(defun spotify-connect-get-device-id (player-status)
  "Get the id if from PLAYER-STATUS of the currently playing device, if any."
  (when player-status
    (gethash 'id (gethash 'device player-status))))


(provide 'spotify-connect)

;;; spotify-connect.el ends here
