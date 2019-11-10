;;; spotify-connect.el --- Spotify.el transport for the Spotify Connect API -*- lexical-binding: t -*-

;;; Commentary:

;; Talks with Spotify Connect via spotify-api.el

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'spotify-api)


(defun spotify--connect-build-json-bool (value &optional false)
  "Converts Emacs t and nil (or values of FALSE) to json t/json-false."
  (let ((false (or false nil)))
    (if (eq value false)
        json-false
      t)))
;; (spotify--connect-build-json-bool nil)
;; (spotify--connect-build-json-bool t)
;; (setq testfoo t)
;; (spotify--connect-build-json-bool testfoo)


(defun spotify--connect-build-json (status)
  "§-TODO-§ [2019-11-04]: This really should get deprecated and just allow
normalized-get to take care of things instead of building a
reduced set out of full return JSON...

Properly use Emacs JSON lib to convert from Spotify Connect API's
full JSON output to a reduced JSON usable by spotify.el.
"
  (if-let* ((status status)
            (track (gethash 'item status))
            (json-object-type 'hash-table)
            (json-key-type 'symbol)
            (json-obj (json-new-object)))
      ;; Read status into new json-obj using json lib functions so the json
      ;; stays properly escaped and e.g. artist "Weird Al" doesn't ruin
      ;; everything for our baby json.
      (progn
        (setq json-obj (json-add-to-object
                        json-obj "artist"
                        (gethash 'name (car (gethash 'artists track)))))
        (setq json-obj (json-add-to-object
                        json-obj "duration"
                        (gethash 'duration_ms track)))
        (setq json-obj (json-add-to-object
                        json-obj "track_number"
                        (gethash 'track_number track)))
        (setq json-obj (json-add-to-object
                        json-obj "name"
                        (gethash 'name track)))
        (setq json-obj (json-add-to-object
                        json-obj "player_state"
                        (if (eq (gethash 'is_playing status)
                                :json-false)
                            "paused"
                          "playing")))
        (setq json-obj (json-add-to-object
                        json-obj "player_shuffling"
                        (spotify--connect-build-json-bool
                         (not (eq (gethash 'shuffle_state status)
                                  :json-false)))))
        (setq json-obj (json-add-to-object
                        json-obj "player_repeating"
                        (spotify--connect-build-json-bool
                         (not (string= (gethash 'repeat_state status)
                                  "off")))))

        ;; convert new baby json to string and return it
        (json-encode json-obj))

    ;; failed to convert json - return nil
    nil))


;; with json-encode on artist/name:
;;   actual string from this with %S:
;;   "{\"artist\":\"\"Bobaflex\"\",\"duration\": 214533,\"track_number\":3,\"name\":\"\"Bury Me With My Guns\"\",\"player_state\":\"playing\",\"player_shuffling\":false,\"player_repeating\":false}"
;;
;;   actual string from this with %s:
;;   {"artist":""Bobaflex"","duration": 214533,"track_number":3,"name":""Bury Me With My Guns"","player_state":"playing","player_shuffling":false,"player_repeating":false}
;;
;; without:
;;   actual string from this with %S:
;;   "{\"artist\":\"Heldmaschine\",\"duration\": 176398,\"track_number\":1,\"name\":\"®\",\"player_state\":\"playing\",\"player_shuffling\":false,\"player_repeating\":false}"
;;
;;   actual string from this with %s:
;;   {"artist":"Heldmaschine","duration": 176398,"track_number":1,"name":"®","player_state":"playing","player_shuffling":false,"player_repeating":false}
;;
(defun spotify-connect-player-status ()
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
  (spotify-api-get-player-status
   (lambda (status)
     (if-let* ((status status)
               (track (gethash 'item status))
               (json (spotify--connect-build-json status)))
                ;; §-TODO-§ [2019-11-09]: delete when I'm sure I don't need anymore.
                ;; (concat
                ;;  "{"
                ;;  (format "\"artist\":\"%s\","
                ;;          ;; encode for "Weird Al".
                ;;          (gethash 'name (car (gethash 'artists track))))
                ;;  (format "\"duration\": %d,"
                ;;          (gethash 'duration_ms track))
                ;;  (format "\"track_number\":%d,"
                ;;          (gethash 'track_number track))
                ;;  (format "\"name\":\"%s\","
                ;;          (gethash 'name track))
                ;;  (format "\"player_state\":\"%s\","
                ;;          (if (eq (gethash 'is_playing status) :json-false) "paused" "playing"))
                ;;  (format "\"player_shuffling\":%s,"
                ;;          (if (not (eq (gethash 'shuffle_state status) :json-false))"true" "false"))
                ;;  (format "\"player_repeating\":%s"
                ;;          (if (string= (gethash 'repeat_state status) "off") "false" "true"))
                ;;  "}")))
         (spotify-player-status-refresh-string json)
       (spotify-player-status-refresh-string nil)))))

(defmacro spotify-when-device-active (body)
  "Evaluate BODY when there is an active device, otherwise shows an error message."
  `(spotify-api-device-list
    (lambda (json)
      (if-let ((json json)
               (devices (gethash 'devices json))
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
