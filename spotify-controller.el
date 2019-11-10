;;; package --- Summary

;;; Commentary:

;;; spotify-controller.el --- Generic player controller interface for Spotify.el

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Code:

(require 'spotify-remote)
(require 'spotify-player-status)
(require 'spotify-player-feedback)

(add-hook 'spotify--cache-player-status-hook
          #'spotify--controller-status-updated)

(defmacro if-gnu-linux (then else)
  "Evaluate THEN form if Emacs is running in GNU/Linux, otherwise evaluate ELSE form."
  `(if (eq system-type 'gnu/linux) ,then ,else))

(defmacro when-gnu-linux (then)
  "Evaluate THEN form if Emacs is running in GNU/Linux."
  `(if-gnu-linux ,then nil))

(defmacro if-darwin (then else)
  "Evaluate THEN form if Emacs is running in OS X, otherwise evaluate ELSE form."
  `(if (eq system-type 'darwin) ,then ,else))

(defmacro when-darwin (then)
  "Evaluate THEN form if Emacs is running in OS X."
  `(if-darwin ,then nil))

(defcustom spotify-transport 'connect
  "How the commands should be sent to Spotify process. Defaults to 'connect, as it provides a consistent UX across all OSes."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)
                 (symbol :tag "Connect" connect))
  :group 'spotify)

(defvar spotify-timer nil)

;; §-TODO-§ [2019-11-09]: Make backends reply to this any time they get status?

(defun spotify-apply (suffix &rest args)
  "Simple facility to emulate multimethods.
Apply SUFFIX to spotify-prefixed functions, applying ARGS."
  (let ((func-name (format "spotify-%s-%s" spotify-transport suffix)))
    (apply (intern func-name) args)))

(defun spotify-player-status-refresh-string (metadata)
  "Compose the playing status string to be displayed in the
player-status from METADATA."
  (spotify-update-player-status (spotify-player-status-get metadata)))

(defun spotify-start-player-status-timer ()
  "Start the timer that will update the mode line according to the Spotify player status."
  (spotify-stop-player-status-timer)
  (when (> spotify-player-status-refresh-interval 0)
    (let ((first-run (format "%d sec" spotify-player-status-refresh-interval))
          (interval spotify-player-status-refresh-interval))
      (setq spotify-timer
            (run-at-time first-run interval 'spotify-player-status)))))

(defun spotify-stop-player-status-timer ()
  "Stop the timer that is updating the mode line."
  (when (and (boundp 'spotify-timer) (timerp spotify-timer))
    (cancel-timer spotify-timer))
  (spotify-player-status))

(defun spotify-player-status ()
  "Update the mode line to display the current Spotify player status."
  (interactive)
  (spotify-apply "player-status")
  ;; §-TODO-§ [2019-11-09]:
  ;;  Only force if: a) using modeline, b) after async CALLBACK.
  ;;  Could force title update if using title instead of modeline...
  (force-mode-line-update t))

(defun spotify-play-uri (uri)
  "Sends a `play' command to Spotify process passing the given URI."
  (interactive "SSpotify URI: ")
  (spotify-apply "player-play-track" uri nil))

(defun spotify-play-track (track &optional context)
  "Sends a `play' command to Spotify process with TRACK passing a CONTEXT id."
  (interactive)
  (spotify-apply
   "player-play-track"
   (when track (spotify-get-item-uri track))
   (when context (spotify-get-item-uri context))))

(defun spotify-toggle-play ()
  "Sends a `playpause' command to Spotify process."
  (interactive)
  (spotify-apply "player-toggle-play"))

(defun spotify-next-track ()
  "Sends a `next track' command to Spotify process."
  (interactive)
  (spotify-apply "player-next-track"))

(defun spotify-previous-track ()
  "Sends a `previous track' command to Spotify process."
  (interactive)
  (spotify-apply "player-previous-track"))

(defun spotify-volume-up ()
  "Increase the volume for the active device."
  (interactive)
  (spotify-apply "volume-up" spotify-volume-adjust-amount))

(defun spotify-volume-down ()
  "Increase the volume for the active device."
  (interactive)
  (spotify-apply "volume-down" spotify-volume-adjust-amount))

(defun spotify-volume-mute-unmute ()
  "Mute/unmute the volume for the active device. But try to be
smartish about it to not blow out anyone's eardrums..."
  (interactive)

  (if (null spotify-cache-player-status-enabled)
      ;; dumb version - ask for mute/unmute up to max volume.
      (spotify-apply "volume-mute-unmute" spotify-volume-unmute-default)

    ;; Don't try to be /too/ smart... yet... here(?). Have backend request fresh
    ;; volume status and then go from there.
    (let ((set-volume (cond
                       ;; use what we remember
                       ((bound-and-true-p spotify--cache-volume-unmute)
                        spotify--cache-volume-unmute)
                       ;; or the max
                       ((bound-and-true-p spotify-volume-unmute-default)
                        spotify-volume-unmute-default)
                       ;; or give up and 100% it
                       (t 100))))
      ;; backend will either mute or unmute to `set-volume'
      (spotify-apply "volume-mute-unmute" set-volume)
      ;; Save what we've set it to?
      (when (and set-volume (> set-volume 0))
        (setq spotify--cache-volume-unmute set-volume)))))

(defun spotify-toggle-repeat ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (spotify-apply "toggle-repeat"))

(defun spotify-toggle-shuffle ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (spotify-apply "toggle-shuffle"))

(defun spotify-is-repeating ()
  "Sends a command to Spotify process to get the current repeating state."
  (spotify-apply "is-repeating"))

(defun spotify-is-shuffling ()
  "Sends a command to the Spotify process to get the current shuffling state."
  (spotify-apply "is-shuffling"))

(defun spotify--controller-status-updated ()
  "Do anything desired from receiving a status cache update."

  ;; Prevent an error somewhere from being super annoying cuz it interrupts
  ;; every N seconds. Demotes them to messages.
  ;; Could do `condition-case-unless-debug' to demote to ignored if desired.
  (with-demoted-errors "Spotify status update error: %S"
    (when spotify-cache-player-status-enabled
      ;; smarter mute - save positive volumes for unmuting to them
      (if-let ((volume (spotify-player-status-field
                        :volume
                        spotify--player-status-translators)))
          (when (> volume 0)
            (setq spotify--cache-volume-unmute volume))))))


(provide 'spotify-controller)

;;; spotify-controller.el ends here
