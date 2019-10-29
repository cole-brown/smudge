;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; spotify-player-status.el --- Player Status functions and cache.

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Maintains a cache of the last received player status. When asking for
;; metadata (artist, track, shuffling, etc) will pull from the cache unless
;; otherwise directed.

;; This should help avoid overdoing the Spotify Connect API calls, for example.

;;; Code:

;;---------------------------------Spotify.el-----------------------------------
;;--                         Player Status and Cache                          --
;;------------------------------------------------------------------------------

(require 'spotify-api)

;; §-TODO-§ [2019-10-25]: translate connect's hashtable to json string, see
;; what's there for use?

;; §-TODO-§ [2019-10-28]: Check cache's age, fire off async request for update
;; if older than refresh interval defcustom? Return stale to maintain
;; sync interface?


;;------------------------------------------------------------------------------
;; Settings (defcustom) Setters
;;------------------------------------------------------------------------------

;; has to come before its defcustom if we want to also use it
;; for :initialize (which is the default functionality).
(defun spotify--player-status-cache-enabled-set (option-name value)
  "Setter for `spotify-player-status-cache-enabled'.
Enables/disables player status redirect in spotify-api."
  (when (eq option-name 'spotify-player-status-cache-enabled)
    (set-default option-name value)
    (setq spotify--player-status-redirect
          (if (null value)
              nil
            #'spotify--player-status-get-closure))))
;; (customize-set-variable 'spotify-player-status-cache-enabled nil)
;; (customize-set-variable 'spotify-player-status-cache-enabled t)
;; spotify-player-status-cache-enabled
;; spotify--player-status-redirect


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defcustom spotify-player-status-cache-enabled nil
  "Enable or disable caching statuses from `spotify-api-get-player-status'.
Use M-x customize-set-variable or call `customize-set-variable' in your code.

Or do something like the following:
 (setq spotify-player-status-cache-enabled nil)
 (setq spotify--player-status-redirect nil)

 (setq spotify-player-status-cache-enabled t)
 (setq spotify--player-status-redirect #'spotify--player-status-get-closure)"
  :type  'boolean
  :group 'spotify
  :set   #'spotify--player-status-cache-enabled-set)


(defcustom spotify-player-status-duration-fmt "%m:%02s"
  "Format string to use with `format-seconds' to get track duration string."
  :type  'string
  :group 'spotify)


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

;;---
;; The Cache
;;---

(defvar spotify--player-status-cache nil
  "Tuple of (current-time raw-status) where raw-status is
untouched return value of `spotify-api-get-player-status'.")


(defvar spotify--player-status-cache-hook nil
  "Listeners for every single status update that hits the cache."
  ;; Could make a public-er version if desired...
  )


;;---
;; Field Names
;;---

(defconst spotify-player-status-fields
  '(;;---
    ;; Track Status
    ;;---
    artist       ;; string of artist's name
    track        ;; string of track's name
    track-number ;; integer of track number
    ;; string of track's duration formatted by `format-seconds'
    ;; with `spotify-player-status-duration-fmt'
    duration
    duration-millisecond ;; integer of milliseconds in track

    ;;---
    ;; Player Status
    ;;---
    shuffling ;; boolean
    repeating ;; boolean
    playing   ;; boolean
    paused    ;; boolean

    volume ;; integer 0-100
    muted)) ;; boolean


;; §-TODO-§ [2019-10-25]: do I need this?
(defconst spotify-player-status-field->json
  '((artist "artist")
    (track  "name")
    (track-number  "track_number")
    (duration "duration")
    (playing "player_state")
    (shuffling "player_shuffling")
    (repeating "player_repeating"))
  "Translates from elisp-friendly symbol names in
  `spotify-player-status-fields' to JSON-friendly field names.")


;;------------------------------------------------------------------------------
;; Get from Cache or Spotify
;;------------------------------------------------------------------------------

(defun spotify-player-status-field (field)
  "Returns value of field in cached status, or nil."
  (if (not (seq-contains spotify-player-status-fields field))
      (error "spotify-player-status: field '%s' unknown. Choose from: %s"
             field spotify-player-status-fields)

    ;; §-TODO-§ [2019-10-25]: json setup?

    ;; §-TODO-§ [2019-10-25]: Allow both hashtable from connect and
    ;; json from others.
    ;; So json ones would return lexically setup'd stuff for json
    ;; and connect would return this?
    (if-let* ((cache spotify--player-status-cache) ;; null check
              (status (nth 1 cache))
              (track (gethash 'item status)))
        ;; if-let* success case
        ;; get the field from the cache
        (cond
         ;;---
         ;; Track Status
         ;;---
         ((eq field 'artist)
          (gethash 'name (car (gethash 'artists track))))

         ((eq field 'track)
          (gethash 'name track))

         ((eq field 'track-number)
          (gethash 'track_number track))

         ;; raw or formatted duration
         ((eq field 'duration)
          (format-seconds (or spotify-player-status-duration-fmt
                              "%m:%02s")
                          (/ (gethash 'duration_ms track) 1000)))
         ((eq field 'duration-millisecond)
          (gethash 'duration_ms track))

         ;;---
         ;; Player Status
         ;;---
         ((eq field 'shuffling)
          (not (eq (gethash 'shuffle_state status) :json-false)))

         ((eq field 'repeating)
          (not (string= (gethash 'repeat_state status) "off")))

         ((eq field 'playing)
          ;; ...if it's not not playing, it's playing!
          (not (eq (gethash 'is_playing status) :json-false)))

         ((eq field 'paused)
          ;; ...if it's not playing, it's paused?
          ;; What about stopped or not started yet? *shrug* Paused I guess.
          (eq (gethash 'is_playing status) :json-false))

         ((eq field 'volume)
          (gethash 'volume_percent (gethash 'device status)))

         ((eq field 'muted)
          (= (gethash 'volume_percent (gethash 'device status)) 0))

         ;;---
         ;; You should hopefully not get here.
         ;;---
         (t
          (error
           "spotify-player-status: field '%s' known but not handled. Sorry."
           field)))

      ;; if-let* fail case. Can debug it, but so far just nil for normal
      ;; things like no music for a while.
      ;; (message
      ;;  (concat "spotify-player-status: Something null in if-let*... "
      ;;          "What's not null? cache?: %s, status?: %s, track?: %s")
      ;;  (not (null spotify--player-status-cache))
      ;;  (not (null (nth 1 spotify--player-status-cache)))
      ;;  (not (null
      ;;        (if (null (nth 1 spotify--player-status-cache))
      ;;            nil
      ;;          (gethash 'item
      ;;                   (nth 1 spotify--player-status-cache))))))

      ;; And return nil in case you're debugging... >.>
      nil)))
;; (spotify-player-status-field 'artist)
;; (spotify-player-status-field 'track)
;; (spotify-player-status-field 'track-number)
;; (spotify-player-status-field 'duration)
;; (spotify-player-status-field 'duration-millisecond)
;; (spotify-player-status-field 'shuffling)
;; (spotify-player-status-field 'repeating)
;; (spotify-player-status-field 'playing)
;; (spotify-player-status-field 'paused)
;; (spotify-player-status-field 'volume)
;; (spotify-player-status-field 'muted)


;; (defun spotify-player-status-field-async (callback field)
;;   "Request a new update of player status from
;; `spotify-api-get-player-status', caches it, then returns the
;; result's FIELD to the CALLBACK. Relies on lexical-bindings."
;;   (spotify-api-get-player-status
;;    (lambda (status)
;;      ;; first, update our cache
;;      (spotify--player-status-callback nil status)
;;      ;; or this?
;;      ;; (funcall #'spotify--player-status-callback
;;      ;;          callback status)

;;      ;; finally, update caller via callback
;;      (funcall callback (spotify-player-status-field field)))))


;; (defun spotify-player-status-async (callback)
;;   "Request a new update of player status from
;; `spotify-api-get-player-status'. Relies on lexical-bindings."
;;   (spotify-api-get-player-status
;;    (lambda (status)
;;      (funcall #'spotify--player-status-callback
;;               callback status))))


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun spotify--player-status-callback (callback json)
  "Receive updated player status, cache it, and return to
callback unmodified."
  ;; cache status w/ timestamp
  (let ((time (current-time)))
    (when (or (null spotify--player-status-cache)
              (time-less-p (car spotify--player-status-cache)
                           (current-time)))
      (setq spotify--player-status-cache (list time json))))

  ;; and pass on unchanged status
  (when (functionp callback)
    (funcall callback json))

  ;; and also run the hooks
  (run-hooks 'spotify--player-status-cache-hook))


(defun spotify--player-status-get-closure (callback)
  "Spotify-api.el isn't using lexical-binding, so do this over
here I guess.

Want to curry \"(spotify--player-status-callback callback status)\"
down to \"(something status)\", so I need to get a closure with
callback bound and json ready to be received as only
parameter, in order to conform to cache-disabled interface.

This will wrap CALLBACK into a `spotify--player-status-callback'
call, which will let spotify-api send out for async status, and
return to CALLBACK - or redirect to us and let us return to
CALLBACK."
  (lambda (status)
    (spotify--player-status-callback callback status)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-player-status)
