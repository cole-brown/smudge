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

;; §-TODO-§ [2019-10-30]: sub-group for player-status settings?


;;------------------------------------------------------------------------------
;; Settings (defcustom) Setters
;;------------------------------------------------------------------------------

;; Has to come before its defcustom if we want to also use it
;; for :initialize (which is the default functionality).
(defun spotify--player-status-cache-enabled-set (option-name value)
  "Setter for `spotify-player-status-cache-enabled'.
Enables/disables player status redirect in spotify-api."
  (when (eq option-name 'spotify-player-status-cache-enabled)
    (set-default option-name value)
    (setq spotify--player-status-redirect
          (if (null value)
              nil
            #'spotify--player-status-caching-closure))))
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
 (setq spotify--player-status-redirect #'spotify--player-status-caching-closure)"
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
    shuffling      ;; string
    shuffling-bool ;; boolean
    repeating      ;; string
    repeating-bool ;; boolean
    playing        ;; string
    playing-bool   ;; boolean
    paused         ;; string
    paused-bool    ;; boolean

    volume         ;; integer 0-100
    muted          ;; string
    muted-bool)    ;; boolean
  "Symbols that can be passed into `spotify-player-status' for
getting values from the player status.")


(defconst spotify--player-status-requires-translating
  '((duration  duration-millisecond)
    (shuffling shuffling-bool)
    (repeating repeating-bool)
    (playing   playing-bool)
    (paused    paused-bool)
    (muted     muted-bool))
  "`spotify-player-status-fields' that will require translation
from their raw values returned from status. e.g. from
milliseconds to formatted string, from t/nil to 'playing'/'',
etc.")


;; §-TODO-§ [2019-10-30]: Caller should supply this built from defcustoms?
(defconst spotify--player-status-translations
  '((duration-millisecond format-seconds spotify-player-status-duration-fmt)
    (shuffling-bool ((nil "-") (t "S")))
    (repeating-bool ((nil "-") (t "R")))
    (playing-bool   ((nil "-") (t "P")))
    (paused-bool    ((nil "P") (t "-")))
    (muted-bool     ((nil "-") (t "M"))))
  "A dictionary for translating fields in
  `spotify--player-status-requires-translating'. Probably should
  use something built from user's defcustom settings instead...")


(defconst spotify--player-status-field->format-spec
  '((artist       "%a")
    (track        "%t")
    (track-number "%n")
    (duration     "%l")
    (playing      "%p")
    (shuffling    "%s")
    (repeating    "%r")
    (volume       "%v")
    (muted        "%m"))
  "Translates from elisp-friendly symbol names for
`spotify-player-status-fields' to
`spotify--player-status-format' field specifications.")


;; ;; §-TODO-§ [2019-10-25]: do I need this?
;; (defconst spotify-player-status-field->json
;;   '((artist       "artist")
;;     (track        "name")
;;     (track-number "track_number")
;;     (duration     "duration")
;;     (playing      "player_state")
;;     (shuffling    "player_shuffling")
;;     (repeating    "player_repeating")
;;     ;; volume...
;;     )
;;   "Translates from elisp-friendly symbol names in
;;   `spotify-player-status-fields' to JSON-friendly field names.")


;;------------------------------------------------------------------------------
;; Player Status Fields
;;------------------------------------------------------------------------------

(defun spotify-player-status-field (field dictionary)
  "Returns value of FIELD in cached status, or nil.

§-TODO-§ [2019-10-30]: dictionary info"
  (if-let* ((cache spotify--player-status-cache) ;; null check
            (status (nth 1 cache)))

      (spotify--player-status-field status field dictionary)

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
    nil))
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

;; §-TODO-§ [2019-10-29]: stopped? spotify-player-status-playing-indicator


(defun spotify--player-status-field (status field dictionary)
  "Returns value of FIELD in STATUS, or nil.

STATUS must be JSON or nil. If STATUS is nil, cached value will be used.

§-TODO-§ [2019-10-30]: talk about translating dictionary, default spotify--player-status-translations"

  (if (not (member spotify-player-status-fields field))
      (error "spotify-player-status: field '%s' unknown. Choose from: %s"
             field spotify-player-status-fields)

    ;; Are we actually getting FIELD, or are we getting another and
    ;; translating it?
    (let* ((dictionary (or dictionary
                           spotify--player-status-translations))
           (trans-entry
            (assoc field spotify--player-status-requires-translating))
           (field-true (or (nth 1 trans-entry)
                           field))
           (value (spotify--player-status-field-raw status field-true)))
      ;; Do we have any translating to do?
      (if (not trans-entry)
          value
        ;; Else translate it using dictionary.
        (spotify--player-status-translate value dictionary)))))


(defun spotify--player-status-field-raw (status field)
  "Returns value of FIELD in STATUS, or nil.

STATUS must be JSON or nil. If STATUS is nil, cached value will be used.

§-TODO-§ [2019-10-30]: talk about raws instead of translated...
And how they're not actually raw, they're just field values elisp'd into numbers, strings, or t/nil..."

    ;; §-TODO-§ [2019-10-25]: json setup?

    ;; §-TODO-§ [2019-10-25]: Allow both hashtable from connect and
    ;; json from others.
    ;; So json ones would return lexically setup'd stuff for json
    ;; and connect would return this?
    (if-let* ((status (or status ;; supplied status or use cache
                          (nth 1 spotify--player-status-cache)))
              (track (gethash 'item status))
              (valid-field
               (and (member field spotify-player-status-fields)
                    (null (assoc spotify--player-status-requires-translating)))))

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

         ;; raw duration
         ((eq field 'duration-millisecond)
          (gethash 'duration_ms track))

         ;;---
         ;; Player Status
         ;;---
         ((eq field 'shuffling-bool)
          (not (eq (gethash 'shuffle_state status) :json-false)))

         ((eq field 'repeating-bool)
          (not (string= (gethash 'repeat_state status) "off")))

         ((eq field 'playing-bool)
          ;; ...if it's not not playing, it's playing!
          (not (eq (gethash 'is_playing status) :json-false)))

         ((eq field 'paused-bool)
          ;; ...if it's not playing, it's paused?
          ;; What about stopped or not started yet? *shrug* Paused I guess.
          (eq (gethash 'is_playing status) :json-false))

         ((eq field 'volume)
          (gethash 'volume_percent (gethash 'device status)))

         ((eq field 'muted-bool)
          (= (gethash 'volume_percent (gethash 'device status)) 0))

         ;;---
         ;; You should hopefully not get here.
         ;;---
         (t
          (error
           "spotify-player-status: field '%s' known but not handled? Sorry."
           field)))

      ;; if-let* fail case. Can debug it, but so far just nil for normal
      ;; things like no music for a while.
      ;; (message
      ;;  (concat "spotify-player-status: Something null in if-let*... "
      ;;          "What's not null? cache?: %s, status?: %s, "
      ;;          "track?: %s, valid?: %s")
      ;;  (not (null spotify--player-status-cache))
      ;;  (not (null (nth 1 spotify--player-status-cache)))
      ;;  (not (null
      ;;        (if (null (nth 1 spotify--player-status-cache))
      ;;            nil
      ;;          (gethash 'item
      ;;                   (nth 1 spotify--player-status-cache)))))
      ;;  (and (member field spotify-player-status-fields)
      ;;               (null (assoc spotify--player-status-requires-translating))))

      ;; And return nil in case you're debugging... >.>
      nil))


(defun spotify--player-status-translate (value dictionary)
  "Translates our VALUE according to the DICTIONARY provided.

DICTIONARY is an alist of format:
 '((value0 func-or-alist arg)
   ...
   (valueN func-or-alist arg))

VALUE's entry in DICTIONARY will be grabbed, and sub-value in
func-or-alist returned if list. Else func-or-alist will be called
via: (apply func-or-alist arg value)"

  (if-let* ((entry (assoc value dictionary))
            (func-or-alist (nth 0 entry))
            (arg (nth 1 entry)))
      (if (functionp func-or-alist)
          ;; Call the function and that's the return.
          (funcall func-or-alist arg value)
        ;; Else func-or-alist is a sub-alist. Get its entry for value.
        (nth 1(assoc value func-or-alist)))

    ;; Probably wise to not error here - will likely be called in hooks and
    ;; other repetitive places.
    ))


;;------------------------------------------------------------------------------
;; Player Status Formatting
;;------------------------------------------------------------------------------

(defun spotify--player-status-format (fmt-str &optional status truncate)
  "Returns a formatted string based on FMT-STR and STATUS.

STATUS must be JSON or nil. If STATUS is nil, cached value will be used.

If TRUNCATE is non-nil, all '%a' and '%t' (artist and track name)
fields will be truncated down to
`spotify-player-status-truncate-length' if needed.

The following format specifications are supported:

* %a - Artist Name
* %t - Track Name
* %n - Track Number
* %l - Track Duration (as formatted by `spotify-player-status-duration-fmt')
* %p - 'Player Status' indicator string for:
       'playing', 'paused', and 'stopped' states
* %s - 'Player Shuffling' indicator
* %r - 'Player Repeating' indicator
* %v - Player Volume (0-100)"

  (if-let* ((status (or status ;; supplied status or use cache
                        (nth 1 spotify--player-status-cache)))
            (ret-val fmt-str)
            ;; (json-object-type 'hash-table)
            ;; (json-key-type 'symbol)
            ;; (json (condition-case nil
            ;;           (json-read-from-string metadata)
            ;;         (error (spotify-update-player-status "")
            ;;                nil)))
            )

      ;; fmt-spec here is '(field-symbol field-spec-string)
      ;; e.g. '(artist "%a")
      (dolist (fmt-spec spotify--player-status-field->format-spec ret-val)
        ;; §-TODO-§ [2019-10-29]: strings instead of ret-val values...
        ;; e.g. playing/etc are bools, need to convert to their strs
        (setq ret-val
              (spotify--player-status-format-field ret-val
                                                   (nth 1 fmt-spec)
                                                   (nth 0 fmt-spec)
                                                   truncate)))

  ;; if-let got a null - return empty string instead of nil to not
  ;; muck up formatting strings
  ""))


(defun spotify--player-status-format-field (input fmt-spec field status truncate)
  "Returns INPUT string with FMT-SPEC replaced by FIELD's value
from STATUS, truncated to TRUNCATE length if non-nil and
non-zero."
  ;; get value, trucate if needed, null checks by if-let.
  (if-let* ((value (spotify-player-status-field field))
            (value (if truncate
                       (truncate-string-to-width
                        value
                        spotify-player-status-truncate-length
                        0 nil "...")
                     value)))
      ;; got value - replace
      (replace-regexp-in-string fmt-spec
                                value
                                input)
    ;; no value - return empty string instead of nil to not
    ;; muck up formatting strings
    ""))


;;------------------------------------------------------------------------------
;; Async Player Status Fields
;;------------------------------------------------------------------------------

;; (defun spotify-player-status-field-async (callback field)
;;   "Request a new update of player status from
;; `spotify-api-get-player-status', caches it, then returns the
;; result's FIELD to the CALLBACK. Relies on lexical-bindings."
;;   (spotify-api-get-player-status
;;    (lambda (status)
;;      ;; first, update our cache
;;      (spotify--player-status-caching-callback nil status)

;;      ;; finally, update caller via callback
;;      (funcall callback (spotify-player-status-field field)))))


;; (defun spotify-player-status-async (callback)
;;   "Request a new update of player status from
;; `spotify-api-get-player-status'. Relies on lexical-bindings."
;;   (spotify-api-get-player-status
;;    (lambda (status)
;;      (funcall #'spotify--player-status-caching-callback
;;               callback status))))


;;------------------------------------------------------------------------------
;; Caching Functions
;;------------------------------------------------------------------------------

(defun spotify--player-status-caching-callback (callback json)
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


(defun spotify--player-status-caching-closure (callback)
  "Spotify-api.el isn't using lexical-binding, so do this over
here I guess.

Want to curry \"(spotify--player-status-caching-callback callback status)\"
down to \"(something status)\", so I need to get a closure with
callback bound and json ready to be received as only
parameter, in order to conform to cache-disabled interface.

This will wrap CALLBACK into a `spotify--player-status-caching-callback'
call, which will let spotify-api send out for async status, and
return to us for caching and redirect to CALLBACK."
  (lambda (status)
    (spotify--player-status-caching-callback callback status)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-player-status)
