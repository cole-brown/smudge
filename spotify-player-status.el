;;; spotify-player-status.el --- Player Status functions and cache. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Concentration of Player Status functions here, as they were growing numerous.

;; Also can maintain a cache of the last received player status. When asking for
;; metadata (artist, track, shuffling, etc) will pull from the cache unless
;; otherwise directed (or disabled).

;; This should help avoid overdoing the Spotify Connect API calls, for example.

;;; Code:

;;---------------------------------Spotify.el-----------------------------------
;;--                         Player Status and Cache                          --
;;------------------------------------------------------------------------------

(require 'spotify-api)
(require 'spotify-json)

;; §-TODO-§ [2019-11-01]: move cache normalizing, cache actually-getting-value
;; functions to backends (connect, etc)

;; §-TODO-§ [2019-10-25]: translate connect's hashtable to json string, see
;; what's there for use?

;; §-TODO-§ [2019-10-28]: Check cache's age, fire off async request for update
;; if older than refresh interval defcustom? Return stale to maintain
;; sync interface?

;; §-TODO-§ [2019-10-31]: Remove the debugging messages and
;; commented out testing func calls

;;------------------------------------------------------------------------------
;; Settings (defcustom) Setters
;;------------------------------------------------------------------------------

;; Has to come before its defcustom if we want to also use it
;; for :initialize (which is the default functionality).
(defun spotify--cache-player-status-enabled-set (option-name value)
  "Setter for `spotify-cache-player-status-enabled'.
Enables/disables player status redirect in spotify-api."
  (when (eq option-name 'spotify-cache-player-status-enabled)
    (set-default option-name value)
    (setq spotify--player-status-redirect
          (if (null value)
              nil
            #'spotify--player-status-caching-closure))))
;; (customize-set-variable 'spotify-cache-player-status-enabled nil)
;; (customize-set-variable 'spotify-cache-player-status-enabled t)
;; spotify-cache-player-status-enabled
;; spotify--player-status-redirect


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defcustom spotify-player-status-refresh-interval 5
  "The interval, in seconds, that the mode line must be updated. When using the
'connect transport, avoid using values smaller than 5 to avoid being rate
limited. Set to 0 to disable this feature."
  :type 'integer
  :group 'spotify)


;; §-TODO-§ [2019-11-09]: implement this?
(defcustom spotify-player-status-refresh-on-action nil
  "Enable to allow player-status to ask for a refresh after
actions are taken by the user. E.g. change track, playlist,
pause, etc.

Useful if you have set `spotify-player-status-refresh-interval' higher."
  :type  'boolean
  :group 'spotify)


(defcustom spotify-player-status-truncate-length 15
  "The maximum number of characters to truncated fields in
`spotify-player-status-format'."
  :type 'integer
  :group 'spotify)


(defcustom spotify-player-status-playing-text "Playing"
  "Text to be displayed when Spotify is playing."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-paused-text "Paused"
  "Text to be displayed when Spotify is paused."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-stopped-text "Stopped"
  "Text to be displayed when Spotify is stopped."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-repeating-text "R"
  "Text to be displayed when repeat is enabled."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-not-repeating-text "-"
  "Text to be displayed when repeat is disabled."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-shuffling-text "S"
  "Text to be displayed when shuffling is enabled."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-not-shuffling-text "-"
  "Text to be displayed when shuffling is disabled."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-muted-text "M"
  "Text to be displayed when muting is enabled."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-not-muted-text "-"
  "Text to be displayed when muting is disabled."
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-format "[%p: %a - %t ◷ %l %r%s]"
  "Format used to display the current Spotify client player status.
The following placeholders are supported:

* %a - Artist name (truncated)
* %t - Track name (truncated)
* %n - Track #
* %l - Track duration, in minutes (i.e. 01:35)
* %p - Player status indicator for 'playing', 'paused', and 'stopped' states
* %s - Player shuffling status indicator
* %r - Player repeating status indicator"
  :type 'string
  :group 'spotify)


(defcustom spotify-player-status-duration-format "%m:%02s"
  "Format string to use with `format-seconds' to get track duration string."
  :type  'string
  :group 'spotify)


(defcustom spotify-volume-unmute-default 75
  "Default in case there was no pre-mute remembered."
  :type  'integer
  :group 'spotify)


(defcustom spotify-volume-adjust-amount 10
  "Adjust volume up/down this amount (percent)."
  :type  'integer
  :group 'spotify)


(defcustom spotify-cache-player-status-enabled nil
  "Enable or disable caching statuses from `spotify-api-get-player-status'.
Use M-x customize-set-variable or call `customize-set-variable' in your code.

Or do something like the following:
 (setq spotify-cache-player-status-enabled nil)
 (setq spotify--player-status-redirect nil)

 (setq spotify-cache-player-status-enabled t)
 (setq spotify--player-status-redirect #'spotify--player-status-caching-closure)"
  :type  'boolean
  :group 'spotify
  :set   #'spotify--cache-player-status-enabled-set)


;;------------------------------------------------------------------------------
;; Consts & Vars
;;------------------------------------------------------------------------------

;;---
;; The Cache
;;---

(defvar spotify--cache-player-status nil
  "Tuple of (current-time raw-status) where raw-status is
untouched return value of `spotify-api-get-player-status'.

NOTE: Probably use these accessors:
  - `spotify--cache-set-status'
  - `spotify--cache-get-status-if'
  - `spotify--cache-get-timestamp-if'")


(defvar spotify--cache-player-status-hook nil
  "Listeners for every single status update that hits the cache."
  ;; Could make a public-er version if desired...
  )


;; Cache Junior
;; Save "unmute-to-this" separate for easier separation of concerns.
(defvar spotify--cache-volume-unmute nil
  "Remember what percent the volume was when we muted.")


;;---
;; Field Names
;;---

(defconst spotify-player-status-fields
  '(;;---
    ;; Track Status
    ;;---
    :artist       ;; string of artist's name
    :track        ;; string of track's name
    :track-number ;; integer of track number
    ;; string of track's duration formatted by `format-seconds'
    ;; with `spotify-player-status-duration-format'
    :duration
    :duration-millisecond ;; integer of milliseconds in track

    ;;---
    ;; Player Status
    ;;---
    ;; device-active      ;; §-TODO-§ [2019-11-01]: string of active device's name?
    :device-active-bool  ;; boolean: state mapped to t, nil, nil, nil
    :device-active-state ;; symbol: t, nil, undefined, unsupported
    :shuffling      ;; string
    :shuffling-bool ;; boolean
    :repeating      ;; string
    :repeating-bool ;; boolean
    :playing        ;; string
    :playing-bool   ;; boolean
    :paused         ;; string
    :paused-bool    ;; boolean

    :volume         ;; integer 0-100
    :muted          ;; string
    :muted-bool)    ;; boolean
  "Symbols that can be passed into `spotify-player-status' for
getting values from the player status.")


(defconst spotify--player-status-translators
  ;; Use symbols and delay their eval to values so we don't have to bother
  ;; keeping this up-to-date.
  '(;; Artist/Track: truncate to length
    (:artist    :artist
                (lambda (len str) (if (stringp str)
                                      (truncate-string-to-width str len
                                                                0 nil "...")
                                    ""))
                spotify-player-status-truncate-length)
    (:track     :track
                (lambda (len str) (if (stringp str)
                                      (truncate-string-to-width str len
                                                                0 nil "...")
                                    ""))
                spotify-player-status-truncate-length)
    ;; Duration: Format duration-ms to duration string
    (:duration  :duration-millisecond
                (lambda (fmt ms) (if (numberp ms)
                                     (format-seconds fmt (/ ms 1000))
                                   ""))
                spotify-player-status-duration-format)

    ;; Device Active bool: translate from each device-active-state to bool
    (:device-active-bool :device-active-state
                         ;; Just map all non-true to nil/false.
                         ((t t) (nil nil)
                          (undefined nil) (unsupported nil)))

    ;; The Rest: translate from bool to text
    (:shuffling :shuffling-bool
                ((t spotify-player-status-shuffling-text)
                 (nil spotify-player-status-not-shuffling-text)))
    (:repeating :repeating-bool
                ((t spotify-player-status-repeating-text)
                 (nil spotify-player-status-not-repeating-text)))
    (:playing   :playing-bool
                ((t spotify-player-status-playing-text)
                 (nil spotify-player-status-paused-text)))
    (:paused    :paused-bool
                ((nil spotify-player-status-paused-text)
                 (t spotify-player-status-playing-text)))
    (:muted     :muted-bool
                ((t spotify-player-status-muted-text)
                 (nil spotify-player-status-not-muted-text))))
  "`spotify-player-status-fields' that will require translation
from their raw values returned from status. e.g. from
milliseconds to formatted string, from t/nil to 'playing'/'',
etc.

Format of alist item can take two forms:
  (symbol-desired symbol-raw translation-func translation-arg)
  (symbol-desired symbol-raw translation-alist)

Translation functions should have signature of:
  (defun trans-func (translation-arg status-field-value) ...)

Translation alists should have format of:
  ((status-field-value0 \"return value 0\")
    ...
   (status-field-valueN \"return value N\"))")


(defconst spotify--player-status-field->format-spec
  '((:artist       "%a")
    (:track        "%t")
    (:track-number "%n")
    (:duration     "%l")
    (:playing      "%p")
    (:shuffling    "%s")
    (:repeating    "%r")
    (:volume       "%v")
    (:muted        "%m"))
  "Translates from elisp-friendly symbol names for
`spotify-player-status-fields' to
`spotify--player-status-format' field specifications.")



;;------------------------------------------------------------------------------
;; Player Status Fields
;;------------------------------------------------------------------------------

(defun spotify-player-status-field (field &optional dictionary)
  "Returns value of FIELD in cached status, or nil.

See `spotify--player-status-translators' for dictionary info.
"
  ;; Allow null cache and status -
  ;; I think asking "playing?" on a null status should do the full check,
  ;; translate, and whatever else, and be allowed to come back with "Paused".
  (let* ((status (spotify--cache-get-status-if nil)))

    (spotify--player-status-field status field dictionary)))

;; if-let*'s else clause?
;; ;; if-let* fail case. Can debug it, but so far just nil for normal
;; ;; things like no music for a while.
;; ;; (message
;; ;;  (concat "spotify-player-status: Something null in if-let*... "
;; ;;          "What's not null? cache?: %s, status?: %s, track?: %s")
;; ;;  (not (null spotify--cache-player-status))
;; ;;  (not (null (nth 1 spotify--cache-player-status)))
;; ;;  (not (null
;; ;;        (if (null (nth 1 spotify--cache-player-status))
;; ;;            nil
;; ;;          (gethash 'item
;; ;;                   (nth 1 spotify--cache-player-status))))))

;; ;; And return nil in case you're debugging... >.>
;; nil))
;; (spotify-player-status-field :artist)
;; (spotify-player-status-field :track)
;; (spotify-player-status-field :track-number)
;; (spotify-player-status-field :duration)
;; (spotify-player-status-field :duration-millisecond)
;; (spotify-player-status-field :shuffling)
;; (spotify-player-status-field :repeating)
;; (spotify-player-status-field :playing)
;; (spotify-player-status-field :paused)
;; (spotify-player-status-field :volume)
;; (spotify-player-status-field :muted)

;; §-TODO-§ [2019-10-29]: "stopped"? spotify-player-status-playing-indicator


(defun spotify--player-status-field (status field dictionary)
  "Returns value of FIELD in STATUS, or nil.

STATUS must be JSON or nil. If STATUS is nil, cached value will be used.

DICTIONARY must be alist or nil. If nil,
`spotify--player-status-translators' is used as a default. See
`spotify--player-status-translators' for dictionary info.
"
  (if (not (member field spotify-player-status-fields))
      (error "spotify--player-status: field '%s' unknown. Choose from: %s"
             field spotify-player-status-fields)

    ;; Are we actually getting FIELD, or are we getting another and
    ;; translating it?
    (let* ((status (spotify--normalized-status-type status))
           (dictionary (or dictionary
                           spotify--player-status-translators))
           (trans-entry
            (assoc field dictionary))
           (field-true (or (nth 1 trans-entry)
                           field))
           (value (spotify--player-status-field-raw status field-true
                                                    dictionary)))

      ;; (message "spotify--player-status-field: %S %S -> %S \ndict: %S"
      ;;          field
      ;;          value

      ;;          (if (not trans-entry)
      ;;              value
      ;;            ;; Else translate it using dictionary.
      ;;            (spotify--player-status-translate field field-true value
      ;;                                                    dictionary))

      ;;          dictionary
      ;;          )


      ;; Do we have any translating to do?
      (if (not trans-entry)
          value
        ;; Else translate it using dictionary.
        (spotify--player-status-translate field field-true value dictionary)))))


(defun spotify--player-status-translate (field field-true value dictionary)
  "Translates our VALUE according to the DICTIONARY provided.

DICTIONARY is an alist of format:
 '((value0 func-or-alist arg)
   ...
   (valueN func-or-alist arg))

VALUE's entry in DICTIONARY will be grabbed, and sub-value in
func-or-alist returned if list. Else func-or-alist will be called
via: (apply func-or-alist arg value)"

  ;; (message "spotify--player-status-translate: %S %S %S %S"
  ;;          field field-true value nil) ;; dictionary)
  (let* ((entry (assoc field dictionary))
         (func-or-alist (nth 2 entry))
         (arg (nth 3 entry))
         ;; do we have symbol we need to eval or just a pass-through?
         (arg (if (and arg ;; have arg
                       (boundp arg)) ;; it's bound to something
                  (symbol-value arg)
                arg)))
    ;; (message "spotify--player-status-translate: %S %S %S %S"
    ;;          field-true entry func-or-alist arg)
    (if (functionp func-or-alist)
        ;; Call the function and that's the return.
        (funcall func-or-alist arg value)

      ;; Else func-or-alist is a sub-alist. Get its entry for value.
      ;; (message "spotify--player-status-translate: alist: %S -> %S"
      ;;          (assoc value func-or-alist)
      ;;          (nth 1 (assoc value func-or-alist)))
      ;; do we need to eval or just a pass-through?
      (let ((return-value (nth 1 (assoc value func-or-alist))))
        ;; (message "spotify--player-status-translate: alist: %S -> %S -%S-> %S"
        ;;          value
        ;;          return-value
        ;;          (if (and return-value
        ;;                   (boundp return-value))
        ;;              "sym-val"
        ;;            "pass")
        ;;          (if (and return-value
        ;;                   (boundp return-value))
        ;;              (symbol-value return-value)
        ;;            return-value))

        (if (and return-value
                 (boundp return-value))
            (symbol-value return-value)
          return-value)))))
;; (spotify--player-status-translate :shuffling :shuffling-bool nil spotify--player-status-translators)
;; (spotify--player-status-translate :shuffling :shuffling-bool t spotify--player-status-translators)
;; (spotify-player-status-field :shuffling)
;; (spotify-player-status-field :duration)


;;------------------------------------------------------------------------------
;; Player Status Formatting
;;------------------------------------------------------------------------------


(defun spotify-player-status-get (status)
  "Returns a formatted string of player status based on STATUS and formatted by
`spotify-player-status-format'.

STATUS must be JSON string, Hashtable or nil. If STATUS is nil, cached value
will be used.

The following format specifications are supported:

* %a - Artist Name
* %t - Track Name
* %n - Track Number
* %l - Track Duration (as formatted by `spotify-player-status-duration-format')
* %p - 'Player Status' indicator string for:
       'playing', 'paused', and 'stopped' states
* %s - 'Player Shuffling' indicator
* %r - 'Player Repeating' indicator
* %v - Player Volume (0-100)

All '%a' and '%t' (artist and track name) fields will be
truncated down to `spotify-player-status-truncate-length' if
needed.
"
  (spotify--player-status-format spotify-player-status-format
                                 status
                                 spotify--player-status-translators))


(defun spotify--player-status-format (fmt-str &optional
                                              status dictionary)
  "Returns a formatted string based on FMT-STR and STATUS.

STATUS must be JSON, hashtable, or nil. If STATUS is nil, cached
value will be used.

The following format specifications are supported:

* %a - Artist Name (truncated)
* %t - Track Name (truncated)
* %n - Track Number
* %l - Track Duration (as formatted by `spotify-player-status-duration-format')
* %p - 'Player Status' indicator string for:
       'playing', 'paused', and 'stopped' states
* %s - 'Player Shuffling' indicator
* %r - 'Player Repeating' indicator
* %v - Player Volume (0-100)
"
  ;; supplied status or use cache
  (if-let* ((status (spotify--cache-get-status-if status))
            (ret-val fmt-str))

      ;; fmt-spec here is '(field-symbol field-spec-string)
      ;; e.g. '(:artist "%a")
      (progn
        ;; (message "%S -hi-> %S" fmt-str ret-val)
        (dolist (fmt-spec spotify--player-status-field->format-spec ret-val)
          (setq ret-val
                (spotify--player-status-format-field ret-val
                                                     (nth 1 fmt-spec)
                                                     (nth 0 fmt-spec)
                                                     status
                                                     dictionary))
          ;; (message "spec: %S, field: %S, value: %S, out: %S"
          ;;          (nth 1 fmt-spec)
          ;;          (nth 0 fmt-spec)
          ;;          (spotify--player-status-field status (nth 0 fmt-spec) dictionary)
          ;;          ret-val)

          ;; (if (string=  fmt-str "%a - %t")
          ;;     (progn
          ;;       (message "ps-fmt: status? %S,  dict? %S, spec: %S, field: %S,\n %S-(%S==%S)->%S"
          ;;                (not (null status))
          ;;                (not (null dictionary))
          ;;                (nth 1 fmt-spec)
          ;;                (nth 0 fmt-spec)

          ;;                ;; newline
          ;;                fmt-str
          ;;                (nth 0 fmt-spec)
          ;;                (spotify--player-status-field status
          ;;                                              (nth 0 fmt-spec)
          ;;                                              dictionary)
          ;;                (spotify--player-status-format-field ret-val
          ;;                                            (nth 1 fmt-spec)
          ;;                                            (nth 0 fmt-spec)
          ;;                                            status
          ;;                                            dictionary)
          ;;                )
          ;;       (message "psf2: %S -%s-> %S" fmt-str fmt-spec ret-val)))
          ))

    ;; if-let got a null - return empty string instead of nil to not
    ;; muck up formatting strings
    ""))

;; (replace-regexp-in-string "%v" nil "hello")
(defun spotify--player-status-format-field (input fmt-spec field
                                                  status dictionary)
  "Returns INPUT string with FMT-SPEC replaced by FIELD's value
from STATUS.

STATUS must be hashtable, string, or nil. See `spotify--normalized-status-type'.
"
  ;; get value, truncated if dictionary says so.
  (if-let* ((value (spotify--player-status-field status
                                                 field
                                                 dictionary)))
      ;; got value - replace
      (replace-regexp-in-string fmt-spec
                                value
                                input)
    ;; no/null value - return input unchanged.
    input))


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
;;      (funcall callback (spotify-player-status-field field dictionary)))))


;; (defun spotify-player-status-async (callback)
;;   "Request a new update of player status from
;; `spotify-api-get-player-status'. Relies on lexical-bindings."
;;   (spotify-api-get-player-status
;;    (lambda (status)
;;      (funcall #'spotify--player-status-caching-callback
;;               callback status))))


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

;; §-TODO-§ [2019-11-10]: "normalized" isn't so great...
;; And we probably shouldn't do this - that's what spotify-apply is for.
(defun spotify--normalized-status-type (status)
  "Takes in a few kinds of STATUSes and returns a valid normalized STATUS.

Caveat Emptor: hides/squelches errors!

Valid inputs are:
  - nil
  - json string
  - (json) hashtable

Valid returns are:
  - nil
  - (json) hashtable
"
  (cond
   ;; null is ok - just pass back another null
   ((null status)
    nil)

   ;; hash table is also ok - pass through
   ((hash-table-p status)
    status)

   ;; string - read into json hash table
   ((stringp status)
    (if-let* ((json-object-type 'hash-table)
              (json-key-type 'symbol)
              (json (spotify--eat-errors
                        ;; demote error to message unless debugging
                        (message "%S caught an error: %S"
                                 'spotify--normalized-status-type
                                 spotify--error)
                      (json-read-from-string status))))
        ;; read status successfully into json hash table
        ;;   - return the json
        json

      ;; failed with a null somewhere (condition-case caught something?)
      ;;   - return nil
      nil))

   (t nil)))


;; §-TODO-§ [2019-11-10]: requiring status to be figured out before it hits
;; spotify-json is meh. Let spotify json handle it?
(defun spotify--player-status-field-raw (status-n field-true translators)
  "STATUS-N must be normalized - i.e. STATUS-N should be passed
through `spotify--normalized-status-type' first.

Allows STATUS-N to be full spotify-connect API return value, or
simplified JSON STATUS-N used by spotify-dbus/spotify-apple.

Gets FIELD-TRUE from STATUS-N. If STATUS-N is null, uses cached value.

FIELD-TRUE must be a member of `spotify-player-status-fields' and
not associated in TRANSLATORS (see
`spotify--player-status-translators' for sample). This function
is for getting the 'raw' value - 'raw' being minimally translated
from JSON to standardize to e.g. bools.

Simplified JSON is e.g.:
{
  \"artist\": \"<artist> \",
  \"duration\": \"<duration-ms>\",
  \"track_number\": \"<number>\",
  \"name\": \"<track-name>\",
  \"player_state\": \"playing|paused|stopped\",
  \"player_shuffling\": \"-\",
  \"player_repeating\": \"-\",
}

So our current check can be: \"Does it have 'item' as a field?\"
"
  ;; FIELD-TRUE validity checked here.
  (let ((translate-entry (assoc field-true
                                spotify--player-status-translators)))
    ;; (message "spotify--player-status-field-raw: %S. ok? %S <- mem? %S and trans-ok? %S <- no trans %S or self-trans %S %S %S %S 0:%S 1:%S assoc:%S"
    ;;          field-true

    ;;          (and (member field-true spotify-player-status-fields)
    ;;            (or (null translate-entry)
    ;;                (not (eq (nth 0 translate-entry)
    ;;                         (nth 1 translate-entry)))))

    ;;          (member field-true spotify-player-status-fields)

    ;;          (or (null translate-entry)
    ;;              (not (eq (nth 0 translate-entry)
    ;;                       (nth 1 translate-entry))))

    ;;          (null translate-entry)

    ;;          ;; not eq --> eq??
    ;;          (eq (nth 0 translate-entry)
    ;;              (nth 1 translate-entry))

    ;;          (eq (nth 0 translate-entry)
    ;;              (nth 1 translate-entry))

    ;;          (eql (nth 0 translate-entry)
    ;;               (nth 1 translate-entry))

    ;;          (equal (nth 0 translate-entry)
    ;;                 (nth 1 translate-entry))

    ;;            (nth 0 translate-entry)
    ;;            (nth 1 translate-entry)
    ;;            translate-entry
    ;;            )

    ;; valid field to pull from json:
    ;; - is a member of `spotify-player-status-fields'
    ;; - is either not a member of the translation dictionary
    ;;   OR is a "self-translation" (e.g. 'artist' truncates itself)
    (when (and (member field-true spotify-player-status-fields)
               (or (null translate-entry) ;; not a translation
                   ;; is a self translation
                   (eq (nth 0 translate-entry)
                       (nth 1 translate-entry))))

      ;; §-TODO-§ [2019-11-10]: spotify-apply instead of guessing based on cache?

      ;; status or default to cache
      (let ((status-n (spotify--cache-get-status-if status-n)))
        ;; Our current checks...
        (cond
         ;; Easy out.
         ((null status-n)
          nil)
         ;; full should have 'item' in status-n
         ((not (null (gethash 'item status-n)))
          ;; (message "ffg: %S %S" field-true (spotify--json-api-status-field status-n field-true))
          (spotify--json-api-status-field status-n field-true))
         ;; else we'll assume simple?
         (t
          (spotify--json-internal-status-field status-n field-true)))))))


;;------------------------------------------------------------------------------
;; Caching Functions
;;------------------------------------------------------------------------------

(defun spotify--cache-set-status (status)
  "Sets `spotify--cache-player-status' to STATUS with current
time as timestamp as long as its fresher than the currently held status."
  (let ((time (current-time)))
    (when (or (null spotify--cache-player-status)
              (time-less-p (car spotify--cache-player-status)
                           time))
      (setq spotify--cache-player-status (list time status)))))


(defun spotify--cache-get-status-if (status)
  "Returns `spotify--cache-player-status' if STATUS is nil."
  (if (null status)
      (nth 1 spotify--cache-player-status)
    status))
;; (spotify--cache-get-status-if nil)


(defun spotify--cache-get-timestamp-if (status)
  "Returns `spotify--cache-player-status' if STATUS is nil."
  (if (null status)
      (nth 0 spotify--cache-player-status)
    status))


(defun spotify--player-status-caching-callback (callback status)
  "Receive updated player status, cache it, and return to
CALLBACK unmodified."
  ;; cache status w/ timestamp
  (spotify--cache-set-status status)

  ;; and pass on unchanged status
  (when (functionp callback)
    (funcall callback status))

  ;; and also run the hooks
  (run-hooks 'spotify--cache-player-status-hook))


(defun spotify--player-status-caching-closure (callback)
  "Want to curry \"(spotify--player-status-caching-callback CALLBACK status)\"
down to \"(something status)\", so I need to get a closure with
CALLBACK bound and status ready to be received as only
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
