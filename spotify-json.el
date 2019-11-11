;;; spotify-json.el --- JSON read/write for statuses. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Cole Brown

;;; Commentary:

;; Reading JSON from Spotify Connect API, and building it for other backends,
;; maybe, are here.

;;; Code:

;;---------------------------------Spotify.el-----------------------------------
;;--                        JSON Reading and Encoding                         --
;;------------------------------------------------------------------------------


(require 'json)


;;------------------------------------------------------------------------------
;; Links
;;------------------------------------------------------------------------------

;; JSON API:
;; https://developer.spotify.com/documentation/web-api/reference/player/


;;------------------------------------------------------------------------------
;; json.el Settings
;;------------------------------------------------------------------------------

(defconst spotify--json-object-type 'hash-table
  "Our value for setting `json-object-type' in lets.")


(defconst spotify--json-key-type 'symbol
  "Our value for setting `json-key-type' in lets.")


(defconst spotify--json-array-type 'list
  "Our value for setting `json-array-type' in lets.")


;;------------------------------------------------------------------------------
;; Setup/Helper Macro
;;------------------------------------------------------------------------------

(defmacro spotify--build-json (&rest body)
  "Macro that sets up json.el's vars in a let-binding, then runs
BODY forms. `json-obj' is the var ready to take in fields.

Let and body are enclosed in a condition-case-unless-debug that demotes errors.

Sets:
  - `json-object-type' -> `spotify--json-object-type'
  - `json-array-type'  -> `spotify--json-array-type'
  - `json-key-type'    -> `spotify--json-key-type'

Creates:
  - `json-obj' <- `json-new-object'
"
  (declare (indent defun))

  `(condition-case-unless-debug spotify--error
       (let* ((json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (json-obj (json-new-object)))
         ;; ok - execute body forms
         ,@body)
     ;; Error handlers:
     (error ;; 'error' signal:
      ;; demote error to message unless debugging
      (message "spotify--build-json: error caught: %S" spotify--error))))
;; (spotify--build-json (message "hi"))
;; (spotify--build-json (error "hi"))


;;------------------------------------------------------------------------------
;; General Helpers
;;------------------------------------------------------------------------------

(defun spotify--json-bool-encode (value &optional false testfn)
  "Converts Emacs t and nil (or values of FALSE) to json t/json-false.

If TESTFN is non-nil, it will be used as the comparator. Default is `eq'.

If FALSE is non-nil, it will be used as the comparison. Default is 'nil.
"
  (let ((false (or false nil))
        (testfn (or testfn #'eq)))
    (if (funcall testfn value false)
        json-false
      t)))
;; (spotify--json-bool-encode nil)
;; (spotify--json-bool-encode t)
;; (spotify--json-bool-encode testfoo)


(defun spotify--json-bool-decode (value &optional false testfn)
  "Converts Spotify API's... wide variety of bools into Emacs t and nil.

If TESTFN is non-nil, it will be used as the comparator. Default is `eq'.

If FALSE is non-nil, it will be used as the comparison. Default is :json-false.
"
  (let ((false (or false :json-false))
        (testfn (or testfn #'eq)))
    (if (funcall testfn value false)
        nil
      t)))


(defun spotify--json-api--get-field (type json field)
  "Checks that FIELD is an expected member of the json TYPE, then tries to get
it from JSON (hash-table).

If not in JSON, this will return nil.
"
  ;; §-TODO-§ [2019-11-10]: allow both 'field and :field? Change lists at bottom to :field?
  (when (not (member field type))
      (error (concat "spotify--json-api--has-field: "
                     "field '%s' unknown for type '%s'. Choose from: %s")
             field (symbol-name type) type))
  (gethash field json))


;;------------------------------------------------------------------------------
;; Our Simpler Internal JSON
;;------------------------------------------------------------------------------

(defun spotify--json-encode-internal (artist name
                                      duration-ms track-number
                                      playing shuffling repeating)
  "Builds json from args.

Requirements:
  - artist:       stringp
  - name:         stringp
  - duration-ms:  integerp
  - track-number: integerp
  - playing:      emacs bool (nil/non-nil)
  - shuffling:    emacs bool (nil/non-nil)
  - repeating:    emacs bool (nil/non-nil)

Outputs:
  - artist:       string (input str)
  - name:         string (input str)
  - duration-ms:  number (input number)
  - track-number: number (input number)
  - playing:      string (nil->\"paused\", non-nil->\"playing\"
  - shuffling:    json bool (false, true)
  - repeating:    json bool (false, true)

Old Example: ;; §-TODO-§ [2019-11-10]: remove old example...?
  {
    \"artist\": \"Aesop Rock\",
    \"duration\": 265333,
    \"track_number\": 9,
    \"name\":  \"Shrunk\",
    \"player_state\": \"playing\",
    \"player_shuffling\": \"t\",
    \"player_repeating\": \"context\"
  }

Correct Example:
  {
    \"artist\": \"Aesop Rock\",
    \"duration\": 265333,
    \"track_number\": 9,
    \"name\":  \"Shrunk\",
    \"player_state\": \"playing\",
    \"player_shuffling\": true,
    \"player_repeating\": false
  }
"
  ;; error checking
  (cond ((or (null artist)
             (null name)
             (null duration-ms)
             (null track-number))
         (error "spotify--json-internal: these fields cannot be nil: %S %S %S %S"
                artist name duration-ms track-number))
        ((or (not (stringp artist))
             (not (stringp name)))
         (error "spotify--json-internal: these fields must be strings: %S %S"
                artist name))
        ((or (not (integerp duration-ms))
             (not (integerp track-number)))
         (error "spotify--json-internal: these fields must be integers: %S %S"
                duration-ms track-number))
        ;; kinda hard to error check nil/non-nil so we're done.
        (t
         nil))

  ;; spotify--build-json will set us up, then we can just stuff json-obj full.
  (spotify--build-json
    ;; Build status into new json-obj using json lib functions so the json
    ;; stays properly escaped and e.g. artist "Weird Al" doesn't ruin
    ;; everything for our json.
    (setq json-obj (json-add-to-object
                    json-obj "artist"
                    artist))
    (setq json-obj (json-add-to-object
                    json-obj "duration"
                    duration-ms))
    (setq json-obj (json-add-to-object
                    json-obj "track_number"
                    track-number))
    (setq json-obj (json-add-to-object
                    json-obj "name"
                    name))
    (setq json-obj (json-add-to-object
                    json-obj "player_state"
                    (if playing
                        "playing"
                      "paused")))
    (setq json-obj (json-add-to-object
                    json-obj "player_shuffling"
                    (spotify--json-bool-encode shuffling)))
    (setq json-obj (json-add-to-object
                    json-obj "player_repeating"
                    (spotify--json-bool-encode repeating)))

    ;; convert new json to string and return it
    (json-encode json-obj)))
;; (setq debug-on-error t)
;; (spotify--json-encode-internal "Aesop Rock" "Shrunk" 265333 9 "playing" t "context")
;; (spotify--json-encode-internal "\"Weird Al\" Yankovic" "Foil" 265333 9 "playing" nil nil)


(defun spotify--json-internal-status-field (status field)
  "Returns value of FIELD in STATUS, or nil. FIELD must be one of the 'raw'
fields in `spotify-player-status-fields', not an actual field in the status.
This function figures out the translation.

STATUS must be simplified JSON structure (e.g.
`spotify--json-encode-internal') or nil. If STATUS is nil, nil
will be return value.
"
  (if (or (not status)
          (not (hash-table-p status)))
      ;; nothing we can do...
      nil

    ;; if-let* success case
    ;; get the field from the cache
    (cond
     ;;---
     ;; Track Status
     ;;---
     ((eq field :artist)
      (gethash 'artist status))

     ((eq field :track)
      (gethash 'name status))

     ((eq field :track-number)
      (gethash 'track_number status))

     ;; raw duration
     ((eq field :duration-millisecond)
      ;; (message "internal duration-ms: %S->%S"
      ;;          (gethash 'duration status)
      ;;          (gethash 'duration status))
      (gethash 'duration status))

     ;;---
     ;; Player Status
     ;;---
     ((eq field :shuffling-bool)
      ;; (message "internal shuffling: %S->%S"
      ;;          (gethash 'player_shuffling status)
      ;;          (not (eq (gethash 'player_shuffling status) :json-false)))
      (not (eq (gethash 'player_shuffling status) :json-false)))

     ((eq field :repeating-bool)
      ;; (message "internal repeating: %S->%S"
      ;;          (gethash 'player_repeating status)
      ;;          (not (eq (gethash 'player_repeating status) :json-false)))
      (not (eq (gethash 'player_repeating status) :json-false)))

     ((eq field :playing-bool)
      ;; (message "internal playing: %S->%S"
      ;;          (gethash 'player_state status)
      ;;          (string= (gethash 'player_state status) "playing"))
      ;; ...if it's not not playing, it's playing!
      (string= (gethash 'player_state status) "playing"))

     ((eq field :paused-bool)
      ;; ...if it's not playing, it's paused?
      ;; What about stopped or not started yet? *shrug* Paused I guess.
      (not (string= (gethash 'player_state status) "playing")))

     ;;---
     ;; Unsupported
     ;;---
     ((eq field :volume)
      nil)

     ((eq field :muted-bool)
      nil)

     ((eq field :device-active-state)
      'unsupported)

     ;;---
     ;; You should hopefully not get here.
     ;;---
     (t
      (error (concat "spotify--json-internal-status-field: "
                     "field '%s' known but not handled? Sorry.")
             field)))))


;;------------------------------------------------------------------------------
;; Spotify Connect API Helpers
;;------------------------------------------------------------------------------

(defun spotify--json-api-to-internal (player-status)
  "§-TODO-§ [2019-11-04]: This really should get deprecated and just allow
normalized-get to take care of things instead of building a
reduced set out of full return json...

PLAYER-STATUS must be a json reply formatted hash-table (by json.el) from
Spotify Connect API. See:
https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/

Uses Emacs's json.el lib to properly convert from Spotify Connect API's
full json output to a reduced json usable by Spotify.el.
"
  (if-let* ((player-status player-status)
            (track (gethash 'item player-status)))

      (spotify--json-encode-internal
       ;; artist
       (gethash 'name (car (gethash 'artists track)))
       ;; name
       (gethash 'name track)
       ;; duration-ms
       (gethash 'duration_ms track)
       ;; track-number
       (gethash 'track_number track)
       ;; playing
       (not (eq (gethash 'is_playing player-status) :json-false))
       ;; shuffling
       (not (eq (gethash 'shuffle_state player-status) :json-false))
       ;; repeating
       (not (string= (gethash 'repeat_state player-status) "off")))

    ;; player-status or track null - can't really do anything
    ;; other than return null
    nil))


(defun spotify--json-api-device-list (json)
  "Deals with JSON status for device list.

JSON must be a json reply formatted hash-table (by json.el) from
Spotify Connect API. See:
https://developer.spotify.com/documentation/web-api/guides/using-connect-web-api/

Returns devices list from the json.
"
  (if-let ((json json)
           (devices (gethash 'devices json)))
      devices
    nil))


(defun spotify--json-api-status-field (status field)
  "Returns value of FIELD in STATUS (wherever it is - even in
json sub-records), or nil. FIELD must be one of the 'raw' fields
in `spotify-player-status-fields', not an actual field in the
status. This function figures out the translation.

STATUS must be json reply formatted hash-table (by json.el) from
Spotify Connect API. See:
https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/

If STATUS is nil, nil will be return value.
"
  (if-let* ((status status) ;; null check
            (track (gethash 'item status))
            (device (gethash 'device status)))

      ;; if-let* success case
      ;; get the field from the cache
      (cond
       ;;---
       ;; Track Status
       ;;---
       ((eq field :artist)
        (spotify--json-api--get-field spotify--json-api-fields--artist-simple
         ;; just first artist
         (car (spotify--json-api--get-field spotify--json-api-fields--track-full
                                            track
                                            'artists))
         'name))

       ((eq field :track)
        (spotify--json-api--get-field spotify--json-api-fields--track-full
                                      track
                                      'name))

       ((eq field :track-number)
        (spotify--json-api--get-field spotify--json-api-fields--track-full
                                      track
                                      'track_number))

       ;; raw duration
       ((eq field :duration-millisecond)
        (spotify--json-api--get-field spotify--json-api-fields--track-full
                                      track
                                      'duration_ms))

       ;;---
       ;; Player Status
       ;;---

       ((eq field :shuffling-bool)
        (spotify--json-bool-decode
         ;; convert this field to bool...
         (spotify--json-api--get-field spotify--json-api-fields--player-status
                                       status
                                       'shuffle_state)
         ;; with :json-false as the false comparator
         :json-false))

       ((eq field :repeating-bool)
        (spotify--json-bool-decode
         ;; convert this field to bool...
         (spotify--json-api--get-field spotify--json-api-fields--player-status
                                       status
                                       'repeat_state)
         ;; with "off" and `string=' as the false comparator
         "off" #'string=))

       ((eq field :playing-bool)
        (spotify--json-bool-decode
         ;; convert this field to bool...
         (spotify--json-api--get-field spotify--json-api-fields--player-status
                                       status
                                       'is_playing)
         ;; with :json-false as the false comparator
         :json-false))

       ((eq field :paused-bool)
        ;; ...if it's not playing, it's paused?
        ;; What about stopped or not started yet? *shrug* Paused I guess.
        (not
         (spotify--json-bool-decode
          ;; convert this field to bool...
          (spotify--json-api--get-field spotify--json-api-fields--player-status
                                        status
                                        'is_playing)
          ;; with :json-false as the false comparator
          :json-false)))

       ((eq field :volume)
        (spotify--json-api--get-field spotify--json-api-fields--device-full
                                      device
                                      'volume_percent))

       ((eq field :muted-bool)
        ;; no special field - just 0 volume
        (= 0
           (spotify--json-api--get-field spotify--json-api-fields--device-full
                                         device
                                         'volume_percent)))

       ((eq field :device-active-state)
        ;; get field
        (let ((active (spotify--json-api--get-field
                       spotify--json-api-fields--device-full
                       device
                       'is_active)))
          ;; Convert - active if 'is_active field says so.
          (cond ((eq active t)
                 t)
                ((eq active :json-false)
                 nil)
                ;; didn't find device - undefined?
                (t 'undefined))))

       ;;---
       ;; You should hopefully not get here.
       ;;---
       (t
        (error (concat "spotify--json-status-field: "
                       "field '%s' known but not handled? Sorry.")
               field)))

    ;; And return nil in case you're debugging and have a message
    ;; just above here... >.>
    nil))


;;------------------------------------------------------------------------------
;; Spotify Connect API: Player Status Context Fields
;;------------------------------------------------------------------------------

;; Take a lot of room, so down here.


(defconst spotify--json-api-fields--player-status
  '(;; Device Object
    ;;   - The device that is currently active
    device

    ;; string
    ;;   - off, track, context
    repeat_state

    ;; boolean
    ;;   - If shuffle is on or off
    shuffle_state

    ;; a Context Object
    ;;   - A Context Object. Can be null (e.g. If private session is enabled
    ;;     this will be null).
    context

    ;; integer
    ;;   - Unix Millisecond Timestamp when data was fetched
    timestamp

    ;; integer
    ;;   - Progress into the currently playing track. Can be null (e.g. If
    ;;     private session is enabled this will be null).
    progress_ms

    ;; boolean
    ;;   - If something is currently playing.
    is_playing

    ;; A Full Track Object
    ;;   - The currently playing track. Can be null (e.g. If private session is
    ;;     enabled this will be null).
    item

    ;; string
    ;;   - The object type of the currently playing item. Can be one of track,
    ;;     episode, ad or unknown.
    currently_playing_type

    ;; An actions object which contains a disallows object
    ;;   - Allows to update the user interface based on which playback actions
    ;;     are available within the current context
    actions)
  "Fields in a player-status response from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/player/get-information-about-the-users-current-playback/
")


(defconst spotify--json-api-fields--track-full
  '(;; a simplified album object
    ;;   - The album on which the track appears. The album object includes a
    ;;     link in href to full information about the album.
    album

    ;; an array of simplified artist objects
    ;;   - The artists who performed the track. Each artist object includes a
    ;;     link in href to more detailed information about the artist.
    artists

    ;; array of strings
    ;;   - A list of the countries in which the track can be played, identified
    ;;     by their ISO 3166-1 alpha-2 code.
    available_markets

    ;; integer
    ;;   - The disc number (usually 1 unless the album consists of more than one
    ;;     disc).
    disc_number

    ;; integer
    ;;   - The track length in milliseconds.
    duration_ms

    ;; Boolean
    ;;   - Whether or not the track has explicit lyrics ( true = yes it does;
    ;;   - false = no it does not OR unknown).
    explicit

    ;; an external ID object
    ;;   - Known external IDs for the track.
    external_ids

    ;; an external URL object
    ;;   - Known external URLs for this track.
    external_urls

    ;; string
    ;;   - A link to the Web API endpoint providing full details of the track.
    href

    ;; string
    ;;   - The Spotify ID for the track.
    id

    ;; boolean
    ;;   - Part of the response when Track Relinking is applied. If true , the
    ;;     track is playable in the given market. Otherwise false.
    is_playable

    ;; a linked track object
    ;;   - Part of the response when Track Relinking is applied, and the
    ;;     requested track has been replaced with different track. The track in
    ;;     the linked_from object contains information about the originally
    ;;     requested track.
    linked_from

    ;; a restrictions object
    ;;   - Part of the response when Track Relinking is applied, the original
    ;;     track is not available in the given market, and Spotify did not have
    ;;     any tracks to relink it with. The track response will still contain
    ;;     metadata for the original track, and a restrictions object containing
    ;;     the reason why the track is not available: "restrictions" : {"reason"
    ;;     : "market"}
    restrictions

    ;; string
    ;;   - The name of the track.
    name

    ;; integer
    ;;   - The popularity of the track. The value will be between 0 and 100,
    ;;     with 100 being the most popular.
    popularity

    ;; string
    ;;   - A link to a 30 second preview (MP3 format) of the track. Can be null
    preview_url

    ;; integer
    ;;   - The number of the track. If an album has several discs, the track number is the number on the specified disc.
    track_number

    ;; string
    ;;   - The object type: “track”.
    type

    ;; string
    ;;   - The Spotify URI for the track.
    uri

    ;; boolean
    ;;   - Whether or not the track is from a local file.
    is_local)

  "Fields in a track object from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full
")


(defconst spotify--json-api-fields--artist-simple
  '(;; an external URL object
    ;;   - Known external URLs for this artist.
    external_urls

    ;; string
    ;;   - A link to the Web API endpoint providing full details of the artist.
    href

    ;; string
    ;;   - The Spotify ID for the artist.
    id

    ;; string
    ;;   - The name of the artist
    name

    ;; string
    ;;   - The object type: "artist"
    type

    ;; string
    ;;   - The Spotify URI for the artist.
    uri)
  "Fields in a simplified artist object from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/object-model/#artist-object-simplified
")


(defconst spotify--json-api-fields--device-full
  '(;; string
    ;;   - The device ID. This may be null.
    id

    ;; boolean
    ;;   - If this device is the currently active device.
    is_active

    ;; boolean
    ;;   - If this device is currently in a private session.
    is_private_session

    ;; boolean
    ;;   - Whether controlling this device is restricted. At present if this is “true” then no Web API commands will be accepted by this device.
    is_restricted

    ;; string
    ;;   - The name of the device.
    name

    ;; string
    ;;   - Device type, such as “Computer”, “Smartphone” or “Speaker”.
    ;;     - Computer
    ;;     - Tablet
    ;;     - Smartphone
    ;;     - Speaker
    ;;     - TV
    ;;     - AVR
    ;;     - STB
    ;;     - AudioDongle
    ;;     - GameConsole
    ;;     - CastVideo
    ;;     - CastAudio
    ;;     - Automobile
    ;;     - Unknown
    type

    ;; integer
    ;;   - The current volume in percent. This may be null.
    volume_percent)
  "Device object from Spotify Connect API.

https://developer.spotify.com/documentation/web-api/reference/player/get-a-users-available-devices/
")


;; Many other objects but we're not using them (that I've seen) yet.
;;   https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full


;;------------------------------------------------------------------------------
;; §-TODO-§ [2019-11-10]: delete all this, or make real tests...
;;------------------------------------------------------------------------------
;; (defun testfoo ()
;;   (interactive)
;;   (let* ((json-object-type 'hash-table)
;;          (json-array-type 'list)
;;          (json-key-type 'symbol)
;;          (json-obj (json-new-object))
;;          (json (json-read-from-string "{  \"artist\": \"Aesop Rock\",  \"name\": \"Shrunk\",  \"duration\": 265333,  \"track_number\": 9,  \"player_state\": \"playing\",  \"player_shuffling\": true,  \"player_repeating\": true }")))
;;     (message "your json: %S... hash? %S" json (hash-table-p json))))

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'spotify-json)
