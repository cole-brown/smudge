;;; smudge-cache.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:

;; This library provides caches and cachcing functions for saving data from
;; Spotify Connect responses.
;;
;; Useful for:
;;   - Saving volume info for mute/unmute.
;;   - Saving player status for custom status strings for e.g. hyrdras.

;;; Code:

;; TODO: Should I move `smudge-controller-player-metadata' here?
;;   - TODO: Should I rename `smudge-controller-player-metadata' to `smudge-cache-active-metadata'?
;;     - TODO: Rename and then defalias and/or mark as deprecated?

(defvar smudge-cache--device nil
  "Alist of Device Names to Device IDs.")

(defvar smudge-cache--status nil
  "Alist of Device IDs to hash tables of cached values w/ timestamps.

Hash-Table Keys & Values:
  - `:volume' - integer
  - `:status' - JSON hash-table player status

Each key/value pair also has a twinned timestamp. The timestamp's key is created
by `smudge-cache--time-keyword' and the value by
`smudge-cache--current-timestamp', though other \"time of day\" values could be
used. See: Info node `(elisp) Time of Day'.")
;; TODO: should this be the simplified JSON created by `smudge-connect-player-status', or the full status?
;;   - I think it should be the full status?

;; NOTE: Using device's ID, so case sensitive comparison is ok. If using
;; device's name, a case insensitive comparison should probably be performed instead.
(defun smudge-cache--hash-test-string-cmp  (a b)
  "Compare strings A and B for hash table."
  ;; `compare-strings' returns t or integer.
  ;; Convert that to a bool t/nil.
  (eq t
      ;; Compare the strings.
      (compare-strings a nil nil
                       b nil nil
                       ;; "Ignore case" boolean.
                       nil)))

;; NOTE: Using device's ID, so case sensitive comparison is ok. If using
;; device's name, a case insensitive comparison should be performed instead.
(defun smudge-cache--hash-test-string-hash (keyword)
  "Get a hashed value for the KEYWORD of the hash table."
  (sxhash-equal keyword))

;; String comparison hash table.
(define-hash-table-test 'smudge-cache--hash-test-string
  'smudge-cache--hash-test-string-cmp
  'smudge-cache--hash-test-string-hash)

(defun smudge-cache--time-keyword (keyword)
  "Return timestamp keyword for KEYWORD's entry in `smudge-cache--status'."
  (intern (concat ":timestamp"
                  (symbol-name keyword))))
;; (smudge-cache--time-keyword :volume)

(defun smudge-cache--put-value (keyword value timestamp hash-table)
  "Put KEYWORD == VALUE into the HASH-TABLE with TIMESTAMP.

Timestamp will be under concatenated keyword: :timestamp + KEYWORD
  example: `:volume' -> `:timestamp:volume'"
  ;; Put the value...
  (puthash keyword
           value
           hash-table)
  ;; ...and put the value's timestamp.
  (puthash (smudge-cache--time-keyword keyword)
           timestamp
           hash-table))

(defun smudge-cache--make-hash-table (keyword value timestamp)
  "Create a new hash table with VALUE and TIMESTAMP entries.

Sets using `smudge-cache--put-value' so that KEYWORD gets corresponding
timestamp keyword.

Adds the hash table to the cache; returns the hash table."
  (let ((cache (make-hash-table :test 'smudge-cache--hash-test-string)))
    (smudge-cache--put-value keyword value timestamp cache)
    cache))

(defun smudge-cache--current-timestamp ()
  "Return current time in the format used in the `smudge-cache--status'."
  ;; Could use `float-time' or `current-time'.
  (float-time))
;; (smudge-cache--current-timestamp)

(defun smudge-cache--get-device-cache (device-id &optional default)
  "Get DEVICE-ID's cached hash table (w/ `:status', `:volume', etc).

Returns DEFAULT if nothing is cached."
  (alist-get device-id
             smudge-cache--status
             default
             nil
             #'string=))
;; (smudge-cache--get-device-cache smudge-cache-test--device-id)

(defun smudge-cache--get (device-id keyword &optional default)
  "Get CACHE's value for DEVICE-ID's KEYWORD.
NOTE: More up-to-date value could be in another keyword. For example, `:volume'
can also exist in the `:status'.

Returns DEFAULT if DEVICE-ID has nothing cached or if KEYWORD is not in
DEVICE-ID's cached JSON hash-table."
  (if-let* ((device-cache (smudge-cache--get-device-cache device-id default))
            (value (gethash keyword device-cache)))
      ;; Found; return the value.
      value
    ;; Not in `smudge-cache--status'; return DEFAULT.
    default))
;; (smudge-cache--get smudge-cache-test--device-id :volume)
;; (smudge-cache--get smudge-cache-test--device-id :volume :dne)
;; (smudge-cache--get smudge-cache-test--device-id (smudge-cache--time-keyword :volume) :also-dne)
;; (smudge-cache--get smudge-cache-test--device-id :status)
;; (smudge-cache--get smudge-cache-test--device-id :status :jeff)
;; (smudge-cache--get smudge-cache-test--device-id (smudge-cache--time-keyword :status))

(defun smudge-cache--set (device-id &rest plist)
  "Update the cache with latest keywords & values from PLIST for DEVICE-ID.

PLIST should be: keyword-0 value-0 ...
Keywords:
  - :volume - integer
  - :status - JSON hash-table

Updates timstamp in cache to now."
  (let ((timestamp    (smudge-cache--current-timestamp))
        (continue     t)
        cache-updated)
    ;; Update all keyword/value pairs provided to the same timestamp.
    (while (and plist continue)
      (if (< (length plist) 2)
          (setq continue nil)

        (let ((device-cache (smudge-cache--get-device-cache device-id))
              (keyword (pop plist))
              (value   (pop plist)))
          ;; Smudge hardly ever raises an error so just ignore invalid keys.
          (when (keywordp keyword)
            ;; Create a new cache.
            (cond ((null smudge-cache--status)
                   (setq smudge-cache--status (list (cons device-id
                                                  (smudge-cache--make-hash-table keyword
                                                                                 value
                                                                                 timestamp)))))

                  ;; Create a new cache value for this DEVICE-ID.
                  ((null device-cache)
                   (push (cons device-id
                               (smudge-cache--make-hash-table keyword
                                                              value
                                                              timestamp))
                         smudge-cache--status))

                  ;; Set/update DEVICE-ID's cache value.
                  (t
                   ;; Set/update the value w/ timestamp.
                   (smudge-cache--put-value keyword
                                            value
                                            timestamp
                                            device-cache)))
            (setq cache-updated t)))))
    ;; Return whether or not we updated a cache value.
    cache-updated))
;; (setq smudge-cache--status nil)
;; smudge-cache--status
;; (smudge-cache--set smudge-cache-test--device-id :status (smudge-cache-test--json-full))
;; (length smudge-cache--status)

(defun smudge-cache--device-get (device-name &optional default)
  "Get device-id for DEVICE-NAME from the devices cache.

Returns DEFAULT if DEVICE-NAME is not cached."
  (alist-get device-name
             smudge-cache--device
             default
             nil
             #'string=))
;; (smudge-cache--device-get smudge-cache-test--device-name)

(defun smudge-cache--device-set (device-name device-id)
  "Save (write/overwrite) DEVICE-NAME and DEVICE-ID to the device cache.

DEVICE-NAME should be a string.

DEVICE-ID should be a string or nil.
If DEVICE-ID is nil, deletes device from devices cache."
  ;; No-op: Remove device from non-existant cache.
  (cond ((and (null smudge-cache--device)
              (null device-id))
         nil)

        ;; Create cache.
        ((null smudge-cache--device)
         (setq smudge-cache--device (list (cons device-name device-id))))

        ;; Delete from cache.
        ((null device-id)
         (setf (alist-get device-name
                          smudge-cache--device
                          nil
                          'remove
                          #'string=)
               nil))

        ;; Update cache.
        (t
         (setf (alist-get device-name
                          smudge-cache--device
                          nil
                          nil
                          #'string=)
               device-id))))

(defun smudge-cache--device-name-from-status (status)
  "Get device's name from JSON hash-table STATUS."
  (gethash 'name (gethash 'device status)))
;; (smudge-cache--device-name-from-status (smudge-cache-test--json-full))

(defun smudge-cache--device-id-from-status (status)
  "Get device's id from JSON hash-table STATUS."
  (gethash 'id (gethash 'device status)))
;; (smudge-cache--device-id-from-status (smudge-cache-test--json-full))

(defun smudge-cache--device-id-from-type (device-type device)
  "Get DEVICE's volume from the cache.

DEVICE-TYPE should be one of these keywords:
  - :id
  - :name

DEVICE should be a string - either the device's ID or the device's name,
depending on DEVICE-TYPE."
  (cond ((eq device-type :id)
         device)
        ((eq device-type :name)
         (smudge-cache--device-get device))
        (t
         nil)))
;; (smudge-cache--device-id-from-type :id smudge-cache-test--device-id)
;; (smudge-cache--device-id-from-type :name smudge-cache-test--device-name)

(defun smudge-cache-update-status (status &optional callback &rest callback-args)
  "Update device cache with STATUS and then invoke CALLBACK.

STATUS should be a JSON hash-table.

CALLBACK should be a function or nil. Calls CALLBACK with CALLBACK-ARGS if
CALLBACK is a function."
  (let* ((device-id (smudge-cache--device-id-from-status status))
         (volume    (gethash 'volume_percent (gethash 'device status)))
         ;; Don't update cached volume if muted.
         (update-volume (and (integerp volume)
                             (> volume 0))))
    ;; Is there enough info to update caches?
    (when (stringp device-id)
      (smudge-cache--device-set (smudge-cache--device-name-from-status status)
                                device-id)
      (smudge-cache--set device-id
                         :status status
                         ;; nil will be ignored as keyword so we provide that when we don't want to update volume.
                         (when update-volume :volume)
                         (when update-volume volume))))

  ;; Invoke callback.
  (when (functionp callback)
    (apply callback callback-args)))

;; (setq smudge-cache--status nil)
;; smudge-cache--status
;; (smudge-cache-update-status (smudge-cache-test--json-full) (lambda () (message "Hello there.")))
;; (length smudge-cache--status)
;; smudge-cache--status

(defun smudge-cache-get-status (device-type device)
  "Get DEVICE's status from the cache.

DEVICE-TYPE should be one of these keywords:
  - :id
  - :name

DEVICE should be a string - either the device's ID or the device's name,
depending on DEVICE-TYPE.

Returns the JSON hash-table status or nil."
  (smudge-cache--get (smudge-cache--device-id-from-type device-type device)
                     :status))
;; (smudge-cache--device-id-from-type :id smudge-cache-test--device-id)
;; (smudge-cache-get-status :id smudge-cache-test--device-id)
;; (smudge-cache-get-status :name smudge-cache-test--device-name)

(defun smudge-cache-update-volume (device-id volume &optional callback &rest callback-args)
  "Update DEVICE-ID in cache with VOLUME and then invoke CALLBACK.

VOLUME should be an integer. It will be clamped to range: [0, 100]

CALLBACK should be a function or nil. Calls CALLBACK with CALLBACK-ARGS if
CALLBACK is a function."
  ;; Is there enough info to update caches?
  (when (and (stringp device-id)
             ;; Don't update cached volume if muted.
             (integerp volume)
             (> volume 0))
    (smudge-cache--set device-id
                       ;; Clamp volume percentage into range [0, 100].
                       :volume (min 100 (max 0 volume))

                       ;; Invoke callback.
                       (when (functionp callback)
                         (apply callback callback-args)))))
;; (smudge-cache--device-id-from-type :id smudge-cache-test--device-id)
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)
;; (smudge-cache-update-volume smudge-cache-test--device-id 55 (message "volume says hello"))

(defun smudge-cache-get-volume (device-type device &optional default)
  "Get DEVICE's volume from the cache.

DEVICE-TYPE should be one of these keywords:
  - :id
  - :name

DEVICE should be a string - either the device's ID or the device's name,
depending on DEVICE-TYPE.

When getting from cache, prefers the most recent non-zero (unmuted) volume.
Returns volume integer or DEFAULT if no cached value exists."
  (let* ((device-id        (smudge-cache--device-id-from-type device-type device))
         (volume-timestamp (smudge-cache--get device-id
                                              (smudge-cache--time-keyword :volume)))
         (volume-cached    (smudge-cache--get device-id
                                              :volume))
         (status-timestamp (smudge-cache--get device-id
                                              (smudge-cache--time-keyword :status)))
         (status           (smudge-cache--get device-id
                                              :status))
         (volume-status    (when status
                             (gethash 'volume_percent (gethash 'device status)))))
    ;; No valid volumes?
    (cond ((and (not (integerp volume-cached))
                (not (integerp volume-status)))
           default)
          ;; One valid volume?
          ((not (integerp volume-cached))
           volume-status)
          ((not (integerp volume-status))
           volume-cached)
          ;; Two valid volumes?
          (t
           ;; Choose the most recent valid time.
           (cond ((and (not (floatp volume-timestamp))
                       (not (floatp status-timestamp)))
                  ;; No valid times - default to cached volume?
                  volume-cached)
                 ;; Use the valid timestamp's volume?
                 ((floatp volume-timestamp)
                  volume-cached)
                 ((floatp status-timestamp)
                  volume-status)
                 ;; Both valid; choose most recent.
                 (t
                  (if (time-less-p volume-timestamp status-timestamp)
                      volume-status
                    volume-cached)))))))
;; (smudge-cache--device-id-from-type :id smudge-cache-test--device-id)
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)

(defun smudge-cache-is-muted (device-type device)
  "Return non-nil if device is muted (cached volume == 0).

DEVICE-TYPE should be one of these keywords:
  - :id
  - :name

DEVICE should be a string - either the device's ID or the device's name,
depending on DEVICE-TYPE.

Will return non-nil if nothing cached."
  (if-let ((volume (smudge-cache-get-volume device-type device)))
      ;; We got something so it /should/ be a volume percentage.
      (= volume 0)
    ;; Don't have any info about the device so...
    ;; assume it's inactive and not playing and thus "muted"?
    t))
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)
;; (smudge-cache-is-muted :id smudge-cache-test--device-id)
;; (smudge-cache-test--force-volume smudge-cache-test--device-id 99)
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)
;; (smudge-cache-is-muted :id smudge-cache-test--device-id)
;; (smudge-cache-test--force-volume smudge-cache-test--device-id 0)
;; (smudge-cache-get-volume :id smudge-cache-test--device-id)
;; (smudge-cache-is-muted :id smudge-cache-test--device-id)


(provide 'smudge-cache)
;;; smudge-cache.el ends here
