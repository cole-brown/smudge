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

(defvar smudge-cache--data nil
  "Alist of Device IDs to 'device data' (w/ timestamps).

'Device data' is an alist of keywords to values:
  - `:volume'  - integer
  - `:status'  - JSON hash-table player status
  - timestamps - twinned to one of the above keywords
    - keys:   keywords from `smudge-cache--time-keyword'
    - values: floats from `smudge-cache--current-timestamp'
      - Other \"time of day\" values could be used.
        See: Info node `(elisp) Time of Day'.")

(defun smudge-cache--time-keyword (keyword)
  "Return timestamp keyword for KEYWORD's entry in `smudge-cache--data'."
  (intern (concat ":timestamp"
                  (symbol-name keyword))))

(defun smudge-cache--current-timestamp ()
  "Return current time in the format used in the `smudge-cache--data'."
  ;; Could use `float-time' or `current-time'.
  (float-time))

(defun smudge-cache--get-data (device-id &optional default)
  "Get DEVICE-ID's cached data (w/ `:status', `:volume', etc).

Returns DEFAULT if nothing is cached."
  (alist-get device-id
             smudge-cache--data
             default
             nil
             #'string=))

(defun smudge-cache--set-values (keyword value timestamp device-data-cache)
  "Put KEYWORD == VALUE into the DEVICE-DATA-CACHE with TIMESTAMP.

Timestamp's keyword will be from: (smudge-cache--time-keyword keyword)
  example: `:volume' -> `:timestamp:volume'

Returns updated device data cache, which may not be DEVICE-DATA-CACHE anymore."
  ;; Nothing to do? No cache and no value to set.
  (cond ((and (null device-data-cache)
              (null value))
         nil)

        ;; NOTE: Always act on both KEYWORD and its timestamp keyword!

        ;; NOTE: The device data alist is keywords, so don't use `string=' for the test function.

        ;; Create cache.
        ((null device-data-cache)
         (setq device-data-cache (list (cons keyword value)
                                       (cons (smudge-cache--time-keyword keyword)
                                             timestamp))))

        ;; Delete from cache.
        ((null value)
         (setf (alist-get keyword
                          device-data-cache
                          nil
                          'remove)
               nil)
         (setf (alist-get (smudge-cache--time-keyword keyword)
                          device-data-cache
                          nil
                          'remove)
               nil))

        ;; Update cache.
        (t
         (setf (alist-get keyword
                          device-data-cache)
               value)
         (setf (alist-get (smudge-cache--time-keyword keyword)
                          device-data-cache)
               timestamp)))

  ;; And return updated DEVICE-DATA-CACHE, which could be a new alist...
  device-data-cache)

(defun smudge-cache--get (device-id keyword &optional default)
  "Get CACHE's value for DEVICE-ID's KEYWORD.
NOTE: More up-to-date value could be in another keyword. For example, `:volume'
can also exist in the `:status'.

Returns DEFAULT if DEVICE-ID has nothing cached or if KEYWORD is not in
DEVICE-ID's cached JSON hash-table."
  (let ((cached-assoc (assoc keyword
                             (smudge-cache--get-data device-id default))))
    (if (null cached-assoc)
        ;; No keyword found in cache.
        default
      ;; Keyword was found; return value (even if nil).
      (cdr cached-assoc))))

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

        (let ((device-cache (smudge-cache--get-data device-id))
              (keyword (pop plist))
              (value   (pop plist)))
          ;; Smudge hardly ever raises an error so just ignore invalid keys.
          (when (keywordp keyword)
            ;; Updated `device-id' cache to new values.
            (setf (alist-get device-id
                             smudge-cache--data
                             nil
                             nil
                             #'string=)
                  (smudge-cache--set-values keyword
                                            value
                                            timestamp
                                            device-cache))
            (setq cache-updated t)))))

    ;; Return whether or not we updated a cache value.
    cache-updated))

(defun smudge-cache--device-get (device-name &optional default)
  "Get DEVICE-ID for DEVICE-NAME from the devices cache.

Returns DEFAULT if DEVICE-NAME is not cached."
  (alist-get device-name
             smudge-cache--device
             default
             nil
             #'string=))

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

(defun smudge-cache--device-id-from-status (status)
  "Get device's id from JSON hash-table STATUS."
  (gethash 'id (gethash 'device status)))

(defun smudge-cache--device-id-from-type (device-type device)
  "Get DEVICE's device-id string based on DEVICE-TYPE.

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

(defun smudge-cache-get-timestamp (device-type device keyword &optional default)
  "Get the timestamp value from the cache for the DEVICE's KEYWORD entry.

DEVICE-TYPE should be one of these keywords:
  - :id
  - :name

DEVICE should be a string - either the device's ID or the device's name,
depending on DEVICE-TYPE.

KEYWORD should be:
  - :status
  - :volume

Returns:
  - raw timestamp value (float; see `smudge-cache--current-timestamp')
  - DEFAULT if nothing found in cache"
  (smudge-cache--get (smudge-cache--device-id-from-type device-type device)
                     (smudge-cache--time-keyword keyword)
                     default))

(defun smudge-cache-update-status (status &optional callback &rest callback-args)
  "Update device's `:status' cache with STATUS and then invoke CALLBACK.

Will also update other cache keywords with values from the status as applicable.
For example, updates `:volume' with device's volume from STATUS.

STATUS should be a JSON hash-table.

CALLBACK should be a function or nil.

CALLBACK functions should have params for STATUS and any CALLBACK-ARGS sent.
So it should, at a minimum, take the STATUS param.
Examples:
  (defun example-0 (status)
    ...)
  (defun example-1 (status &rest args)
    ...)
  (defun example-2 (status arg0 &optional arg1)
    ...)"
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

  ;; Invoke callback w/ status and its args.
  (when (functionp callback)
    (apply callback status callback-args)))


(defun smudge-cache-lambda (&optional callback)
  "Return a lambda that will: take status, update cache, and call CALLBACK."
  (lambda (status)
    "Update smudge cache with STATUS, then calls callback with same STATUS."
    (smudge-cache-update-status status callback)))


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
                  ;; No valid times - default to... `:volume' cached value?
                  ;; NOTE: Could change to default to the status if that seems more reliable but this seems simpler.
                  volume-cached)
                 ;; Use the valid timestamp's volume?
                 ((not (floatp volume-timestamp))
                  volume-status)
                 ((not (floatp status-timestamp))
                  volume-cached)
                 ;; Both valid; choose most recent.
                 (t
                  (if (time-less-p volume-timestamp status-timestamp)
                      volume-status
                    volume-cached)))))))

(defun smudge-cache-is-muted (device-type device)
  "Return non-nil if device is muted (cached volume == 0).

DEVICE-TYPE should be one of these keywords:
  - :id
  - :name

DEVICE should be a string - either the device's ID or the device's name,
depending on DEVICE-TYPE.

Will return non-nil if nothing cached."
  (let ((volume (smudge-cache-get-volume device-type device)))
    (if (integerp volume)
        ;; We got something so it /should/ be a volume percentage.
        (= volume 0)
      ;; Don't have any info about the device so...
      ;; assume it's inactive and not playing and thus "muted"?
      t)))

(provide 'smudge-cache)
;;; smudge-cache.el ends here
