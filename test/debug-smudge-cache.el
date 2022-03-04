;;; debug-smudge-cache.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Cole Brown
;;
;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:
;;
;;  Debugging functions for 'smudge-cache.el'.
;;
;;; Code:

(require 'smudge-cache)


(defun debug-smudge-cache-status (arg)
  "Print some status info on caches to the `*Messages*' buffer.

If ARG is non-nil (e.g. interactively with prefix arg), prints out full device
status hash tables."
  (interactive "P")
  (let ((display-full-status? (not (null arg)))
        (line-width     90)
        (name-width     30) ;; Just "something big enough".
        (id-width       40) ;; They seem to always be 40 characters wide?
        (data-key-width 20) ;; `:status', `:volume', `:timestamp:volume', etc.
        (line-00        "│    ")
        (section-01     "├──┬ ")
        (line-01        "│  │ ")
        (section-02     "│  ├──┬ ")
        (section-02-too "│  │  ├ ")
        ;; (line-02        "│  │  │ ")
        (section-03     "│  │  ├──┬ ")
        (section-03-too "│  │  │  ├ ")
        (section-04     "│  │  │  ├──┬ ")
        (section-04-too "│  │  │  │  ├ ")
        lines)
    ;;------------------------------
    ;; Title Line
    ;;------------------------------
    (let ((title "┤ Smudge Cache ├"))
      (push (format "%s%s"
                    title
                    (make-string (- line-width (length title)) ?─ :multibyte))
            lines))

    ;;------------------------------
    ;; Device Cache:
    ;;------------------------------
    (push (format "%s%s"
                  section-01
                  "Devices:")
          lines)
    (if smudge-cache--device
        (let* (;; Left-align string fields for the title.
               (fmt-title   (concat "%s"
                                    "    %-"
                                    (number-to-string name-width)
                                    "s   %-"
                                    (number-to-string id-width)
                                    "s"))
               ;; Only left-align the id.
               (fmt-device  (concat "%s"
                                    "  - %"
                                    (number-to-string name-width)
                                    "s : %-"
                                    (number-to-string id-width)
                                    "s")))
          (push (format fmt-title
                        line-01
                        ;; Center by adding half the spaces before or after, depending on left/right alignment of the field.
                        (concat (make-string (/ (- name-width (length "Device Name")) 2)
                                             ?\s)
                                "Device Name")
                        (concat (make-string (/ (- name-width (length "Device ID")) 2)
                                             ?\s)
                                "Device ID"))
                lines)
          (dolist (entry smudge-cache--device)
            (push (format fmt-device
                          line-01
                          (car entry)
                          (cdr entry))
                  lines)))

      (push (format "%s  - none" line-01) lines))

    ;;------------------------------
    ;; Data Cache:
    ;;------------------------------
    (push line-00 lines)
    (push (format "%s%s"
                  section-01
                  "Data:")
          lines)

    ;; Formats
    (let ((fmt-device-name (concat "%s"
                              "name: %-"
                              (number-to-string name-width)
                              "s"))
          (fmt-device-id (concat "%s"
                                 "  id: %-"
                                 (number-to-string id-width)
                                 "s"))
          (fmt-data (concat "%s"
                            " %-"
                            (number-to-string data-key-width)
                            "s : %s"))
          (fmt-status (concat "%s %-10s : %s"))
          (first-device t))

      ;; Print each device's cached data.
      (dolist (cache smudge-cache--data)
        ;; Separate subsequent devices by a blank line.
        (if first-device
            (setq first-device nil)
          (push line-01 lines))

        ;; Print cache.
        (let* ((device-id    (car cache))
               (device-cache (cdr cache))
               ;; Grab the name for human readability.
               (device-name  (smudge-cache--device-name-from-type :id device-id))
               ;; We'll deal with this specially after the general print cached data loop.
               (status       (smudge-cache--get device-id :status))
               (first-line t))

          ;; Print Name & ID
          (push (format fmt-device-name
                        section-02
                        device-name)
                lines)
          (push (format fmt-device-id
                        section-02-too
                        device-id)
                lines)

          ;; Print cached data.
          (dolist (entry device-cache)
            (let ((key   (car entry))
                  (value (cdr entry)))
              (if (eq key :status)
                  ;; Print some info *about* status, not status itself.
                  (progn
                    (push (format fmt-data
                                  (if first-line section-03 section-03-too)
                                  key
                                  ;; Summary of status.
                                  (cond ((null status)
                                         status)
                                        ((and (hash-table-p status)
                                              (hash-table-empty-p status))
                                         "<hash-table:EMPTY>")
                                        ((hash-table-p status)
                                         "<hash-table:w/data>")
                                        (t
                                         (format "<invalid:%S>" (type-of status)))))
                          lines)

                    ;; Pick & choose from status.
                    (let ((status-device (gethash 'device status))
                          (status-track  (gethash 'item status))
                          (first-status t))
                      (when status-device
                        (when (gethash 'is_active status-device)
                          (push (format fmt-status
                                        (if first-status section-04 section-04-too)
                                        "active?"
                                        (gethash 'is_active status-device))
                                lines)
                          (setq first-status nil))
                        (when (gethash 'volume_percent status-device)
                          (push (format fmt-status
                                        (if first-status section-04 section-04-too)
                                        "volume"
                                        (gethash 'volume_percent status-device))
                                lines)
                          (setq first-status nil)))
                      (when status-track
                        (when (gethash 'name (car (gethash 'artists status-track)))
                          (push (format fmt-status
                                        (if first-status section-04 section-04-too)
                                        "artist"
                                        (gethash 'name (car (gethash 'artists status-track))))
                                lines)
                          (setq first-status nil))
                        (when (gethash 'name status-track)
                          (push (format fmt-status
                                        (if first-status section-04 section-04-too)
                                        "track"
                                        (gethash 'name status-track))
                                lines)
                          (setq first-status nil))
                        (when (gethash 'is_playing status)
                          (push (format fmt-status
                                        (if first-status section-04 section-04-too)
                                        "playing"
                                        (gethash 'is_playing status))
                                lines)
                          (setq first-status nil)))))

                ;; Else print out the value.
                (push (format fmt-data
                              (if first-line section-03 section-03-too)
                              key
                              value)
                      lines)
                (setq first-line nil))))

          ;; Print out status value (hash table)?
          (when display-full-status?
            (push (format fmt-data
                          section-03-too
                          "----Status-Cached---"
                          status)
                  lines)))))

    ;;------------------------------
    ;; Output
    ;;------------------------------
    (message (mapconcat #'identity
                        (nreverse lines)
                        "\n"))))

(provide 'debug-smudge-cache)
;;; debug-smudge-cache.el ends here
