;;; musicbrainz-interactive.el --- Interactive commands for MusicBrainz related things  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Oliwier Czerwiński

;; Author:  Oliwier Czerwiński <oliwier.czerwi@proton.me>
;; Keywords: data, convenience, music
;; Version: 20260802
;; Package-Requires: ((emacs "28.1") (musicbrainz "0.1"))
;; URL: https://github.com/zzkt/metabrainz

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Interactive commands that allow for searching MusicBrainz entities.
;; Also includes tools for making custom search functions.
;; The code is written in a way that allows support from packages like
;; Embark or Marginalia.

;;; Code:

(require 'musicbrainz)

(defvar musicbrainz-interactive-annotation-format-function
  #'musicbrainz-interactive-annotation-format
  "The formatting function to be used for annotations.")

(defvar musicbrainz-interactive-annotation-annotate-function
  #'musicbrainz-interactive-annotation-annotate
  "The annotating function to be used for annotations.")

(defvar musicbrainz-interactive-annotation-history nil
  "The list of recently chosen annotations.")

(defvar musicbrainz-interactive-annotation-query-history nil
  "The list of recent annotation queries.")

(defvar musicbrainz-interactive-area-format-function
  #'musicbrainz-interactive-area-format
  "The formatting function to be used for areas.")

(defvar musicbrainz-interactive-area-annotate-function
  #'musicbrainz-interactive-area-annotate
  "The annotating function to be used for areas.")

(defvar musicbrainz-interactive-area-history nil
  "The list of recently chosen areas.")

(defvar musicbrainz-interactive-area-query-history nil
  "The list of recent area queries.")

(defvar musicbrainz-interactive-artist-format-function
  #'musicbrainz-interactive-artist-format
  "The formatting function to be used for artists.")

(defvar musicbrainz-interactive-artist-annotate-function
  #'musicbrainz-interactive-artist-annotate
  "The annotating function to be used for artists.")

(defvar musicbrainz-interactive-artist-history nil
  "The list of recently chosen artists.")

(defvar musicbrainz-interactive-artist-query-history nil
  "The list of recent artist queries.")

(defvar musicbrainz-interactive-cdstub-format-function
  #'musicbrainz-interactive-cdstub-format
  "The formatting function to be used for CD stubs.")

(defvar musicbrainz-interactive-cdstub-annotate-function
  #'musicbrainz-interactive-cdstub-annotate
  "The annotating function to be used for CD stubs.")

(defvar musicbrainz-interactive-cdstub-history nil
  "The list of recently chosen cdstubs.")

(defvar musicbrainz-interactive-cdstub-query-history nil
  "The list of recent cdstub queries.")

(defvar musicbrainz-interactive-event-format-function
  #'musicbrainz-interactive-event-format
  "The formatting function to be used for events.")

(defvar musicbrainz-interactive-event-annotate-function
  #'musicbrainz-interactive-event-annotate
  "The annotating function to be used for events.")

(defvar musicbrainz-interactive-event-history nil
  "The list of recently chosen events.")

(defvar musicbrainz-interactive-event-query-history nil
  "The list of recent event queries.")

(defvar musicbrainz-interactive-instrument-format-function
  #'musicbrainz-interactive-instrument-format
  "The formatting function to be used for instruments.")

(defvar musicbrainz-interactive-instrument-annotate-function
  #'musicbrainz-interactive-instrument-annotate
  "The annotating function to be used for instruments.")

(defvar musicbrainz-interactive-instrument-history nil
  "The list of recently chosen instruments.")

(defvar musicbrainz-interactive-instrument-query-history nil
  "The list of recent instrument queries.")

(defvar musicbrainz-interactive-label-format-function
  #'musicbrainz-interactive-label-format
  "The formatting function to be used for labels.")

(defvar musicbrainz-interactive-label-annotate-function
  #'musicbrainz-interactive-label-annotate
  "The annotating function to be used for labels.")

(defvar musicbrainz-interactive-label-history nil
  "The list of recently chosen labels.")

(defvar musicbrainz-interactive-label-query-history nil
  "The list of recent label queries.")

(defvar musicbrainz-interactive-place-format-function
  #'musicbrainz-interactive-place-format
  "The formatting function to be used for places.")

(defvar musicbrainz-interactive-place-annotate-function
  #'musicbrainz-interactive-place-annotate
  "The annotating function to be used for places.")

(defvar musicbrainz-interactive-place-history nil
  "The list of recently chosen places.")

(defvar musicbrainz-interactive-place-query-history nil
  "The list of recent place queries.")

(defvar musicbrainz-interactive-recording-format-function
  #'musicbrainz-interactive-recording-format
  "The formatting function to be used for recordings.")

(defvar musicbrainz-interactive-recording-annotate-function
  #'musicbrainz-interactive-recording-annotate
  "The annotating function to be used for recordings.")

(defvar musicbrainz-interactive-recording-history nil
  "The list of recently chosen recordings.")

(defvar musicbrainz-interactive-recording-query-history nil
  "The list of recent recording queries.")

(defvar musicbrainz-interactive-release-format-function
  #'musicbrainz-interactive-release-format
  "The formatting function to be used for releases.")

(defvar musicbrainz-interactive-release-annotate-function
  #'musicbrainz-interactive-release-annotate
  "The annotating function to be used for releases.")

(defvar musicbrainz-interactive-release-history nil
  "The list of recently chosen releases.")

(defvar musicbrainz-interactive-release-query-history nil
  "The list of recent release queries.")

(defvar musicbrainz-interactive-release-group-format-function
  #'musicbrainz-interactive-release-group-format
  "The formatting function to be used for release groups.")

(defvar musicbrainz-interactive-release-group-annotate-function
  #'musicbrainz-interactive-release-group-annotate
  "The annotating function to be used for release groups.")

(defvar musicbrainz-interactive-release-group-history nil
  "The list of recently chosen release groups.")

(defvar musicbrainz-interactive-release-group-query-history nil
  "The list of recent release group queries.")

(defvar musicbrainz-interactive-series-format-function
  #'musicbrainz-interactive-series-format
  "The formatting function to be used for series.")

(defvar musicbrainz-interactive-series-annotate-function
  #'musicbrainz-interactive-series-annotate
  "The annotating function to be used for series.")

(defvar musicbrainz-interactive-series-history nil
  "The list of recently chosen series.")

(defvar musicbrainz-interactive-series-query-history nil
  "The list of recent serie queries.")

(defvar musicbrainz-interactive-tag-format-function
  #'musicbrainz-interactive-tag-format
  "The formatting function to be used for tags.")

(defvar musicbrainz-interactive-tag-annotate-function
  #'musicbrainz-interactive-tag-annotate
  "The annotating function to be used for tags.")

(defvar musicbrainz-interactive-tag-history nil
  "The list of recently chosen tags.")

(defvar musicbrainz-interactive-tag-query-history nil
  "The list of recent tag queries.")

(defvar musicbrainz-interactive-url-format-function
  #'musicbrainz-interactive-url-format
  "The formatting function to be used for URLs.")

(defvar musicbrainz-interactive-url-annotate-function
  #'musicbrainz-interactive-url-annotate
  "The annotating function to be used for URLs.")

(defvar musicbrainz-interactive-url-history nil
  "The list of recently chosen urls.")

(defvar musicbrainz-interactive-url-query-history nil
  "The list of recent url queries.")

(defvar musicbrainz-interactive-work-format-function
  #'musicbrainz-interactive-work-format
  "The formatting function to be used for works.")

(defvar musicbrainz-interactive-work-annotate-function
  #'musicbrainz-interactive-work-annotate
  "The annotating function to be used for works.")

(defvar musicbrainz-interactive-work-history nil
  "The list of recently chosen works.")

(defvar musicbrainz-interactive-work-query-history nil
  "The list of recent work queries.")

(defvar musicbrainz-interactive-alias-default-locale "en"
  "The default locale for retrieving aliases.")

;;; Generic functions

(defun musicbrainz-interactive-prepare-for-completion
    (response type format-function)
  "Prepare the RESPONSE data to be used for `completing-read'.
TYPE is what to extract (artists, releases etc.).
FORMAT-FUNCTION is applied to every result from the extracted RESPONSE
and makes a name that will be used in `completing-read'.
Data about entity is attached as a text property and as a cdr of the
cons to allow filling `minibuffer-completion-table' with data and allow
other packages (embark for example) to have access to the data."
  (let ((data (append (alist-get type response) nil)))
    (mapcar
     (lambda (item)
       (propertize (funcall format-function item) 'data item))
     data)))

(defun musicbrainz-interactive--annotation-value (spec item)
  "Return annotation value from SPEC using ITEM.

SPEC may be:
- a symbol      -> looked up with `alist-get'
- a function    -> called with ITEM"
  (cond
   ((symbolp spec)
    (alist-get spec item))
   ((functionp spec)
    (funcall spec item))
   (t (error "Invalid annotation spec: %S" spec))))

(defun musicbrainz-interactive--annotation-values (specs item)
  "Resolve SPECS against ITEM and return non-nil string values."
  (delq nil
        (mapcar
         (lambda (spec)
           (let ((value (musicbrainz-interactive--annotation-value
                         spec item)))
             (cond
              ((null value) nil)
              ;; Convert vectors/lists/etc. into printed strings.
              ((stringp value) value)
              (t (format "%s" value)))))
         specs)))

(defun musicbrainz-interactive-get-alias (item &optional locale)
  "Retrieve primary alias from an ITEM with specified LOCALE.
The primary alias is preffered.
The default value of LOCALE is in
 `musicbrainz-interactive-alias-default-locale'."
  (let ((locale (or locale musicbrainz-interactive-alias-default-locale))
        (aliases (alist-get 'aliases item)))
    (when aliases
      (alist-get 'name
                 (or
                  (cl-find-if
                   (lambda (alias)
                     (and (equal (alist-get 'locale alias) locale)
                          (equal (alist-get 'primary alias) t)))
                   (append aliases nil))
                  (cl-find-if
                   (lambda (alias)
                     (equal (alist-get 'locale alias) locale))
                   (append aliases nil)))))))

(defmacro musicbrainz-interactive-annotate (&rest args)
  "Build annotation string from ITEM using ARGS (DATA and ITEMS specs).

Example:

  (musicbrainz-interactive-annotate
   :data item
   :items (sort-name
           (lambda (item)
             (alist-get \\='gender item))
           disambiguation
           (lambda (item)
             (alist-get \\='tags item)))"
  (let ((data  (plist-get args :data))
        (items (plist-get args :items)))
    `(let* ((values
             (musicbrainz-interactive--annotation-values
              ',items ,data)))
      (if values
          (format " (%s)" (string-join values ", "))
        ""))))

(defun musicbrainz-interactive-open-mbid (mbid)
  "Open the MBID in a MusicBrainz webpage."
  (interactive "sMBID: ")
  (browse-url (format "https://musicbrainz.org/mbid/%s" mbid)))

(defun musicbrainz-interactive-search
    (entity
     query
     collection-symbol
     completion-prompt
     format-function
     annotate-function
     result-id-function
     open-function
     &optional limit offset)
  "Generic MusicBrainz interactive search function.
ENTITY is the entity the function will search for.
QUERY is the query used for the search.
COLLECTION-SYMBOL is used for retrieving the results from API response.
FORMAT-FUNCTION is described in
`musicbrainz-interactive-prepare-for-completion'.
COMPLETION-PROMPT is used when choosing the candidate.
ANNOTATE-FUNCTION is used for making annotations for all candidates.
RESULT-ID-FUNCTION is used to retrieve the identifier for the candidate
\(not all MusicBrainz entities use MBID for identification).
OPEN-FUNCTION takes RESULT-ID-FUNCTION's returned value as an argument
and should open it in a web browser for example.
LIMIT specifies the amount of search results (25 by default).
OFFSET defines an offset from which to start getting results (by default
there is none)."
  (let* ((limit (or limit 25))
         (data
          (musicbrainz-interactive-prepare-for-completion
           (musicbrainz-search entity query limit offset)
           collection-symbol
           format-function))
         (completion-extra-properties
          `(:category ,(intern (format "musicbrainz-interactive-%s"
                                entity))
            :annotation-function ,annotate-function))
         (choice
          (completing-read
           completion-prompt
           data nil t nil
           (intern (format "musicbrainz-interactive-%s-history"
                           entity))))
         (item
          (get-text-property
           0 'data
           (cl-find choice data :test #'equal))))
    (funcall open-function
             (funcall result-id-function item))))

;;; Annotation functions

(defun musicbrainz-interactive-annotation-format (annotation)
  "Formatting function for ANNOTATION to be used in `completing-read'."
  (format "[%s] %s"
          (substring (alist-get 'entity annotation) 0 8)
          (alist-get 'text annotation)))

(defun musicbrainz-interactive-annotation-annotate (annotation)
  "Annotate the ANNOTATION."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data annotation)
   :items (type name)))

;;;###autoload
(defun musicbrainz-interactive-search-annotation
    (query &optional limit offset)
  "Entery a QUERY to search for an annotation.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Annotation: " nil
                 'musicbrainz-interactive-annotation-query-history)))
  (musicbrainz-interactive-search
   "annotation"
   query
   'annotations
   "Annotation: "
   musicbrainz-interactive-annotation-format-function
   musicbrainz-interactive-annotation-annotate-function
   (lambda (item)
     (alist-get 'entity item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Area functions

(defun musicbrainz-interactive-area-format (area)
  "Formatting function for AREA to be used in `completing-read'."
  (format "[%s] %s"
          (substring (alist-get 'id area) 0 8)
          (alist-get 'name area)))

(defun musicbrainz-interactive-area-annotate (area)
  "Annotate the AREA."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data area)
   :items (type)))

;;;###autoload
(defun musicbrainz-interactive-search-area
    (query &optional limit offset)
  "Enter a QUERY to search for an area.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Area: " nil
                 'musicbrainz-interactive-area-query-history)))

  (musicbrainz-interactive-search
   "area"
   query
   'areas
   "Area: "
   musicbrainz-interactive-area-format-function
   musicbrainz-interactive-area-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Artist functions

(defun musicbrainz-interactive-artist-format (artist)
  "Formatting function for ARTIST to be used in `completing-read'."
  (concat
   (format "[%s] %s"
           (substring (alist-get 'id artist) 0 8)
           (alist-get 'name artist))
   (if-let ((alias (musicbrainz-interactive-get-alias artist)))
       (format " (%s)" alias)
     (unless (equal (alist-get 'sort-name artist)
                    (alist-get 'name artist))
       (format " (%s)" (alist-get 'sort-name artist))))))

(defun musicbrainz-interactive-artist-annotate (artist)
  "Annotate the ARTIST."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data artist)
   :items (disambiguation)))

;;;###autoload
(defun musicbrainz-interactive-search-artist
    (query &optional limit offset)
  "Enter a QUERY to search for an artist.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Artist: " nil
                 'musicbrainz-interactive-artist-query-history)))
  (musicbrainz-interactive-search
   "artist"
   query
   'artists
   "Artist: "
   musicbrainz-interactive-artist-format-function
   musicbrainz-interactive-artist-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; CD stub functions

(defun musicbrainz-interactive-cdstub-format (cdstub)
  "Formatting function for CDSTUB to be used in `completing-read'."
  (format "%s - %s - %s"
          (alist-get 'id cdstub)
          (alist-get 'title cdstub)
          (alist-get 'artist cdstub)))

(defun musicbrainz-interactive-cdstub-annotate (cdstub)
  "Annotate the CDSTUB."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data cdstub)
   :items (disambiguation)))

(defun musicbrainz-interactive-open-cdstub (cdstub)
  "Open the CDSTUB in a MusicBrainz webpage."
  (interactive
   (list
    (read-string "CD stub: " nil
                 'musicbrainz-interactive-cdstub-query-history)))
  (browse-url (format "https://musicbrainz.org/cdstub/%s" cdstub)))

;;;###autoload
(defun musicbrainz-interactive-search-cdstub
    (query &optional limit offset)
  "Enter a QUERY to search for a CD stub.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "CD stub: " nil
                 'musicbrainz-interactive-cd-stub-query-history)))
  (musicbrainz-interactive-search
   "cdstub"
   query
   'cdstubs
   "CD stub: "
   musicbrainz-interactive-cdstub-format-function
   musicbrainz-interactive-cdstub-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-cdstub
   limit
   offset))

;;; Event functions

(defun musicbrainz-interactive-event-format (event)
  "Formatting function for EVENT to be used in `completing-read'."
  (format "[%s] %s"
          (substring (alist-get 'id event) 0 8)
          (alist-get 'name event)))

(defun musicbrainz-interactive-event-annotate (event)
  "Annotate the EVENT."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data event)
   :items (disambiguation
           type
           (lambda (item)
             (let* ((life-span (alist-get 'life-span item))
                    (begin (alist-get 'begin life-span))
                    (end (alist-get 'end life-span)))
               (cond
                ((equal begin end)
                 begin)
                ((and begin end)
                 (format "%s - %s" begin end))
                (t
                 (or begin end))))))))

;;;###autoload
(defun musicbrainz-interactive-search-event
    (query &optional limit offset)
  "Enter a QUERY to search for an event.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Event: " nil
                 'musicbrainz-interactive-event-query-history)))
  (musicbrainz-interactive-search
   "event"
   query
   'events
   "Event: "
   musicbrainz-interactive-event-format-function
   musicbrainz-interactive-event-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Instrument functions

(defun musicbrainz-interactive-instrument-format (instrument)
  "Formatting function for INSTRUMENT to be used in `completing-read'."
  (format "[%s] %s"
          (substring (alist-get 'id instrument) 0 8)
          (alist-get 'name instrument)))

(defun musicbrainz-interactive-instrument-annotate (instrument)
  "Annotate the INSTRUMENT."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data instrument)
   :items (type)))

;;;###autoload
(defun musicbrainz-interactive-search-instrument
    (query &optional limit offset)
  "Enter a QUERY to search for an instrument.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Instrument: " nil
                 'musicbrainz-interactive-instrument-query-history)))
  (musicbrainz-interactive-search
   "instrument"
   query
   'instruments
   "Instrument: "
   musicbrainz-interactive-instrument-format-function
   musicbrainz-interactive-instrument-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Label functions

(defun musicbrainz-interactive-label-format (label)
  "Formatting function for LABEL to be used in `completing-read'."
  (format "[%s] %s%s"
          (substring (alist-get 'id label) 0 8)
          (alist-get 'name label)
          (if-let ((alias (musicbrainz-interactive-get-alias label)))
              (format " (%s)" alias)
            "")))

(defun musicbrainz-interactive-label-annotate (label)
  "Annotate the LABEL."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data label)
   :items ((lambda (item)
             (if (equal (alist-get 'name item)
                        (alist-get 'sort-name item))
                 nil
               (alist-get 'sort-name item)))
           disambiguation)))

;;;###autoload
(defun musicbrainz-interactive-search-label
    (query &optional limit offset)
  "Enter a QUERY to search for a label.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Label: " nil
                 'musicbrainz-interactive-label-query-history)))
  (musicbrainz-interactive-search
   "label"
   query
   'labels
   "Label: "
   musicbrainz-interactive-label-format-function
   musicbrainz-interactive-label-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Place functions

(defun musicbrainz-interactive-place-format (place)
  "Formatting function for PLACE to be used in `completing-read'."
  (format "[%s] %s%s"
          (substring (alist-get 'id place) 0 8)
          (alist-get 'name place)
          (if-let ((alias (musicbrainz-interactive-get-alias place)))
              (format " (%s)" alias)
            "")))

(defun musicbrainz-interactive-place-annotate (place)
  "Annotate the PLACE."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data place)
   :items (type
           (lambda (item)
             (if (equal (alist-get 'name item)
                        (alist-get 'disambiguation item))
                 nil
               (alist-get 'disambiguation item))))))

;;;###autoload
(defun musicbrainz-interactive-search-place
    (query &optional limit offset)
  "Enter a QUERY to search for a place.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Place: " nil
                 'musicbrainz-interactive-place-query-history)))
  (musicbrainz-interactive-search
   "place"
   query
   'places
   "Place: "
   musicbrainz-interactive-place-format-function
   musicbrainz-interactive-place-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Recording functions

(defun musicbrainz-interactive-recording-format (recording)
  "Formatting function for RECORDING to be used in `completing-read'."
  (format "[%s] %s - %s"
          (substring (alist-get 'id recording) 0 8)
          (alist-get 'title recording)
          (mapconcat
           (lambda (artist)
             (concat (alist-get 'name artist)
                     (alist-get 'joinphrase artist)))
           (append (alist-get 'artist-credit recording) nil))))

(defun musicbrainz-interactive-recording-annotate (recording)
  "Annotate the RECORDING."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data recording)
   :items ((lambda (item)
             (when (assoc 'length item)
               (let* ((total-seconds (/ (alist-get 'length item)
                                        1000))
                      (minutes (/ total-seconds 60))
                      (seconds (% total-seconds 60)))
                 (format "%d:%02d" minutes seconds))))
           disambiguation)))

;;;###autoload
(defun musicbrainz-interactive-search-recording
    (query &optional limit offset)
  "Enter a QUERY to search for a recording.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Recording: " nil
                 'musicbrainz-interactive-recording-query-history)))
  (musicbrainz-interactive-search
   "recording"
   query
   'recordings
   "Recording: "
   musicbrainz-interactive-recording-format-function
   musicbrainz-interactive-recording-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Release functions

(defun musicbrainz-interactive-release-format (release)
  "Formatting function for RELEASE to be used in `completing-read'."
  (format "[%s] %s - %s"
          (substring (alist-get 'id release) 0 8)
          (alist-get 'title release)
          (mapconcat
           (lambda (artist)
             (concat (alist-get 'name artist)
                     (alist-get 'joinphrase artist)))
           (append (alist-get 'artist-credit release) nil))))

(defun musicbrainz-interactive-release-annotate (release)
  "Annotate the RELEASE."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data release)
   :items (status date disambiguation)))

;;;###autoload
(defun musicbrainz-interactive-search-release
    (query &optional limit offset)
  "Enter a QUERY to search for a release.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Release: " nil
                 'musicbrainz-interactive-release-query-history)))
  (musicbrainz-interactive-search
   "release"
   query
   'releases
   "Release: "
   musicbrainz-interactive-release-format-function
   musicbrainz-interactive-release-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Release group functions

(defun musicbrainz-interactive-release-group-format (release-group)
  "Formatting function for RELEASE-GROUP to be used in `completing-read'."
  (format "[%s] %s - %s"
          (substring (alist-get 'id release-group) 0 8)
          (alist-get 'title release-group)
          (mapconcat
           (lambda (artist)
             (concat (alist-get 'name artist)
                     (alist-get 'joinphrase artist)))
           (append (alist-get 'artist-credit release-group) nil))))

(defun musicbrainz-interactive-release-group-annotate
    (release-group)
  "Annotate the RELEASE-GROUP."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data release-group)
   :items (primary-type
           (lambda (item)
             (when (assoc 'first-release-date item)
               (format "released: %s" (alist-get
                                       'first-release-date
                                       item))))
           disambiguation)))

;;;###autoload
(defun musicbrainz-interactive-search-release-group
    (query &optional limit offset)
  "Enter a QUERY to search for a release group.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Release group: " nil
                 'musicbrainz-interactive-release-group-query-history)))
  (musicbrainz-interactive-search
   "release-group"
   query
   'release-groups
   "Release group: "
   musicbrainz-interactive-release-group-format-function
   musicbrainz-interactive-release-group-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Series functions

(defun musicbrainz-interactive-series-format (series)
  "Formatting function for SERIES to be used in `completing-read'."
  (format "[%s] %s%s"
          (substring (alist-get 'id series) 0 8)
          (alist-get 'name series)
          (if-let ((alias (musicbrainz-interactive-get-alias series)))
              (format " (%s)" alias)
            "")))

(defun musicbrainz-interactive-series-annotate
    (series)
  "Annotate the SERIES."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data series)
   :items (type disambiguation)))

;;;###autoload
(defun musicbrainz-interactive-search-series
    (query &optional limit offset)
  "Enter a QUERY to search for a series.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Series: " nil
                 'musicbrainz-interactive-series-query-history)))
  (musicbrainz-interactive-search
   "series"
   query
   'series
   "Series: "
   musicbrainz-interactive-series-format-function
   musicbrainz-interactive-series-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Tag functions

(defun musicbrainz-interactive-tag-format (tag)
  "Formatting function for TAG to be used in `completing-read'."
  (alist-get 'name tag))

(defun musicbrainz-interactive-tag-annotate (tag)
  "Annotate the TAG.
Tags have only `name' and `score' entries to their metadata so this
function returns nil."
  (ignore tag)
  nil)

;;;###autoload
(defun musicbrainz-interactive-search-tag
    (query &optional limit offset)
  "Enter a QUERY to search for a tag.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Tag: " nil
                 'musicbrainz-interactive-tag-query-history)))

  (musicbrainz-interactive-search
   "tag"
   query
   'tags
   "Tag: "
   musicbrainz-interactive-tag-format-function
   musicbrainz-interactive-tag-annotate-function
   (lambda (item)
     (alist-get 'name item))
   (lambda (tag)
     (browse-url (format "https://musicbrainz.org/tag/%s" tag)))
   limit
   offset))

;;; URL functions

(defun musicbrainz-interactive-url-format (url)
  "Formatting function for URL to be used in `completing-read'."
  (alist-get 'resource url))

(defun musicbrainz-interactive-url-annotate (url)
  "Annotate the URL."
  (ignore url)
  nil)

;;;###autoload
(defun musicbrainz-interactive-search-url
    (query &optional limit offset)
  "Enter a QUERY to search for an URL.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "URL: " nil
                 'musicbrainz-interactive-url-query-history)))

  (musicbrainz-interactive-search
   "url"
   query
   'urls
   "URL: "
   musicbrainz-interactive-url-format-function
   musicbrainz-interactive-url-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

;;; Work functions

(defun musicbrainz-interactive-work-format (work)
  "Formatting function for WORK to be used in `completing-read'."
  (format "[%s] %s%s"
          (substring (alist-get 'id work) 0 8)
          (alist-get 'title work)
          (if-let ((alias (musicbrainz-interactive-get-alias work)))
              (format " (%s)" alias)
            "")))

(defun musicbrainz-interactive-work-annotate (work)
  "Annotate the WORK."
  (musicbrainz-interactive-annotate
   :data (get-text-property 0 'data work)
   :items (type disambiguation)))

;;;###autoload
(defun musicbrainz-interactive-search-work
    (query &optional limit offset)
  "Enter a QUERY to search for a work.
Look at `musicbrainz-interactive-search' for LIMIT and OFFSET
descriptions."
  (interactive
   (list
    (read-string "Work: " nil
                 'musicbrainz-interactive-work-query-history)))

  (musicbrainz-interactive-search
   "work"
   query
   'works
   "Work: "
   musicbrainz-interactive-work-format-function
   musicbrainz-interactive-work-annotate-function
   (lambda (item)
     (alist-get 'id item))
   #'musicbrainz-interactive-open-mbid
   limit
   offset))

(provide 'musicbrainz-interactive)

;;; musicbrainz-interactive.el ends here
