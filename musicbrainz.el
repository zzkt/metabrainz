;;; musicbrainz.el --- MusicBrainz API interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright 2023 FoAM
;;
;; Author: nik gaffney <nik@fo.am>
;; Created: 2023-05-05
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (request "0.3"))
;; Keywords: music, scrobbling, multimedia
;; URL: https://github.com/zzkt/metabrainz

;; This file is not part of GNU Emacs.

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

;; - basic MusicBrainz interface
;; - partial & incomplete
;; - no error checks
;; - sync -> async


;;; Code:

(require 'request)
(require 'json)


;;; ;; ;; ;  ; ;   ;  ;      ;
;;
;; API config
;;
;;; ; ;; ;;

(defcustom musicbrainz-api-url "https://musicbrainz.org/ws/2"
  "URL for musicbrainz API.
Documentation available at https://musicbrainz.org/doc/MusicBrainz_API"
  :type 'string
  :group 'musicbrainz)

(defcustom musicbrainz-api-token ""
  "An auth token is required for some functions."
  :type 'string
  :group 'musicbrainz)

;;; ; ; ;;;   ;  ;
;;
;; API entities
;;  https://musicbrainz.org/doc/MusicBrainz_API#Browse
;;
;; On each entity resource, you can perform three different GET requests:
;;  lookup:   /<ENTITY_TYPE>/<MBID>?inc=<INC>
;;  browse:   /<RESULT_ENTITY_TYPE>?<BROWSING_ENTITY_TYPE>=<MBID>&limit=<LIMIT>&offset=<OFFSET>&inc=<INC>
;;  search:   /<ENTITY_TYPE>?query=<QUERY>&limit=<LIMIT>&offset=<OFFSET>
;;
;; Note: Keep in mind only the search request is available without an MBID
;; (or, in specific cases, a disc ID, ISRC or ISWC). If all you have is the
;; name of an artist or album, for example, you'll need to make a search and
;; pick the right result to get its MBID; only then will you able to use it
;; in a lookup or browse request.
;;
;; On the genre resource, we support an "all" sub-resource to fetch all genres,
;; paginated, in alphabetical order:
;;
;;  all:      /genre/all?limit=<LIMIT>&offset=<OFFSET>
;;
;; ; ;;; ;

(defconst musicbrainz-entities-core
  (list "area" "artist" "event" "genre" "instrument" "label" "place"
        "recording" "release" "release-group" "series" "work" "url")
  "API resources which represent core entities in the MusicBrainz database.")

(defconst musicbrainz-entities-non-core
  (list "rating" "tag" "collection")
  "API resources which represent non-core entities in the MusicBrainz database.")

(defconst musicbrainz-entities-uids
  (list "discid" "isrc" "iswc")
  "API resources based on other unique identifiers in the MusicBrainz database.")

(defconst musicbrainz-entities-linked
  (list "area" "artist" "collection" "event" "instrument" "label" "place"
        "recording" "release" "release-group" "series" "work" "url")
  "API resources for linked entites in the MusicBrainz database.")


;; Linked entities

(defun musicbrainz-linked-entity-p (entity)
  "Check if ENTITY can be used in a browse request (incomplete).

The following list shows which linked entities you can use in a browse request:

 /ws/2/area            collection
 /ws/2/artist          area, collection, recording, release, release-group, work
 /ws/2/collection      area, artist, editor, event, label, place, recording,
                       release, release-group, work
 /ws/2/event           area, artist, collection, place
 /ws/2/instrument      collection
 /ws/2/label           area, collection, release
 /ws/2/place           area, collection
 /ws/2/recording       artist, collection, release, work
 /ws/2/release         area, artist, collection, label, track, track_artist,
                       recording, release-group
 /ws/2/release-group   artist, collection, release
 /ws/2/series          collection
 /ws/2/work            artist, collection
 /ws/2/url             resource"

  (if (member entity musicbrainz-entities-linked) t nil))


;; utils & aux

(defun musicbrainz-mbid-p (mbid)
  "Check (permissive) if MBID is valid and/or well formatted.
An MBID is a 36 character Universally Unique Identifier, see https://musicbrainz.org/doc/MusicBrainz_Identifier for details."
  (if (and (length= mbid 36)
           (string-match-p
            (rx (repeat 8 hex)        ;;  [A-F0-9]{8}
                "-" (repeat 4 hex)    ;; -[A-F0-9]{4}
                "-4" (repeat 3 hex)   ;; -4[A-F0-9]{3}
                "-" (repeat 4 hex)    ;; -[89AB][A-F0-9]{3}
                "-" (repeat 12 hex))  ;; -[A-F0-9]{12}
            mbid))
      t nil))


;;; ;; ;; ;  ; ;   ;  ;      ;
;;
;; Search API
;;  https://musicbrainz.org/doc/MusicBrainz_API/Search
;;
;; ;; ; ;  ;


;;;###autoload
(defun musicbrainz-search (entity query &optional limit)
  "Search the MusicBrainz database for ENTITY matching QUERY.
Optionally return only LIMIT number of results.

The QUERY field supports the full Lucene Search syntax, some details
can be found near https://musicbrainz.org/doc/MusicBrainz_API/Search
or in the Lucene docs."

  (message "musicbrainz: searching %s=%s" entity query)
  (let* ((max (if limit limit 1))
         (response
          (request-response-data
           (request
            (url-encode-url
             (format "%s/%s?query=%s&fmt=json&limit=%s"
                     musicbrainz-api-url entity query max))
            :type "GET"
            :parser 'json-read
            :sync t
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        (if (eq t (assoc-default 'valid data))
                            (message "Token is valid for user: %s"
                                     (assoc-default 'user_name data))
                            (message "Not a valid user token"))))))))
    response))


;; various specific searches

;;;###autoload
(defun musicbrainz-search-artist (artist &optional limit)
  "Search for an ARTIST and show matches.
Optionally return LIMIT number of results."
  (let ((data (musicbrainz-search "artist" artist limit)))
    (let-alist
      data
              (seq-map
               (lambda (i)
                 (let-alist i
                            (if (not limit)
                                (format "%s | %s |\n" .name .id)
                                (format "%s | %s | %s |\n"
                                        .score .name .id))))
               .artists))))


;;;###autoload
(defun musicbrainz-artist-to-mbid (artist)
  "Find an MBID for ARTIST (with 100% match).
See `musicbrainz-disambiguate-artist' if there are multiple matches."
  (let ((data (musicbrainz-search "artist" artist)))
    (let-alist data
               (car (remove nil (seq-map
                            (lambda (i)
                              (let-alist i
                                         (when (= 100 .score)
                                           (format "%s" .id))))
                            .artists))))))


;;;###autoload
(defun musicbrainz-disambiguate-artist (artist &optional limit)
  "More ARTIST data. less ambiguity (with optional LIMIT).
Outputs an `org-mode' table with descriptions and MBID link to artists pages."
    (let ((data (musicbrainz-search "artist" artist limit)))
    (let-alist data
               (cons (format "| Artist: %s| MBID |\n" artist)
               (seq-map
                (lambda (i)
                  (let-alist i
                             (format "%s | %s, %s | [[https://musicbrainz.org/artist/%s][%s]] |\n"
                                     .score .name .disambiguation .id .id)))
                .artists)))))


;;;###autoload
(defun musicbrainz-search-label (label &optional limit)
  "Search for a LABEL and show matches.
Optionally return LIMIT number of results."
  (let ((data (musicbrainz-search "label" label limit)))
    (let-alist
      data
      (seq-map
       (lambda (i)
         (let-alist i
                    (if (not limit)
                        (format "%s | %s |\n" .name .id)
                        (format "%s | %s | %s (%s%s) | %s |\n"
                                .score .name
                                (if .disambiguation .disambiguation "")
                                (if .life-span.begin
                                    (format "%s " .life-span.begin) "")
                                (if .life-span.end
                                    (format "—%s" .life-span.end)
                                    "ongoing")
                                .id))))
       .labels))))



;;;;;; ; ; ;; ;   ;     ;  ; ; ;;   ;
;;
;; Browse API
;;  https://musicbrainz.org/doc/MusicBrainz_API#Browse
;;
;;;; ; ; ; ; ;

;; Browse requests are a direct lookup of all the entities directly linked
;; to another entity ("directly linked" here meaning it does not include
;; entities linked by a relationship). For example, you may want to see all
;; releases on the label ubiktune:

;; /ws/2/release?label=47e718e1-7ee4-460c-b1cc-1192a841c6e5

;; Note that browse requests are not searches: in order to browse all the releases
;; on the ubiktune label you will need to know the MBID of ubiktune.

;; The order of the results depends on what linked entity you are browsing
;; by (however it will always be consistent). If you need to sort the entities,
;; you will have to fetch all entities and sort them yourself.

;; Keep in mind only the search request is available without an MBID (or, in
;; specific cases, a disc ID, ISRC or ISWC). If all you have is the name of an
;; artist or album, for example, you'll need to make a search and pick the right
;; result to get its MBID to use it in a lookup or browse request.


;;;###autoload
(defun musicbrainz-browse (entity link lookup &optional type)
  "Search the MusicBrainz database for ENTITY with LINK matching LOOKUP.
Optionally limit the search to TYPE results for ENTITY."
  (message "musicbrainz: browsing %s linked to %s" entity link)
  (message "url: %s/%s?%s=%s&type=%s&fmt=json" musicbrainz-api-url entity link lookup type)
  (let ((response
          (request-response-data
           (request
            (url-encode-url
             (format "%s/%s?%s=%s&type=%s&fmt=json" musicbrainz-api-url entity link lookup type))
            :type "GET"
            :parser 'json-read
            :sync t
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        (message "ok")))))))
    response))


;;;

(provide 'musicbrainz)

;;; musicbrainz.el ends here
