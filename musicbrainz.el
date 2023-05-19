;;; musicbrainz.el --- MusicBrainz API interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright 2023 FoAM
;;
;; Author: nik gaffney <nik@fo.am>
;; Created: 2023-05-05
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (request "0.3"))
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

(defconst musicbrainz-search-types
  (list "annotation" "area" "artist" "cdstub" "event" "instrument"
        "label" "place" "recording" "release" "release-group"
        "series" "tag" "work" "url")
  "Valid TYPE parameters for MusicBrainz searches.")


;; entity checks

(defun musicbrainz-core-entity-p (entity)
  "Check if ENTITY is a core entity."
  (if (member entity musicbrainz-entities-core) t nil))

(defun musicbrainz-non-core-entity-p (entity)
  "Check if ENTITY is a non-core entity."
  (if (member entity musicbrainz-entities-non-core) t nil))

(defun musicbrainz-uid-entity-p (entity)
  "Check if ENTITY is a unique identifier entity."
  (if (member entity musicbrainz-entities-uids) t nil))

(defun musicbrainz-search-type-p (type)
  "Check if TYPE is a valid search type."
  (if (member type musicbrainz-search-types) t nil))


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

  (if (and (length= mbid 36) ;; length= requires emacs > 28.1
           (string-match-p
            (rx (repeat 8 hex)        ;;  [A-F0-9]{8}
                "-" (repeat 4 hex)    ;; -[A-F0-9]{4}
                "-4" (repeat 3 hex)   ;; -4[A-F0-9]{3}
                "-" (repeat 4 hex)    ;; -[89AB][A-F0-9]{3}
                "-" (repeat 12 hex))  ;; -[A-F0-9]{12}
            mbid))
      t nil))


(defun musicbrainz-format (response)
  "Format a generic RESPONSE."
  (format "%s" (pp response)))


;;; ;; ;; ;  ; ;   ;  ;      ;
;;
;; Search API
;;  https://musicbrainz.org/doc/MusicBrainz_API/Search
;;
;; The MusicBrainz API search requests provide a way to search for MusicBrainz
;; entities based on different sorts of queries and are provided by a search
;; server built using Lucene technology.
;;
;; Parameters common to all resources
;;
;;  type    Selects the entity index to be searched: annotation, area, artist,
;;          cdstub, event, instrument, label, place, recording, release,
;;          release-group, series, tag, work, url
;;
;;  query   Lucene search query. This is mandatory
;;
;;  limit   An integer value defining how many entries should be returned.
;;          Only values between 1 and 100 (both inclusive) are allowed.
;;          If not given, this defaults to 25.
;;
;;  offset  Return search results starting at a given offset.
;;          Used for paging through more than one page of results.
;;
;;  dismax  If set to "true", switches the Solr query parser from edismax to dismax,
;;          which will escape certain special query syntax characters by default
;;          for ease of use. This is equivalent to switching from the "Indexed search
;;          with advanced query syntax" method to the plain "Indexed search" method
;;          on the website. Defaults to "false".
;;
;; ;; ; ;  ;

;;;###autoload
(defun musicbrainz-search (type query &optional limit offset)
  "Search the MusicBrainz database for TYPE matching QUERY.
Optionally return only LIMIT number of results from OFFSET.

The QUERY field supports the full Lucene Search syntax, some details
can be found near https://musicbrainz.org/doc/MusicBrainz_API/Search
or in the Lucene docs."

  (message "musicbrainz: searching %s=%s" type query)
  (let* ((max (if limit limit 1))
         (from (if offset offset ""))
         (response
           (request-response-data
            (request
             (url-encode-url
              (format "%s/%s?query=%s&fmt=json&limit=%s&offset=%s"
                      musicbrainz-api-url type query max from))
             :type "GET"
             :parser 'json-read
             :sync t
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (message "ok")))))))
    response))



;;;###autoload
(defun musicbrainz-find (query &rest extras)
  "Search the MusicBrainz database for QUERY or recommend a more specific search.
MusicBrainz makes a distinction between `search' and `browse' this a more general
entry point to searching/browsing the database.

Heuristics.
- if QUERY is an MBID, check artist, recording, etc
- if QUERY is text, search for artists or recordings, etc"

  (message "musicbrainz: finding: %s" query)
  (if (musicbrainz-mbid-p query)
      ;; search (lookup) for things that could have an mbid
      (let ((mbid query))
        (message "searching mbid: %s" mbid))
      ;; search (query) for other things
      (progn
        (message "searching other: %s" mbid)
        ;; (message "searching artist: %s" query)
        ;; (musicbrainz-format (musicbrainz-search "artist"  query))
        ;; (message "searching label: %s" query)
        ;; (musicbrainz-format (musicbrainz-search "label"  query))
        ;; (message "searching release: %s" query)
        ;; (musicbrainz-format (musicbrainz-search "release"  query))
        )))



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
  (let* ((max (if limit limit 11))
         (data (musicbrainz-search "artist" artist max)))
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
                       (format "%s | [[https://musicbrainz.org/label/%s][%s]] |\n" .name .id .id)
                       (format "%s | %s | %s (%s%s) |  [[https://musicbrainz.org/label/%s][%s]]  |\n"
                               .score .name
                               (if .disambiguation .disambiguation "")
                               (if .life-span.begin
                                   (format "%s " .life-span.begin) "")
                               (if .life-span.end
                                   (format "—%s" .life-span.end)
                                   "ongoing")
                               .id .id))))
      .labels))))


;;;###autoload
(defun musicbrainz-search-recording (query &optional limit)
  "Search for a recording using QUERY and show matches.
Optionally return LIMIT number of results."
  (let ((data (musicbrainz-search "recording" query limit)))
    (let-alist
     data
     (seq-map
      (lambda (i)
        (let-alist i
                   (format "%s | %s, %s | [[https://musicbrainz.org/release/%s][%s]] |\n"
                           .score .title (musicbrainz--unwrap-0 .artist-credit) .id .id)))
      .recordings))))


(defun musicbrainz--unwrap-0 (entity)
  "Unwrap (fragile) .artist-credit ENTITY -> .name more or less."
  (format "%s" (cdar (aref entity 0))))

;;;###autoload
(defun musicbrainz-search-release (query &optional limit)
  "Search for a release using QUERY and show matches.
Optionally return LIMIT number of results."
  (let ((data (musicbrainz-search "release" query limit)))
    (let-alist
     data
     (seq-map
      (lambda (i)
        (let-alist i
                   (format "%s | %s, %s | [[https://musicbrainz.org/release/%s][%s]] |\n"
                           .score .title (musicbrainz--unwrap-0 .artist-credit) .id .id)))
      .releases))))


;;; ;; ;; ;  ; ;   ;  ;      ;
;;
;; Lookups
;;  https://musicbrainz.org/doc/MusicBrainz_API#Lookups
;;
;;; ;; ;;   ; ;

;;;###autoload
(defun musicbrainz-lookup (entity mbid &optional inc)
  "Search (lookup not browse) the MusicBrainz database for ENTITY with MBID.
Optionally add an INC list.

Subqueries
 /ws/2/area
 /ws/2/artist            recordings, releases, release-groups, works
 /ws/2/collection        user-collections (includes private collections, requires authentication)
 /ws/2/event
 /ws/2/genre
 /ws/2/instrument
 /ws/2/label             releases
 /ws/2/place
 /ws/2/recording         artists, releases, isrcs, url-rels
 /ws/2/release           artists, collections, labels, recordings, release-groups
 /ws/2/release-group     artists, releases
 /ws/2/series
 /ws/2/work
 /ws/2/url"

  (message "musicbrainz: lookup: %s/%s" entity mbid)
  (if (and (musicbrainz-core-entity-p entity)
           (musicbrainz-mbid-p mbid))
      (let* ((add (if inc inc ""))
             (response
               (request-response-data
                (request
                 (url-encode-url
                  (format "%s/%s/%s?inc=%s&fmt=json"
                          musicbrainz-api-url entity mbid add))
                 :type "GET"
                 :parser 'json-read
                 :sync t
                 :success (cl-function
                           (lambda (&key data &allow-other-keys)
                             (message "%s data: %s" entity mbid)))))))
        response)
      (error "MusicBrainz: search requires a valid MBID and entity (i.e. one of %s)"
             musicbrainz-entities-core)))


;; specific MBID subrequests (limited to 25 results?)

(defun musicbrainz-lookup-artist (mbid)
  "MusicBrainz lookup for artist with MBID."
  (let ((response
          (musicbrainz-lookup "artist" mbid)))
    (let-alist response
               (format "| %s | %s | %s | [[https://musicbrainz.org/artist/%s][%s]] |\n"
                       .name .disambiguation .type .id .id))))


(defun musicbrainz-lookup-release (mbid)
  "MusicBrainz lookup for release with MBID."
  (let ((response
          (musicbrainz-lookup "release" mbid)))
    (let-alist response
               (format "| %s | %s | %s | [[https://musicbrainz.org/release/%s][%s]] |\n"
                       .date .title .packaging .id .id))))

(defun musicbrainz-lookup-recording (mbid)
  "MusicBrainz lookup for recording with MBID."
  (let ((response
          (musicbrainz-lookup "recording" mbid)))
    (let-alist response
               (format "%s | [[https://musicbrainz.org/recording/%s][%s]] |\n"
                       .title .id .id))))


(defun musicbrainz-lookup-artist-releases (mbid)
  "MusicBrainz lookup for releases from artist with MBID."
  (let ((response
          (musicbrainz-lookup "artist" mbid "releases")))
    (let-alist response
               (seq-map
                (lambda (i)
                  (let-alist i
                             (format "%s | %s | %s | [[https://musicbrainz.org/release/%s][%s]] |\n"
                                     .date .title .packaging .id .id)))
                .releases))))


(defun musicbrainz-lookup-artist-recordings (mbid)
  "MusicBrainz lookup for recordings from artist with MBID."
  (let ((response
          (musicbrainz-lookup "artist" mbid "recordings")))
    (let-alist response
               (seq-map
                (lambda (i)
                  (let-alist i
                             (format "%s | [[https://musicbrainz.org/recording/%s][%s]] |\n"
                                     .title .id .id)))
                .recordings))))



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
(defun musicbrainz-browse (entity link query &optional type)
  "Search the MusicBrainz database for ENTITY with LINK matching QUERY.
Optionally limit the search to TYPE results for ENTITY."
  (message "musicbrainz: browsing %s linked to %s" entity link)
  (message "url: %s/%s?%s=%s&type=%s&fmt=json" musicbrainz-api-url entity link query type)
  (let ((response
          (request-response-data
           (request
            (url-encode-url
             (format "%s/%s?%s=%s&type=%s&fmt=json" musicbrainz-api-url entity link query type))
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
