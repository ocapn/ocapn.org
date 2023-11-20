(use-modules (commonmark)
             (haunt asset)
             (haunt site)
             (haunt post)
             (haunt artifact)
             (haunt html)
             (haunt reader)
             (haunt builder blog)
             (haunt builder atom)
             (haunt reader commonmark)
             (haunt builder assets)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-19)
             (ice-9 rdelim))

(define-record-type <meeting>
  (make-meeting start-time end-time issue-number)
  meeting?
  (start-time meeting-start-time)
  (end-time meeting-end-time)
  (issue-number meeting-issue-number))

(define (post->relative-url site post)
  (let ((filename (post-file-name post))
        (slug (site-post-slug site post)))
    (cond [(string-prefix? "posts/news" filename)
           (string-append "news/" slug ".html")]
          [(string-prefix? "posts/minutes" filename)
           (string-append "meeting-minutes/" slug ".html")]
          [else (error "Unknown type of post" post filename)])))

(define (make-anchor text link)
  `(a (@ (href ,link)) ,text))

(define (make-box title content)
  `(div (@ (class "box"))
    (h2 (@ (class "heading")) ,title)
    ,content))

(define (number->ordinal-number-suffix num)
  (let ((ones (modulo num 10)))
    (cond [(= ones 1) "st"]
          [(= ones 2) "nd"]
          [(= ones 3) "rd"]
          [else "th"])))

(define (make-time/date-only time)
  `(time (@ (datetime ,(date->string time "~1")))
    ,(date->string time "~A the ~e" time)
    ,(number->ordinal-number-suffix (date-day time))
    ,(date->string time " of ~B ~Y")))

(define (make-time/short-date-only time)
  `(time (@ (datetime ,(date->string time "~1")))
    ,(date->string time "~1")))

(define (make-time/time-only time)
  (unless (= (date-zone-offset time) 0)
    (error "No support for rendering times with UTC offsets"))

  `(time (@ (datetime ,(date->string time "~H:~M")))
    ,(date->string time "~H:~M (~I:~M ~p)")))


(define next-meeting
  (make-meeting (make-date 0 0 0 19 12 12 2023 0)
                (make-date 0 0 0 20 12 12 2023 0)
                99))

;; Sanity check the meeting time, don't forget to update it.
(let ((now (time-second (current-time)))
      (next-meeting-start
       (time-second (date->time-monotonic (meeting-start-time next-meeting)))))
  (when (< next-meeting-start now)
    (error "The next meeting is in the past, please update the next meeting time.")))

(define next-meeting-info
  `((p "Our next meeting is on "
       (strong ,(make-time/date-only (meeting-start-time next-meeting)))
       " between "
       (strong ,(make-time/time-only (meeting-start-time next-meeting)))
       " and "
       (strong ,(make-time/time-only (meeting-end-time next-meeting)))
       " UTC. If you'd like to join us, find more information on "
       ,(make-anchor "the GitHub issue"
                     (string-append "https://github.com/ocapn/ocapn/issues/"
                                    (number->string (meeting-issue-number next-meeting)))))))

(define ocapn-intro
  `((p "We are a group focused on converging and working on pre-standardization
of OCapN. OCapN stands for \"Object Capability Network\" and provides:")
    (ul
     (li "CapTP (Capability Transport Protocol) which is the heart of OCapN.
This protocol allows for networked programming which, with the appropriate
tooling, has the convenience of programming against \"networked objects\" which
are little different from any other asynchronous programming in the host
language.")
     (li "A generalized \"netlayer\" interface and specifications of compatible
implementations. OCapN's CapTP can be run over different \"netlayer\"
implementations ranging from "
         ,(make-anchor "Tor Onion Services"
                      "https://2019.www.torproject.org/docs/onion-services.html.en")

         " to "
         ,(make-anchor "IBC"
                      "https://ibcprotocol.org/")
         " to "
         ,(make-anchor "I2P"
                      "https://geti2p.net/")
         " to "
         ,(make-anchor "libp2p"
                      "https://libp2p.io/")
         " to perhaps carrier pigeons with backpacks full of encrypted microsd
cards.")
     (li "A URI structure for addressing machines and specific objects on
machines."))
    (p "The group does its work mainly on our "
       ,(make-anchor "GitHub"
                    "https://github.com/ocapn/ocapn/")
       " page, if you're interested in joining us head over there for more
information on our current work and upcoming meetings. The group is comprised
of:"
       (ul
        (li ,(make-anchor "Agoric" "https://agoric.com/"))
        (li ,(make-anchor "The Spritely Institute"
                          "https://www.spritely.institute/"))
        (li ,(make-anchor "Cap'n Proto" "https://capnproto.org/"))
        (li "Many other independent experts and interested parties"))
    (p "Feel free to join our IRC channel #ocapn on "
       ,(make-anchor "libera.chat" "https://libera.chat")
       " (" ,(make-anchor "channel logs" "https://logs.guix.gnu.org/ocapn/") ")"))))


(define (ocapn-website-theme)
  (define (layout site page-title body)
    `((doctype html)
      (html
       (@ (lang "en-US"))
       (head
        (meta (@ (charset "utf-8")))
        (meta (@ (name "viewport") (content "width=device-width")))
        (link (@ (rel "stylesheet") (type "text/css") (href "/theme/style.css")))
        (link (@ (rel "alternate") (type "application/atom") (href "/news.xml") (title "News")))
        (link (@ (rel "alternate") (type "application/atom") (href "/minutes.xml") (title "Meeting Minutes")))
        (title ,page-title))
       (body
        (div (@ (id "main"))
             (h1 (@ (id "title") (class "heading")) ,(site-title site))
             (nav (@ (class "menu"))
              (ul
               (li ,(make-anchor "Home" "/"))
               (li ,(make-anchor "GitHub" "https://github.com/ocapn/ocapn"))
               (li (a (@ (style "color:gray")) "Draft Specifications"))))
             (div)
             ,body)))))

  (define (post post)
    (make-box (post-ref post 'title)
              (list `(div (@ (class "timestamp"))
                      ,(make-time/date-only (post-date post)))
                    `(div (@ (class "delimiter")))
                    (post-sxml post))))

  (define (post-summary site post)
    `(span (@ (class "post-summary"))
      ,(make-time/short-date-only (post-date post))
      ,(make-anchor
        (post-ref post 'title)
        (post->relative-url site post))))
  
  (define (collection site title posts url-prefix)
    (make-box
     title
     `(div (@ (class "posts"))
       (ol
        ,(map (lambda (post)
                `(li ,(post-summary site post)))
              posts)))))

  (theme #:name "ocapn"
         #:layout layout
         #:post-template post
         #:collection-template collection))

(define (news-filter posts)
  (filter
   (lambda (post) (string-prefix? "posts/news" (post-file-name post)))
   posts))

(define (meeting-minutes-filter posts)
  (define filtered
    (filter
      (lambda (post) (string-prefix? "posts/minutes" (post-file-name post)))
      posts))
  ;; Sort them so that the most recent meeting comes first.
  (sort filtered (lambda (a b) (string> (post-file-name a) (post-file-name b)))))

(define (make-meeting-commonmark-reader)
  (define (filename->date filename)
    (string->date
     (last (string-split filename #\/))
     "~Y-~m-~d.md"))
  (define (filename->title filename)
    (date->string (filename->date filename)
                  "~B ~Y"))

  (make-reader
   (lambda (filename) (and (string-prefix? "posts/minutes" filename)
                      (string-suffix? ".md" filename)))
   (lambda (filename)
     (values `((title . ,(filename->title filename))
               (date . ,(filename->date filename)))
             (call-with-input-file filename
               (lambda (port)
                 ;; For asthetic reasons, read the first line of the
                 ;; markdown file as it'll be the title, we use our own.
                 (unless (string-prefix? "#" (read-line port))
                   (error "The minutes file doesn't begin with a title element" filename))
                 (commonmark->sxml port)))))))

(define* (ocapn-homepage theme #:key (title "") (boxes '()) (collections '()))
  (define layout (theme-layout theme))
  (define collection-template (theme-collection-template theme))
  (define post-template (theme-post-template theme))
  (lambda (site posts)
    (define (render-post post)
      (serialized-artifact
       (post->relative-url site post)
       (layout site (post-ref post 'title) (post-template post))
       sxml->html))

    ;; Build all the collections
    (define rendered-collections
      (map
       (lambda (collection)
         (define matched-posts
           ((car (cdr collection)) posts))
         (if (null? matched-posts)
             '()
             (collection-template site (car collection) matched-posts "/")))
       collections))

    ;; Build all the content in boxes
    (define rendered-boxes
      (map
       (lambda (content)
         (make-box (car content) (car (cdr content))))
       boxes))

    ;; Render with layout.
    (define serialized-homepage
      (serialized-artifact
       "index.html"
       (layout
        site
        title
        (append rendered-boxes rendered-collections))
       sxml->html))

    (append (map render-post posts)
            (list serialized-homepage))))

(site #:title "OCapN Pre-standardization Group"
      #:domain "ocapn.org"
      #:default-metadata
      '((author . "Jessica Tallon")
        (email . "tsyesika@tsyesika.se"))
      #:readers (list (make-meeting-commonmark-reader) commonmark-reader)
      #:builders (list (ocapn-homepage
                        (ocapn-website-theme)
                        #:title "OCapN Pre-standardization Group"
                        #:boxes
                        `(("OCapN" ,ocapn-intro)
                          ("Next Meeting" ,next-meeting-info))
                        #:collections
                        `(("Meeting Minutes" ,meeting-minutes-filter)
                          ("News" ,news-filter)))

                       (atom-feed #:file-name "minutes.xml"
                                  #:subtitle "Meeting Minutes"
                                  #:max-entries 60
                                  #:filter meeting-minutes-filter)
                       (atom-feed #:file-name "news.xml"
                                  #:subtitle "News"
                                  #:max-entries 60
                                  #:filter news-filter)
                       (static-directory "files")
                       (static-directory "theme")))
