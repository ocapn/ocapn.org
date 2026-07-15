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
             (srfi srfi-19)
             (ice-9 rdelim))


(define (post->relative-url site post)
  (let ((filename (post-file-name post))
        (slug (site-post-slug site post)))
    (cond [(string-prefix? "posts/news" filename)
           (string-append "news/" slug ".html")]
          [else (error "Unknown type of post" post filename)])))

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

(define (make-anchor text link)
  `(a (@ (href ,link)) ,text))

(define (make-box title content)
  `(div (@ (class "box"))
    (h2 (@ (class "heading")) ,title)
    ,content))

(define ocapn-intro
  `((p "We are a group focused on converging and working on pre-standardization
of OCapN. OCapN stands for \"Object Capability Network\" and provides:")
    (ul
     (li "CapTP (Capability Transport Protocol) which is the heart of OCapN.
This protocol allows for networked programming which, with the appropriate
tooling, has the convenience of programming against \"networked objects\" which
are little different from any other asynchronous programming in the host
language.")
     (li "3" (sup "rd") " party handoffs: These allow peers to seamlessly and
securely hand off references to actors/objects on other peers. This is
implemented taking a certificate based approach (a walkthrough of 3" (sup "rd")
 " party handoffs was done at January 2023's meeting. The recording is available
on "
   ,(make-anchor "PeerTube" "https://share.tube/w/44DhG6pcEyCFgnUHhxnUTn")
   " or "
   ,(make-anchor "YouTube" "https://www.youtube.com/watch?v=rQJB6MSmang")
   ". The " ,(make-anchor "slides" "/files/spritely-goblins-third-party-handoffs-implementation.pdf")
   " are also available).")
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
    (p "There are several implementations of OCapN:"
       ;; Please keep alphabetically sorted.
       (ul
	(li ,(make-anchor "DObjects (Dart)" "https://codeberg.org/ridley/DObjects"))
	(li ,(make-anchor "Endo (JavaScript)" "https://github.com/endojs/endo/tree/master/packages/captp"))
        (li ,(make-anchor "Goblins (Guile)" "https://codeberg.org/spritely/goblins/src/branch/main/goblins/ocapn"))
        (li ,(make-anchor "Goblins (Racket)" "https://codeberg.org/spritely/racket-goblins/src/branch/master/goblins/ocapn"))))
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

(define next-meeting
  `((p "The meetings are held on the 2nd Tuesday of every month, please refer to the "
       ,(make-anchor "issue tracker" "https://github.com/ocapn/ocapn/issues?q=is%3Aissue%20state%3Aopen%20in%3Atitle%20meeting")
       " for more information.")
    (p "Anyone who'd like to participate is welcome.")))

(define (ocapn-website-theme)
  (define (layout site page-title body)
    `((doctype html)
      (html
       (@ (lang "en-US"))
       (head
        (meta (@ (charset "utf-8")))
        (meta (@ (name "viewport") (content "width=device-width")))
        (link (@ (rel "stylesheet") (type "text/css") (href "/theme/style.css")))
        (title ,page-title))
       (body
        (div (@ (id "main"))
             (h1 (@ (id "title") (class "heading")) ,(site-title site))
             (nav (@ (class "menu"))
              (ul
               (li ,(make-anchor "GitHub" "https://github.com/ocapn/ocapn"))
               (li ,(make-anchor "Past Meeting Minutes" "https://github.com/ocapn/ocapn/tree/main/meeting-minutes"))
               (li ,(make-anchor "Draft Specifications" "https://github.com/ocapn/ocapn/tree/main/draft-specifications"))
               (li ,(make-anchor "Implementation Guide" "https://github.com/ocapn/ocapn/blob/main/implementation-guide/Implementation%20Guide.md"))))
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


(define* (ocapn-homepage theme #:key (title "") (boxes '()) (collections '()))
  (define layout (theme-layout theme))
  (define collection-template (theme-collection-template theme))
  (define post-template (theme-post-template theme))
  (lambda (site posts)
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
        (append rendered-boxes))
       sxml->html))

    (append (list serialized-homepage))))

(site #:title "OCapN Pre-standardization Group"
      #:domain "ocapn.org"
      #:default-metadata
      '((author . "Jessica Tallon")
        (email . "tsyesika@tsyesika.se"))
      #:readers (list commonmark-reader)
      #:builders (list (ocapn-homepage
                        (ocapn-website-theme)
                        #:title "OCapN Pre-standardization Group"
                        #:boxes
                        `(("OCapN" ,ocapn-intro)
                          ("Meetings" ,next-meeting)))
                       (static-directory "files")
                       (static-directory "theme")))
