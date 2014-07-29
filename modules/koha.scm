;;; glean --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (store koha)
  #:use-module (glean library core-templates)
  #:export (koha-module))

(define basic-search
  (set 'basic-search
       #:name "Basic Searching"
       #:version "0.1"
       #:synopsis "Learn to perform quick keyword searches and
to navigate the results."
       #:contents
       (list (problem (q "After watching the video below…"
                         (media #:videos
                                '("//player.vimeo.com/video/14802047")))
                      #f)
             (problem (q "What can you do on the online catalogue?")
                      ;; (list (s "Ipso Lorum Facto <-- correct")
                      ;;       (s "Ipso Lorum Facta <-- correct"))
                      (s "Ipso Lorum Facto <-- correct")
                      (o "Ipso Lorum Facto <-- correct")
                      (o "Upso Lorum Facto")
                      (o "Ipso Lorum Facta"))
             (problem (q "Perform a quick search for 'james' at http://asdev.koha-ptfs.co.uk. Then past the resulting URL below.")
                      (s "http://asdev.koha-ptfs.co.uk/cgi-bin/koha/opac-search.pl?q=james")
                      (p string=?))
             (problem (q "To perform an 'Author Search' you should…")
                      (s "Click on the dropdown labeled 'Library Catalog' and selecting 'Author' before entering your search.")
                      (o "Click on the dropdown labeled 'Library Catalog' and selecting 'Author' before entering your search.")
                      (o "Click on Advanced search, from whence you can search by Author")
                      (o "Search first, then click on the 'Author' button.")))))

(define koha-module
  (module
    'koha
    #:name "Koha Library Catalogue"
    #:version "0.1"
    #:synopsis "Learn to use the Koha online catalogue to its full
potential."
    #:description "Koha is a fully featured, scalable library
management system. Development is sponsored by libraries of varying
types and sizes, volunteers, and support companies worldwide.  This
module will cover basic and advanced searching, placing reservations,
managing your loans and amending your details."
    #:creator "Alex Sassmannshausen"
    #:attribution
    (list
     (media #:urls '("http://koha-community.org/")))
    #:resources
    (list
     (media #:urls '("http://koha-community.org/documentation/"
                     "http://koha-community.org/documentation/other-docs/"
                     "http://koha-community.org/documentation/koha-bibliography/"
                     "http://wiki.koha-community.org/wiki/Main_Page")))
    #:logo     "http://git.koha-community.org/gitweb/Koha-logo.jpg"
    #:contents `(,basic-search)))
