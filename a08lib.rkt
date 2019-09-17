#lang racket

  ;; this "provides" the following so they are visible when you "require" them 
  
  (provide    
      
   make-filedata
   filedata?
   filedata-name
   filedata-format 
   filedata-size
   
   make-dir
   dir?
   dir-name
   dir-contents
   
   fs-print
   
   sample-fs
   
   )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Data and type definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (Don't worry about the #:transparent at the end of the define-struct lines.
  ;; That's a technical detail that doesn't matter in the teaching languages.)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-struct dir (name contents) #:transparent)
  ;; A Dir is a (make-dir Str FDList)

  ;; An FDList is one of:
  ;;   * empty
  ;;   * (cons File/Dir FDList)

  ;; A File/Dir is one of:
  ;;   * FileData
  ;;   * Dir

  (define-struct filedata (name format size) #:transparent)
  ;; A FileData is a (make-filedata Str Sym Num)
  ;; requires:
  ;;    name is a non-empty string 
  ;;      that contains only alphanumeric characters
  ;;    format is one of 'doc, 'txt, 'rtf, 'zip,
  ;;                     'raw, 'jpg, 'wav, 'mp3
  ;;    size > 0, and represents the size of the file in kilobytes  
  
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Code for Pretty Printing a FileSystem with fs-print
  ;; (minimal design recipe)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; (lofd-print lofd lob) traverses a directory (lofd), dispatching each entry
  ;;          to fd-print.  lob 'accumulates' information about whether
  ;;          or not it's the last entry in the dir
  ;; lofd-print: FDList (listof Bool) -> Str
  (define (lofd-print lofd lob)
    (cond
      [(empty? lofd) ""]
      [else (string-append 
             (fd-print (first lofd)
                       (append lob (list (empty? (rest lofd)))))
             (lofd-print (rest lofd) lob))]))
  
  ;; (fd-print fd lob) prints a FileData or a Dir
  ;;     (and then recurse if it's a Dir)
  ;;     The lob ride-along is used for indentation
  ;; fd-print: File/Dir (listof Bool) -> Str
  (define (fd-print fd lob)
    (cond 
      [(filedata? fd) (string-append (fd-print-indent lob)
                                 (string #\u2500 #\space )
                                 (filedata-name fd) "\n")]
      [else (string-append (fd-print-indent lob)
                           (string #\u25bc #\space ) 
                           (dir-name fd) "\n"
                           (lofd-print (dir-contents fd) lob))]))
  
  ;; (fd-print-indent) prints the indentation / lines of the Dir view
  ;;          each Bool corresponds to a "level of indentation"
  ;;          and is true if we are on the last entry of that level
  ;; fd-print-indent: (listof Bool) -> Str
  (define (fd-print-indent lob)
    (cond [(empty? lob) ""]
          [else 
           (string-append 
            (cond
              ;; lowest level and last entry, so use "L"
              [(and (first lob) (empty? (rest lob))) (string #\u2514 #\u2500)]
              ;; lowest level so use "T"
              [(empty? (rest lob)) (string #\u251c #\u2500)]
              ;; higher level, but last entry, so just blank
              [(first lob) (string #\space #\space)]
              ;; higher level, but not last entry so use "|"
              [else (string #\u2502 #\space)])
            ;; recurse to next (lower) level
            (fd-print-indent (rest lob)))]))
  
  ;; (fs-print fs) prints a pretty Dir, a wrapper for fd-print
  ;; fs-print: Dir -> void
  (define (fs-print fs)
    (display (fd-print fs empty)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define sample-fs
    (make-dir "root" (list 
      (make-filedata "readme" 'txt 187/100)
      (make-dir "photos" (list 
        (make-filedata "doctor" 'jpg 3669)
        (make-filedata "mark" 'jpg 2866)
        (make-filedata "spock" 'jpg 2709)
        (make-dir "vacation" (list                    
          (make-filedata "beach1" 'jpg 3297)
          (make-filedata "beach2" 'jpg 2173)
          (make-filedata "beach3" 'jpg 2747)))
        (make-filedata "k9" 'jpg 3287)
        (make-filedata "dalek" 'jpg 2294)))
      (make-dir "music" (list 
        (make-dir "rock" (list 
          (make-filedata "rhcp-under-the-bridge" 'mp3 10184)
          (make-filedata "u2-one" 'mp3 9693)))
        (make-dir "dance" (list 
          (make-filedata "katy-perry-roar" 'mp3 9376)
          (make-filedata "daft-punk-lose-yourself-to-dance" 'mp3 17669)))))
      (make-dir "schoolwork" empty)
      (make-dir "notes" (list 
      (make-filedata "readme" 'txt 213/100)
        (make-filedata "shopping" 'txt 573/100)
        (make-filedata "todo" 'txt 301/1000))))))
  
