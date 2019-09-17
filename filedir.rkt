;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname filedir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Sebastian Regalado (ID# 20759473)
;; CS 135 Winter 2019
;; Assignment 08, Question 2
;; *****************************************
;;


(require "a08lib.rkt")

(define file1 (make-filedata "oldfile" 'txt 1))
(define file2 (make-filedata "newfile" 'doc 10))
(define dir0 (make-dir "emptydir" empty))
(define dir1 (make-dir "onefile" (list file1)))
(define dir1a (make-dir "onefilea" (list file1)))
(define dir1b (make-dir "onefileb" (list file1)))
(define dir2 (make-dir "twofiles" (list file1 file2)))
(define dir2b (make-dir "twofiles" (list file1)))
(define dir3 (make-dir "onesies" (list dir1 dir1a dir1b)))
(define dir4 (make-dir "hola" (list dir0 dir1a)))
(define fs1 (make-dir "rootdir" (list file1 dir0 dir1 dir2 dir3 file2)))
(define fs2 (make-dir "u" (list file2 dir0 dir1)))

;; ==== Question 2a ============================================================

;; filedata-template: FileData -> Any
(define (filedata-template fd)
  (... (filedata-name fd) ...
       (filedata-format fd) ...
       (filedata-size fd) ...))

;; dir-template: Dir -> Any
(define (dir-template a-dir)
  (... (dir-name a-dir) ...
       (fdlist-template (dir-contents a-dir)) ...))

;; file/dir-template: File/Dir -> Any
(define (file/dir-template a-filedir)
  (... (cond [(filedate? a-filedir) (filedate-template a-filedir)]
             [else (dir-template a-filedir)]) ...))

;; fdlist-template: FDlist -> Any
(define (fdlist-template a-fdlist)
  (... (cond [(empty? a-fdlist) ...]
             [else (... (file/dir-template (first a-fdlist)) ...
                        (fdlist-template (rest a-fdlist)) ...)]) ...))

;; ==== Question 2b ============================================================

;; (count-files a-dir) produces the total number of files in a-dir
;; count-files: Dir -> Nat
;; Example:
(check-expect (count-files dir0) 0)

(define (count-files a-dir)
  (local [;; (count-files-fdlist a-fdlist) produces the total number of files
          ;;   in a-fdlist
          ;; count-files-fdlist: FDList -> Nat
          (define (count-files-fdlist a-fdlist)
            (cond [(empty? a-fdlist) 0]
                  [(filedata? (first a-fdlist))
                   (+ 1 (count-files-fdlist (rest a-fdlist)))]
                  [else (+ (count-files (first a-fdlist))
                           (count-files-fdlist (rest a-fdlist)))]))]
    
    (count-files-fdlist (dir-contents a-dir))))

;; Tests
(check-expect (count-files dir3) 3)
(check-expect (count-files fs1) 8)


;; ==== Question 2c ============================================================

;; (empty-dir-exist? a-dir) determines whether a-dir contains an empty directory
;; empty-dir-exist?: Dir -> Bool
;; Example:
(check-expect (empty-dir-exist? dir0) true)

(define (empty-dir-exist? a-dir)
  (cond [(empty? (dir-contents a-dir)) true]
        [else
         (local
           [;;(empty-dir-exist?-fdlist a-fdlist) determines whether a-dir 
            ;;   contains an empty directory
            ;; empty-dir-exist?-fdlist: FDList -> Bool
            (define (empty-dir-exist?-fdlist a-fdlist)
              (cond [(empty? a-fdlist) false]
                    [(filedata? (first a-fdlist))
                     (empty-dir-exist?-fdlist (rest a-fdlist))]
                    [else
                     (or (empty-dir-exist? (first a-fdlist))
                         (empty-dir-exist?-fdlist (rest a-fdlist)))]))]
                
           (empty-dir-exist?-fdlist (dir-contents a-dir)))]))

;; Tests
(check-expect (empty-dir-exist? dir1) false)
(check-expect (empty-dir-exist? dir3) false)
(check-expect (empty-dir-exist? fs1) true)
(check-expect (empty-dir-exist? fs2) true)


;; ==== Question 2d ============================================================

;; (largest-filesize a-dir) produces the size of the largest file in a-dir if
;;   there is at least one, otherwise 0
;; largest-filesize: Dir -> Nat
;; Example:
(check-expect (largest-filesize dir0) 0)

(define (largest-filesize a-dir)
  (cond [(empty? (dir-contents a-dir)) 0]
        [else
         (local
           [;; (largest-filesize-fdlist a-fdlist) produces the size of the 
            ;;   largest file in a-fdlist if there is at least one, otherwise 0
            ;; largest-filesize-fdlist: FDList -> Nat
            (define (largest-filesize-fdlist a-fdlist)
              (cond [(empty? a-fdlist) 0]
                    [(dir? (first a-fdlist))
                     (largest-filesize-fdlist (rest a-fdlist))]
                    [else (max (filedata-size (first a-fdlist))
                               (largest-filesize-fdlist (rest a-fdlist)))]))]
           
           (largest-filesize-fdlist (dir-contents a-dir)))]))

;; Tests
(check-expect (largest-filesize dir1) 1)
(check-expect (largest-filesize dir2) 10)
(check-expect (largest-filesize fs2) 10)


;; ==== Question 2e ============================================================

;; (list-file-paths a-dir) produces a list that contains the hierarchical names
;;   for all of the files in a-dir
;; list-file-paths: Dir -> (list Str)
;; Example:
(check-expect (list-file-paths dir0) empty)

(define (list-file-paths a-dir)
  (cond [(empty? (dir-contents a-dir)) empty]
        [else
         (map (lambda (x) (string-append "/" (dir-name a-dir) x))
              (local
                [;; (list-file-paths-fdlist a-fdlist) produces a list that 
                 ;;   contains the hierarchical names for all of the files 
                 ;;   in a-fdlist
                 ;; list-file-paths-fdlist: FDList -> (list Str)
                 (define (list-file-paths-fdlist a-fdlist)
                   (cond [(empty? a-fdlist) empty]
                         [(dir? (first a-fdlist))
                          (append (list-file-paths (first a-fdlist))
                                  (list-file-paths-fdlist (rest a-fdlist)))]
                         [else
                          (cons (string-append "/"
                                               (filedata-name (first a-fdlist)))
                                (list-file-paths-fdlist (rest a-fdlist)))]))]
                
                (list-file-paths-fdlist (dir-contents a-dir))))]))

;; Tests
(check-expect (list-file-paths dir3) (list
                                      "/onesies/onefile/oldfile"
                                      "/onesies/onefilea/oldfile"
                                      "/onesies/onefileb/oldfile"))
(check-expect (list-file-paths fs2) (list "/u/newfile" "/u/onefile/oldfile"))
(check-expect (list-file-paths fs1)
              (list
               "/rootdir/oldfile"
               "/rootdir/onefile/oldfile"
               "/rootdir/twofiles/oldfile"
               "/rootdir/twofiles/newfile"
               "/rootdir/onesies/onefile/oldfile"
               "/rootdir/onesies/onefilea/oldfile"
               "/rootdir/onesies/onefileb/oldfile"
               "/rootdir/newfile"))

;; ==== Question 2f ============================================================

;; (backup-fs a-dir s) produces a Dir with the same directory as a-dir
;;   containing only the files whose format is s
;; backup-fs: Dir Sym -> (anyof Dir empty)
;; Example:
(check-expect (backup-fs dir0 'txt) empty)

(define (backup-fs a-dir s)
  (cond [(empty? (dir-contents a-dir)) empty]
        [else (make-dir
               (dir-name a-dir)
               (local
                 [;; (backup-fs-fdlist a-fdlist) eliminates the files in 
                  ;;   a-fdlist whose format is not s and the directories
                  ;;   in a-fdlist whose FDList is empty
                  ;; backup-fs-fdlist: FDList -> FDList
                  (define (backup-fs-fdlist a-fdlist)
                    (cond [(empty? a-fdlist) empty]
                          [(and (dir? (first a-fdlist))
                                (empty? (dir-contents (first a-fdlist))))
                           (backup-fs-fdlist (rest a-fdlist))]
                          [(dir? (first a-fdlist))
                           (cons (backup-fs (first a-fdlist) s)
                                 (backup-fs-fdlist (rest a-fdlist)))]
                          [(not (symbol=? s (filedata-format (first a-fdlist))))
                           (backup-fs-fdlist (rest a-fdlist))]
                          [else (cons (first a-fdlist)
                                      (backup-fs-fdlist (rest a-fdlist)))]))]
                 
                 (backup-fs-fdlist (dir-contents a-dir))))]))

;; Tests
(check-expect (backup-fs dir2 'txt)
              (make-dir "twofiles" (list file1)))
(check-expect (backup-fs dir4 'txt)
              (make-dir "hola" (list dir1a)))
(check-expect (backup-fs dir3 'txt)
              (make-dir "onesies" (list dir1 dir1a dir1b)))
(check-expect (backup-fs fs1 'txt)
              (make-dir "rootdir" (list file1 dir1 dir2b dir3)))

