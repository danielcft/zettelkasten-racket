#lang racket/base

(require racket/date
         racket/file
         racket/port
         racket/sequence
         racket/string
         racket/system)

(provide add
         delete
         edit
         ls
         grep
         view)

; globals
(define zet-dir (getenv "ZET_DIR"))
(define editor (getenv "EDITOR"))
(define (zet-path zet-id)
  (string-append zet-dir "/" (substring zet-id 0 2) "/" zet-id ".md"))

;; Add a new zet
(define (add)
  (let* ([date-str (string-replace
                    (date->string (current-date)
                                  (date-display-format 'iso-8601))
                    (regexp "[-:T]")
                    "")]
         [zet-id (substring date-str 2 12)]
         [zet-file (zet-path zet-id)])
    (with-output-to-file zet-file
                         (lambda () (printf "# ~a TITLE\n\ntags:#\n\n" zet-id)))
    (void (system (string-append editor " " zet-file)))))

;; Edit an existing zet
(define (edit zet-id)
  (void (system/exit-code (string-append editor " " (zet-path zet-id)))))

;; View a specific zet
(define (view zet-id)
  (let ([zet-file (zet-path zet-id)])
    (when (file-exists? zet-file)
      (with-input-from-file
       zet-file
       (lambda () (displayln (port->string (current-input-port))))))))

;; Delete an existing zet
(define (delete zet-id)
  (let ([zet-file (zet-path zet-id)])
    (when (file-exists? zet-file)
      (delete-file zet-file))))

;; List all zets
(define (ls)
  (let ([display-header (λ (filename)
                          (displayln (list-ref (file->lines filename) 0)))])
    (treewalk display-header)))

;; Grep through all zets
; TODO: replace cond with when (result eq 0)
(define (grep regex)
  (letrec ([cat-file (λ (filename) (displayln (file->string filename)))]
           [display-file-on-match
            (λ (filename)
              (cond
                [(system (string-append "grep -Eq " regex " " filename))
                 (cat-file filename)]))])
    (treewalk display-file-on-match)))

(define (treewalk func)
  (letrec ([is-zet-file? (λ (filepath-str)
                           (string-contains? filepath-str ".md"))]
           [all-zets (filter is-zet-file?
                             (map path->string
                                  (sequence->list (in-directory zet-dir))))])
    (for ([filepath all-zets])
      (func filepath))))
