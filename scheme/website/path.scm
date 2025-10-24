;; :REF:
;; :ID: 634abe5e-cab0-4ee5-af3e-ab4cd4d8f229

(define-module (website path))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-13))

(define (Path#components path)
  (string-split path #\/))

(define (Path#directory-check p)
  (unless (and (string? p) (file-is-directory? p))
    (raise-exception (format #f "p is not the path of a directory. p = ~a" p))))

(define (Path#file-is-regular? path)
  (eq? (stat:type (stat path)) 'regular))

(define (Path#regular-check p)
  (unless (and (string? p) (Path#file-is-regular? p))
    (raise-exception (format #f "p is not the path of a regular file. p = ~a" p))))

(define (Path#mtime p)
  (stat:mtime (stat p)))

(define (Path#join . parts)
  (string-join parts "/"))

(define (Path#extension path)
  (let* ((name (basename path))
         (dot-pos (string-rindex name #\.)))
    (if dot-pos
        (string-drop name (+ dot-pos 1))
        #f)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Path#components
        Path#extension
        Path#mtime
        Path#directory-check
        Path#regular-check
        Path#join)
