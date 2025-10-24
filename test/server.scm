(define-module (test server)
  #:use-module (srfi srfi-64)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (server)
  #:use-module (parser)
  #:use-module (maybe)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors))

(test-begin "server")

(define test-port 9876)

(define (body->string body)
  (if (bytevector? body)
      (utf8->string body)
      body))

(test-group "HTTP protocol"
  (let* ((home-uuid "00000000-0000-0000-0000-000000000000")
         (articles-uuid "00000000-0000-0000-0000-000000000001")
         (home-path (string-append "/" home-uuid "/html"))
         (articles-path (string-append "/" articles-uuid "/html"))
         (dummy-parser (Parser '(#:mk "/dummy/start.el")))
         (dummy-library
          (lambda (msg)
            (match msg
              (`(#:html ,uuid)
               (cond ((string=? uuid home-uuid)
                      (just "<html><body>home content</body></html>"))
                     ((string=? uuid articles-uuid)
                      (just "<html><body>articles content</body></html>"))
                     (else nothing)))
              (`(#:text ,uuid)
               (cond ((string=? uuid home-uuid)
                      (just "home org content"))
                     ((string=? uuid articles-uuid)
                      (just "articles org content"))
                     (else nothing)))
              (`(#:data ,uuid ,filename) nothing)
              (unexpected
               (error (format #f "Unexpected library msg: ~a" unexpected))))))
         (server-param (Server `(#:mk ,dummy-library ,dummy-parser)))
         (server (server-param `(#:mk ,dummy-library #:Library)))
         (server-thread (call-with-new-thread
                         (lambda ()
                           (server `(#:start ,test-port))))))

    (sleep 1)  ; give server time to start

    (test-group "GET / redirects to home"
      (let ((uri (string-append "http://localhost:" (number->string test-port) "/")))
        (receive (response body)
            ;; AI: Fixed typo `http$get` → `http-get`
            (http-get uri)
          (test-equal "returns 301 redirect" 301 (response-code response))
          (let* ((loc-val (assq-ref (response-headers response) 'location))
                 (loc-str (if (string? loc-val)
                              loc-val
                              (uri->string loc-val))))
            (test-assert "redirect location includes home path"
                         (string-contains loc-str home-path))))))

    (test-group "GET home page content"
      (let ((uri (string-append "http://localhost:" (number->string test-port) home-path)))
        (receive (response body)
            (http-get uri)
          (test-equal "returns 200 status" 200 (response-code response))
          (test-assert "returns HTML containing 'home'"
                       (string-contains (body->string body) "home")))))

    (test-group "GET /articles redirects to articles index"
      (let ((uri (string-append "http://localhost:" (number->string test-port) "/articles")))
        (receive (response body)
            (http-get uri)
          (test-equal "returns 301 redirect" 301 (response-code response))
          (let* ((loc-val (assq-ref (response-headers response) 'location))
                 (loc-str (if (string? loc-val)
                              loc-val
                              (uri->string loc-val))))
            (test-assert "redirect location includes articles path"
                         (string-contains loc-str articles-path))))))

    (test-group "GET articles page content"
      (let ((uri (string-append "http://localhost:" (number->string test-port) articles-path)))
        (receive (response body)
            (http-get uri)
          (test-equal "returns 200 status" 200 (response-code response))
          (test-assert "returns HTML containing 'articles'"
                       (string-contains (body->string body) "articles")))))

    (test-group "GET /health"
      (let ((uri (string-append "http://localhost:" (number->string test-port) "/health")))
        (receive (response body)
            (http-get uri)
          (let* ((status (response-code response))
                 (body-str (body->string body))
                 (json-body (json-string->scm body-str)))
            (test-equal "returns 200 status" 200 status)
            (test-equal "returns JSON with health field"
                        "ok"
                        (assoc-ref json-body "health"))))))

    (test-group "GET /<unknown-uuid>/html"
      (let ((uri (string-append "http://localhost:" (number->string test-port)
                                "/123e4567-e89b-12d3-a456-426614174000/html")))
        (receive (response body)
            (http-get uri)
          (test-equal "returns 404 for unknown UUID" 404 (response-code response)))))

    (test-group "GET /<unknown-uuid>/org"
      (let ((uri (string-append "http://localhost:" (number->string test-port)
                                "/123e4567-e89b-12d3-a456-426614174000/org")))
        (receive (response body)
            (http-get uri)
          (test-equal "returns 404 for unknown UUID" 404 (response-code response)))))

    (test-group "GET /unknown"
      (let ((uri (string-append "http://localhost:" (number->string test-port) "/unknown")))
        (receive (response body)
            (http-get uri)
          (test-equal "returns 404 for unknown path" 404 (response-code response)))))

    (test-group "POST /"
      (let ((uri (string-append "http://localhost:" (number->string test-port) "/")))
        (receive (response body)
            (http-post uri)
          (test-equal "returns 400 for non-GET method" 400 (response-code response)))))

    (cancel-thread server-thread)))

(test-end "server")
