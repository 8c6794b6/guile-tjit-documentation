#!./meta/guile \
-e main
!#
;;;; stat.scm - script for analyzing benchmark results.
;;;;
;;;; Script to get normalized result and geometric mean from benchmark
;;;; result from pycket-bench.

(use-modules (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 rdelim)
             ((srfi srfi-1) #:select (delete-duplicates! find))
             (srfi srfi-9)
             (srfi srfi-9 gnu))

;; Record type to represent single line of benchmark data.
(define-record-type <line-data>
  (make-line-data datetime elapsed unit computation name vm suite
                  n1 n2 specialize)
  line-data?
  (datetime line-data-datetime)
  (elapsed line-data-elapsed)
  (unit line-date-unit)
  (computation line-data-computation)
  (name line-data-name)
  (vm line-data-vm)
  (suite line-data-suite)
  (n1 line-data-n1)
  (n2 line-data-n2)
  (specialize line-data-specialize))

(define-record-type <z>
  (make-z name score elapsed)
  z?
  (name z-name)
  (score z-score set-z-score!)
  (elapsed z-elapsed set-z-slapsed!))

(define-record-type <standard-score>
  (make-standard-score name n mean sigma scores)
  standard-score?
  (name standard-score-name)
  (n standard-score-n)
  (mean standard-score-mean)
  (sigma standard-score-sigma)
  (scores standard-score-scores))

(define (parse-file path)
  "Make list of <line-data> from file PATH."
  (define (parse-line line)
    (if (eq? #\# (string-ref line 0))
        #f
        (call-with-input-string line
          (lambda (port)
            (let lp ((read (read-delimited "\t" port)) (acc '()))
              (match read
                ((? eof-object?)
                 (match (reverse! acc)
                   ((datetime elapsed unit computation name vm suite _
                              n1 n2 _ specialize)
                    (let ((data
                           (make-line-data datetime
                                           (string->number elapsed)
                                           (string->symbol unit)
                                           (string->symbol computation)
                                           (string->symbol name)
                                           (string->symbol vm)
                                           (string->symbol suite)
                                           (string->number n1)
                                           (string->number n2)
                                           (string->symbol specialize))))
                      data))
                   (_ #f)))
                (_
                 (lp (read-delimited "\t" port) (cons read acc)))))))))
  (call-with-input-file path
    (lambda (port)
      (let lp ((line (%read-line port)) (acc '()))
        (if (eof-object? (car line))
            (filter line-data? acc)
            (let ((line-data (parse-line (car line))))
              (lp (%read-line port) (cons line-data acc))))))))

;; List of ignored tests.
(define ignored
  '(slatex))

(define (*-if-true a b)
  (if b (* a b) a))

(define native-code-compilers
  '(Bigloo Chez Chicken Gambit GuileTjit Larceny MIT Pycket Racket))

(define (add-standard-score! tbl name results)
  "Add standard score of benchmark NAME cauculated from RESULTS to TBL."
  (let* ((valid-results (filter (lambda (x) x) results))
         (n (length valid-results))
         (sum-elapsed
          (let lp ((vs valid-results) (acc 0))
            (if (null? vs)
                acc
                (lp (cdr vs) (+ acc (line-data-elapsed (car vs)))))))
         (mean (/ sum-elapsed n))
         (sigma
          (sqrt (/ (apply +
                          (map (lambda (x)
                                 (expt (- (line-data-elapsed x) mean) 2))
                               valid-results))
                   (- n 1))))
         (z-scores
          (map (lambda (x)
                 (make-z (line-data-vm x)
                         (/ (- (line-data-elapsed x) mean) sigma)
                         (line-data-elapsed x)))
               valid-results)))
    (hashq-set! tbl name (make-standard-score name n mean sigma z-scores))))

(define (summarize line-datas numeric-sort?)
  "Summarize LINE-DATAS.

Shows results normalized to vm-regular for each benchmark, and geometric
mean of all benchmarks. Sorts results numerically if NUMERIC-SORT? is
true, otherwise sorts alphabetically by benchmark name."
  (let ((names (delete-duplicates! (map line-data-name line-datas)))
        (counts (make-hash-table))
        (standard-scores (make-hash-table)))
    (define (increment-count! line-data)
      (let* ((name (line-data-vm line-data))
             (current (hashq-ref counts name))
             (next (or (and current (+ current 1)) 1)))
        (hashq-set! counts name next)))
    (let lp ((names names) (acc '())
             (ntjit 1.0) (nbigloo 1.0) (nchez 1.0) (nchicken 1.0)
             (ngambit 1.0) (ngauche 1.0) (ninterp 1.0) (nlarceny 1.0)
             (nmit 1.0) (npycket 1.0) (nracket 1.0) (nracketj 1.0))
      (match names
        ((name . names)
         (let ((total-elapsed-datas
                (filter
                 (lambda (line-data)
                   (and (eq? name (line-data-name line-data))
                        (not (memq name ignored))
                        (eq? 'total (line-data-computation line-data))))
                 line-datas)))
           (define (data-by-vm vm)
             (find (lambda (data)
                     (eq? vm (line-data-vm data)))
                   total-elapsed-datas))
           (match total-elapsed-datas
             ((? pair?)
              (let* ((regular (data-by-vm 'Guile))
                     (normalize
                      (lambda (name)
                        (let ((vm (data-by-vm name)))
                          (and vm regular
                               (increment-count! vm)
                               (/ (line-data-elapsed vm)
                                  (line-data-elapsed regular))))))
                     (normalized-tjit (normalize 'GuileTjit))
                     (normalized-bigloo (normalize 'Bigloo))
                     (normalized-chez (normalize 'Chez))
                     (normalized-chicken (normalize 'Chicken))
                     (normalized-gambit (normalize 'Gambit))
                     (normalized-gauche (normalize 'Gauche))
                     (normalized-interp (normalize 'GuileInterp))
                     (normalized-larceny (normalize 'Larceny))
                     (normalized-mit (normalize 'MIT))
                     (normalized-pycket (normalize 'Pycket))
                     (normalized-racket (normalize 'Racket))
                     (normalized-racketj (normalize 'RacketNoJit))
                     (vms (map data-by-vm native-code-compilers)))
                (add-standard-score! standard-scores name vms)
                (lp names
                    (cons (list name
                                normalized-tjit
                                normalized-chez
                                normalized-racket
                                normalized-pycket)
                          acc)
                    (*-if-true ntjit normalized-tjit)
                    (*-if-true nbigloo normalized-bigloo)
                    (*-if-true nchez normalized-chez)
                    (*-if-true nchicken normalized-chicken)
                    (*-if-true ngambit normalized-gambit)
                    (*-if-true ngauche normalized-gauche)
                    (*-if-true ninterp normalized-interp)
                    (*-if-true nlarceny normalized-larceny)
                    (*-if-true nmit normalized-mit)
                    (*-if-true npycket normalized-pycket)
                    (*-if-true nracket normalized-racket)
                    (*-if-true nracketj normalized-racketj))))
             (_
              (format #t "Skipping `~s'~%" name)
              (lp names acc ntjit nbigloo nchez nchicken ngambit ngauche
                  ninterp nlarceny nmit npycket nracket nracketj)))))
        (_
         (define gm-by-name
           (lambda (name product)
             (let ((gm (expt product (/ 1 (hashq-ref counts name 1)))))
               gm)))
         (format #t "Standard scores of native code compilers:~%")
         (for-each
          (match-lambda
            ((name . ($ <standard-score> name n mean sigma scores))
             (format #t "~s: n=~s mean=~s sd=~s~%" name n mean sigma)
             (for-each
              (match-lambda
                (($ <z> name z elapsed)
                 (format #t "~12@s: ~5,3@f (~7,2f ms)~%" name z elapsed)))
              (sort scores (lambda (a b)
                             (< (z-score a) (z-score b)))))))
          (sort (hash-map->list cons standard-scores)
                (lambda (a b)
                  (match (cons a b)
                    (((_ . a-scores) . (_ . b-scores))
                     (let ((tjit-score
                            (lambda (scores)
                              (z-score
                               (find (lambda (z)
                                       (eq? 'GuileTjit (z-name z)))
                                     (standard-score-scores scores))))))
                       (< (tjit-score a-scores)
                          (tjit-score b-scores))))))))
         (let ((acc (sort acc
                          (if numeric-sort?
                              (lambda (a b)
                                (< (cadr a) (cadr b)))
                              (lambda (a b)
                                (string<= (symbol->string (car a))
                                          (symbol->string (car b)))))))

               (format-gm
                (lambda (name gm)
                  (format #t "~16@s: ~6,3f (~s benchmarks) ~%"
                          name gm (hashq-ref counts name))))
               (gm-tjit (gm-by-name 'GuileTjit ntjit))
               (gm-bigloo (gm-by-name 'Bigloo nbigloo))
               (gm-chez (gm-by-name 'Chez nchez))
               (gm-chicken (gm-by-name 'Chicken nchicken))
               (gm-gambit (gm-by-name 'Gambit ngambit))
               (gm-gauche (gm-by-name 'Gauche ngauche))
               (gm-interp (gm-by-name 'GuileInterp ninterp))
               (gm-racket (gm-by-name 'Racket nracket))
               (gm-racketj (gm-by-name 'RacketNoJit nracketj))
               (gm-larceny (gm-by-name 'Larceny nlarceny))
               (gm-mit (gm-by-name 'MIT nmit))
               (gm-pycket (gm-by-name 'Pycket npycket)))
           ;; (format #t "~:{~12@s: ~5,3f ~5,3f ~5,3f ~5,3f~%~}" acc)
           (call-with-output-file "gm2.dat"
             (lambda (port)
               (format port "~18s ~5s ~5s ~5s ~5s~%"
                       'Benchmark 'Tjit 'Chez 'Racket 'Pycket)
               ;; (format port "~:{~18s ~5,3f ~5,3f ~5,3f ~5,3f~%~}" acc)
               ))
           (call-with-output-file "gm2-mean.dat"
             (lambda (port)
               (format port "~18s ~5s ~5s ~5s ~5s~%"
                       'Benchmark 'Tjit 'Chez 'Racket 'Pycket)
               ;; (format port "~18s ~5,3f ~5,3f ~5,3f ~5,3f~%"
               ;;         "geometric\nmean"
               ;;         gm-tjit gm-chez gm-racket gm-pycket)
               ))
           (format #t "Geometric means normalized to vm-regular~%")
           (format #t " Non-native compilers~%")
           (format-gm 'Gauche gm-gauche)
           (format-gm 'GuileInterp gm-interp)
           (format-gm 'RacketNoJit gm-racketj)
           (format #t " Native code compilers~%")
           (format-gm 'Bigloo gm-bigloo)
           (format-gm 'Chez gm-chez)
           (format-gm 'Chicken gm-chicken)
           (format-gm 'Gambit gm-gambit)
           (format-gm 'GuileTjit gm-tjit)
           (format-gm 'Larceny gm-larceny)
           (format-gm 'MIT gm-mit)
           (format-gm 'Pycket gm-pycket)
           (format-gm 'Racket gm-racket)))))))

(define (show-usage)
  (display "USAGE: gm2.scm [OPTIONS] FILE

OPTIONS:
  -n : sort numerically by normalized result.
"))

(define (main args)
  (let* ((grammer `((nsort (required? #f)
                           (value #f)
                           (single-char #\n))))
         (opts (getopt-long args grammer))
         (nsort (option-ref opts 'nsort #f))
         (result-file-path (option-ref opts '() #f)))
    (match result-file-path
      ((path) (summarize (parse-file path) nsort))
      (_      (show-usage)))))
