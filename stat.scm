#!guile \
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


;;;; Data types

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

(define-record-type <normalized>
  (make-normalized name product times)
  normalized?
  (name normalized-name)
  (product normalized-product set-normalized-product!)
  (times normalized-times set-normalized-times!))

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


;;;; Constants

(define native-code-compilers
  '(Bigloo Chez Chicken Gambit GuileTjit Ikarus Larceny MIT Pycket Racket))

(define byte-code-compilers
  '(Gauche Kawa MITBc RacketNoJit Sagittarius))

(define interpreters
  '(GuileInterp))

(define all-implementations
  (append native-code-compilers byte-code-compilers interpreters))

(define ignored-benchmarks
  '(slatex))


;;;; Parser

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


;;;; Analysis

(define (add-geometric-standard-score! tbl name results)
  (let* ((valid-results (filter (lambda (x) x) results))
         (n (length valid-results))
         (product-elapsed
          (let lp ((vs valid-results) (acc 1.0))
            (if (null? vs)
                acc
                (lp (cdr vs) (* acc (line-data-elapsed (car vs)))))))
         (gmean (expt product-elapsed (/ 1 n)))
         (log-sigma
          (sqrt
           (/ (apply +
                     (map (lambda (x)
                            (let ((m (- (log (line-data-elapsed x))
                                        (log gmean))))
                              (* m m)))
                          valid-results))
              (- n 1))))
         (z-scores
          (map (lambda (x)
                 (make-z (line-data-vm x)
                         (/ (log (/ (line-data-elapsed x) gmean))
                            log-sigma)
                         (line-data-elapsed x)))
               valid-results))
         (standard-score
          (make-standard-score name n gmean (exp log-sigma) z-scores)))
    (hashq-set! tbl name standard-score)))

(define (summarize line-datas numeric-sort?)
  "Summarize LINE-DATAS.

Shows results normalized to vm-regular for each benchmark, and geometric
mean of all benchmarks. Sorts results numerically if NUMERIC-SORT? is
true, otherwise sorts alphabetically by benchmark name."
  (let ((names (delete-duplicates! (map line-data-name line-datas)))
        (products (make-hash-table))
        (missings (make-hash-table))
        (standard-scores (make-hash-table)))
    (define (update-normalized! vm-name bench-name normalized)
      (let ((current (hashq-ref products vm-name #f)))
        (if current
            (begin
              (set-normalized-product! current
                                       (* (normalized-product current)
                                          normalized))
              (set-normalized-times! current
                                     (cons (cons bench-name normalized)
                                           (normalized-times current))))
            (hashq-set! products vm-name
                        (make-normalized vm-name normalized
                                         (list (cons bench-name
                                                     normalized)))))))
    (define (add-missing! vm-name bench-name)
      (let* ((old (hashq-ref missings vm-name))
             (new (if old (cons bench-name old) (list bench-name))))
        (hashq-set! missings vm-name new)))
    (define (format-gm name)
      (let* ((normalized (hashq-ref products name))
             (p (normalized-product normalized))
             (n (length (normalized-times normalized)))
             (gm (expt p (/ 1 n))))
        (format #t "~16@s: ~6,3f (~s benchmarks" name gm n)
        (let ((missing-results (hashq-ref missings name)))
          (when missing-results
            (format #t ", missing: ~{~a~^, ~}" missing-results)))
        (display ")")
        (newline)))
    (let lp ((names names) (acc '()))
      (match names
        ((name . names)
         (let ((total-elapsed-datas
                (filter
                 (lambda (line-data)
                   (and (eq? name (line-data-name line-data))
                        (not (memq name ignored-benchmarks))
                        (eq? 'total (line-data-computation line-data))))
                 line-datas)))
           (define (data-by-vm vm)
             (find (lambda (data)
                     (eq? vm (line-data-vm data)))
                   total-elapsed-datas))
           (define (update-one! regular vm-name)
             (let ((vm (data-by-vm vm-name)))
               (if (and vm regular)
                   (let ((n (/ (line-data-elapsed vm)
                               (line-data-elapsed regular))))
                     (update-normalized! vm-name name n))
                   (add-missing! vm-name name))))
           (match total-elapsed-datas
             ((? pair?)
              (let* ((regular (data-by-vm 'Guile))
                     (vms (map data-by-vm native-code-compilers)))
                (for-each (lambda (impl)
                            (update-one! regular impl))
                          all-implementations)
                (add-geometric-standard-score! standard-scores name vms)
                (lp names (cons name acc))))
             (_
              (format #t "Skipping `~s'~%" name)
              (lp names acc)))))
        (_
         (format #t "Geometric standard scores of native code compilers:~%")
         (print-standard-scores standard-scores)
         (format #t "Geometric means normalized to vm-regular~%")
         (format #t "  Interpreters:~%")
         (format-gm 'GuileInterp)
         (format #t "  Byte code compilers:~%")
         ;; (format-gm 'Guile)
         (format-gm 'Gauche)
         (format-gm 'Sagittarius)
         (format-gm 'RacketNoJit)
         (format-gm 'MITBc)
         (format #t "  Native code compilers:~%")
         (format-gm 'Chez)
         (format-gm 'Bigloo)
         (format-gm 'Ikarus)
         (format-gm 'Pycket)
         (format-gm 'Gambit)
         (format-gm 'Larceny)
         (format-gm 'Racket)
         (format-gm 'GuileTjit)
         (format-gm 'Chicken)
         (format-gm 'MIT)
         (values products missings standard-scores))))))


;;;; Printer

(define (find-score-by-name impl scores)
  (z-score (find (lambda (z)
                   (eq? impl (z-name z)))
                 (standard-score-scores scores))))

(define (find-tjit-score scores)
  (find-score-by-name 'GuileTjit scores))

(define (find-pycket-score scores)
  (find-score-by-name 'Pycket scores))

(define (compare-tjit-scores a b)
  (match (cons a b)
    (((_ . a-scores) . (_ . b-scores))
     (< (find-tjit-score a-scores)
        (find-tjit-score b-scores)))))

(define (find-z-score impl scores)
  (let lp ((scores scores))
    (match scores
      ((score . scores)
       (if (eq? impl (z-name score))
           score
           (lp scores)))
      (_ #f))))

(define (compare-benchmark-times a b)
    (< (cdr a) (cdr b)))

(define (print-histogram-data products)
  (let* ((results (hash-map->list cons products))
         (tjit-times (normalized-times (hashq-ref products 'GuileTjit)))
         (interp-times (normalized-times (hashq-ref products 'GuileInterp)))
         (bench-names
          (map car (sort tjit-times compare-benchmark-times))))
    (call-with-output-file "hist.dat"
      (lambda (port)
        (format port "~12s  ~5a    ~5a~%" 'Benchmark 'Nash 'Interp)
        (do ((bench-names bench-names (cdr bench-names)))
            ((null? bench-names))
          (let* ((bench-name (car bench-names))
                 (tjit (assq-ref tjit-times bench-name))
                 (interp (assq-ref interp-times bench-name)))
            (format port "~12s  ~5,3f    ~5,3f~%" bench-name tjit interp)))))))

(define (print-standard-scores standard-scores)
  (for-each
   (match-lambda
     ((name . ($ <standard-score> name n mean sigma scores))
      (format #t "~s: n=~s gmean=~5,3f gsd=~5,3f~%" name n mean sigma)
      (for-each
       (match-lambda
         (($ <z> name score elapsed)
          (format #t "~12@s: ~5,3@f (~7,2f ms)~%" name score elapsed)))
       (sort scores (lambda (a b)
                      (< (z-score a) (z-score b)))))))
   (sort (hash-map->list cons standard-scores) compare-tjit-scores)))

(define (print-tex-table products missings standard-scores)
  (call-with-output-file "tracingjits.inc"
    (lambda (port)
      (define (floored-int n)
        (inexact->exact (floor n)))
      (define (print-row bench-name)
        (match (hashq-ref standard-scores bench-name)
          (($ <standard-score> name n mean sigma scores)
           (let ((tjit (find-z-score 'GuileTjit scores))
                 (pycket (find-z-score 'Pycket scores)))
             (format port "~a & ~4,2f & ~5,2@f & ~5,2@f \\\\~%"
                     name sigma
                     (z-score tjit)
                     (z-score pycket))))))
      (format port "\\begin{tabular}{rrcc}~%")
      (format port " & GSD & Nash & Pycket \\\\~%")
      (format port "\\toprule~%")
      (for-each print-row '(sumfp mbrot sum array1 sumloop))
      (format port "\\midrule~%")
      (for-each print-row '(matrix peval dynamic))
      (format port "\\midrule~%")
      (for-each print-row '(ctak fibc))
      (format port "\\midrule~%")
      (for-each print-row '(fibfp simplex nucleic parsing))
      (format port "\\end{tabular}~%"))))

(define (print-distribution-data standard-scores)
  (let* ((scores (hash-map->list cons standard-scores))
         (scores (sort scores compare-tjit-scores))
         (bench-names (map car scores)))
    (define (print-points file name)
      (call-with-output-file file
        (lambda (port)
          (format port "~a~%~{~a~%~}"
                  name
                  (map (lambda (bench-name)
                         (let ((std-score
                                (hashq-ref standard-scores bench-name)))
                           (find-score-by-name name std-score)))
                       bench-names)))))
    (call-with-output-file "dist.dat"
      (lambda (port)
        (format port "# ~{~a ~}~%" bench-names)
        (do ((impls native-code-compilers (cdr impls)))
            ((null? impls))
          (let ((impl (car impls)))
            (do ((bench-names bench-names (cdr bench-names)))
                ((null? bench-names))
              (let ((bench-name (car bench-names)))
                (match (hashq-ref standard-scores bench-name)
                  (($ <standard-score> name n mean sigma scores)
                   (let ((z (find-z-score impl scores)))
                     (format port "~a " (if z (z-score z) 'n/a))))
                  (_ (values)))))
            (newline port)))))
    (print-points "dist-nash.dat" 'GuileTjit)
    (print-points "dist-pycket.dat" 'Pycket)))

(define (print-geometric-means products missings)
  (define (format-gm port name)
    (let* ((normalized (hashq-ref products name))
           (p (normalized-product normalized))
           (n (length (normalized-times normalized)))
           (gm (expt p (/ 1 n))))
      (format port "~s & ~6,3f & ~s & " name gm n)
      (let ((missing-results (hashq-ref missings name)))
        (when missing-results
          (format port "~{~a~^, ~} " missing-results)))
      (display "\\\\" port)
      (newline port)))
  (define impls
    '(Chez
      Bigloo Ikarus Pycket Gambit Larceny Racket GuileTjit Chicken MIT))
  (call-with-output-file "gm-native.dat"
    (lambda (port)
      (format port "\\begin{tabular}{r|ccl}~%")
      (format port "\\toprule~%")
      (format port "& G-mean & N & Failed \\\\~%")
      (format port "\\midrule~%")
      (do ((impls impls (cdr impls)))
          ((null? impls))
        (format-gm port (car impls)))
      (format port "\\bottomrule~%")
      (format port "\\end{tabular}~%"))))


;;;; Main

(define (main args)
  (define (display-usage)
    (display "USAGE: stat.scm [OPTIONS] FILE

OPTIONS:
  -n : sort numerically by normalized result.
"))
  (let* ((grammer `((nsort (required? #f)
                           (value #f)
                           (single-char #\n))))
         (opts (getopt-long args grammer))
         (nsort (option-ref opts 'nsort #f))
         (result-file-path (option-ref opts '() #f)))
    (match result-file-path
      ((path)
       (call-with-values
           (lambda ()
             (summarize (parse-file path) nsort))
         (lambda (products missings standard-scores)
           (print-tex-table products missings standard-scores)
           (print-histogram-data products)
           (print-distribution-data standard-scores)
           (print-geometric-means products missings))))
      (_ (display-usage)))))
