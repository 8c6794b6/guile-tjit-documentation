(use-modules (ice-9 time))

(define *bailout* 16.0)
(define *max-iterations* 1000)

(define (mandelbrot x y)
  (let ((cr (- y 0.5))
        (ci x)
        (zi 0.0)
        (zr 0.0))
    (let lp ((i 0) (zr zr) (zi zi))
      (if (< *max-iterations* i)
          0
          (let ((zi2 (* zi zi))
                (zr2 (* zr zr)))
            (if (< *bailout* (+ zi2 zr2))
                i
                (lp (+ i 1)
                    (+ (- zr2 zi2) cr)
                    (+ (* 2.0 zr zi) ci))))))))

(define (mandelbrot-main size)
  (do ((y (- (- size 1)) (+ y 1)))
      ((= y (- size 1)))
    (newline)
    (do ((x (- (- size 1)) (+ x 1))
         (output '()
                 (let* ((size (exact->inexact size))
                        (c (if (zero? (mandelbrot (/ x size) (/ y size)))
                               #\*
                               #\space)))
                   (cons c output))))
        ((= x (- size 1))
         (display (list->string output)))))
  (newline))

(define (main args)
  (let* ((argc (length args))
         (n (or (and (<= 2 argc)
                     (string->number (cadr args))))))
    (time (mandelbrot-main n))))

(time (mandelbrot-main 80))
