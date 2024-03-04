(define font
  (load-font
    "/mnt/code/apps/mon/data/fonts/JetBrainsMono-Regular.ttf"
    24))


(set! %load-path (cons "." %load-path))
(use-modules
  (buffer))

(define **running** #f)

(define +WHITE+ '(255 255 255 255))
(define **background-color** '(#x28 #x28 #x28 255))

(define *screen* (screen/new 80 24))

(define (traverse f x)
  (if (pair? x)
    (f (map (lambda (y) (traverse f y)) x))
    (f x)))

(define (map-quote-to-sexpr x)
  (traverse
    (lambda (y)
      (sexpr #t y))
    x))

(screen/write-sexpr!
  *screen*
  (map-quote-to-sexpr
    '(if (equal? "banana" "apple")
       (display "banana is apple")
       (display "banana is not apple")))

  (p/mk 0 0))

(define (process-event event)
  (case (assoc-ref event 'type)
    ('quit (set! **running** #f)))
  (display event)
  (newline)
  '())



(define (step)
  (let ((event (poll-event)))
    (when event
      (process-event event)))
  (begin-frame)
  (let ((size (get-window-size)))
    (draw-rect
      0 0
      (assoc-ref size 'width) (assoc-ref size 'height)
      **background-color**)
;;    (draw-text font "yahallo!" 0 0 +WHITE+))

    (draw-buffer
      font
      (screen/buffer *screen*)
      (screen/red *screen*)
      (screen/green *screen*)
      (screen/blue *screen*)
      (screen/width *screen*)
      (screen/height *screen*)
      0
      0))


  (end-frame))

(set! **running** #t)
(let loop ()
  (when **running**
    (step)
    (loop)))
