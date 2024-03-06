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

(define example-code
  (map-quote-to-sexpr
    '(if (equal? "banana" "apple")
       '()
       ((display "banana is apple"))
       (display "banana is not apple"))))

(define *current-buffer* (sexpr-buffer/mk example-code))

(sexpr-buffer/write *current-buffer* *screen*)

(define *shift* #f)

(define (process-key-pressed key)
  (cond
    ((string-contains key "shift")
     (set! *shift* #t))

    ((equal? key "d")
     (sexpr-buffer/delete! *current-buffer*)
     (screen/clear! *screen*)
     (sexpr-buffer/write *current-buffer* *screen*))

    ((equal? key "space")
     (sexpr-buffer/toggle-split! *current-buffer*)
     (screen/clear! *screen*)
     (sexpr-buffer/write *current-buffer* *screen*))

    ((and  (equal? key "j"))
     (sexpr-buffer/next! *current-buffer*)
     (sexpr-buffer/write *current-buffer* *screen*))

    ((and (equal? key "k"))
     (sexpr-buffer/prev! *current-buffer*)
     (sexpr-buffer/write *current-buffer* *screen*))

    ((equal? key "l")
     (sexpr-buffer/down! *current-buffer*)
     (sexpr-buffer/write *current-buffer* *screen*))

    ((equal? key "h")
     (sexpr-buffer/up! *current-buffer*)
     (sexpr-buffer/write *current-buffer* *screen*))))

(define (process-key-released key)
  (cond
    ((string-contains key "shift")
     (set! *shift* #f))))

(define (trace x)
  (write x)
  (newline))

(define (process-event event)
  (case (assoc-ref event 'type)

    ('quit
     (set! **running** #f))

    ('key-pressed
     (let ((key (assoc-ref event 'key)))
       (process-key-pressed key)))

    ('key-released
     (let ((key (assoc-ref event 'key)))
       (process-key-released key)))

    (else => trace))



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
      0
      **background-color**))


  (end-frame))

(set! **running** #t)
(let loop ()
  (when **running**
    (step)
    (loop)))
