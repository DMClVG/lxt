(define font
  (load-font
    "/mnt/code/apps/mon/data/fonts/JetBrainsMono-Regular.ttf"
    12))


(set! %load-path (cons "." %load-path))
(use-modules
  (buffer))

(define **running** #f)

(define +WHITE+ '(255 255 255 255))
(define **background-color** '(#x28 #x28 #x28 255))

(define *screen* (screen/new 80 60))

(define (traverse f x)
  (if (pair? x)
    (f (map (lambda (y) (traverse f y)) x))
    (f x)))

(define (map-quote-to-sexpr x)
  (traverse
    (lambda (y)
      (sexpr #t y))
    x))

(define code
  (call-with-input-file "buffer.scm"
    (lambda (f)
      (read f))))

(display code)
(newline)

;;(define example-code
;;  (map-quote-to-sexpr
;;    '(if (equal? "banana" "apple")
;;       '()
;;       ((display "banana is apple"))
;;       (display "banana is not apple"))))


(define example-code (map-quote-to-sexpr code))
(define *current-buffer* (sexpr-buffer/mk example-code))

(sexpr-buffer/write *current-buffer* *screen*)

(define *shift* #f)
(define *edit-debounce* #f)

(define (process-text-input key)
  (if *edit-debounce*
    (set! *edit-debounce* #f)
    (when (sexpr-buffer/edit? *current-buffer*)
      (sexpr-buffer/input! *current-buffer* key))))

(define (process-key-pressed key)
  (if (sexpr-buffer/edit? *current-buffer*)
   (cond
     ((or
        (equal? key "escape")
        (equal? key "space"))
      (sexpr-buffer/stop-edit! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "backspace")
      (sexpr-buffer/input-delete! *current-buffer*))

     (else '()))
   (cond
     ((string-contains key "shift")
      (set! *shift* #t))

     ((equal? key "c")
      (sexpr-buffer/start-edit! *current-buffer*)
      (set! *edit-debounce* #t))

     ((equal? key "a")
      (sexpr-buffer/insert! *current-buffer* (sexpr #f 'a))
      (sexpr-buffer/next! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*)
      (sexpr-buffer/start-edit! *current-buffer*)
      (set! *edit-debounce* #t))

     ((equal? key "i")
      (when (sexpr-buffer/insert-in! *current-buffer* (sexpr #f 'a))
        (sexpr-buffer/down! *current-buffer*)
        (sexpr-buffer/start-edit! *current-buffer*)
        (screen/clear! *screen*)
        (sexpr-buffer/write *current-buffer* *screen*)
        (set! *edit-debounce* #t)))

     ((equal? key "d")
      (sexpr-buffer/delete! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "space")
      (sexpr-buffer/toggle-split! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "o")
      (sexpr-buffer/insert-list! *current-buffer*)
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
      (sexpr-buffer/write *current-buffer* *screen*)))))

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

    ('text-input
     (let ((text (assoc-ref event 'text)))
       (process-text-input text)))

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
      **background-color**)

    (when (sexpr-buffer/edit? *current-buffer*)
      (draw-text font (sexpr-buffer/editbuf *current-buffer*) 0 (- (assoc-ref size 'height) 30) +WHITE+)))


  (end-frame))

(set! **running** #t)
(let loop ()
  (when **running**
    (step)
    (loop)))
