(set! %load-path (cons "." %load-path))
(define font-path (string-append (getcwd) "/data/fonts/JetBrainsMono-Regular.ttf"))

(use-modules
  (buffer))

(define +WHITE+ '(255 255 255 255))
(define **background-color** '(#x28 #x28 #x28 255))

(define example-code)
(define *screen*)
(define *font*)
(define *current-buffer*)
(define **running** #f)
(define *shift* #f)
(define *edit-debounce* #f)
(define empty (string->symbol ""))

(define (traverse f x)
  (if (pair? x)
    (f (map (lambda (y) (traverse f y)) x))
    (f x)))



(define (map-quote-to-sexpr x)
  (traverse
    (lambda (y)
      (sexpr #f y))
    x))

(define code
  (call-with-input-file "buffer.scm"
    (lambda (f)
      (reverse
       (let loop ((out '()))
         (let ((x (read f)))
           (if (eof-object? x)
             out
             (loop (cons x out)))))))))

(define (process-text-input key)
  (if *edit-debounce*
    (set! *edit-debounce* #f)
    (when (sexpr-buffer/edit? *current-buffer*)
      (sexpr-buffer/input! *current-buffer* key)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))))



(define (process-key-pressed key)
  (if (sexpr-buffer/edit? *current-buffer*)
   (cond
     ((equal? key "escape")
      (sexpr-buffer/stop-edit! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "backspace")
      (sexpr-buffer/input-delete! *current-buffer*))

     (else '()))
   (cond
     ((string-contains key "shift")
      (set! *shift* #t))

     ;;((equal? key "/")
     ;; (sexpr-buffer/search! *current-buffer*))

     ((equal? key "c")
      (sexpr-buffer/insert! *current-buffer* (sexpr #f empty))
      (sexpr-buffer/delete! *current-buffer*)
      (sexpr-buffer/start-edit! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*)
      (set! *edit-debounce* #t))

     ((equal? key "u")
      (sexpr-buffer/undo! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "r")
      (sexpr-buffer/redo! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "o")
      (sexpr-buffer/toggle-split! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((and (not *shift*) (equal? key "space"))
      (sexpr-buffer/insert! *current-buffer* (sexpr #f empty))
      (sexpr-buffer/next! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*)
      (sexpr-buffer/start-edit! *current-buffer*)
      (set! *edit-debounce* #t))

     ((and *shift* (equal? key "i"))
      (when (sexpr-buffer/insert-in! *current-buffer* (sexpr #f empty))
        (sexpr-buffer/down! *current-buffer*)
        (sexpr-buffer/start-edit! *current-buffer*)
        (screen/clear! *screen*)
        (sexpr-buffer/write *current-buffer* *screen*)
        (set! *edit-debounce* #t)))

     ((equal? key "i")
      (sexpr-buffer/insert-back! *current-buffer*) (sexpr #f empty)
      (sexpr-buffer/prev! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*)
      (sexpr-buffer/start-edit! *current-buffer*)
      (set! *edit-debounce* #t))

     ((equal? key ";")
      (sexpr-buffer/toggle-closer/farther! *current-buffer*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "d")
      (sexpr-buffer/yank! *current-buffer*)
      (sexpr-buffer/delete! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "y")
      (sexpr-buffer/yank! *current-buffer*))

     ((equal? key "p")
      (sexpr-buffer/paste! *current-buffer*)
      (sexpr-buffer/next! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((and *shift* (equal? key "o"))
      (sexpr-buffer/insert-back! *current-buffer* (sexpr #f '()))
      (sexpr-buffer/prev! *current-buffer*)
      (sexpr-buffer/insert-in! *current-buffer* (sexpr #f empty))
      (sexpr-buffer/down! *current-buffer*)
      (sexpr-buffer/start-edit! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*)
      (set! *edit-debounce* #t))

     ((and *shift* (equal? key "space"))
      (sexpr-buffer/insert-list! *current-buffer*)
      (sexpr-buffer/next! *current-buffer*)
      (sexpr-buffer/insert-in! *current-buffer* (sexpr #f empty))
      (sexpr-buffer/down! *current-buffer*)
      (sexpr-buffer/start-edit! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*)
      (set! *edit-debounce* #t))

     ((and *shift* (equal? key "j")
       (sexpr-buffer/offset! *current-buffer* (p/y+ (sexpr-buffer/offset *current-buffer*) 4))
       (screen/clear! *screen*)
       (sexpr-buffer/write *current-buffer* *screen*)))

     ((and *shift* (equal? key "k")
       (sexpr-buffer/offset! *current-buffer* (p/y+ (sexpr-buffer/offset *current-buffer*) -4))
       (screen/clear! *screen*)
       (sexpr-buffer/write *current-buffer* *screen*)))

     ((and  (equal? key "j"))
      (sexpr-buffer/goto-down! *current-buffer*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((and (equal? key "k"))
      (sexpr-buffer/goto-up! *current-buffer*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((and *shift* (equal? key "l"))
      (sexpr-buffer/slurp-right! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((and *shift* (equal? key "h"))
      (sexpr-buffer/slurp-left! *current-buffer*)
      (screen/clear! *screen*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "l")
      (sexpr-buffer/goto-right! *current-buffer*)
      (sexpr-buffer/write *current-buffer* *screen*))

     ((equal? key "h")
      (sexpr-buffer/goto-left! *current-buffer*)
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



(define (loop)
  (let ((event (poll-event)))
    (when event
      (process-event event)))
  (begin-frame)
  (let ((size (get-window-size)))
    (draw-rect
      0 0
      (assoc-ref size 'width) (assoc-ref size 'height)
      **background-color**)

    (draw-buffer
      *font*
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
      (draw-text *font* (sexpr-buffer/editbuf *current-buffer*) 0 (- (assoc-ref size 'height) 30) +WHITE+)))


  (end-frame)
  (when **running**
    (loop)))

(define (start-editor)
  (initialize-window)
  (with-exception-handler
    (lambda (ex)
      (destroy-window)
      (raise-exception ex))

    (lambda ()
      (set! example-code (map-quote-to-sexpr code))
      (set! *current-buffer* (sexpr-buffer/mk example-code))
      (set! *screen* (screen/new 250 140))

      (sexpr-buffer/write *current-buffer* *screen*)

      (set! *font*
         (load-font font-path 24))
      (set! **running** #t)
      (loop)
      (destroy-window))))
