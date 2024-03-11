(define-module
  (buffer)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)

  #:export
    (screen/new
     screen?
     screen/width
     screen/height
     screen/buffer
     screen/red
     screen/green
     screen/blue
     screen/character
     screen/clear!
     screen/write-string!
     screen/write-sexpr!
     sexpr
     sexpr/datum
     sexpr/split?
     sexpr-buffer/mk
     sexpr-buffer/write
     sexpr-buffer/next!
     sexpr-buffer/prev!
     sexpr-buffer/down!
     sexpr-buffer/up!
     sexpr-buffer/goto-left!
     sexpr-buffer/goto-right!
     sexpr-buffer/jump-left!
     sexpr-buffer/jump-right!
     sexpr-buffer/goto-down!
     sexpr-buffer/goto-up!
     sexpr-buffer/delete!
     sexpr-buffer/toggle-split!
     sexpr-buffer/insert-list!
     sexpr-buffer/insert!
     sexpr-buffer/insert-back!
     sexpr-buffer/insert-in!
     sexpr-buffer/slurp-right!
     sexpr-buffer/slurp-left!
     sexpr-buffer/yank!
     sexpr-buffer/paste!
     sexpr-buffer/toggle-closer/farther!
     sexpr-buffer/start-edit!
     sexpr-buffer/stop-edit!
     sexpr-buffer/undo!
     sexpr-buffer/redo!
     sexpr-buffer/editbuf
     sexpr-buffer/edit?
     sexpr-buffer/input!
     sexpr-buffer/input-delete!
     sexpr-buffer/offset
     sexpr-buffer/offset!
     p/mk
     p/x
     p/x+
     p/y
     p/y+))



;;(define (buffer/to-utf8 screen)
;;  ()

;;(define (screen/new w h))

(define-record-type <screen>
  (screen/new-raw width height buffer red green blue)
  screen?
  (width screen/width)
  (height screen/height)
  (buffer screen/buffer)
  (red screen/red)
  (green screen/green)
  (blue screen/blue))

(define (screen/clear! screen)
  (bytevector-fill! (screen/buffer screen) 0))

(define (screen/new width height)
  (let ((buffer (make-bytevector (* width height 4) 0))
        (red (make-bytevector (* width height) 127))
        (green (make-bytevector (* width height) 127))
        (blue (make-bytevector (* width height) 127)))

    (screen/new-raw
      width
      height
      buffer
      red
      green
      blue)))

;;(define (screen/color screen x y)
;;  (list
;;    (bytevector-u8-ref
;;      (screen/red screen)
;;      (+ (* (screen/width screen) y) x))
;;    (bytevector-u8-ref
;;      (screen/green screen)
;;      (+ (* (screen/width screen) y) x))
;;    (bytevector-u8-ref
;;      (screen/blue screen)
;;      (+ (* (screen/width screen) y) x))))
;;
;;(define (screen/character screen x y)
;;  (bytevector-u32-native-ref
;;     (screen/buffer screen)
;;     (+ (* (screen/width screen) y) x)))
;;
;;(define (screen/set-character! screen x y character color)
;;  (bytevector-u32-native-set!
;;   (screen/buffer screen)
;;   (+ (* (screen/width screen) y) x)
;;   (char->integer character)
;;
;;
;;   (bytevector-u8-ref
;;     (screen/red screen)
;;     (+ (* (screen/width screen) y) x))
;;   (bytevector-u8-ref
;;     (screen/green screen)
;;     (+ (* (screen/width screen) y) x))
;;   (bytevector-u8-ref
;;     (screen/blue screen)
;;     (+ (* (screen/width screen) y) x))))

(define (color/red color) (first color))
(define (color/green color) (second color))
(define (color/blue color) (third color))

(define (screen/write-string! screen s color inverted? p)
  (let* ((width (screen/width screen))
         (height (screen/height screen))
         (slen (string-length s))
         (s (string->utf32 s 'little))
         (x (p/x p))
         (y (p/y p)))

     (when (and (>= y 0) (< y height) (< x width))
         (if inverted?
           (for-each
             (lambda (i)
               (bytevector-u32-native-set!
                 s
                 (* i 4)
                 (+ (bytevector-u32-native-ref s (* i 4)) #x80000000)))
             (iota (floor/ (bytevector-length s) 4))))

         ;; write UTF-32 characters
         (bytevector-copy!
           s
           0
           (screen/buffer screen)
           (* 4 (+ (* width y) (min x width)))
           (min
             (* (max (- width x) 0) 4)
             (bytevector-length s)))


         (let
           ((fill-color
              (lambda (buffer color)
                (bytevector-fill!
                  buffer
                  color

                  (+ (* width y) (min x width))
                  (+ (* width y) (min (+ x slen) width))))))

           ;; write colors
           (fill-color (screen/red screen) (color/red color))
           (fill-color (screen/green screen) (color/green color))
           (fill-color (screen/blue screen) (color/blue color))))

    (p/x+ p slen)))

(define-record-type <sexpr>
  (sexpr split? datum)
  sexpr?
  (split? sexpr/split? sexpr/split!)
  (datum sexpr/datum sexpr/datum!))

(define (sexpr/list? sexpr)
  (pair? (sexpr/datum sexpr)))

(define color-paren '(100 100 100))
(define color-green '(#xb8 #xbb #x26))
(define color-white '(#xfb #xf1 #xc7))
(define color-red '(#xcc #x24 #x1d))
(define color-constant '(#xfe #x80 #x19))
(define color-purple '(#xb1 #x62 #x86))
(define color-yellow '(#xd7 #x99 #x21))
;;(define color-white '(#xff #xff #xff))

(define procedures
  '(
    procedure?
    first
    second
    third
    last
    car
    cdr
    eq?
    equal?
    eqv?
    error
    map
    pair?
    null?
    display
    newline
    *
    +
    /
    floor/
    -
    <
    >=
    <=
    >
    =
    list-ref
    list-set!
    list?
    reverse
    cons
    symbol?
    not
    boolean?
    pair?
    number?
    list
    keyword?
    string?))

(define keywords
  '(define
    if
    and
    or
    define-record-type
    let*
    cond
    or
    else
    let
    when
    unless
    lambda
    use-modules
    define-module
    set!
    quote))


(define (p/mk x y)
  (list x y))

(define (p/x! x p)
  (p/mk x (p/y p)))


(define (p/y! y p)
  (p/mk (p/x p) y))

(define (p/x point) (first point))
(define (p/y point) (second point))
(define (p/x+ p n)
  (p/mk
    (+ n (p/x p))
    (p/y p)))

(define (p/y+ p n)
  (p/mk
    (p/x p)
    (+ n (p/y p))))

(define (screen/write-list! screen cursor split? ls begin-point)
  (let next
    ((ls ls)
     (first? #t)
     (point begin-point))

    (cond
      ((pair? ls)
       (next
         (cdr ls)
         #f
         (screen/write-sexpr! screen cursor (car ls) split?
           (let
             ((point
                (if
                  (or first? split?)
                  point
                  (screen/write-string! screen " " color-white #f point))))

             (if (and split? (not first?))
               (p/mk
                (+ (p/x begin-point) 0)
                (+ (p/y point) 1))

               point)))))

      ((null? ls)
       point) ;; exit function

      (else
        (screen/write-sexpr! screen cursor ls split?
           (screen/write-string! screen " . " color-white #f point))))))

(define (assert x)
  (display x)
  (unless x (error "assert failure")))

(define-record-type <cursor>
  (cursor/mk path farther?)
  cursor?
  (path cursor/path)
  (farther? cursor/farther?))

(define (screen/write-sexpr! screen cursor sexpr parent-split? point)

  (let* ((selected? (eq? (cursor/current cursor) sexpr))
         (datum (sexpr/datum sexpr))
         (split? (and (sexpr/split? sexpr) parent-split?))
         (in-path? (and (not selected?) (memq sexpr (cursor/path cursor)))))

       (cond
          ((pair? datum)
           (screen/write-string!
             screen
             ")"
             (cond
               (selected? (if (cursor/farther? cursor) color-white color-paren))
               (in-path? color-white)
               (else color-paren))
             selected?
             ((if split?
                (lambda (p) (p/mk (+ (p/x point) 0) (+ (p/y p) 1)))
                identity)
              (screen/write-list!
                 screen
                 cursor
                 split?
                 datum
                 (screen/write-string!
                   screen
                   "("
                   (cond
                     (selected? (if (cursor/farther? cursor) color-paren color-white))
                     (in-path? color-white)
                     (else color-paren))
                   selected?
                   point)))))

          ((symbol? datum)
           (screen/write-string!
             screen
             (symbol->string datum)
             (cond
               ((member datum keywords) color-red)
               ((member datum procedures) color-green)
               (else color-white))
             selected?
             point))

          ((string? datum)
           (screen/write-string! screen "\"" color-green selected?
             (screen/write-string! screen datum color-green selected?
               (screen/write-string! screen "\"" color-green selected? point))))

          ((number? datum)
           (let ((str (number->string datum)))
             (screen/write-string! screen str color-constant selected? point)))

          ((null? datum)
           (screen/write-string! screen "()" color-white selected? point))

          ((keyword? datum)
           (screen/write-string! screen (symbol->string (keyword->symbol datum)) color-purple selected?
             (screen/write-string! screen "#:" color-purple selected? point)))

          ((boolean? datum)
           (if datum
            (screen/write-string! screen "#t" color-constant selected? point)
            (screen/write-string! screen "#f" color-constant selected? point)))

          (else (error datum)))))

(define (cursor/current cursor)
  (first (cursor/path cursor)))

(define (cursor/get-up cursor)
  (second (cursor/path cursor)))

(define (cursor/has-up? cursor)
  (> (length (cursor/path cursor)) 1))

(define (cursor/leaf? cursor)
  (not (sexpr/list? (cursor/current cursor))))

(define-record-type <sexpr-buffer>
  (sexpr-buffer/mk-raw cursor toplevel editbuf offset clipboard undo-list redo-list)
  sexpr-buffer?
  (cursor sexpr-buffer/cursor sexpr-buffer/cursor!)
  (toplevel sexpr-buffer/toplevel)
  (editbuf sexpr-buffer/editbuf sexpr-buffer/editbuf!)
  (offset sexpr-buffer/offset sexpr-buffer/offset!)
  (clipboard sexpr-buffer/clipboard sexpr-buffer/clipboard!)
  (undo-list sexpr-buffer/undo-list sexpr-buffer/undo-list!)
  (redo-list sexpr-buffer/redo-list sexpr-buffer/redo-list!))

;;(define (sexpr-buffer/cursor! buf cursor)
;;  (sexpr-buffer/cursor-raw! buf cursor)
;;  (let ((current-y (cursor/current cursor)))
;;    (cond
;;        (>= (p/y ())))))

(define (sexpr-buffer/mk toplevel)
  (sexpr-buffer/mk-raw (cursor/mk (list toplevel) #f) toplevel '() (p/mk 0 0) '() '() '()))

(define (sexpr-buffer/edit? buf)
  (not (null? (sexpr-buffer/editbuf buf))))

(define (second-or-null x)
  (if (< (length x) 2)
    '()
    (second x)))

(define (last-or-null x)
  (if (null? x)
    '()
    (last x)))

(define (cursor/closer cursor)
  (cursor/mk (cursor/path cursor) #f))

(define (cursor/farther cursor)
  (cursor/mk (cursor/path cursor) #t))

(define (cursor/next cursor)
  (if (cursor/has-up? cursor)
    (let*
        ((current (cursor/current cursor))
         (up (cursor/get-up cursor))
         (next (second-or-null (find-tail (lambda (sexpr) (eq? sexpr current)) (sexpr/datum up)))))

        (if (not (null? next))
          (cursor/mk (cons next (cdr (cursor/path cursor))) #f)
          #f))
    #f))

(define (cursor/next-or-up cursor)
  (let ((next (cursor/next cursor)))
    (if (not next)
      (cursor/farther (cursor/up cursor))
      next)))


(define (cursor/prev-or-up cursor)
  (let ((prev (cursor/prev cursor)))
    (if (not prev)
      (cursor/closer (cursor/up cursor))
      prev)))


(define (cursor/skip cursor)
  ;; move away from current sexpr no matter what (prefers siblings and next over previous)
  ;; returns #f if can't move away
  (if (cursor/has-up? cursor)
    (let*
        ((current (cursor/current cursor))
         (up (cursor/get-up cursor))
         (next (second-or-null (find-tail (lambda (sexpr) (eq? sexpr current)) (sexpr/datum up))))
         (prev (last-or-null
                 (take-while
                   (lambda (sexpr) (not (eq? sexpr current)))
                   (sexpr/datum up)))))

        (cond
          ((not (null? next))
           (cursor/mk (cons next (cdr (cursor/path cursor))) #f))
          ((not (null? prev))
           (cursor/mk (cons prev (cdr (cursor/path cursor))) #t))
          (else
           (cursor/up cursor))))

    #f))


(define (cursor/prev cursor)
  (if (cursor/has-up? cursor)
    (let*
        ((current (cursor/current cursor))
         (up (cursor/get-up cursor))
         (prev (last-or-null
                 (take-while
                   (lambda (sexpr) (not (eq? sexpr current)))
                   (sexpr/datum up)))))

        (if (not (null? prev))
          (cursor/mk (cons prev (cdr (cursor/path cursor))) #t)
          #f))
    #f))

(define (cursor/down cursor)
  (let*
    ((current (cursor/current cursor))
     (path (cursor/path cursor))
     (datum (sexpr/datum current)))
    (if (and (not (cursor/leaf? cursor)) (not (null? datum)))
      (cursor/mk (cons (first datum) path) #f)
      cursor)))

(define (cursor/up cursor)
  (let* ((path (cursor/path cursor)))
    (if (cursor/has-up? cursor)
      (cursor/mk (cdr path) (cursor/farther? cursor))
      cursor)))

(define (sexpr-buffer/up! buf)
  (sexpr-buffer/cursor! buf (cursor/up (sexpr-buffer/cursor buf))))

(define (sexpr-buffer/down! buf)
  (sexpr-buffer/cursor! buf (cursor/down (sexpr-buffer/cursor buf))))

(define (sexpr-buffer/next! buf)
  (sexpr-buffer/cursor! buf (cursor/next (sexpr-buffer/cursor buf))))

(define (sexpr-buffer/prev! buf)
  (sexpr-buffer/cursor! buf (cursor/prev (sexpr-buffer/cursor buf))))


(define (sexpr-buffer/goto-up! buf)
  (sexpr-buffer/cursor! buf (cursor/goto-up (sexpr-buffer/cursor buf))))

(define (sexpr-buffer/goto-down! buf)
  (sexpr-buffer/cursor! buf (cursor/goto-down (sexpr-buffer/cursor buf))))

(define (sexpr-buffer/goto-left! buf)
  (sexpr-buffer/cursor! buf (cursor/goto-left (sexpr-buffer/cursor buf))))

(define (sexpr-buffer/goto-right! buf)
  (sexpr-buffer/cursor! buf (cursor/goto-right (sexpr-buffer/cursor buf))))

(define (sexpr-buffer/delete! buf)
  (when (cursor/has-up? (sexpr-buffer/cursor buf))
    (let* ((cursor (sexpr-buffer/cursor buf))
           (to-be-deleted (cursor/current cursor))
           (up (cursor/get-up cursor)))

      (sexpr-buffer/save-undo!
        buf
        (list up (sexpr/datum up)))

      (sexpr-buffer/cursor! buf (cursor/skip cursor))

      (sexpr/datum!
        up
        (filter
          (lambda (x)
            (not (eq? x to-be-deleted)))
          (sexpr/datum up))))))


(define (sexpr/traverse sexpr f)
  (f sexpr)
  (when (sexpr/list? sexpr)
    (for-each
       (lambda (x) (sexpr/traverse x f))
       (sexpr/datum sexpr))))

(define (sexpr/disable-split! sexpr) (sexpr/split! sexpr #f))
(define (sexpr/enable-split! sexpr) (sexpr/split! sexpr #t))

;;(define (sexpr/toggle-split! sexpr)
;;  (sexpr/split! sexpr (not (sexpr/split? sexpr))))

(define (sexpr-buffer/toggle-split! buf)
  (let* ((cursor (sexpr-buffer/cursor buf)))
    (when (cursor/leaf? cursor)
      (set! cursor (cursor/up cursor)))

    (if (sexpr/split? (cursor/current cursor))
      (sexpr/traverse (cursor/current cursor) sexpr/disable-split!)
      (for-each sexpr/enable-split! (cursor/path cursor)))))

(define (sexpr-buffer/push-undo! buf cmd)
  (sexpr-buffer/undo-list! buf (cons cmd (sexpr-buffer/undo-list buf))))

(define (sexpr-buffer/save-undo! buf cmd)
  (sexpr-buffer/push-undo! buf cmd)
  (sexpr-buffer/redo-list! buf '()))

(define (sexpr-buffer/insert-back! buf sexpr)
  (let ((undo-list (sexpr-buffer/undo-list buf))
        (cursor (sexpr-buffer/cursor buf)))
   (if (cursor/has-up? cursor)
     (let*
       ((current (cursor/current cursor))
        (up (cursor/get-up cursor))
        (datum-up (sexpr/datum up)))

       (sexpr-buffer/save-undo!
        buf
        (list up (sexpr/datum up)))

       (sexpr/datum! up
        (let loop ((res '()) (ls datum-up))
          (if (eq? (car ls) current)
             (append (reverse (cons sexpr res)) ls)
             (loop (cons (car ls) res) (cdr ls)))))))))

(define (sexpr-buffer/insert! buf sexpr)
  (let ((cursor (sexpr-buffer/cursor buf)))
    (if (cursor/has-up? cursor)
      (let*
        ((current (cursor/current cursor))
         (up (cursor/get-up cursor))
         (datum-up (sexpr/datum up)))

        (sexpr-buffer/save-undo!
          buf
          (list up (sexpr/datum up)))

        (sexpr/datum! up
          (let loop ((res '()) (ls datum-up))
            (if (eq? (car ls) current)
              (append (reverse res) (cons (car ls) (cons sexpr (cdr ls))))
              (loop (cons (car ls) res) (cdr ls)))))))))

(define (sexpr-buffer/insert-in! buf sexpr)
  (let ((cursor (sexpr-buffer/cursor buf)))
    (if (list? (sexpr/datum (cursor/current cursor)))
      (let*
         ((current (cursor/current cursor))
          (datum (sexpr/datum current)))
         (sexpr-buffer/save-undo! buf (list current (sexpr/datum current)))
         (sexpr/datum! current (cons sexpr datum))
         #t)
      #f)))


(define (sexpr-buffer/insert-in-end! buf sexpr)
  (let ((cursor (sexpr-buffer/cursor buf)))
    (if (list? (sexpr/datum (cursor/current cursor)))
      (let*
         ((current (cursor/current cursor))
          (datum (sexpr/datum current)))
         (sexpr-buffer/save-undo! buf (list current (sexpr/datum current)))
         (sexpr/datum! current (append datum (list sexpr)))
         #t)
      #f)))

(define (sexpr-buffer/insert-list! buf)
  (sexpr-buffer/insert! buf (sexpr #f '())))
  ;;(sexpr-buffer/next! buf))

(define (sexpr-buffer/start-edit! buf)
  (let* ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor)))
    (if (or
          (string? (sexpr/datum current))
          (symbol? (sexpr/datum current)))
      (sexpr-buffer/editbuf! buf ""))))

(define (insert-at x k ls)
  (cond ((null? ls)
         (list x))
        ((zero? k)
         (cons x ls))
        (else
         (cons (car ls)
               (insert-at x (- k 1) (cdr ls))))))

(define (sexpr-buffer/pop-undo! buf)
  (sexpr-buffer/undo-list! buf (cdr (sexpr-buffer/undo-list buf))))

(define (sexpr-buffer/push-redo! buf cmd)
  (sexpr-buffer/redo-list! buf (cons cmd (sexpr-buffer/redo-list buf))))

(define (sexpr-buffer/pop-redo! buf)
  (sexpr-buffer/redo-list! buf (cdr (sexpr-buffer/redo-list buf))))

(define (sexpr-buffer/undo! buf)
  (unless (null? (sexpr-buffer/undo-list buf))
    (let* ((cmd (car (sexpr-buffer/undo-list buf)))
           (sexpr (list-ref cmd 0))
           (undo-datum (list-ref cmd 1)))

      (sexpr-buffer/push-redo! buf (list sexpr (sexpr/datum sexpr)))

      (sexpr/datum! sexpr undo-datum)

      (sexpr-buffer/pop-undo! buf))))


(define (sexpr-buffer/redo! buf)
  (unless (null? (sexpr-buffer/redo-list buf))
    (let* ((cmd (car (sexpr-buffer/redo-list buf)))
           (sexpr (list-ref cmd 0))
           (redo-datum (list-ref cmd 1)))

      (sexpr-buffer/push-undo! buf (list sexpr (sexpr/datum sexpr)))

      (sexpr/datum! sexpr redo-datum)

      (sexpr-buffer/pop-redo! buf))))

(define (sexpr-buffer/stop-edit! buf)
  (let* ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor))
         (editbuf (sexpr-buffer/editbuf buf)))
    (if (> (string-length editbuf) 0)
      (begin
        (sexpr-buffer/save-undo! buf (list current (sexpr/datum current)))
        (cond
          ((symbol? (sexpr/datum current))
           (sexpr/datum! current (string->symbol editbuf)))
          ((string? (sexpr/datum current))
           (sexpr/datum! current (string-copy editbuf)))))
      (sexpr-buffer/delete! buf)))

  (sexpr-buffer/editbuf! buf '()))

(define (sexpr-buffer/extend-right! buf)
  (let* ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor)))
    (if (list? (sexpr/datum current))
      (let* ((next (cursor/next cursor)))
        (when next
          (let ((next-after? (cursor/next next)))
            (sexpr-buffer/next! buf)
            (sexpr-buffer/delete! buf)
            (when next-after?
             (sexpr-buffer/prev! buf))
            (sexpr-buffer/insert-in-end! buf (sexpr/make-unique (cursor/current next))))))
      '())))

(define (sexpr-buffer/extend-left! buf)
  (let* ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor)))
    (if (list? (sexpr/datum current))
      (let* ((prev (cursor/prev cursor)))
        (when prev
          (sexpr-buffer/prev! buf)
          (sexpr-buffer/delete! buf)
          (sexpr-buffer/insert-in! buf (sexpr/make-unique (cursor/current prev)))))
      '())))

(define (sexpr-buffer/shrink-left! buf)
  (let* ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor)))
    (if (pair? (sexpr/datum current))
      (let* ((last (cursor/last cursor)))
        (if (cursor/prev last)
          (begin
            (sexpr-buffer/cursor! buf last)
            (sexpr-buffer/delete! buf)
            (sexpr-buffer/up! buf)
            (sexpr-buffer/insert! buf (sexpr/make-unique (cursor/current last))))
          (begin
            (sexpr-buffer/cursor! buf last)
            (sexpr-buffer/delete! buf)
            (sexpr-buffer/insert! buf (sexpr/make-unique (cursor/current last))))))
      '())))

(define (sexpr-buffer/shrink-right! buf)
  (let* ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor)))
    (if (pair? (sexpr/datum current))
      (let* ((first (cursor/down cursor)))
        (when first
          (if (cursor/next first)
            (begin
              (sexpr-buffer/down! buf)
              (sexpr-buffer/delete! buf)
              (sexpr-buffer/up! buf)
              (sexpr-buffer/insert-back! buf (sexpr/make-unique (cursor/current first))))
            (begin
              (sexpr-buffer/down! buf)
              (sexpr-buffer/delete! buf)
              (sexpr-buffer/insert-back! buf (sexpr/make-unique (cursor/current first)))))))
      '())))


(define (sexpr-buffer/slurp-right! buf)
  (let ((cursor (sexpr-buffer/cursor buf)))
    (if (or (cursor/farther? cursor)
            (null? (sexpr/datum (cursor/current cursor))))
     (sexpr-buffer/extend-right! buf)
     (sexpr-buffer/shrink-right! buf))))

(define (sexpr-buffer/slurp-left! buf)
  (let ((cursor (sexpr-buffer/cursor buf)))
    (if (or (not (cursor/farther? cursor))
            (null? (sexpr/datum (cursor/current cursor))))
        (sexpr-buffer/extend-left! buf)
        (sexpr-buffer/shrink-left! buf))))

(define (sexpr-buffer/input! buf input)
 (cond
   ((equal? input "") '())

   ((and (equal? input " ")
         (symbol? (sexpr/datum (cursor/current (sexpr-buffer/cursor buf)))))
    (sexpr-buffer/stop-edit! buf)
    (sexpr-buffer/insert! buf (sexpr #f (string->symbol "")))
    (sexpr-buffer/next! buf)
    (sexpr-buffer/start-edit! buf))

   ((equal? (substring input 0 1) "\"")
    (sexpr-buffer/insert! buf (sexpr #f ""))
    (sexpr-buffer/delete! buf))

   ((equal? (substring input 0 1) "(")
    (sexpr-buffer/insert-list! buf)
    (sexpr-buffer/delete! buf)
    (sexpr-buffer/insert-in! buf (sexpr #f 'a))
    (sexpr-buffer/down! buf)
    (sexpr-buffer/input! buf (substring input 1)))

   (else
    (sexpr-buffer/editbuf!
      buf
      (string-append (sexpr-buffer/editbuf buf) input)))))


(define (sexpr-buffer/input-delete! buf)
  (let ((editbuf (sexpr-buffer/editbuf buf)))
    (when (> (string-length editbuf) 0)
      (sexpr-buffer/editbuf! buf (substring editbuf 0 (- (string-length editbuf) 1))))))

(define (sexpr-buffer/yank! buf)
  (let* ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor)))
    (sexpr-buffer/clipboard! buf current)))

(define (sexpr/make-unique x)
  (if (sexpr/list? x)
    (sexpr (sexpr/split? x) (map sexpr/make-unique (sexpr/datum x)))
    (sexpr (sexpr/split? x) (sexpr/datum x))))

(define (sexpr-buffer/paste! buf)
  (sexpr-buffer/insert! buf (sexpr/make-unique (sexpr-buffer/clipboard buf))))

(define (cursor/goto-up cursor)
  (let*
    ((current (cursor/current cursor)))
    (if (cursor/has-up? cursor)
      (if (and (not (cursor/leaf? cursor))
               (cursor/farther? cursor)
               (sexpr/split? current))
        (cursor/farther (cursor/last cursor))

        (if (sexpr/split? (cursor/get-up cursor))
          (cursor/prev-or-up cursor)
          (cursor/goto-up (cursor/closer (cursor/up cursor)))))
      cursor)))


(define (cursor/goto-down cursor)
  (let*
    ((current (cursor/current cursor)))
    (if (cursor/has-up? cursor)
      (if (and (not (cursor/leaf? cursor))
               (not (cursor/farther? cursor))
               (sexpr/split? current))
        (cursor/closer (cursor/down cursor))

        (if (sexpr/split? (cursor/get-up cursor))
          (cursor/next-or-up cursor)
          (cursor/goto-down (cursor/farther (cursor/up cursor)))))
      cursor)))

(define (cursor/last cursor)
  (let
    ((current (cursor/current cursor)))
    (if (cursor/leaf? cursor)
      cursor
      (cursor/mk
        (cons (last (sexpr/datum current)) (cursor/path cursor))
        #t))))

(define (cursor/goto-left cursor)
 (if (cursor/has-up? cursor)
   (cond

     ((and (not (cursor/leaf? cursor))
           (cursor/farther? cursor))
      (cursor/last cursor))

     (else
       (if (sexpr/split? (cursor/get-up cursor))
         (if (cursor/leaf? cursor)
           (cursor/prev-or-up cursor)
           (cursor/last cursor))
         (cursor/prev-or-up cursor))))

   cursor))

(define (cursor/goto-right cursor)
 (if (cursor/has-up? cursor)
   (cond

     ((and (not (cursor/leaf? cursor))
           (not (cursor/farther? cursor)))
      (cursor/down cursor))

     (else
       (if (sexpr/split? (cursor/get-up cursor))
         (if (cursor/leaf? cursor)
           (cursor/next-or-up cursor)
           (cursor/down cursor))
         (cursor/next-or-up cursor))))

   (cursor/down cursor)))

(define (sexpr-buffer/write buf screen)
  (let ((offset (sexpr-buffer/offset buf)))
    (screen/write-sexpr!
      screen
      (sexpr-buffer/cursor buf)
      (sexpr-buffer/toplevel buf)
      #t
      (p/mk (- (p/x offset)) (- (p/y offset))))))

(define (sexpr-buffer/toggle-closer/farther! buf)
  (let ((cursor (sexpr-buffer/cursor buf)))
    (if (cursor/leaf? cursor)
       (begin
         (sexpr-buffer/up! buf)
         (sexpr-buffer/toggle-closer/farther! buf))
       (sexpr-buffer/cursor!
         buf
         (cursor/mk
           (cursor/path cursor)
           (not (cursor/farther? cursor)))))))



;;(define (sexpr-buffer/prev buf))

;;(define (sexpr-buffer/toggle-split buf))
;;
;;(define (sexpr-buffer/toggle-edit buf))
;;
;;(define (sexpr-buffer/create-paren buf))
;;
;;(define (sexpr-buffer/create-value buf))
;;
;;(define (sexpr-buffer/delete buf))
