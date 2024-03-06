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
     sexpr-buffer/delete!
     sexpr-buffer/toggle-split!
     p/mk
     p/x
     p/y))



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
  (list? (sexpr/datum sexpr)))

(define color-paren '(100 100 100))
(define color-string '(#xb8 #xbb #x26))
(define color-symbol '(#xfb #xf1 #xc7))
(define color-number '(#xfe #x80 #x19))
(define color-white '(#xff #xff #xff))

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
         (screen/write-sexpr! screen cursor (car ls)
           (let ((point (if (or first? split?) point (screen/write-string! screen " " color-white #f point))))

              (if (and split? (not first?))
                (p/mk
                 (p/x begin-point)
                 (+ (p/y point) 1))

                point)))))

      ((null? ls)
       point) ;; exit function

      (else
        (screen/write-sexpr! screen cursor ls
           (screen/write-string! screen " . " color-white #f point))))))

(define (assert x)
  (display x)
  (unless x (error "assert failure")))

(define (screen/write-sexpr! screen cursor sexpr point)

  (let ((selected? (eq? (cursor/current cursor) sexpr))
        (datum (sexpr/datum sexpr))
        (split? (sexpr/split? sexpr)))

       (cond
          ((pair? datum)
           (screen/write-string! screen ")" (if selected? color-white color-paren) selected?
            (screen/write-list! screen cursor split? datum
             (screen/write-string! screen "(" (if selected? color-white color-paren) selected? point))))

          ((symbol? datum)
           (screen/write-string! screen (symbol->string datum) color-symbol selected? point))

          ((string? datum)
           (screen/write-string! screen "\"" color-string selected?
             (screen/write-string! screen datum color-string selected?
               (screen/write-string! screen "\"" color-string selected? point))))

          ((number? datum)
           (let ((str (number->string datum)))
             (screen/write-string! screen str color-number selected? point)))

          ((null? datum)
           (screen/write-string! screen "()" color-white selected? point))

          (else (error "bad")))))

(define-record-type <cursor>
  (cursor/mk path)
  cursor?
  (path cursor/path))

(define (cursor/current cursor)
  (first (cursor/path cursor)))

(define (cursor/up cursor)
  (second (cursor/path cursor)))

(define (cursor/has-up? cursor)
  (> (length (cursor/path cursor)) 1))

(define (cursor/leaf? cursor)
  (not (sexpr/list? (cursor/current cursor))))

(define-record-type <sexpr-buffer>
  (sexpr-buffer/mk-raw cursor toplevel)
  sexpr-buffer?
  (cursor sexpr-buffer/cursor sexpr-buffer/cursor!)
  (toplevel sexpr-buffer/toplevel))

(define (sexpr-buffer/mk toplevel)
  (sexpr-buffer/mk-raw (cursor/mk (list toplevel)) toplevel))

(define (sexpr-buffer/up buf)
  (sexpr-buffer/cursor!
    buf
    (cursor/up (sexpr-buffer/cursor buf))))

(define (second-or-null x)
  (if (< (length x) 2)
    '()
    (second x)))

(define (last-or-null x)
  (if (null? x)
    '()
    (last x)))

(define (sexpr-buffer/down! buf)
  (let*
    ((cursor (sexpr-buffer/cursor buf))
     (current (cursor/current cursor))
     (path (cursor/path cursor))
     (datum (sexpr/datum current)))
    (when (and (not (cursor/leaf? cursor)) (not (null? datum)))
      (sexpr-buffer/cursor! buf (cursor/mk (cons (first datum) path))))))


(define (sexpr-buffer/up! buf)
  (let*
    ((cursor (sexpr-buffer/cursor buf))
     (path (cursor/path cursor)))
    (when (cursor/has-up? cursor)
      (sexpr-buffer/cursor! buf (cursor/mk (cdr path))))))


(define (sexpr-buffer/next! buf)
  (if (cursor/has-up? (sexpr-buffer/cursor buf))
    (let*
        ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor))
         (up (cursor/up cursor))
         (next (second-or-null (find-tail (lambda (sexpr) (eq? sexpr current)) (sexpr/datum up)))))

        (if (not (null? next))
          (sexpr-buffer/cursor! buf (cursor/mk (cons next (cdr (cursor/path cursor)))))
          (begin
            (sexpr-buffer/up! buf))))))


(define (sexpr-buffer/prev! buf)
  (if (cursor/has-up? (sexpr-buffer/cursor buf))
    (let*
        ((cursor (sexpr-buffer/cursor buf))
         (current (cursor/current cursor))
         (up (cursor/up cursor))
         (prev (last-or-null
                 (take-while
                   (lambda (sexpr) (not (eq? sexpr current)))
                   (sexpr/datum up)))))

        (if (not (null? prev))
          (sexpr-buffer/cursor! buf (cursor/mk (cons prev (cdr (cursor/path cursor)))))
          (sexpr-buffer/up! buf)))))

(define (sexpr-buffer/delete! buf)
  (when (cursor/has-up? (sexpr-buffer/cursor buf))
    (let ((to-be-deleted (cursor/current (sexpr-buffer/cursor buf))))
      (sexpr-buffer/up! buf)
      (let ((current (cursor/current (sexpr-buffer/cursor buf))))
         (sexpr/datum!
           current
           (filter
             (lambda (x)
               (not (eq? x to-be-deleted)))
             (sexpr/datum current)))))))


(define (sexpr/traverse sexpr f)
  (f sexpr)
  (when (sexpr/list? sexpr)
    (for-each
       (lambda (x) (sexpr/traverse x f))
       (sexpr/datum sexpr))))

(define (sexpr/split-off! sexpr) (sexpr/split! sexpr #f))

(define (sexpr-buffer/toggle-split! buf)
  (let ((current (cursor/current (sexpr-buffer/cursor buf))))
    (if (sexpr/split? current)
       (sexpr/traverse current sexpr/split-off!) ; toggle off
       (sexpr/split! current #t)))) ; toggle on

(define (sexpr-buffer/write buf screen)
  (screen/write-sexpr!
    screen
    (sexpr-buffer/cursor buf)
    (sexpr-buffer/toplevel buf)
    (p/mk 0 0)))


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
