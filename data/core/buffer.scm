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
     screen/set-character!
     screen/write-string!
     screen/write-sexpr!
     sexpr
     sexpr/datum
     sexpr/split?
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

(define (screen/write-string! screen s color p)
  (let* ((width (screen/width screen))
         (height (screen/height screen))
         (slen (string-length s))
         (s (string->utf32 s 'little))
         (x (p/x p))
         (y (p/y p)))

     (when (and (>= y 0) (< y height) (< x width))
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

(define color-paren '(100 100 100))
(define color-string '(#xb8 #xbb #x26))
(define color-symbol '(#xfb #xf1 #xc7))
(define color-number '(#xfe #x80 #x19))
(define color-white '(#xff #xff #xff))

(define (p/mk x y)
  (list x y))

(define (p/x point) (first point))
(define (p/y point) (second point))
(define (p/x+ p n)
  (p/mk
    (+ n (p/x p))
    (p/y p)))


(define (screen/write-list! screen split? ls begin-point)
  (let next
    ((ls ls)
     (first? #t)
     (point (screen/write-string! screen "(" color-paren begin-point)))

    (cond
      ((pair? ls)
       (next
         (cdr ls)
         #f
         (screen/write-sexpr! screen (car ls)
           (let ((point (if (or first? split?) point (screen/write-string! screen " " color-white point))))

              (if (and split? (not first?))
                (p/mk
                 (+ (p/x begin-point) 1)
                 (+ (p/y point) 1))

                point)))))


      ((null? ls)
       (screen/write-string! screen ")" color-paren point))

      (else
        (screen/write-string! screen ")" color-paren
          (screen/write-sexpr! screen ls
            (screen/write-string! screen " . " color-paren point)))))))

(define (screen/write-sexpr! screen sexpr point)

  (let ((datum (sexpr/datum sexpr))
        (split? (sexpr/split? sexpr)))

       (cond
          ((pair? datum)
           (screen/write-list! screen split? datum point))

          ((symbol? datum)
           (screen/write-string! screen (symbol->string datum) color-symbol point))

          ((string? datum)
           (screen/write-string! screen "\"" color-string
             (screen/write-string! screen datum color-string
               (screen/write-string! screen "\"" color-string point))))

          ((number? datum)
           (let ((str (number->string datum)))
             (screen/write-string! screen str color-number point)))

          (else (error "bad")))))

;;(define-record-type <cursor>
;;  (cursor/new sexpr index)
;;  cursor?
;;  (sexpr cursor/sexpr)
;;  ())
;;
;;
;;(define-record-type <sexpr-buffer>
;;  (sexpr-buffer/new-raw cursor toplevel)
;;  sexpr-buffer?
;;  (cursor sexpr-buffer/cursor sexpr-buffer/cursor!)
;;  (toplevel sexpr-buffer/toplevel))
;;
;;(define (sexpr-buffer/new toplevel)
;;  (sexpr-buffer/new-raw))
;;
;;(define (sexpr-buffer/up buf)
;;  ())
;;
;;(define (sexpr-buffer/down buf)
;;  ())
;;
;;(define (sexpr-buffer/next buf))
;;
;;(define (sexpr-buffer/prev buf))
;;
;;(define (sexpr-buffer/toggle-split buf))
;;
;;(define (sexpr-buffer/toggle-edit buf))
;;
;;(define (sexpr-buffer/create-paren buf))
;;
;;(define (sexpr-buffer/create-value buf))
