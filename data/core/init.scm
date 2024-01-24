(define font
  (load-font
    "/mnt/code/apps/mon/data/fonts/JetBrainsMono-Regular.ttf"
    24))

(define **running** #f)

(define +WHITE+ '(255 255 255 255))
(define **background-color** '(38 38 38 255))

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
    (draw-text font "yahallo!" 0 0 +WHITE+))
  (end-frame))

(set! **running** #t)
(let loop ()
  (when **running**
    (step)
    (loop)))
