#lang racket

(provide
 (contract-out
  ;; data
  [DIR/C contract?]
  [POSN/C contract?]
  [SNAKE/C contract?]
  [WORLD/C contract?]
  [struct posn ([x real?] [y real?])]
  [posn=? (POSN/C POSN/C . -> . boolean?)]
  [struct snake ([dir DIR/C] [segs (and/c cons? (listof POSN/C))])]
  [struct world ([snake SNAKE/C] [food POSN/C])]
  ;; const
  [WORLD WORLD/C]
  [BACKGROUND image/c]
  [FOOD-IMAGE image/c]
  [SEGMENT-IMAGE image/c]
  [GRID-SIZE real?]
  [BOARD-HEIGHT-PIXELS real?]
  [BOARD-WIDTH real?]
  [BOARD-HEIGHT real?]
  ;; collide
  [snake-wall-collide? (SNAKE/C . -> . boolean?)]
  [snake-self-collide? (SNAKE/C . -> . boolean?)]
  ;; cut-tail
  [cut-tail ((and/c cons? (listof POSN/C)) . -> . (listof POSN/C))]
  ;; motion-help
  [snake-slither (SNAKE/C . -> . SNAKE/C)]
  [snake-grow (SNAKE/C . -> . SNAKE/C)]
  ;; motion
  [world-change-dir (WORLD/C DIR/C . -> . WORLD/C)]
  [world->world (WORLD/C . -> . WORLD/C)]
  ;; handlers
  [handle-key (WORLD/C string? . -> . WORLD/C)]
  [game-over? (WORLD/C . -> . boolean?)]
  ;; scenes
  [world->scene (WORLD/C . -> . image/c)]
  [food+scene (POSN/C image/c . -> . image/c)]
  [place-image-on-grid (image/c real? real? image/c . -> . image/c)]
  [snake+scene (SNAKE/C image/c . -> . image/c)]
  [segments+scene ((listof POSN/C) image/c . -> . image/c)]
  [segment+scene (POSN/C image/c . -> . image/c)]
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Dummy functions `image`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct image ())
(define image/c (struct/c image))
(define (circle r m c) (image))
(define (empty-scene w h) (image))
(define (place-image i₁ r c i₂) (image))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct posn (x y) #:transparent)
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))
(struct snake (dir segs) #:transparent)
(struct world (snake food) #:transparent)
(define DIR/C (one-of/c 'up 'down 'left 'right))
(define POSN/C (struct/c posn real? real?))
(define SNAKE/C (struct/c snake DIR/C (and/c cons? (listof POSN/C))))
(define WORLD/C (struct/c world SNAKE/C POSN/C))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; const
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define GRID-SIZE 30)
(define BOARD-HEIGHT 20)
(define BOARD-WIDTH 30)
(define BOARD-HEIGHT-PIXELS (* GRID-SIZE BOARD-HEIGHT))
(define BOARD-WIDTH-PIXELS (* GRID-SIZE BOARD-WIDTH))
(define BACKGROUND (empty-scene BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS))
(define SEGMENT-RADIUS (/ GRID-SIZE 2))
(define SEGMENT-IMAGE  (circle SEGMENT-RADIUS "solid" "red"))
(define FOOD-RADIUS SEGMENT-RADIUS)
(define FOOD-IMAGE  (circle FOOD-RADIUS "solid" "green"))
(define WORLD (world (snake 'right (cons (posn 5 3) empty))
                       (posn 8 12)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; collide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; snake-wall-collide? : Snake -> Boolean
;; Is the snake colliding with any/c of the walls?
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))

;; head-collide? : Posn -> Boolean
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

;; snake-self-collide? : Snake -> Boolean
(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

;; segs-self-collide? : Posn Segs -> Boolean
(define (segs-self-collide? h segs)
  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; cut-tail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NeSegs is one of:
;; - (cons Posn empty)
;; - (cons Posn NeSegs)

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(define (cut-tail segs)
  (let ([r (cdr segs)])
    (cond [(empty? r) empty]
          [else (cons (car segs) (cut-tail r))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; motion-help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-head : Posn Direction -> Posn
;; Compute next position for head.
(define (next-head seg dir)
  (cond [(equal? 'right dir) (posn (add1 (posn-x seg)) (posn-y seg))]
        [(equal? 'left  dir) (posn (sub1 (posn-x seg)) (posn-y seg))]
        [(equal? 'down  dir) (posn (posn-x seg) (sub1 (posn-y seg)))]
        [else                (posn (posn-x seg) (add1 (posn-y seg)))]))

;; snake-slither : Snake -> Snake
;; move the snake one step
(define (snake-slither snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (cut-tail (snake-segs snk))))))

;; snake-grow : Snake -> Snake
;; Grow the snake one segment.
(define (snake-grow snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (snake-segs snk)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; motion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world->world : World -> World
(define (world->world w)
  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))
;; eating? : World -> Boolean
;; Is the snake eating the food in the world.
(define (eating? w)
  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))
;; snake-change-direction : Snake Direction -> Snake
;; Change the direction of the snake.
(define (snake-change-direction snk dir)
  (snake dir
         (snake-segs snk)))
;; world-change-dir : World Direction -> World
;; Change direction of the world.
(define (world-change-dir w dir)
  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))
;; snake-eat : World -> World
;; Eat the food and generate a new one.
(define (snake-eat w)
  (world (snake-grow (world-snake w))
         #;(posn (random BOARD-WIDTH) (random BOARD-HEIGHT))
         (posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle-key : World String -> World
(define (handle-key w ke)
  (cond [(equal? ke "w") (world-change-dir w 'up)]
        [(equal? ke "s") (world-change-dir w #|HERE|# 'd)]
        [(equal? ke "a") (world-change-dir w 'left)]
        [(equal? ke "d") (world-change-dir w 'right)]
        [else w]))

;; game-over? : World -> Boolean
(define (game-over? w)
  (or (snake-wall-collide? (world-snake w))
      (snake-self-collide? (world-snake w))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; scenes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world->scene : World -> Image
;; Build an image of the given world.
(define (world->scene w)
  (snake+scene (world-snake w)
               (food+scene (world-food w) BACKGROUND)))

;; food+scene : Food Image -> Image
;; Add image of food to the given scene.
(define (food+scene f scn)
  (place-image-on-grid FOOD-IMAGE (posn-x f) (posn-y f) scn))

;; place-image-on-grid : Image Number Number Image -> Image
;; Just like PLACE-IMAGE, but use grid coordinates.
(define (place-image-on-grid img x y scn)
  (place-image img
               (* GRID-SIZE x)
               (- BOARD-HEIGHT-PIXELS (* GRID-SIZE y))
               scn))

;; snake+scene : Snake Image -> Image
;; Add an image of the snake to the scene.
(define (snake+scene snk scn)
  (segments+scene (snake-segs snk) scn))

;; segments+scene : Segs Image -> Image
;; Add an image of the snake segments to the scene.
(define (segments+scene segs scn)
  (cond [(empty? segs) scn]
        [else (segments+scene (cdr segs) ;; tail recursion
                              (segment+scene (car segs) scn))]))

;; segment+scene : Posn Image -> Image
;; Add one snake segment to a scene.
(define (segment+scene seg scn)
  (place-image-on-grid SEGMENT-IMAGE (posn-x seg) (posn-y seg) scn))
