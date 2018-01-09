;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; q2.rkt;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SYSTEM PURPOSE STATEMENT
;;This system mainly consists of three components
;; BallList
;; Racket
;; World


;; BallList list of balls described below
;; The Ball has four properties mainly x y vx vy
;; The velocity and position changes based on the object it is colliding
;; with and ball's previous values

;; The Racket is initially static
;; upon key events, it moves
;; the velocity of the racket is one of the factors
;; for the resultant velocity when they collde
;; It is draggable by the mouse and velocity remains as before 

;; The State is the one which holds one of the values of
;; READY RALLY PAUSED
;; READY is a static state before RALLY
;; When the ball and racket are moving the state is set to "RALLY""
;; Once it is stopped, it is set to "PAUSED" for the specified time


;; The Timer represents number of ticks left
;; used in transition from "PAUSED" to "READY"
;; calculated as soon as the user inputs the speed


;; The Speed is the number of seconds per tick (User input)
;; larger the number, slower the game will be
;; Obtained using the function "simulation"


;;***********************************************************
(require rackunit)
(require "extras.rkt")
(check-location "06" "q2.rkt")

(require 2htdp/universe)
(require 2htdp/image)


(
 provide
 simulation
 initial-world
 world-ready-to-serve?
 world-after-tick
 world-after-key-event
 world-balls
 world-racket
 ball-x
 ball-y
 racket-x
 racket-y
 ball-vx
 ball-vy
 racket-vx
 racket-vy
 world-after-mouse-event
 racket-after-mouse-event
 racket-selected?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ===============================================================

;;; simulation : PosReal -> World
;;; GIVEN: sec-per-second represents in seconds per tick
;;;         , the speed of the simulation
;;;        (so larger numbers run slower)
;;; EFFECT: runs the simulation, starting with the initial world
;;; RETURNS: the final state of the world

;; STRATEGY : Using simpler functions
;;; EXAMPLES:
;;;     (simulation 1) runs in super slow motion
;;;     (simulation 1/24) runs at a more realistic speed

;; ************ FUNCTION DEFINITION ****************************
(define (simulation sec-per-tick )
  (big-bang (initial-world sec-per-tick)
            (on-tick world-after-tick sec-per-tick )  ;; seconds per tick 
            (on-draw world-to-scene)
            (on-key  world-after-key-event)
            (on-mouse world-after-mouse-event ))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define SECONDS-PER-PAUSE   3)

(define pause-last-tick     1);; represents the last tick of pause stage
;; state will be changed based on this

(define  COURT-COLOR-WHITE "WHITE")
(define  COURT-COLOR-YELLOW "YELLOW")

;; dimensions of the COURT
(define COURT-WIDTH 425)
(define COURT-HEIGHT 649)

;;Scene constants
;; EMPTY-COURT  : For normal play
;; YELLOW-COURT : For end game indication. Displayed till reset
(define EMPTY-COURT
  (empty-scene COURT-WIDTH COURT-HEIGHT))  
;; 
(define YELLOW-COURT
  (empty-scene COURT-WIDTH COURT-HEIGHT COURT-COLOR-YELLOW))


(define LEFT-WALL-X   0  )
(define RIGHT-WALL-X  COURT-WIDTH )
(define FRONT-WALL-Y  0  )
(define BACK-WALL-Y  COURT-HEIGHT  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS


(define TYPE-SOLID "solid")
(define MOUSE-PTR-COLOR  "BLUE")
(define MOUSE-PTR-RADIUS 4)


(define BALL-RADIUS   3)      ;;represents the radius of the ball in pixels
(define BALL-COLOR    "BLACK") ;;represents the color  of the ball

;; the following represents the
;;initial x and y of the ball repectively
(define INITIAL-X      330)    
(define INITIAL-Y      384)    

;; the following represents the
;;initial vx and vy of the ball repectively
(define INITIAL-VX      0)   
(define INITIAL-VY      0)   


;;the following  represents the ball velocity
;;in X and Y axis repectively after ready state
(define BALL-VX-AFTER-READY  3) 
(define BALL-VY-AFTER-READY -9)



;;the following  represents the racket velocity
;;in X and Y axis repectively after ready state
(define RACKET-VX-AFTER-READY 0)
(define RACKET-VY-AFTER-READY 0)


;; RACKET-SELECTABLE-DIST represents
;; the maximum distance at which
;; the racket is selectable
;;   If Racket is at x1,y1 and mouse pointer is at x2,y2
;; The racket is selectable by mouse iff
;; the distance between them is <= RACKET-SELECTABLE-DIST
(define RACKET-SELECTABLE-DIST 25)



;; Mouse events :
;; The following are the constants for the mouse events
;; each defining an event accordingly
;; The actual name is the string given 
(define DRAG        "drag")
(define BUTTON-UP   "button-up")
(define BUTTON-DOWN "button-down")

;; *******************************************************************
;; DATA DEFINITIONS

;; REPRESENTATION:
;; A Ball is represented as structure(ball x y vx vy) 
;; with the following fields:

;; x    : Integer        the x-axis coordinate of the center of the ball,
;;                       relative to the origin of the scene.
;;                       in pixels

;; y    : Integer        the y-axis coordinate of the center of the ball,
;;                       relative to the origin of the scene.
;;                       in pixels

;; vx   : Integer        represents the velocity in x axis
;;                       i.e number of pixels that the ball
;;                       can move per tick
;;                       in x axis


;; vy   : Integer        represents the velocity in y axis
;;                       i.e number of pixels that the ball
;;                       can move per tick
;;                       in y axis

;; IMPLEMENTATION:
(define-struct ball (x y vx vy ))

;; CONSTRUCTOR TEMPLATE
;; (make-ball Integer Integer Integer Integer)

;; OBSERVER TEMPLATE
;; ball-fn : Ball -> ??
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))

;; ********************BALLLIST**********************
;; DATA DEFINITIONS

;; REPRESENTATION:
;; A BallList is represented as (list (ball1 ball2 ball3... balln))
;;                                list of balls
;; Each of the element in the list is of type Ball
;; with the following fields:

;; IMPLEMENTATION:

;; No seperate implementation,
;; as balls is a list of (make-ball x y  vx vy)

;; CONSTRUCTOR TEMPLATE:
;; empty
;; (cons ball)
;; -- WHERE
;;    ball  is a Ball


;; OBSERVER TEMPLATE:
;; balls-fn :  BallList -> ??
;; define (balls-fn balls-li)
;; (cond
;;     [empty? balls) ...]
;;     [else (...
;;           (first balls-li)
;;           (balls-fn        (rest  balls-li))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define NEW-BALL-RALLY
  (make-ball
   INITIAL-X INITIAL-Y BALL-VX-AFTER-READY BALL-VY-AFTER-READY))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define RACKET-LENGTH   47)    ;;represents the horizontal length
;;                               of the racket, in pixels
(define RACKET-BREADTH  7)     ;;represents the vertical length
;;                               of the racket, in pixels              
(define RACKET-COLOR    "GREEN") ;; represents the color of the racket 

;; DATA DEFINITIONS

;; REPRESENTATION:
;; A racket is represented as structure(racket x y vx vy) 
;; with the following fields:

;; x    : Integer        the x-axis coordinate of the center of the racket,
;;                       relative to the origin of the scene.
;;                       in pixels

;; y    : Integer        the y-axis coordinate of the center of the racket,
;;                       relative to the origin of the scene.
;;                       in pixels

;; vx   : Integer        represents the velocity in x axis
;;                       i.e number of pixels that the racket
;;                       can move per tick
;;                       in x axis


;; vy   : Integer        represents the velocity in y axis
;;                       i.e number of pixels that the racket
;;                       can move per tick
;;                       in y axis

;; mx  : Integer       represents the x co-od of mouse pointer

;; my  : Integer       represenets the x co-od of mouse pointer 

;; mev : Integer       represents the mouse event of type MouseEvent
;;                     may be drag, button-down or button-up


;; IMPLEMENTATION:
(define-struct racket (x y vx vy mx my mev))

;; CONSTRUCTOR TEMPLATE
;; (make-racket
;;      Integer Integer Integer Integer Integer Integer MouseEvent)


;; OBSERVER TEMPLATE
;; racket-fn : racket -> ??
(define (racket-fn r)
  (... (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       (racket-mx r)
       (racket-my r)
       (racket-mev r)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Definition of State
;; A State is one of
;; -- "READY"
;; -- "PAUSED"
;; -- "RALLY"


;; CONSTRUCTOR TEMPLATE: Not needed.

;; OBSERVER TEMPLATE:
;; state-fn : STATE -> ?
#;
(define (state-fn m)
  (cond
    [(string=? m "READY") ...]
    [(string=? m "PAUSED")  ...]
    [(string=? m "RALLY") ...]
    ))

;; CONSTANTS:
(define READY     "READY")    ;; represents the ready state
(define PAUSED    "PAUSED")    ;; represents the paused state
(define RALLY     "RALLY")    ;; represents the rally state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITIONS

;; Definition of Timer:
;; A Timer is a Non-negative Integer
;; It can hold any non negative integer values from 0
;; Example values:
;; 0, 1, 2, 3, 5, 100, ....

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITIONS

;; REPRESENTATION:
;; A World is represented as a struct
;;                         (make-world  )
;; INTERPRETATION:
;; balls       : BallList ,  represents a list of balls
;; racket     : Racket, represents a racket
;; state      : State,  represents the state
;; timer      : Timer,  represents the time remaining in seconds
;; speed      : Speed,  represents the time remaining in seconds

;; IMPLEMENTATION:
(define-struct world (balls racket state timer speed))

;; CONSTRCTOR TEMPLATE:
;; (make-world Ball Racket State Timer)

;; OBSERVER TEMPLATE:
;; world-fn : World -> ??
(define (world-fn w)
  (... (world-ball w)
       (world-racket w)
       (world-state w)
       (world-timer w)
       (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racket-left-end-x : Racket-> PosReal
;; racket-right-end-x
;; racket-top-end-x
;; racket-right-end-x

;; GIVEN   : r, Racket 

;; RETURNS : the left   end co-ordinate x
;;           the right  end co-ordinate x
;;           the top    end co-ordinate y
;;           the bottom end co-ordinate y

;; STRATEGY: Transcribing formula
;;           center + or - (length/2)

;;

;; **************FUNTION DEFINITIONS*************************************

( define (racket-left-end-x r)
   (- (racket-x r )  ( / RACKET-LENGTH 2) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( define (racket-right-end-x r)
   (+ (racket-x r ) ( / RACKET-LENGTH 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( define (racket-top-end-y r)
   (- (racket-y r ) ( / RACKET-BREADTH 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( define (racket-bottom-end-y r)
   (+ (racket-y r ) ( / RACKET-BREADTH 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state-after-key : KeyEvent State -> State

;; GIVEN   : key-pressed,     the key pressed (Comes in as String)
;;           current-state,   represented by State

;; RETURNS :  represented by State
;;            for space key on READY returns RALLY
;;            for space key on RALLY returns PAUSED
;;            for any   key on PAUSED returns PAUSED (no change)
;;            for any   key other space
;;                          on RALLY returns RALLY (no change)

;; STRATEGY: Divide into cases on
;;           combinations of current-state and key-pressed

;; EXAMPLE : TEST  cases are self explanatory

;; **************FUNTION DEFINITION************************************

(define (state-after-key    key-pressed current-state )
  ( cond
     ((and
       (string=? key-pressed   " ")
       (string=? current-state READY))     RALLY)

     ((and
       (string=? key-pressed   " ")
       (string=? current-state RALLY))     PAUSED)
     ((string=?   current-state PAUSED)    PAUSED)
     
     ((and
       (not
        (string=? key-pressed   " "))
       (string=? current-state RALLY))     RALLY)

     (else  current-state)))

;; **************************TESTS************************************

( begin-for-test
   (check-equal?
    (state-after-key " "   READY)
    RALLY
    " The output state must be RALLY" )
   (check-equal?
    (state-after-key "c"   READY)
    READY
    " The output state must be READY" )
   
   (check-equal?
    (state-after-key " "   RALLY)
    PAUSED
    " The output state must be PAUSED" )
   
   (check-equal?
    (state-after-key " "   PAUSED)
    PAUSED
    " The output state must be RALLY" )

   (check-equal?
    (state-after-key "k"   RALLY)
    RALLY
    " The output state must be RALLY" ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others

;; EXAMPLES:
;;  (world-after-key-event INITIAL-WORLD B-KEY)

;;   (make-world
;;    (list (make-ball 330 384 0 0))
;;    (make-racket 330 384 0 0 330 384 "button-up")
;;    READY 6 0.5)
;;
;; STRATEGY: Using constructor template of World on w and
;;            Combining simpler functions

(define (world-after-key-event   w kev)
  (make-world
   ( balls-after-key             kev  w )
   ( racket-vel-after-key-press  kev (world-racket w) )
   ( state-after-key             kev (world-state  w))
   ( world-timer                 w)
   ( world-speed                 w)))


;; ****************TESTS************************************
(begin-for-test
  (check-equal?
   (world-after-key-event INITIAL-WORLD B-KEY)

   (make-world
    (list (make-ball 330 384 0 0))
    (make-racket 330 384 0 0 330 384 "button-up")
    READY 6 0.5)

   "THe result world must be containing a ball with ready state"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balls-after-key : KeyEvent World => BallList

;; GIVEN   : ke, KeyEvent,  is a an event whenever any key is pressed
;;           w,  World, represents the world containing
;;                       BallList,Racket and state
;; RETURNS : (world-balls w) of type BallList where
;;           world-balls is given as a list
;;            with only one new ball if it was in READY STATE 

;;           A new ball is added to the list of balls- world-balls
;;           if it was in the state RALLY

;;          else returns the same world-balls as it was before

;; STRATEGY:  Dividing into cases of (world-state w)
;;            and combining simpler functions
;; ***********************FUNCTION DEFINITION********************
(define (balls-after-key  ke  w )
  (
   cond
    ((and
      (string=? (world-state w)  RALLY)
      ( is-key-event-b?  ke ))
     (append (world-balls w ) (list NEW-BALL-RALLY )))

    ((and
      ( string=? (world-state w ) READY)
      ( is-key-event-space?  ke ))
     (list NEW-BALL-RALLY ))
    
    (else (world-balls  w) ))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal?
   (balls-after-key
    " "
    (make-world (list
                 ( make-ball
                   INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY))
                (make-racket
                 INITIAL-X INITIAL-Y
                 BALL-VX-AFTER-READY BALL-VY-AFTER-READY
                 INITIAL-X INITIAL-Y BUTTON-UP)
                READY 100 3/100))
   (list(make-ball  INITIAL-X INITIAL-Y
                    BALL-VX-AFTER-READY BALL-VY-AFTER-READY))
   "Ready state ->Rally state: Velocities Should have been updated " )

  (check-equal?
   (balls-after-key  "b"
                     (make-world
                      ( list( make-ball 1 1 1 1))
                      (make-racket
                       INITIAL-X INITIAL-Y
                       BALL-VX-AFTER-READY BALL-VY-AFTER-READY
                       INITIAL-X INITIAL-Y BUTTON-DOWN)
                      RALLY 100 3/100))
   (list (make-ball 1 1 1 1)
         ( make-ball INITIAL-X INITIAL-Y
                     BALL-VX-AFTER-READY
                     BALL-VY-AFTER-READY))
   "The new ball must have been appended")
  
  (balls-after-key  "up"
                    (make-world
                     ( list( make-ball 1 1 1 1))
                     (make-racket
                      INITIAL-X INITIAL-Y
                      BALL-VX-AFTER-READY BALL-VY-AFTER-READY
                      INITIAL-X INITIAL-Y BUTTON-DOWN)
                     RALLY 100 3/100))
  (list (make-ball 1 1 1 1))
  "No changes to be observered in the ball or list ")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;; All the following functions follow the same contract
;; only the KeyEvent (ke) Changes


;; is-key-event-space? 
;; is-key-event-up?    : KeyEvent -> Boolean
;; is-key-event-down?
;; is-key-event-left?
;; is-key-event-right?

;; GIVEN: ke
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others

;; STRATEGY: Using simpler functions

;; EXAMPLES:
;; (is-key-event-space? " " )    => True
;; (is-key-event-up?    "up" )   => True
;; (is-key-event-down?  "down" ) => True
;; (is-key-event-down?  "up" )   => False

;; All cases and functions have been addressed in test cases


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key event
(define (is-key-event-space? ke)
  (key=? ke " "))
(define (is-key-event-up? ke)
  (key=? ke "up"))
(define (is-key-event-down? ke)
  (key=? ke "down"))
(define (is-key-event-left? ke)
  (key=? ke "left"))
(define (is-key-event-right? ke)
  (key=? ke "right"))
(define (is-key-event-b? ke)
  (key=? ke "b"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;racket-vel-after-key-press :  KeyEvent Racket-> Racket

;; GIVEN   :  k, of type KeyEvent i.e the key pressed
;;            r, of type Racket   passed to the function

;; RETURNS : the racket with the updated velocities after key presses
;;           the result is obtained based on the type of key press

;; STRATEGY: Dividing cases on the key-press

;; EXAMPLE : The test case below this group of functions
;; are self explanatory

;; **************FUNTION DEFINITION*************************************



;; velocity-changes for key presses:
(define ( racket-vel-after-key-press ke r)
  ( cond
     [(is-key-event-left?   ke)  (racket-vx-change-by  r   -1)]
     [(is-key-event-right?  ke)  (racket-vx-change-by  r   +1)]
     [(is-key-event-up?     ke)  (racket-vy-change-by  r   -1)]
     [(is-key-event-down?   ke)  (racket-vy-change-by  r   +1)]
     [else r]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racket-vx-change-by :   Racket Integer -> Racket
;; racket-vy-change-by

;;
;; GIVEN   :  r, of type Racket  that has all the parameters suchas
;;                x y vx vy mx  my and mev
;;
;;         change-by, of type Int
;;         represents the increment (in + ) or decrement (in minus) 
           

;; RETURNS : the racket with the updated velocities after key presses

;; STRATEGY: Use observer template on  Racket

;; EXAMPLE :
;;           (racket-vx-change-by
;;             (make-racket 12 2 12 2)
;;             1) ->   make-racket 13 2 12 2)
;; All other cases  are very simiar to this 
;;
;; The test case below this group of functions
;; are self explanatory

;; **************FUNTION DEFINITION**************************************

;; racket-vx-change-by
( define (racket-vx-change-by r change-by)
   (
    make-racket
    (racket-x r)
    (racket-y r)
    (+ (racket-vx r) change-by)
    (racket-vy r)
    (racket-mx r)
    (racket-my r)
    (racket-mev r)))
  
;; racket-vy increase
( define (racket-vy-change-by r change-by)
   ( make-racket
     (racket-x r)
     (racket-y r)
     (racket-vx r)
     ( + (racket-vy r) change-by)
     (racket-mx r)
     (racket-my r)
     (racket-mev r)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is used when the ball collides with the racket
;; and the racket's vy becomes zero

;;racket-vy-0 : Racket  => Racket

;; GIVEN      : r, representing the racket to modified

;; RETURNS    : r, represents  the racket with zero y velocity (vy)

;; STRATEGY : Using constructor template of Racket

;; EXAMPLES;
;;( make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN))
;;( make-racket 300 500 5 0  INITIAL-X INITIAL-Y BUTTON-DOWN)


;; ***************************FUNCTION DEFINITION*************


;;****************************TESTS***************************
(define ( racket-vy-0 r )
  (if  (<  (racket-vy r ) 0)
   
       ( make-racket
         (racket-x r)
         (racket-y r)
         (racket-vx r)
         0
         (racket-mx r)
         (racket-my r)
         (racket-mev r))
       ;;else
       r))
;;;;; ***** TESTS *****************************
(begin-for-test
  (check-equal?
   ( racket-vy-0
     ( make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN))
   ( make-racket 300 500 5 0  INITIAL-X INITIAL-Y BUTTON-DOWN)
   "Only vy should be changed if less less than 0")
  (check-equal?
   ( racket-vy-0
     ( make-racket 300 500 5 9  INITIAL-X INITIAL-Y BUTTON-DOWN))
   ( make-racket 300 500 5 9  INITIAL-X INITIAL-Y BUTTON-DOWN)
   "Only vy should be changed if less less than 0"))
   
 
;;; **********Tests**********************************

( begin-for-test
   (check-equal?
    (is-key-event-space? " " )
    true
    "The space bar key must be pressed")
   (check-equal?
    (is-key-event-up? "up" )
    true
    "The UP ARROW key must be pressed")
   (check-equal?
    (is-key-event-down? "down" )
    true
    "The DOWN ARROW key must be pressed")
   (check-equal?
    (is-key-event-left? "left" )
    true
    "The DOWN ARROW key must be pressed")
   (check-equal?
    (is-key-event-right? "right" )
    true
    "The RIGHT ARROW key must be pressed")

   ;; Checking velocity changes
   (check-equal?
    (racket-vel-after-key-press
     " "
     (make-racket 12 12 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket 12 12 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN )
    "No changes must be there" )

   (check-equal?
    (racket-vel-after-key-press
     "up"
     (make-racket 12 12 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket 12 12 12 11  INITIAL-X INITIAL-Y BUTTON-DOWN)
    "racket-vy must be down by 1" )

   (check-equal?
    (racket-vel-after-key-press
     "down"
     (make-racket 12 12 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket 12 12 12 13 INITIAL-X INITIAL-Y BUTTON-DOWN)
    "racket-vy must be up by 1" )
   
   (check-equal?
    (racket-vel-after-key-press
     "left"
     (make-racket 12 12 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket 12 12 11 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
    "racket-vx must be down by 1" )

   (check-equal?
    (racket-vel-after-key-press
     "right"
     (make-racket 12 12 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket 12 12 13 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    "racket-vx must be up by 1")

   ;; Checking velocity  to  zero when racket hits the ball
   ( check-equal? 
     (racket-vy-0
      (make-racket 12 12 13 12  INITIAL-X INITIAL-Y BUTTON-DOWN))
     (make-racket 12 12 13 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
     "racket-vy must be 0"))


;;; ================================================================

;; initial-world : PosReal -> World
;; Given : speed, i.e seconds per tick
;; RETURNS: a world with ball racket and state
;; all of these in their initial state
;; STRATEGY: Using constructor template of World  on world

;; Examples: Test cases are self explanatory
;; **************Function definition***************************
(define (initial-world  speed )
  (make-world
   (list(make-ball
         INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY))
   (make-racket   INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY
                  INITIAL-X INITIAL-Y BUTTON-UP)
   READY
   (ticks-per-pause speed) speed))
;; *******************TESTS************************
  
(begin-for-test
  (check-equal?
   (initial-world 1/20 )
   (make-world
    ( list( make-ball   INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY ))
    (make-racket INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY
                 INITIAL-X INITIAL-Y BUTTON-UP)
    READY
    (ticks-per-pause 1/20)
    1/20 )
   "It must me same"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  world-ready-to-serve? : World -> Boolean

;; GIVEN   : w, of type World

;; RETURNS : if the world is in the state READY

;; STRATEGY: Use observer template on world

;; EXAMPLE :
;; world-ready-to-serve
;;(( make-ball 10 10 12)(make-racket 20 30 5 9) READY 100 3/100)
;;For the above it is true


;; **************FUNTION DEFINITION****************************************
(define  (world-ready-to-serve? w)
  ( string=? (world-state w ) READY))

(begin-for-test
  (check-equal?
   (world-ready-to-serve?
    (make-world ( make-ball 10 10 12 21)
                (make-racket 20 30 5 9 INITIAL-X INITIAL-Y BUTTON-DOWN)
                READY  100 3/100))
   true
   "It must be true" ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-tick : World-> World

;; GIVEN: w, of Word ype
;; RETURNS: the world that should follow w after a tick.  If the world
;;   is paused, returns it unchanged.  Otherwise, builds a new world

;; STRATEGY: Divide into cases on state if paused

;; EXAMPLE : All subsequent test cases are self-explanatory

;; **************FUNTION DEFINITION***********************************


(define ( world-after-tick w)
  ( cond
     ((is-paused?       w )    (world-after-pause w))

     ((racket-selected?
       (world-racket w ))

      (next-world
       (make-world
        (remove empty (balls-next-tentative (world-balls   w)))
        (world-racket  w)
        (world-state w)
        (world-timer w)
        (world-speed w))))
     
     (else
      (next-world
       (make-world
        (remove empty (balls-next-tentative (world-balls   w)))
        (racket-next-adjusted (world-racket w))
        (world-state w)
        (world-timer w)
        (world-speed w))))))
       
;; **************TEST*************
(begin-for-test
   
  (check-equal?
   (next-world
    (make-world
     ( list( make-ball   2   1000 12 12))
     (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))

   (make-world (list ) (make-racket 100 42 12 12 330 384 "button-down")
               RALLY 100 3/100)
   "ball collision with back ball, hence it must be paused")
  (check-equal?
   (next-world
    (make-world
     ( list( make-ball -3 23 -5 -9))
     (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))
   (make-world
    ( list( make-ball 3 23 5 -9))
    (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    RALLY 100 3/100)
   "ball collision with back ball, hence it must be paused")

    

  (check-equal?
   (next-world
    (make-world
     ( list( make-ball   2   12 12 12))
     (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))
   (make-world
    ( list( make-ball   2   12 12 12))
    (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    RALLY 100 3/100)
   "No collision, hence it must be same")


  
  (check-equal?
   (next-world
    (make-world
     ( list( make-ball   2   1000 12 12))
     (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))
   
   (make-world (list)
               (make-racket 100 42 12 12 330 384 "button-down")
               RALLY 100 3/100)
   "ball collision with back ball, hence it must be paused")

  (check-equal?
   (next-world
    (make-world
     ( list( make-ball -3 23 -5 -9))
     (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))
   (make-world
    ( list( make-ball 3 23 5 -9))
    (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    RALLY 100 3/100)
   "ball collision with back ball, hence it must be paused")

  (check-equal?
   (next-world
    (make-world
     ( list( make-ball   2   12 12 12))
     (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))
   (make-world
    ( list( make-ball   2   12 12 12))
    (make-racket 100 42 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    RALLY 100 3/100)
   "No collision, hence it must be same")

  (check-equal?
   ( world-after-tick
     ( make-world
       ( list( make-ball   INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY ))
       (make-racket INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY
                    INITIAL-X INITIAL-Y BUTTON-DOWN)
       PAUSED 100 3/100)) 
   ( make-world
     ( list( make-ball   INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY ))
     (make-racket INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY
                  INITIAL-X INITIAL-Y BUTTON-DOWN)
     PAUSED 99 3/100)
   "Only count down must be performed"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; is-paused?=>

;; GIVEN   :

;; RETURNS :

;; STRATEGY : Combining simpler functions

;; EXAMPLES;


;; ***************************FUNCTION DEFINITION*************


;;****************************TESTS***************************


(define (is-paused? w)
  (string=? (world-state w) PAUSED))

(begin-for-test
  (check-equal?
   (is-paused?
    ( make-world
      ( list( make-ball   INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY ))
      (make-racket INITIAL-X INITIAL-Y  INITIAL-VX INITIAL-VY
                   INITIAL-X INITIAL-Y BUTTON-DOWN)
      PAUSED 100 3/100) )
   true  "This must be true " ))


;; ==================================================================

;; scene-with-mouse-ptr :Integer Integer Scene -> Scene
;; Given: mx, represents the x co-ordinate of the mouse pointer
;;        my, represents the y co-ordinate of the mouse pointer
;; RETURNS: a scene like the given one,
;;           but with the given mouse pointer painted on it.
;;STRATEGY: Combining simpler functions

(define (scene-with-mouse-ptr mx my s)
  (place-image
   (circle MOUSE-PTR-RADIUS TYPE-SOLID MOUSE-PTR-COLOR)
   mx  my s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (check-equal?
   (scene-with-mouse-ptr 1 2 EMPTY-COURT)
   (place-image
   (circle MOUSE-PTR-RADIUS TYPE-SOLID MOUSE-PTR-COLOR)
   1  2 EMPTY-COURT)
   "The image produced must be same as expected"))
;; ====================================================
;; scene-with-ball :Ball Scene -> Scene
;; RETURNS: a scene like the given one, but with the given ball painted
;; on it.
;;STRATEGY: Combining simpler functions
(define (scene-with-ball b s)
  (place-image
   (circle BALL-RADIUS TYPE-SOLID BALL-COLOR)
   (ball-x b) (ball-y b)
   s))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(begin-for-test
  (check-equal?
   (scene-with-ball (make-ball 1 2 3 4) EMPTY-COURT)
   (place-image
   (circle BALL-RADIUS TYPE-SOLID BALL-COLOR)
   1 2 EMPTY-COURT)
   "The image produced must be same as expected"))

;; ====================================================
;; scene-with-balls :BallList Scene -> Scene
;; GIVEN  : balls-li, of type BallList
;;          s,        of Scene

;; RETURNS: a scene like the given one, but with the given ball painted
;; on it.

;; STRATEGY: using HOF foldl with on balls-li, scene
;;                             with function scene-with-ball

(define (scene-with-balls balls-li s)
  (  foldl  scene-with-ball s balls-li))

;; TESTS
(begin-for-test
  (check-equal?
   (scene-with-balls empty EMPTY-COURT)
   (  foldl  scene-with-ball EMPTY-COURT empty)
   "It must be a empty scene"))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scene-with-racket :Ball Scene -> Scene
;; RETURNS: a scene like the given one,
;; but with the given carackett painted
;; on it.
(define (scene-with-racket r s)
  (place-image
   (rectangle RACKET-LENGTH RACKET-BREADTH TYPE-SOLID RACKET-COLOR )
   (racket-x r) (racket-y r)
   s))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE:
;; (world-to-scene paused-world-at-20) should return a canvas with

;; STRATEGY:  ??.




(define (world-to-scene w)
  (
   cond(
 
        (racket-selected? (world-racket w))
        (scene-with-mouse-ptr
         (racket-mx (world-racket w))
         (racket-my (world-racket w))
         (scene-with-balls
          (world-balls w)
          (scene-with-racket
           (world-racket w) EMPTY-COURT))))
   
         
       ((is-paused? w)
        (scene-with-balls (world-balls w)
                          (scene-with-racket (world-racket w)
                                             YELLOW-COURT)))
       
       (else
        (scene-with-balls(remove empty (world-balls w))
                         (scene-with-racket(world-racket w)
                                           EMPTY-COURT)))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; is-ball-lwall-collision?
;; is-ball-rwall-collision? 
;; is-ball-fwall-collision?
;; is-ball-bwall-collision?      Balll  => Boolean
;; is-ball-flwall-collision?
;; is-ball-frwall-collision?

;; GIVEN   : b, of Ball type

;; RETURNS : all of the following functions are grouped together
;;           as they return if the collision is true with the corresponding
;;          wall
;;          When ever there are two walls collision, they are combined
;;         by and 

;; STRATEGY : Combining simpler functions

;; EXAMPLES;
;; (is-ball-lwall-collision? (make-ball -3 10 -3 10))
;; tHE ABOVE will cause a collission with left wall

;; TESTS:
;; ALl the following functions have been tested through other functions


;; ***************************FUNCTION DEFINITIONS*************

;;Collision with left wall
(define (is-ball-lwall-collision?  b )
  ( <  (ball-x b)  LEFT-WALL-X ))

;;Collision with RIGHT wall;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-ball-rwall-collision?  b )
  ( >  (ball-x b)  RIGHT-WALL-X ))
;;Collision with Front wall;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is-ball-fwall-collision?  b )
  ( <  (ball-y b)  FRONT-WALL-Y ))

;;Collision with Back wall;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is-ball-bwall-collision?  b )
  ( >  (ball-y b)  BACK-WALL-Y ))

;;Collision with Front-left wall;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is-ball-flwall-collision?  b )
  ( and
    ( is-ball-fwall-collision?  b)
    ( is-ball-lwall-collision?  b)))

;;Collision with Front-right wall;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is-ball-frwall-collision?  b )
  ( and
    ( is-ball-fwall-collision?  b)
    ( is-ball-rwall-collision?  b)))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

;; TESTS:
( begin-for-test
   (check-equal?
    (is-ball-lwall-collision?
     ( make-ball -3 23 -5 -9))
    true "It must be true")
   
   (check-equal?
    (is-ball-rwall-collision? (make-ball 1000 12 5 -9))
    true "It must be true")
   
   (check-equal? 
    (is-ball-fwall-collision? (make-ball 10  -2 -9 5))
    true "It must be true")
   
   (check-equal?
    (is-ball-bwall-collision?(make-ball 10  1000 -9 5))
    true "It must be true"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  ball-after-wall-coll :   Ball  => Ball

;; GIVEN   : b, reresenting a ball

;; RETURNS : Ball, with the updated velocity after collisions(if found)

;; STRATEGY: Divide into cases on condition - wall collision

;; EXAMPLE :
;; ( ball-after-wall-coll   (make-ball  -4 -5 -5 -9)) =>
;; (make-ball  4 5 5 9)



;; **************FUNTION DEFINITION**************************************


( define ( ball-after-wall-coll  b )
   (cond
     
     [(is-ball-flwall-collision? b)   (vel-pos-flwall b)]

     [(is-ball-frwall-collision? b)   (vel-pos-frwall b)]

     [( is-ball-lwall-collision?  b)  (vel-pos-lwall b)]

     [( is-ball-rwall-collision?  b)  (vel-pos-rwall b)]

     [( is-ball-fwall-collision?  b)  (vel-pos-fwall b)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Velocity and position chages after collision

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vel-pos-fwall:
;;vel-pos-flwall:
;;vel-pos-frwall: Ball  => Ball
;; vel-pos-lwall:
;; vel-pos-rwall:


;; GIVEN   : b,ball of Ball type

;; RETURNS : ball, with the new  position and velocity 

;; STRATEGY: Use observer template of Ball

;; EXAMPLE :
;; ( ball-after-wall-coll   (make-ball  -4 -5 -5 -9)) =>
;; (make-ball  4 5 5 9)


;; **************FUNTION DEFINITION**************************************

(define (vel-pos-fwall b)
  (make-ball
   (ball-x b)
   (- FRONT-WALL-Y (ball-y b) )
   (ball-vx b)
   (-(ball-vy b)) ) )

(define (vel-pos-flwall b)
  (make-ball
   (-(ball-x b))
   (- FRONT-WALL-Y (ball-y b) )
   (-(ball-vx b))
   (-(ball-vy b)) ))

(define (vel-pos-frwall b)
  (make-ball
   (-
    RIGHT-WALL-X
    (- (ball-x b) RIGHT-WALL-X) )
   (- FRONT-WALL-Y (ball-y b) )
   (-(ball-vx b) )
   (-(ball-vy b)) ))

(define (vel-pos-lwall b)
  (make-ball
   (-(ball-x b))
   (ball-y b)
   (-(ball-vx b))
   (ball-vy b)))

(define (vel-pos-rwall b)
  (make-ball
   (-
    RIGHT-WALL-X
    (- (ball-x b) RIGHT-WALL-X) )
   (ball-y b)
   (-(ball-vx b) )
   (ball-vy b)))

;; **************************TESTS*******************

( begin-for-test
   ;;front -left wall collision
   (check-equal?
    ( ball-after-wall-coll   (make-ball  -4 -5 -5 -9))
    (make-ball  4 5 5 9)
    " 2nd and 4th parameter must me negated: 5  9")

   ;;front -right  wall collision
   (check-equal?
    ( ball-after-wall-coll   (make-ball  429 -5 5 -9))
    (make-ball  421 5 -5 9)
    " 2nd and 4th parameter must me negated: 5  9")

   ;; front wall collision 
   (check-equal?
    ( ball-after-wall-coll   (make-ball  4 -5 -5 -9))
    (make-ball  4 5 -5 9)
    " 2nd and 4th parameter must me negated: 5  9")

   ;; left wall collision
   (check-equal?
    ( ball-after-wall-coll
      (make-ball -3 23 -5 -9))
    (make-ball 3 23 5 -9)
    " 1st and 3rd parameter must me negated: 3  5")

   ;; right wall collision
   (check-equal?
    ( ball-after-wall-coll   (make-ball  429 23 5 -9))
    (make-ball  421 23 -5 -9)
    "3rd parameter must me negated: 3 "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RACKET COLLISSIONS

;;is-racket-lwall-collision?
;;is-racket-rwall-collision?   Racket-> Boolean
;;is-racket-bwall-collision?
;;is-racket-bwall-collision?




;; GIVEN   : r, of Racket type  

;; RETURNS : if the racket has hit any of the wall (in Boolean)

;; STRATEGY: Use observer template of Ball

;; EXAMPLE : Refer test cases


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Collision with left back wall
(define (is-racket-blwall-collision?  r )
  (and
   (>  (racket-bottom-end-y r)  BACK-WALL-Y )
   (<  (racket-left-end-x r)    LEFT-WALL-X )))

;;Collision with right back wall
(define (is-racket-brwall-collision?  r )
  (and
   (>  (racket-bottom-end-y r)    BACK-WALL-Y )
   (>  (racket-right-end-x   r)   RIGHT-WALL-X )))

;;Collision with left wall
(define (is-racket-lwall-collision?  r )
  ( <  (racket-left-end-x r)  LEFT-WALL-X ))

;;Collision with RIGHT wall;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-racket-rwall-collision?  r )
  ( >  (racket-right-end-x r)  RIGHT-WALL-X ))
;; Collsion with FRONT wall;;;;;;;;;;;;;;;;;;
(define (is-racket-fwall-collision?  r )
  ( <  (racket-top-end-y r)  FRONT-WALL-Y ))

;; Collsion with BACK wall;;;;;;;;;;;;;;;;;;
(define (is-racket-bwall-collision?  r )
  ( >  (racket-bottom-end-y r)  BACK-WALL-Y ))

#;;;Collision with Front-left wall;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is-racket-flwall-collision?  r )
  ( and
    ( is-racket-fwall-collision?  r)
    ( is-racket-lwall-collision?  r)))

#;;Collision with Front-right wall;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is-racket-frwall-collision?  r )
  ( and
    ( is-racket-fwall-collision?  r)
    ( is-racket-rwall-collision?  r)))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS:
( begin-for-test
   (check-equal?
    (is-racket-lwall-collision?
     (make-racket -3 23 -5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN))
    true "It must be true")
   
   (check-equal?
    (is-racket-rwall-collision?
     (make-racket 1000 12 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN))
    true "It must be true")
   
   (check-equal? 
    (is-racket-fwall-collision?
     (make-racket 10  -2 -9 5  INITIAL-X INITIAL-Y BUTTON-DOWN))
    true "It must be true")

   (check-equal? 
    (is-racket-fwall-collision?
     (make-racket 10  -2 -9 5  INITIAL-X INITIAL-Y BUTTON-DOWN))
    true "It must be true")
   
   (check-equal?
    (is-racket-bwall-collision?
     (make-racket 10  1000 -9 5  INITIAL-X INITIAL-Y BUTTON-DOWN))
    true "It must be true"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; is-ball-racket-collision? :  Ball Racket -> Boolean

;; GIVEN   : b, ball of Ball type
;;           r, racket of Racket rype

;; RETURNS : if the racket collides with the ball
;;      i.e if the ball is moving upwards the vy < 0
;; no collision

;; STRATEGY: Use observer template of Ball and Racket

;; EXAMPLE :  Test cases are self explanatory

;; **************FUNTION DEFINITION*************************************


(define (is-ball-racket-collision?  b  r)
  (and
   (> ( ball-vy b)  0 )
   ( and
     (and
      ( >= ( ball-x b) ( racket-left-end-x  r))
      ( <= ( ball-x b) ( racket-right-end-x r)))
     (and
      ( >= (ball-y b)  (racket-top-end-y    r))
      ( <= (ball-y b)  (racket-bottom-end-y r))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The new velocity to be updated in the ball
;;after collision with the racket
(define (ball-vel-racket-coll b r)
  ( make-ball
    ( ball-x  b )
    ( ball-y  b )
    ( ball-vx b )
    (-(racket-vy r) ( ball-vy b ))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:
( begin-for-test
   (check-equal?
    (is-ball-racket-collision?
     (make-ball 310 501 5 4)
     (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN))
    true "It must be true")

   (check-equal?
    (ball-vel-racket-coll
     (make-ball 310 501 5 4)
     (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-ball 310 501 5 -13)
    "expected-ball: vx no change vy: (vy of racket) -( old vy of ball)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ticks-per-pause : PosReal-> INT

;; GIVEN   : sec-per-tick

;; RETURNS : ticks 

;; STRATEGY: Transcribe formula 

;; EXAMPLE : The testcase itslef is self explanatory

;; **************FUNTION DEFINITION****************************************
(define (ticks-per-pause sec-per-tick)
  ( * SECONDS-PER-PAUSE
      ( /
        1
        sec-per-tick)))

;; **************TESTS*****************************************************
( begin-for-test
   (check-equal?
    (ticks-per-pause 1/20 ) 60
    "The result must be 60"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  racket-after-coll :   Racket->Racket 

;; GIVEN   : r, of type Racket

;; RETURNS : ball fields get updated if it collides,
;;            else it is returned the same as it is

;; STRATEGY: Divide into cases on condition - wall collision

;; EXAMPLE : Test cases are self explanatory


;; **************FUNTION DEFINITION**************************************

;; If the function returns the same racket with all fields same,
;; it means its time as paused 

( define (racket-after-coll  r )
   (cond

     
     [(is-racket-blwall-collision?  r)  (racket-vel-pos-bwall
                                         (racket-vel-pos-lwall r)) ]

     [(is-racket-brwall-collision?  r)  (racket-vel-pos-bwall
                                         (racket-vel-pos-rwall r)) ]


     [(is-racket-bwall-collision?  r)  (racket-vel-pos-bwall r) ]
     
     [(is-racket-lwall-collision?  r)  (racket-vel-pos-lwall r)]

     [(is-racket-rwall-collision?  r)   (racket-vel-pos-rwall r)]

     [else r ]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  racket-vel-pos-lwall :   Racket->Racket
;;racket-vel-pos-rwall
;;racket-vel-pos-blwal
;;racket-vel-pos-brwall
;;racket-vel-pos-bwall

;; GIVEN   : r, of type Racket

;; RETURNS : ball fields get updated if it collides,
;;            else it is returned the same as it is

;; STRATEGY: using Constructor template of racket on racket

;; EXAMPLE : Test cases are self explanatory


;; **************FUNTION DEFINITION**************************************

(define (racket-vel-pos-lwall r)
  (make-racket
   (+ (racket-x r)
      (-
       LEFT-WALL-X
       (racket-left-end-x r)))
   (racket-y r)
   (racket-vx r)
   (racket-vy r)
   (racket-mx r)
   (racket-my r)
   (racket-mev r)))

(define (racket-vel-pos-rwall r)
  (make-racket
   (-
    (racket-x r)
    (-
     (racket-right-end-x r)
     RIGHT-WALL-X ))
   (racket-y r)
   (racket-vx r)
   (racket-vy r)
   (racket-mx r)
   (racket-my r)
   (racket-mev r)))


(define (racket-vel-pos-bwall r)
  (make-racket
   (racket-x r)
   (-
    (racket-y r)
    (-
     ( racket-bottom-end-y r) BACK-WALL-Y ))
   (racket-vx r)
   (racket-vy r)
   (racket-mx r)
   (racket-my r)
   (racket-mev r)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS:

( begin-for-test

   ;; back left wall collision
   ;; half of the rectangle will be outside
   (check-equal?
    (racket-after-coll
     (make-racket   LEFT-WALL-X  BACK-WALL-Y -5 9
                    INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket ( / RACKET-LENGTH 2) 1291/2 -5 9
                 INITIAL-X INITIAL-Y BUTTON-DOWN)
    "The x co-ord of racket must be moved right and y must be move up")


   ;; back right wall collision
   ;; half of the rectangle will be outside
   (check-equal?
    (racket-after-coll
     (make-racket   RIGHT-WALL-X  BACK-WALL-Y -5 9
                    INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket (-  RIGHT-WALL-X  ( / RACKET-LENGTH 2))1291/2 -5 9
                 INITIAL-X INITIAL-Y BUTTON-DOWN)
    "The x co-ord of racket must be moved left and y must be move up")
   
   
   ;; left wall collision
   ;; half of the rectangle will be outside
   (check-equal?
    (racket-after-coll
     (make-racket   LEFT-WALL-X  23 -5 -9
                    INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket ( / RACKET-LENGTH 2) 23 -5 -9
                 INITIAL-X INITIAL-Y BUTTON-DOWN)
    "The x co-ordinate of racket must be moved right")
   ;; right wall collision
   (check-equal?
    (racket-after-coll
     (make-racket   RIGHT-WALL-X  23 5 -9
                    INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket
     (-
      RIGHT-WALL-X  ( / RACKET-LENGTH 2))
     23 5 -9
     INITIAL-X INITIAL-Y BUTTON-DOWN)
    "The x co-ordinate of racket must be moved right")
   ;; Back wall collision
   (check-equal?
    (racket-after-coll (make-racket   24  BACK-WALL-Y -5 -9
                                      INITIAL-X INITIAL-Y BUTTON-DOWN))
    (make-racket  24
                  (- BACK-WALL-Y
                     (/ RACKET-BREADTH 2))
                  -5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
    "The y co-ordinate of racket must be moved up"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-next-tentative : (Ball -> Ball) BallList -> BallList

;; GIVEN   : balls-li,  BallList , represents the list of balls in world
              
;; RETURNS : balls-li, a BallList Type
;;           The new list generated in this function is a list of balls
;;           where each of the ball is updated with the next position
;;           The list is iteterated and ball-next-tentative is applied
;;          through map.

;;           NOTE : the empty elements in the list are removed, as
;;           empty elements in the middle will cause unexpected behaviour
;;           in the follow up functions

;; STRATEGY: Using HOF map on balls-li  with function ball-next-tentative

;; EXAMPLES :
;; case1 : balls-li is empty 
;; balls-next-tentative (list)) =>  '()


;; case2 : balls-li is a non empty list of balls 
;; balls-next-tentative (list (make-ball 2 2 2 3))) =>
;;                                 (list (make-ball 4 5 2 3))

;; **************FUNTION DEFINITION****************************************

( define (balls-next-tentative balls-li )
   (remove empty
           ( map ball-next-tentative  balls-li)))
   
        
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (check-equal?
   (balls-next-tentative (list(make-ball 2 2 2 3)))
   (list (make-ball 4 5 2 3))
   "In each of the ball in list,
ball-x must be increased by vx and ball-y by vy")


  (check-equal?
   (balls-next-tentative
    (list(make-ball 2 2 2 3)
         (make-ball 2 1000 2 3)
         (make-ball 2 2 2 3)))
   
   (list(make-ball 4 5 2 3)
        (make-ball 4 1003 2 3)
        (make-ball 4 5 2 3))
   "In each of the ball in list,
ball-x must be increased by vx and ball-y by vy"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-next-tentative : Ball -> Ball

;; GIVEN   : b,  Ball , represents the ball in the world

;; RETURNS :  Ball , represents the ball
;;           generated tentatively in the world in an assumption that
;;           they dont collide, hence the velocity remains the same
;;           Here only the positions are updated (x and y)
;;           vx and vy are not updated
;;            

;; STRATEGY: user Observer template on Ball

;; EXAMPLE : test cases are self explanotory

;; **************FUNTION DEFINITION****************************************



( define (ball-next-tentative b)
   (cond

     ((empty? b) empty)

     (else
      (make-ball
       (+ (ball-x b) (ball-vx b))
       (+ (ball-y b) (ball-vy b))
       (ball-vx b)
       (ball-vy b)))))
   
   

(begin-for-test
  (check-equal?
   (ball-next-tentative (make-ball 2 2 2 3))
   (make-ball 4 5 2 3)
   "ball-x must be increased by and ball-y by vy"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-next-tentative : Racket -> Racket

;; GIVEN   : r,  Racket , represents the racket in the world

;; RETURNS : Racket , represents the racket
;;           generated tentatively in the world in an assumption that
;;           they dont collide, hence the velocity remains the same
;;           Here only the positions are updated (x and y)
;;           vx and vy are not updated
;;            

;; STRATEGY: use Observer template on  r - (Racket type)

;; EXAMPLE :

;; **************FUNTION DEFINITION****************************************



( define (racket-next-tentative r)
   (make-racket
    (+ (racket-x r) (racket-vx r))
    (+ (racket-y r) (racket-vy r))
    (racket-vx r)
    (racket-vy r)
    (racket-mx r)
    (racket-my r)
    (racket-mev r)))


(begin-for-test
  (check-equal?
   (racket-next-tentative
    (make-racket 2 2 2 3  INITIAL-X INITIAL-Y BUTTON-DOWN))
   (make-racket 4 5 2 3  INITIAL-X INITIAL-Y BUTTON-DOWN)
   "racket-x must be increased by and racket-y by vy"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (racket-next-adjusted r)
  ( racket-after-coll (racket-next-tentative r )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-endgame-check: ->

;; GIVEN   :w, World represents the real world

;; RETURNS : w with the updated  state field once
;;             ball and racket inside are updated
;;              if state is PAUSED
;;              if ball hits the back wall or
;;              if racket hits the front wall

;; STRATEGY: divide into cases on possibilites of end game

;; EXAMPLE :

;; **************FUNTION DEFINITION****************************************


(define (world-after-endgame-check w)
  (if
   (or
    (empty? (world-balls w))
    (is-racket-fwall-collision? (world-racket w)))
   
   (set-world-to-paused w)  ;; if the above is true

   w))                      ;; if the above is false
;;                               return the same world
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-world-to-paused :  World -> World

;; GIVEN   : w, World represents the real world

;; RETURNS : w with the updated  state field as PAUSED

;; STRATEGY: use observer template on world

;; EXAMPLE : Please refer the example 

;; **************FUNTION DEFINITION**************************************


(define (set-world-to-paused w)
  (make-world
   (world-balls    w)
   (world-racket  w)
   PAUSED
   (world-timer  w)
   (world-speed  w)))


;; ***************TESTS**********************

(begin-for-test
  (check-equal?
   (set-world-to-paused
    (make-world
     (list(make-ball   12 12 12 12 ))
     (make-racket 12 42 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))
   
   (make-world
    ( list( make-ball   12 12 12 12))
    (make-racket 12 42 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
    PAUSED 100 3/100 )
   "The state field must be set to PAUSED" )


  ;; case for world-after-endgame-check  ; empty ball list
  (check-equal?
   (world-after-endgame-check
    (make-world
     ( list)
     (make-racket 12 42 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))
   
   (make-world
    ( list)
    (make-racket 12 42   12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    PAUSED 100 3/100)))

  
;; case for world-after-endgame-check  ; backwall ball collision
;; this should be false, because one ball still exists in the list
(check-equal?
 (world-after-endgame-check
  (make-world
   ( list( make-ball   12 300 12 12))
   (make-racket 12 42 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
   RALLY 100 3/100))
   
 (make-world
  ( list( make-ball   12 300 12 12))
  (make-racket 12 42   12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
  RALLY 100 3/100))

;; case for world-after-endgame-check: racket hitting front wall
(check-equal?
 (world-after-endgame-check
  (make-world
   ( list( make-ball   12 12 12 12))
   (make-racket 12 0 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
   RALLY 100 3/100))

 (make-world
  ( list( make-ball   12 12 12 12))
  (make-racket 12 0   12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
  PAUSED 100 3/100))

;; case for checking PAUSED state
(check-equal?
 (world-after-endgame-check
  (make-world
   ( list( make-ball   12 12 12 12))
   (make-racket 12 22 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
   PAUSED 100 3/100))

 (make-world
  ( list( make-ball   12 12 12 12))
  (make-racket 12 22 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
  PAUSED 100 3/100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;newly-added
;; countdown-timer-on-pause :  World -> World

;; GIVEN   : w, World represents the real world

;; RETURNS : w with the updated  state field as PAUSED

;; STRATEGY: use observer template on world

;; EXAMPLE : Please refer the example 

;; **************FUNTION DEFINITION**************************************


(define (countdown-timer-on-pause w)
  (if ( string=? (world-state w) PAUSED)
      (make-world
       (world-balls   w)
       (world-racket  w)
       PAUSED
       ( sub1 (world-timer w))
       (world-speed w))
      ;else
      w))
;; *************** TEST*****************
(begin-for-test
  (check-equal?
   (countdown-timer-on-pause
    (make-world
     ( list( make-ball   12 12 12 12))
     (make-racket 12 22 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     PAUSED 32 1/20))
   (make-world
    ( list( make-ball   12 12 12 12))
    (make-racket 12 22 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    PAUSED 31 1/20 )
   "The resultant world-timer must be decremented by 1")

  (check-equal?
   (countdown-timer-on-pause 
    (make-world
     ( list( make-ball   12 12 12 12))
     (make-racket 12 22 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     READY 32 1/20))
   (make-world
    ( list( make-ball   12 12 12 12))
    (make-racket 12 22 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    READY 32 1/20)
   "No change in the value of timer or any other field"))
   
   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;newly-added
;; world-after-pause :  World -> World

;; GIVEN   : w, World represents the real world

;; RETURNS : w with the updated  state field as PAUSED

;; STRATEGY: use observer template on world

;; EXAMPLE : Please refer the example 

;; **************FUNTION DEFINITION**************************************


(define (world-after-pause w)
  (if ( = (world-timer (countdown-timer-on-pause w)) 0 )
      (initial-world (world-speed w))
      ;else
      (countdown-timer-on-pause w)))
;; *************** TEST*****************
(begin-for-test
  (check-equal?
   (world-after-pause
    (make-world
     ( list( make-ball   12 12 12 12))
     (make-racket 12 22 12 12  INITIAL-X INITIAL-Y BUTTON-DOWN)
     PAUSED 32 3/100))
   (make-world
    ( list( make-ball   12 12 12 12))
    (make-racket 12 22 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
    PAUSED 31 3/100)
   "The resultant world-timer must be decremented by 1")

  (check-equal?
   (world-after-pause
    (make-world
     ( list( make-ball   12 12 12 12))
     (make-racket 12 22 12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     READY 0 3/100))
   ( initial-world 3/100)
   "No change in the value of timer or any other field"))
   
   
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-world :World -> World

;; GIVEN   : w, World  with the tentative postition of the ball is given

;; RETURNS : w, the next World that has
;;              The ball racket and state updated
                 

;; STRATEGY: Divide into cases based on the collision 

;; EXAMPLE : Test cases are self explanatory

;; *********************** Function definition****************************

(define (next-world w)
  (cond
    ((string=? (world-state w ) READY)
     w)
        
    ((not (equal?(world-after-endgame-check w)  w ))
     (world-after-endgame-check w))

    (else
     (make-world
      (balls-after-tick
       (world-racket w)(world-balls w))
      (racket-after-tick
       (remove empty (world-balls w))
       (world-racket w ))
      (world-state w)
      (world-timer w)
      (world-speed w)))))


;; ************************TESTS******************************************
(begin-for-test

  
  (check-equal?
   (next-world
    (make-world
     ( list( make-ball 310 501 5 4))
     (make-racket 300 501 5 -9 INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))

   (make-world
    ( list( make-ball 310 501 5 -13))
    (make-racket 300 501 5 -9 INITIAL-X INITIAL-Y BUTTON-DOWN)
    RALLY 100 3/100)
   "no Ball racket collision, as ball-vx is in negative (Moving upwards)"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;is-ball-wall-collision? : -> Boolean

;; GIVEN   : b, represents ball of Ball type

;; RETURNS : if the ball has hit any wall in boolean
;;           (other than end game - back wall)

;; STRATEGY: Divide into cases  on wall the ball is colliding with

;; EXAMPLE :
;; The test cases are self explanatory

;; **************FUNTION DEFINITION*************************************

(define(is-ball-wall-collision? b)
  (or
   (is-ball-flwall-collision?  b)
   (is-ball-frwall-collision?  b)
   (is-ball-fwall-collision?   b)
   (is-ball-lwall-collision?   b)
   (is-ball-rwall-collision?   b)))


(begin-for-test
  ;; lwall
  (check-equal?
   (is-ball-wall-collision?
    ( make-ball   -2 12 12 12))
   true  "The result should be true" ))

;;rwall
(check-equal?
 (is-ball-wall-collision?
  ( make-ball   800 12 12 12))
 true   "The result be true" )

;;fwall
(check-equal?
 (is-ball-wall-collision?
  (make-ball   12 -12 12 12))
 true   "The result be true" )

;; fl wall
(check-equal?
 (is-ball-wall-collision?
  (make-ball   800 -2 12 12))
 true  "The result be true" )

;;frwall
(check-equal?
 (is-ball-wall-collision?
  ( make-ball   800 -12 12 12))
 true  "The result be true" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-state-to-color :  World -> String

;; GIVEN   : w, world

;; RETURNS : the color according to the state of the world
;;           returns yellow, if the state of the world is PAUSED
;;           return white , if anything else

;; STRATEGY: Use observer template on world

;; EXAMPLE : The test cases are self explanatory

;; **************FUNTION DEFINITION*************************************

(define (world-state-to-color w)
  (
   if (string=? (world-state w) PAUSED)
      COURT-COLOR-YELLOW
      ;;else
      COURT-COLOR-WHITE ))

(begin-for-test
  (check-equal?
   (world-state-to-color
    (make-world
     ( list( make-ball     22  33   11 22))
     (make-racket   100 120  12 12 INITIAL-X INITIAL-Y BUTTON-DOWN)
     RALLY 100 3/100))
   "WHITE"
   "The color must be white")

  (check-equal?
   (world-state-to-color
    (make-world
     ( list( make-ball     22  33   11 22))
     (make-racket   100 120  12 12 INITIAL-X INITIAL-Y BUTTON-DOWN) 
     PAUSED 100 3/100))
   "YELLOW"
   "The color must be white") )


;; =============================MOUSE EVENTS=========================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-mouse-event:
;;;   World Integer Integer MouseEvent-> World

;; GIVEN   : w, of type World represents the world
;;           mx, Int represents the x co-ordinate of the mouse position
;;           my, Int represents the y co-ordinate of the mouse position

;; RETURNS : world, with the updated postion for racket 

;; STRATEGY:Use constructor template of racket 


;; **************FUNTION DEFINITION**************************************


(define (world-after-mouse-event w mx my mev)
  (if (string=? (world-state w ) RALLY)
      (make-world
       (world-balls w)
       (racket-after-mouse-event (world-racket w) mx my mev)
       (world-state w)
       (world-timer w)
       (world-speed w))
      ;;else
      w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racket-after-mouse-event
;;;   Racket Integer Integer MouseEvent-> World

;; GIVEN   : r, of type Racket represents the racket
;;           mx, Int represents the x co-ordinate of the mouse position
;;           my, Int represents the x co-ordinate of the mouse position
;;           mev,MouseEvent represent the mouse events

;; RETURNS : racket, with the updated postion for racket 

;; STRATEGY: Use constructor template of racket 


;; **************FUNTION DEFINITION*************************************

(define (racket-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN)
     (racket-after-button-down r mx my)]
    
    [(mouse=? mev DRAG)
     (racket-after-drag r mx my)]
    
    [(mouse=? mev BUTTON-UP) (racket-after-button-up r mx my)]
    [else r]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racket-after-button-down
;;;   World Integer Integer MouseEvent-> World

;; GIVEN   : r, of type Racket represents the racket
;;           mx, Int represents the x co-ordinate of the mouse position
;;           my, Int represents the y co-ordinate of the mouse position


;; RETURNS : racket, with the updated postion for racket 

;; STRATEGY: Use constructor template of racket 



;; **************FUNTION DEFINITION*************************************

(define (racket-after-button-down r mx my)
  (if (in-racket? r mx my)
      (make-racket
       (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       mx
       my
       BUTTON-DOWN)

      ;;else
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;racket-after-drag :  Racket Int Int->

;; GIVEN   : r, of type Racket represents the racket
;;           x, Int represents the x co-ordinate of the mouse position
;;           y, Int represents the y co-ordinate of the mouse position

;; RETURNS : racket, with the updated postion for racket 

;; STRATEGY: Use constructor template of racket 



;; **************FUNTION DEFINITION**************************************

(define (racket-after-drag r  mx my)
  (if (racket-selected? r )
      (racket-new-rel-pos r mx my)
      ;;else
      r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;racket-after-button-up :  Racket Int Int-> Racket

;; GIVEN   : r, of type Racket represents the racket
;;           mx, Int represents the x co-ordinate of the mouse position
;;           my, Int represents the y co-ordinate of the mouse position

;; RETURNS : racket, with the updated postion for racket 

;; STRATEGY: Use constructor template of racket 


;; **************FUNTION DEFINITION**************************************

(define (racket-after-button-up r mx my)
  (if (in-racket? r mx my)
      (make-racket
       (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       mx my BUTTON-UP)

      ;; else
      r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;in-racket? :  Racket Int Int-> Boolean

;; GIVEN   : r, of type Racket represents the racket
;;           x, Int represents the x co-ordinate of the mouse position
;;           y, Int represents the y co-ordinate of the mouse position

;; RETURNS : racket, with the updated postion for racket 

;; STRATEGY: Divide into cases on values of x and y

;; EXAMPLE :
;;((make-racket  25 1 0 0 ) 25 1) this is in the racket, hence it must
;; return true

;; **************FUNTION DEFINITION************************************

 
(define (in-racket? r x y)
  (and
   (>= x (- (racket-x r) RACKET-SELECTABLE-DIST))
   (<= x (+ (racket-x r) RACKET-SELECTABLE-DIST))
   (>= y (- (racket-y r) RACKET-SELECTABLE-DIST))
   (<= y (+ (racket-y r) RACKET-SELECTABLE-DIST)) ))
;; *************************TESTS***********************************
(begin-for-test
  (check-equal?
   ( in-racket?
     (make-racket  25 1 0 0  INITIAL-X INITIAL-Y BUTTON-DOWN) 25 1)
   true "It must return true" )
  (check-equal?
   ( in-racket?
     (make-racket  25 1 0 0  INITIAL-X INITIAL-Y BUTTON-DOWN) 30 -10)
   true "It must return true" )
  (check-equal?
   ( in-racket?
     (make-racket  25 1 0 0  INITIAL-X INITIAL-Y BUTTON-DOWN) 30 10)
   true "It must return true" )
  (check-equal?
   ( in-racket?
     (make-racket  25 1 0 0  INITIAL-X INITIAL-Y BUTTON-DOWN) 11 10)
   true "It must return true" ))

; =======================================================================


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dist-between-2points :   PosReal PosReal -> PosReal

;; GIVEN   : x1, PosReal represents x co-ordinate of point 1
;;           y1, PosReal represents x co-prdinate of point 2
;;           x2, PosReal represents y co-ordinate of point 1
;;           y2, PosReal represents y co-prdinate of point 2

;; RETURNS : the distance between two points in units 

;; STRATEGY: Transcribe mathematical formula for dist between 2 points

;; EXAMPLE :
;; (dist-between-2points  2 1 4 1) => 2
;; (dist-between-2points  2 1 8 1) => 6


;; **************FUNTION DEFINITION**************************************


(define (dist-between-2points x1 y1 x2 y2)
  (sqrt
   (+
    (sqr (- x2 x1))
    (sqr (- y2 y1)))))
;; **************************Tests**************************************

(begin-for-test
  (check-equal? (dist-between-2points  2 1 4 1)
                2 "Distance must be true")
  (check-equal? (dist-between-2points  2 1 10 1)
                8 "Distance must be true"))

;; ********************************************************************

;; racket-selected : Racket-> Boolean
;;; GIVEN: a racket
;;; RETURNS: true iff the racket is selected
;;            only if thes two checks are true
;;           else false
;;            1. if the distance between racket & mouse pointer
;;                      is <=RACKET-SELECTABLE-DIST
;;                      

;; **********************************************************
(define (racket-selected? r)
  (and
   ( <=
     ( dist-between-2points
       ( racket-x r)( racket-y r)( racket-mx r)( racket-my r))
     RACKET-SELECTABLE-DIST)

   (or
    ( is-mev-button-down? (racket-mev r))
    ( is-mev-drag?        (racket-mev r)))))

;;;;;;;;;;;;;;;;;TEST;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (check-equal?
   (racket-selected?
    (make-racket INITIAL-X             INITIAL-Y
                 BALL-VX-AFTER-READY   BALL-VY-AFTER-READY
                 INITIAL-X             INITIAL-Y        BUTTON-DOWN ))
   true "It must be true")

  (check-equal?
   (racket-selected?
    (make-racket INITIAL-X INITIAL-Y
                 BALL-VX-AFTER-READY BALL-VY-AFTER-READY
                 INITIAL-X INITIAL-Y DRAG ))
   true "It must be true"))




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;; is-mev-drag? : MouseEvent
;; is-mev-button-up?
;; is-mev-button-down?


;; GIVEN  : mev, MouseEvent
;; RETURNS: Boolean
;; after the given mouse event.

;; EXAMPLES: see tests below
;; STRATEGY: Dividing into cases on mev

;; mouse event
(define (is-mev-drag? mev)
  (mouse=? mev DRAG))
(define (is-mev-button-up?   mev)
  (mouse=? mev BUTTON-UP))
(define (is-mev-button-down? mev)
  (mouse=? mev BUTTON-DOWN))

;;****************************** TESTS***************************
( begin-for-test
   (check-equal?
    (is-mev-drag? DRAG)        true
    "It must be true")
   (check-equal?
    (is-mev-button-up? BUTTON-UP)   true
    "It must be true")
   (check-equal?
    (is-mev-button-down? BUTTON-DOWN) true
    "It must be true"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;racket-new-rel-pos :     Racket  Int Int => Racket
;;GIVEN :
;; r- of Type Racket represents a racket
;; mx- Int, new mouse pointer x- cordinate
;; my- Int, new mouse pointer y- cordinate

(define(racket-new-rel-pos r  mx   my)
  (make-racket
   (+ (racket-x r)     (racket-rel-diff   (racket-mx r)  mx))
   (+ (racket-y r)     (racket-rel-diff   (racket-my r)  my))
   (racket-vx          r  )
   (racket-vy          r  )
   mx my
   DRAG))


;; ;;;;;;;;;;;;;;;TESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



   

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This function can be used to generate the relational difference
;; of both x and y
;; For x, pass in mx in m and prev mx in prev-m
;; For x, pass in mx in m and prev mx in prev-m
;; racket-new-rel-x
(define (racket-rel-diff prev-m m)
  ( -  m prev-m ))


(begin-for-test
  (check-equal?
   (racket-rel-diff 3 4)
   1 "Diff must be 1")
  (check-equal?
   (racket-rel-diff 4 3)
   -1 "Diff must be -1")
  (check-equal?
   (racket-rel-diff 4 4)
   0 "Diff must be 0")

  (check-equal?
   (racket-new-rel-pos
    (make-racket
     10 110 3 3 10 115 DRAG) 10 116)
   (make-racket
    10 111 3 3 10 116 DRAG)
   "racket-my must be incremented by 1"))

;;;;;;;;;;;;;ball-after-tick;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-after-tick :  Racket Ball => Ball

;;GIVEN    :b
;;RETURNS  : ball after each tick
;;           if ball hits the back wall, then empty is returned
;;             for front wall the value is updated from
;;            ball after wall collision
;;           if colliding with racket then the value is updated from
;;          ball-vel-racket-coll
;;             
;;STRATEGY : Combining simpler functions such as,
;;                ;is-ball-bwall-collision?
;;                is-ball-wall-collision?
;;                is-ball-racket-collision?

;;;;;;;;;;;;;;;;;;FUNCTION DEFINIITION;;;;;;;;;;;;;;;;;;;;;
(define (ball-after-tick  r b)
  (cond
   
    ((is-ball-bwall-collision? b)      empty)

    ;; walls other than back wall
    ((is-ball-wall-collision? b)       (ball-after-wall-coll b))

    ;; if ball hits the racket
    ((is-ball-racket-collision? b r)   (ball-vel-racket-coll b   r))

    (else b)))

;;****************TESTS**********************************

(begin-for-test
  (check-equal?
   (ball-after-tick
    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
    (make-ball 10  -2 -9 5))              
   (ball-after-wall-coll(make-ball 10  -2 -9 5))                
   "It must be a wall collision" )

  (check-equal?
   (ball-after-tick
    
    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
    (make-ball 10  1000 -9 5))
   empty 
   "It must be a back wall collision, hence empty return expected" )

  
  (check-equal?
   (ball-after-tick
    
    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
    (make-ball 310 501 5 4))
   (ball-vel-racket-coll
    (make-ball 310 501 5 4)
    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN))
   "It must be a back wall collision, hence empty return expected")

  ;; no collsion- returns the same ball
  (check-equal?
   (ball-after-tick
    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
    (make-ball 10  300 -9 5))
   (make-ball 10  300 -9 5)
   "No collision,hence the ball will be same" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racket-after-tick : Ball Racket => Racket
;; GIVEN:b, Ball type
;;       r,racket type
;; RETURNS: 
;;for each ball 
;; if racket collision with front wall is set as endgame condition
;;then it will be taken care of end game check
;; here the racket will be left as it is in same x y and vx vy

;; if racket doesnt collide with any thing
;; then racket  moves with the same velocity
;; but the racket that was supplied to this function
;; has got vx and vy updated already, so no changes again
;; that will a
    
;;((is-racket-fwall-collision?    r)


;; on the whole,
;; one ball  collission is same as
;; multiple balls colliding with racket at the same tick
;; so the racket is immediately returned with updated values at
;; the first collision

;; STRATEGY

;; *********FUNCTION DEFINITION*****************
(define (racket-after-tick   balls-li r)
  (
   cond
    ((racket-selected? r) r)
    
    ((empty? (rest balls-li ))  r)
    
    ((is-ball-racket-collision? (first balls-li) r)
     (racket-vy-0 r))
    (else
     (racket-after-tick  (rest balls-li) r))))             


;; ************ TESTS***********************
(begin-for-test
  (check-equal?
   (racket-after-tick
    (list (make-ball 310 501 5 4))
    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN))

   (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN) 
   "racket-vy must not be 0 as the vel of racket is in "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-after-tick: Racket BallList => BallList

;; GIVEN   : r          - Racket    type - represents a Racket in the world
;;         : balls-li   - BallList type - represents a list of balls 

;; RETURNS : balls-li   - BallList type, where
;;                        each and every ball in balls-li is passed to
;;                        the function ball-after-tick

;;  NOTE : the empty elements in the list are removed, as
;;  empty elements in the middle will cause unexpected behaviour
;;  in the follow up functions

;; STRATEGY :  Using HOF map along with lambda on balls-li 

;; EXAMPLES :
;;  case 1:
;; If all the balls in balls-li collides with the back wall

;;  (balls-after-tick
;;    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
;;    (list
;;      (make-ball 10  1000 -9 5)))
;;                             =>   empty

;;  case 2:
;; If any of the balls in the middle of llst collides with the back-wall
;;(balls-after-tick
;;    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
;;    (list
;;     (make-ball 10  1000 -9 5)
;;      empty
;;     (make-ball 10  1000 -9 5)))    =>
;;
;; (list
;;     (make-ball 10  10 -9 5)
;;     (make-ball 10  10 -9 5))

;; ********************FUNCTION DEFINITION**************************

( define (balls-after-tick r balls-li )
   (remove empty
           (map (lambda (b) (ball-after-tick r b))  balls-li)))
;; ********** TESTS**************************************

(begin-for-test
  (check-equal?
   (balls-after-tick
    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
    (list
     (make-ball 10  1000 -9 5)))
   empty
   "Ball list should be empty, since the only ball in the list
will also disappear" )

  (check-equal?
   (balls-after-tick
    (make-racket 300 500 5 -9  INITIAL-X INITIAL-Y BUTTON-DOWN)
    (list
     (make-ball 10  10 -9 5)
     (make-ball 10  1000 -9 5)
     (make-ball 10  10 -9 5)))
   (list
    (make-ball 10  10 -9 5)
    (make-ball 10  10 -9 5))
   "Ball list should be empty, since the only ball in the list
will also disappear" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST CASES FROM PREVIOUS PROBLEMS

;; Abbreviation
(define ++ string-append)

;; Defining some constants
(define SPACE-BAR " ")
(define B-KEY "b")
;;(define BUTTON-UP "button-up")
;;(define BUTTON-DOWN "button-down")
;;(define DRAG "drag")

(define UP "up")
(define SPEED 0.5)


;;Initial world with 0.5 secs per tick
(define INITIAL-WORLD (initial-world SPEED))


;; Initial number of balls
(define INIT-BALL 1)


;; World in a rally state
(define WORLD-RALLY-STATE (world-after-key-event
                           INITIAL-WORLD SPACE-BAR))


;; World with a racket with 3 speed in up direction.
(define WORLD-RACKET-WITH-3SPEED
  (world-after-key-event
   (world-after-key-event
    (world-after-key-event WORLD-RALLY-STATE UP) UP) UP))


;; world-after-n-ticks : World PosInt -> World
;; Given a state of a World and a positive integer n
;; Returns World after n number of ticks.
(define (world-after-n-ticks w n)
  (if (= n 0)
      w
      (world-after-n-ticks (world-after-tick w) (- n 1))))


;; World with 2 new ball and 10 tick gap
(define WORLD-2NEW-BALLS-10-TICK
  (world-after-key-event
   (world-after-n-ticks
    (world-after-key-event WORLD-RALLY-STATE B-KEY) 10) B-KEY))


;; World with new two balls at the same position
(define NEW-BALL (second
                  (world-balls
                   (world-after-key-event
                    (world-after-key-event
                     (world-after-key-event
                      INITIAL-WORLD SPACE-BAR) B-KEY) B-KEY))))



;; world after 114 ticks with first ball near bottom wall
;; and with two new balls in the starting position
(define WORLD-AFTER-114-TICKS-2NEW-BALLS
  (world-after-key-event
   (world-after-key-event
    (world-after-n-ticks WORLD-RALLY-STATE 114) B-KEY) B-KEY))


;; Racket in selected state 
(define WORLD-SELECTED-RACKET
  (world-after-mouse-event
   WORLD-AFTER-114-TICKS-2NEW-BALLS 330 384 BUTTON-DOWN))


;; racket in new position (336, 367)
(define WORLD-DRAG-SELECTED-RACKET
  (world-after-mouse-event
   (world-after-mouse-event
    WORLD-SELECTED-RACKET 336 367 DRAG)
   336 367 BUTTON-UP))


;; WORLD-DRAG-SELECTED-RACKET after 2 ticks (116 ticks total)
;; (world has only two newly created balls)
(define WORLD-DRAG-2TICKS
  (world-after-n-ticks WORLD-DRAG-SELECTED-RACKET 2))


;; World where racket and 2 balls collide with tentative negative
;; velocity and with racket velocity as -3
(define WORLD-WITH-COLLISION
  (world-after-n-ticks
   (world-after-key-event
    (world-after-key-event
     (world-after-key-event
      (world-after-mouse-event
       (world-after-mouse-event
        (world-after-mouse-event
         (world-after-n-ticks WORLD-AFTER-114-TICKS-2NEW-BALLS 62)
         330 384 BUTTON-DOWN)
        330 198 DRAG)
       330 198 BUTTON-UP)
      UP)
     UP)
    UP)
   2))



;; (test-case
;; "Test #1"
;pressing key b in ready-to-serve state
(begin-for-test
  (check-equal?
   (length (world-balls (world-after-key-event INITIAL-WORLD B-KEY)))
   INIT-BALL
   "pressing key b in ready-to-serve state should not add new ball")
   
   
   
  ;; (test-case
  ;; "Test #2"
  ;pressing key b in rally state
  (check-equal?
   (length (world-balls
            (world-after-key-event WORLD-RALLY-STATE B-KEY)))
   (+ INIT-BALL 1)
   "pressing key b in rally state should add new ball")
   
   
  ;; (test-case
  ;; "Test #3"
  ;; world after 115 ticks(first new ball hits the bottom wall) + 1 tick
  ;; should remove the first added new ball
  (check-equal?
   (length (world-balls
            (world-after-n-ticks WORLD-2NEW-BALLS-10-TICK 106)))
   1
   (++ "world after 115 ticks
(first new ball hits the bottom wall) + 1 tick "
       "should remove the first added new ball"))
   
   
  ;; (test-case
  ;; "Test #4"
  ;; world after 125 ticks(first new ball hits the bottom wall) + 1 tick
  ;; should remove the second added new ball
  (check-equal?
   (length (world-balls
            (world-after-n-ticks WORLD-2NEW-BALLS-10-TICK 116)))
   0
   (++ "world after 125 ticks
(second new ball hits the bottom wall) + 1 tick "
       "should remove the second added new ball"))
   
  ;; (test-case
  ;; "Test #5"
  ;; world after 125 ticks(first new ball hits the bottom wall)
  ;;   + 6 ticks(3 seconds) + 1 tick (here tick = .5s)
  ;; should not give a world at ready to serve state
  (check-equal?
   (world-ready-to-serve?
    (world-after-n-ticks
     WORLD-2NEW-BALLS-10-TICK (+ 116 (/ 3 SPEED ))))
   true
   (++ "The world should be in ready-to-serve state after 3 seconds of "
       "real time after all balls had collided
bottom wall and disappeared "
       "after 125 tick"))
   
   
  ;; test for initial position of newly added ball
   
  ;; (test-case
  ;; "Test #6"
  ;velocity of new ball vy
  (check-equal? (ball-vy NEW-BALL) -9
                " initial velocity along Y axis is -9 ")
   
   
  ;; (test-case
  ;; "Test #7"
  ;velocity of new ball vx
  (check-equal? (ball-vx NEW-BALL) 3
                " initial velocity along X axis is 3 ")
   
   
   
  ;; (test-case
  ;; "Test #8"
  ;position of new ball y
  (check-equal? (ball-y NEW-BALL) 384
                " initial position along Y axis is 384 ")
   
   
  ;; (test-case
  ;; "Test #9"
  ;position of new ball x
  (check-equal? (ball-x NEW-BALL) 330
                " initial position along X axis is 330 ")
   
   
   
  ;; (test-case
  ;; "Test #10"
  ;Ball with negative velocity should not collide with racket
  (check-equal?
   (and ( = (ball-vy (first (world-balls WORLD-DRAG-2TICKS))) -9)
        ( = (ball-vy (second (world-balls WORLD-DRAG-2TICKS))) -9))
   #t
   " both the balls should not collide with racket ")
   
   
   
  ;; (test-case
  ;; "Test #11"
  ;Ball with positive velocity should  collide with racket
  (check-equal?
   (and ( = (ball-vy (first (world-balls WORLD-WITH-COLLISION))) -12)
        ( = (ball-vy (second (world-balls WORLD-WITH-COLLISION))) -12))
   #t
   " both the balls should collide with racket independently")
   
   
  ;; (test-case
  ;; "Test #12"
  ;Racket's velocity should change to zero after collision
  (check-equal?
   (racket-vy (world-racket WORLD-WITH-COLLISION)) 0
   " racket's vy velocity should reset to zero after collision")
   
   
  ;; (test-case
  ;; "Test #13 (was Test #12 from set03 q1)"
  ;; world after 43 ticks (ball hits y axis top) (391,3)
  (check-equal?
   (ball-y
    (first (world-balls
            (world-after-n-ticks WORLD-RALLY-STATE 43))))
   3
   "ball's position in y should be 3 after 43 ticks")
   
   
  ;; (test-case
  ;; "Test #14 (was Test #20 from set03 q1)"
  ;; world after 115 ticks(ball hits the bottom wall)
  ;;   + 6 ticks(3 seconds) + 1 tick (here tick = .5s)
  ;; should give a world at ready to serve state
  (check-equal?
   (world-ready-to-serve?
    (world-after-n-ticks WORLD-RALLY-STATE (+ 116 (/ 3 SPEED ))))
   true
   (++ "The world should be in ready-to-serve state after 3 seconds of "
       "real time the ball collides bottom wall after 115 tick"))
   
  ;; (test-case
  ;; "Test #15 (was Test #7 from set03 q2)"
  ;Racket should not move when it is selected
  (check-equal?
   (racket-y
    (world-racket
     (world-after-tick
      (world-after-mouse-event WORLD-RACKET-WITH-3SPEED
                               335 388 BUTTON-DOWN))))
   384
   "Racket should not move when it is selected"))