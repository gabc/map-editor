(defpackage "MAP-EDITOR"
  (:use :clim :clim-lisp)
  (:export "MAP-EDITOR-MAIN"))

(in-package :map-editor)

(defvar *taille-carre* 30)
(defvar *click-val* 0)
(defvar *map*
  '((0 0 0 0 0)
    (0 0 0 0 0)
    (1 0 0 0 1)
    (1 1 1 1 1)
    (2 2 3 2 2)))

(setq *map*
  '((0 0 0 0 0)
    (0 0 0 0 0)
    (1 0 0 0 1)
    (1 1 1 1 1)
    (2 2 3 2 2)))

(defclass canvas-pane (application-pane)
  ((first-point-x :initform nil)
   (first-point-y :initform nil)))

(define-application-frame map-editor ()
  ()
  (:panes
   (canvas (make-pane 'canvas-pane
		      :name 'canvas
		      :incremental-redisplay t
		      :display-function 'display-canvas))
   (int :interactor
	:width 100
	:height 50))
   (:layouts
    (default canvas int)))

(defun display-canvas (frame pane)
  (draw)
  (fill-canvas))
  
(defun draw ()
  (loop for i from 0 upto 100
     do (draw-line* (find-pane-named *application-frame* 'canvas) (* i *taille-carre*) 0
		    (* i *taille-carre*) (* 200 *taille-carre*)))
  (loop for i from 0 upto 100
     do (draw-line* (find-pane-named *application-frame* 'canvas) 0 (* i *taille-carre*)
		    (* 500 *taille-carre*) (* i *taille-carre*))))

(defun draw-colored-square (x y color)
  (draw-rectangle* (find-pane-named *application-frame* 'canvas)
					    (+ (* x *taille-carre*) 1) (+ (* y *taille-carre*) 1)
					    (- (+ *taille-carre* (* x *taille-carre*)) 1) (- (+ *taille-carre* (* y *taille-carre*)) 1)
					    :ink color))

(defun fill-canvas ()
  (dotimes (x (length *map*))
    (dotimes (y (length *map*))
      (let ((cas (elt (elt *map* y) x)))
	(cond
	  ((= 0 cas) (draw-colored-square x y +blue+))
	  ((= 1 cas) (draw-colored-square x y +green+))
	  ((= 2 cas) (draw-colored-square x y +red+))
	  ((= 3 cas) (draw-colored-square x y +grey+))
	  (t (draw-colored-square x y +white+)))))))

(define-map-editor-command (com-refresh :name t) ()
  ())

(define-map-editor-command (com-set-value :name t) ((n 'integer))
  (setf *click-val* n))

(define-map-editor-command (com-touche :name t) ()
  (handle-pointer (find-pane-named *application-frame* 'canvas) *click-val*))

(define-map-editor-command (com-set-size :name t) ((x 'integer) (y 'integer))
  (dotimes (i (length *map*))		; x
    (setf *map* (resize-map *map* x)))
  (dotimes (i (length *map*))
    (setf (elt *map* i) (resize-map (elt *map* i) y)))
  (dotimes (i (length *map*))
    (dotimes (j (length (elt *map* i)))
      (if (eq (elt (elt *map* i) j) nil)
	  (setf (elt (elt *map* i) j) 0)))))
  
(define-map-editor-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(defun handle-pointer (pane value)
  (tracking-pointer (pane)
    (:pointer-button-release (&key event x y)
			     (change-map (truncate (/ x *taille-carre*)) (truncate (/ y *taille-carre*)) value)
			     (return-from handle-pointer))))

(defun resize-map (set n)
  (cond 
    ((= (length set) n) nil)
    ((> (length set) n)			; Plus grand
     (setf set (subseq set 0 n)))
    ((< (length set) n)			;Plus petit
     (dotimes (i (- n (length set)))
       (setf set (cons nil set)))))
  set)

(defun change-map (x y &optional value)
  (setf (nth x (nth y *map*)) (or value (if (= 0 (nth x (nth y *map*))) 1 0))))

(defun map-editor-main ()
  (run-frame-top-level (make-application-frame 'map-editor)))
