(defpackage "MAP-EDITOR"
  (:use :clim :clim-lisp)
  (:export "MAP-EDITOR-MAIN"))

(in-package :map-editor)

(defvar *taille-carre* 30)
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

(defun fill-canvas ()
  (dotimes (x (length *map*))
    (dotimes (y (length *map*))
      (let ((cas (nth x (nth y *map*))))
	(cond
	  ((not (= 0 cas)) (draw-rectangle* (find-pane-named *application-frame* 'canvas)
					    (* x *taille-carre*) (* y *taille-carre*)
					    (+ *taille-carre* (* x *taille-carre*)) (+ *taille-carre* (* y *taille-carre*)))))))))

(define-map-editor-command (com-refresh :name t) ()
  ())

(define-map-editor-command (com-touche :name t) ()
  (handle-pointer (find-pane-named *application-frame* 'canvas)))

(define-map-editor-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(defun handle-pointer (pane)
  (tracking-pointer (pane)
    (:pointer-button-release (&key event x y)
			     (change-map (truncate (/ x *taille-carre*)) (truncate (/ y *taille-carre*)))
			     (return-from handle-pointer))))

(defun change-map (x y &optional value)
  (setf (nth x (nth y *map*)) (or value (if (= 0 (nth x (nth y *map*))) 1 0))))

(defun map-editor-main ()
  (run-frame-top-level (make-application-frame 'map-editor)))
