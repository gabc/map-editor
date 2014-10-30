(defpackage "MAP-EDITOR"
  (:use :clim :clim-lisp)
  (:export "MAP-EDITOR-MAIN"))

(in-package :map-editor)

(defclass canvas-pane (application-pane)
  ((first-point-x :initform nil)
   (first-point-y :initform nil)))

(define-application-frame map-editor ()
  ()
  (:panes
   (canvas (make-pane 'canvas-pane
		      :name 'canvas
		      :display-time nil))
   (int :interactor
	:width 100
	:height 100))
   (:layouts
    (default canvas int)))

(define-map-editor-command (com-draw :name t) ((n 'integer))
  (draw-line* (find-pane-named *application-frame* 'canvas) 0 0 10 n))

(define-map-editor-command (com-touche :name t) ()
  (handle-pointer (find-pane-named *application-frame* 'canvas)))

(defun handle-pointer (pane)
  (tracking-pointer (pane)
    (:pointer-button-release (&key event x y)
			     (format t "~a ~a~%" x y)
			     (return-from handle-pointer))))

(defun map-editor-main ()
  (run-frame-top-level (make-application-frame 'map-editor)))
