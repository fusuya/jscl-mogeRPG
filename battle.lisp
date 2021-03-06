

(defstruct status
  (hp 30)
  (agi 30)
  (str 30))

(defparameter *cmd-y* '(430 460 490))

(let ((moge (make-status))
      (cmd-cs 0))
(defun render-battle (ctx w h)
  (labels ((rgb (r g b)
             (format nil "rgb(~A,~A,~A)"
                     (truncate (* 255 r))
                     (truncate (* 255 g))
                     (truncate (* 255 b))))
           (draw-bg ()
             (setf (jscl::oget ctx "fillStyle")
                   (rgb 0 0 0))
             ((jscl::oget ctx "fillRect") 0 0 w h))
	   (draw-status-waku ()
	     (setf (jscl::oget ctx "strokeStyle")
		   (rgb 1 1 1))
	     ((jscl::oget ctx "strokeRect") 5 5 100 100))
	   
	   (draw-status ()
	     (setf (jscl::oget ctx "font")
		   (format nil "20px monospace")
		   (jscl::oget ctx "fillStyle") (format nil "white"))
	     ((jscl::oget ctx "fillText") (format nil "HP  ~d" (status-hp moge)) 10 30)
	     ((jscl::oget ctx "fillText") (format nil "STR ~d" (status-str moge)) 10 55)
	     ((jscl::oget ctx "fillText") (format nil "AGI ~d" (status-agi moge)) 10 80))
	   (draw-cmd-waku ()
	     (setf (jscl::oget ctx "strokeStyle")
		   (rgb 1 1 1))
	     ((jscl::oget ctx "strokeRect") 5 400 200 100))
	   (draw-cmd ()
	     (let ((cmd-cs-y (nth cmd-cs *cmd-y*)))
	     (setf (jscl::oget ctx "font")
		   (format nil "20px monospace")
		   (jscl::oget ctx "fillStyle") (format nil "white"))
	     ((jscl::oget ctx "fillText") "▶" 10 cmd-cs-y 200)
	     ((jscl::oget ctx "fillText") "突く" 40 430 200)
	     ((jscl::oget ctx "fillText") "ダブルアタック" 40 460 200)
	     ((jscl::oget ctx "fillText") "なぎ払う" 40 490 200))))
    (draw-bg)
    (draw-status-waku)
    (draw-status)
    (draw-cmd-waku)
    (draw-cmd)))



(defun iter (timestamp)
    (let* ((canvas (#j:document:getElementById "screen"))
	   (kabe-img (#j:document:getElementById "kabe"))
	   (moge-img (#j:document:getElementById "moge"))
	   (ctx ((jscl::oget canvas "getContext") "2d"))
	   (w (jscl::oget canvas "width"))
	   (h (jscl::oget canvas "height")))
      
      (render-battle ctx w h))
  (#j:window:requestAnimationFrame #'iter)))

(#j:window:requestAnimationFrame #'iter)
