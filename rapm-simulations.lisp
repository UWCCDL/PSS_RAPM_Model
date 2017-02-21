;;; ================================================================
;;; RAPM SIMULATIONS
;;; ----------------------------------------------------------------
;;; Contains code for running simulations of the RAPM model
;;; ----------------------------------------------------------------

(defun simulate (n &optional (res-file "results.csv"))
  (with-open-file (file res-file
			   :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
    (let ((names (list 'd2 'ticks 'alpha 'egs 'accuracy 'problem 'choice)))
      (format file "~{~a~^, ~}~%" names))

    (dolist (d2 '(1/2 1 3/2 2 5/2 3 7/2 4))
      (dolist (ticks '(10 15 20 25 30 35 40))
	(dolist (alpha '(0 2/10 4/10 6/10 8/10 1))
	  (dolist (egs '(0 1/10 2/10 3/10 4/10 5/10))
	    (format t "~{~a~^, ~}~%" (list 'd2 d2 'ticks ticks 'alpha alpha 'egs egs))
	    (dotimes (j n)
	      (rapm-reload)  ; Reload
	      (setf *d2* d2)
	      (setf *ticks* ticks)
	      (sgp-fct `(:egs ,egs :alpha ,alpha :v nil)) ; Sets the params
	      (run 1000 :real-time nil)
	      (let* ((trial (first (experiment-log (current-device))))
		     (res (list d2 ticks alpha egs
				(trial-accuracy trial)
				(trial-problem-rt trial)
				(trial-choice-rt trial))))
		(format file "~{~a~^, ~}~%" (mapcar #'float res))))))))))
