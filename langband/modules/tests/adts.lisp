
(in-package :cl-user)

#||
(defun init-pool (num)
  (setf lb-ds::*pq-elem-pool* (make-array num :fill-pointer t))
  (dotimes (i num)
    (setf (aref lb-ds::*pq-elem-pool* i) (lb-ds::make-pq-elem))))
||#	

(defun run-queue-test (num pq)

  (lb-engine::lbsys/time
    (loop for i from 0 to num
	  do (lb-ds:pq.put pq i (- num i))))
    
  (loop for i from 0 to num
	do (print (lb-ds:pq.get pq)))
    #||
    (loop for i from 0 to num
	  do (lb-ds:pq.put pq (random num) (random num)))

    (loop for i from 0 to (lb-ds::heap-size pq)
		     do (print (aref(lb-ds::heap-array pq) i)))
    (warn "----")
    
    (loop for i from 0 to num
	  do (progn
	       (warn "Size on go ~s is ~s" (1+ i) (lb-ds::heap-size pq))
	       (loop for i from 1 to (lb-ds::heap-size pq)
		     do (print (aref (lb-ds::heap-array pq) i)))
	       (warn "+++")
	       (warn "removed ~s" (lb-ds:pq.get pq))))

    (warn "-end-")
    (lb-ds:pq.count pq)
    ||#
    )

(defun s (num)
  (let ((pq (lb-ds:make-pq)))
    ;;(init-pool num)
    ;;(lb-engine::lbsys/time (run-queue-test (- num 5) pq))
    (run-queue-test (- num 5) pq)
    ;;(lb-engine::tricky-profile (run-queue-test (- num 5) pq) :space)
    ))
