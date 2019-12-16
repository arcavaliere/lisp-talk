(defstruct markov-node
;; We track two words because English language generation feels more "natural" if we use 2 word prefixs
  first-prefix
  second-prefix
  suffixes)

(defun choose-random (items)
  (nth (random (length items)) items))

(defun make-prefix-word (first second) (concatenate 'string first " " second))

(defun load-file-contents (file-path)
  (remove "" (uiop:split-string (format nil "~{~^ ~a~^ ~}" (uiop:read-file-lines file-path)) :separator " ") :test #'equal))

(defun init-markov-table (words table)
  (let ((hash-table table))
    (dotimes (i (- (length words) 3))
      (let* ((prefix-part-one (elt words i))
             (prefix-part-two (elt words (+ i 1)))
             (suffix (elt words (+ i 2)))
             (prefix (make-prefix-word prefix-part-one prefix-part-two)))
        (if (equal (nth-value 1 (gethash prefix hash-table)) nil)
            (setf (gethash prefix hash-table) (make-markov-node :first-prefix prefix-part-one :second-prefix prefix-part-two :suffixes (list suffix)))
            (push suffix (markov-node-suffixes (nth-value 0 (gethash prefix hash-table)))))))
    hash-table))

(defun generate (table iterations)
  (let* ((prefix-list (loop for key being the hash-keys of table collect key))
         (node (gethash (choose-random prefix-list) table))
         (word (choose-random (markov-node-suffixes node))))
    (format t "~a ~a " (markov-node-first-prefix node) (markov-node-second-prefix node))
    (dotimes (count iterations)
      (format t "~a " word)
      (setq node (gethash (format nil "~a ~a" (markov-node-second-prefix node) word) table))
      (setq word (choose-random (markov-node-suffixes node))))))


(defun run-text-generation (file-paths iterations)
  (let ((hash-table (make-hash-table :test #'equal)))
    (loop :for file in file-paths
          do (init-markov-table (load-file-contents file) hash-table))
    (generate hash-table iterations)))
