(defvar *book-path* "./pg10002.txt")
(defvar *boats-of-the-glen-carrig* "./pg10542.txt")
(defvar *file-contents* (remove "" (uiop:split-string (format nil "~{~^ ~a~^ ~}" (uiop:read-file-lines *book-path*)) :separator " ") :test #'equal))
(defvar *boats-contents* (remove "" (uiop:split-string (format nil "~{~^ ~a~^ ~}" (uiop:read-file-lines *boats-of-the-glen-carrig*)) :separator " ") :test #'equal))

(defstruct node
  word
  following-words)

(defun node-in-list (word nodes)
  (cond ((equal (car nodes) nil) nil) ; base case
	((equal word (node-word (car nodes))) (car nodes)) ; return the node!
	(t (node-in-list word (cdr nodes))))) ; keep digging!

(defun create-node (word nodes)
  (let ((new-nodes nodes))
    (push (make-node :word word :following-words nil) new-nodes)))

(defun update-node-list (word following nodes)
  (push following (node-following-words (node-in-list word nodes))))

(defun make-prefix-word (first second) (concatenate 'string first " " second))

(defun create-word-nodes (words nodes)
  (cond ((equal (cadr words) nil) nodes)
        ((equal (node-in-list (make-prefix-word (first words) (second words)) nodes) nil)
         (create-word-nodes (cdr words) (create-node (make-prefix-word (first words) (second words)) nodes)))
        (t (create-word-nodes (cdr words) nodes))))

(defun populate-nodes (words nodes)
  (let ((max (length words))
        (new-nodes nodes))
    (dotimes (i max)
       (if (= (+ i 2) max)
           (return new-nodes)
           (update-node-list (make-prefix-word (elt words i) (elt words (+ i 1))) (elt words (+ i 2)) new-nodes)))))


(defun generate (node-list iterations)
  (let* ((start (nth (random (length node-list)) node-list))
        (current-prefix (node-word start))
        (current-suffix (node-following-words start))
        (word (nth (random (length current-suffix)) current-suffix))
        (nodes node-list))
    (dotimes (count (- iterations 2))
      (format t "~a " word)
      (setq start (node-in-list (make-prefix-word (second (uiop:split-string current-prefix :separator " ")) word) nodes))
      (setq current-prefix (node-word start))
      (setq current-suffix (node-following-words start))
      (setq word (nth (random (length current-suffix)) current-suffix)))))

(defun generate (node-list iterations)
  (let* ((node (nth (random (length node-list)) node-list))
         (word (nth (random (length (node-following-words node))) (node-following-words node))))
    (dotimes (count (- iterations 2))
      (format t "~a " word)
      (setq node (node-in-list (make-prefix-word (second (uiop:split-string (node-word node) :separator " ")) word) node-list))
      (setq word (nth (random (length (node-following-words node))) (node-following-words node))))))

;; -----------------------------------------------------------------------------------------------------------

(defstruct markov-node
  first-prefix
  second-prefix
  suffixes)

(defun compare-markov-nodes (node-a node-b)
  (if (and (equal
            (markov-node-first-prefix node-a)
            (markov-node-first-prefix node-b))
           (equal
            (markov-node-second-prefix node-a)
            (markov-node-second-prefix node-b)))
      t
      nil))

(defun find-node (first-prefix second-prefix nodes)
  (loop :for node in nodes do
    (cond ((and
         (equal
          (markov-node-first-prefix node)
          first-prefix)
         (equal
          (markov-node-second-prefix node)
          second-prefix))
        (return node)))))

(defun initialize-nodes (words)
  (let ((nodes '()))
    (dotimes (i (- (length words) 1))
      (cond ((equal (find-node (elt words i) (elt words (+ i 1)) nodes) nil)
             (push
              (make-markov-node
               :first-prefix (elt words i)
               :second-prefix (elt words (+ i 1))
               :suffixes nil) nodes))))
  nodes))

(defun populate-suffixes (words nodes)
  (dotimes (i (- (length words) 2))
    (push (elt words (+ i 2)) (markov-node-suffixes (find-node (elt words i) (elt words (+ i 1)) nodes))))
  nodes)

(defun build-node-list (words)
  (let ((nodes (initialize-nodes words)))
    (setq nodes (populate-suffixes words nodes))
    nodes))

(defun choose-random (items)
  (nth (random (length items)) items))

(defun generate (nodes iterations)
  (let* ((node (choose-random nodes))
         (word (choose-random (markov-node-suffixes node))))
    (format t "~a ~a " (markov-node-first-prefix node) (markov-node-second-prefix node))
    (dotimes (count iterations)
      (format t "~a " word)
      (setq node (find-node (markov-node-second-prefix node) word nodes))
      (setq word (choose-random (markov-node-suffixes node))))))

