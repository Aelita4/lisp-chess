;; starting board
(defvar board '(
    ("r" "n" "b" "q" "k" "b" "n" "r")
    ("p" "p" "p" "p" "p" "p" "p" "p")
    ("." "." "." "." "." "." "." ".")
    ("." "." "." "." "." "." "." ".")
    ("." "." "." "." "." "." "." ".")
    ("." "." "." "." "." "." "." ".")
    ("P" "P" "P" "P" "P" "P" "P" "P")
    ("R" "N" "B" "Q" "K" "B" "N" "R")
))

;; 2 kings close
;; (defvar board '(
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "K" "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "k" "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;; ))

;; check threat
;; (defvar board '(
;;     ("." "." "." "." "k" "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "P" "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "K" "Q" "." ".")
;; ))

;; checkmate threat
;; (defvar board '(
;;     ("." "." "." "." "." "." "k" ".")
;;     ("." "." "." "." "Q" "." "." ".")
;;     ("." "." "p" "." "." "." "." ".")
;;     ("." "." "." "." "K" "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "R" "." ".")
;; ))

;; another checkmate threat
;; (defvar board '(
;;     ("." "." "." "." "." "." "k" ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "R" "." ".")
;;     ("K" "." "." "." "." "." "." "R")
;; ))

;; promotion
;; (defvar board '(
;;     ("." "." "." "." "r" "." "." "k")
;;     ("." "." "." "P" "." "P" "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "." "." "." "." ".")
;;     ("." "." "." "p" "p" "." "." ".")
;;     ("K" "." "." "." "." "R" "." ".")
;; ))

;; flag for checking future attacks
(defvar checking-attack-mode nil)

;; current player ("W" or "B")
(defvar current-player "W")

;; en passant column
(defvar en-passant-col -1)

;; display board
(defun display-board ()
    (let ((row-index 8))
        (princ "   | A | B | C | D | E | F | G | H |")
        (write-line "")
        (princ "---+---+---+---+---+---+---+---+---+")
        (write-line "")
        (dolist (row board)
            (princ " ")
            (princ row-index)
            (setf row-index (- row-index 1))
            (princ " | ")
            (dolist (cell row)
                (princ cell)
                (princ " | ")   
            )
            (write-line "")
            (write-line "---+---+---+---+---+---+---+---+---+")
        )
    )
)

;; convert char A-H to 1-8
(defun char-to-num (ch)
    (let ((ascii (char-int ch)))
        (cond 
            ((equal ascii 49) 1)
            ((equal ascii 50) 2)
            ((equal ascii 51) 3)
            ((equal ascii 52) 4)
            ((equal ascii 53) 5)
            ((equal ascii 54) 6)
            ((equal ascii 55) 7)
            ((equal ascii 56) 8)
            (t -1)
        )
    )
)

;; convert num 1-8 to char A-H
(defun index-to-col (index)
    (cond 
        ((equal index 1) "A")
        ((equal index 2) "B")
        ((equal index 3) "C")
        ((equal index 4) "D")
        ((equal index 5) "E")
        ((equal index 6) "F")
        ((equal index 7) "G")
        ((equal index 8) "H")
        (t "X")
    )
)

;; convert char A-H to 1-8
(defun col-to-index (col)
    (cond 
        ((equal col (char "A" 0)) 1)
        ((equal col (char "B" 0)) 2)
        ((equal col (char "C" 0)) 3)
        ((equal col (char "D" 0)) 4)
        ((equal col (char "E" 0)) 5)
        ((equal col (char "F" 0)) 6)
        ((equal col (char "G" 0)) 7)
        ((equal col (char "H" 0)) 8)
        (t -1)
    )
)

;; concat 2 strings
(defun concat (a b)
    (concatenate 'string a b)
)

;; check if square is empty (".")
(defun is-square-empty (row column)
    (equal (nth (- column 1) (nth (- row 1) board)) ".")
)

;; determine whose piece is on square
(defun whose-piece (row column)
    (if (equal (nth (- column 1) (nth (- row 1) board)) ".") 'empty
        (if (upper-case-p (char (nth (- column 1) (nth (- row 1) board)) 0)) "W" "B")
    )
)

;; get piece on square
(defun get-piece (row column)
    (nth (- column 1) (nth (- row 1) board))
)

;; get opposite color
(defun get-opposite-color (color)
    (if (equal color "W") "B" "W")
)

;; convert row 1-8 to list index 0-7
(defun row-to-list-index (row)
    (cond 
        ((equal row 1) 7)
        ((equal row 2) 6)
        ((equal row 3) 5)
        ((equal row 4) 4)
        ((equal row 5) 3)
        ((equal row 6) 2)
        ((equal row 7) 1)
        ((equal row 8) 0)
        (t -1)
    )
)

;; convert list index 0-7 to row 1-8
(defun list-index-to-row (index)
    (cond 
        ((equal index 8) 1)
        ((equal index 7) 2)
        ((equal index 6) 3)
        ((equal index 5) 4)
        ((equal index 4) 5)
        ((equal index 3) 6)
        ((equal index 2) 7)
        ((equal index 1) 8)
        (t -1)
    )
)

;; determine piece color
(defun piece-to-color (piece)
    (if (upper-case-p (char piece 0)) "W" "B")
)

;; check if square is attacked
(defun check-if-square-attacked (row column player)
    (if (or (equal row -1) (equal column -1)) (return-from check-if-square-attacked nil))
    (let ((row-index 8) (column-index 1) (is-attacked nil))
        (dolist (row2 board)
            (dolist (cell row2)
                (unless (equal cell ".")
                       (when (equal (piece-to-color cell) (get-opposite-color player))
                           (if (or (equal cell "p") (equal cell "P")) ;; pawn attack differs from how it moves
                               (progn
                                    (when (> column 1)
                                        (when (and
                                            (equal row (+ (list-index-to-row row-index) (if (equal player "W") 1 -1)))
                                            (equal column (+ column-index 1))
                                        )
                                            (setf is-attacked t)
                                        )
                                    )                              
                                   
                                    (when (< column 8)
                                        (when (and
                                            (equal row (+ (list-index-to-row row-index) (if (equal player "W") 1 -1)))
                                            (equal column (- column-index 1))
                                        ) 
                                            (setf is-attacked t)
                                        )
                                    )
                               )
                               (when
                                   (is-in-list
                                       (concat
                                           (index-to-col column)
                                           (write-to-string (list-index-to-row row))
                                       )
                                       (check-legal-moves (list-index-to-row row-index) column-index cell (get-opposite-color player) nil))
                                   (setf is-attacked t)
                               )
                           )
                       )
                )
                (setf column-index (+ column-index 1))
            )
            (setf row-index (- row-index 1))
            (setf column-index 1)
        )
        is-attacked
    )
)

;; search for player's king and check if it is attacked
(defun check-for-check (player)
    (let ((king-row -1) (king-column -1) (row-index 8) (column-index 1))
        (dolist (row board)
            (dolist (cell row)
                (when (equal cell (if (equal player "W") "K" "k"))
                    (setf king-row (list-index-to-row row-index))
                    (setf king-column column-index)
                )
                (setf column-index (+ column-index 1))
            )
            (setf row-index (- row-index 1))
            (setf column-index 1)
        )

        (check-if-square-attacked king-row king-column player)
    )
)

;; simulate move and check if king is attacked
(defun check-for-future-attack (player row-from column-from row-to column-to)
    (when checking-attack-mode (return-from check-for-future-attack nil))
    (setf checking-attack-mode t)
    (let ((backup-board (mapcar #'copy-list board)) (is-attacked nil))
        (setf (nth (- column-to 1) (nth (- row-to 1) board)) (nth (- column-from 1) (nth (- row-from 1) board)))
        (setf (nth (- column-from 1) (nth (- row-from 1) board)) ".")
        (setf is-attacked (check-for-check player))
        (setf board backup-board)
        (setf checking-attack-mode nil)
        is-attacked
    )
)

;; check for mate (no legal moves)
(defun check-for-mate (player)
    (let ((row-index 8) (column-index 1))
        (dolist (row board)
            (dolist (cell row)
                (when (equal (piece-to-color cell) player)
                    (when (check-legal-moves (list-index-to-row row-index) column-index cell player T)
                        (return-from check-for-mate nil)
                    )
                )
                (setf column-index (+ column-index 1))
            )
            (setf row-index (- row-index 1))
            (setf column-index 1)
        )
        T
    )
)

(defun check-legal-moves-pawn (row column player)
    (let ((legal-moves '()))
        (cond 
            ((equal player "W") 
                ;; check square in front
                (when (is-square-empty (- row 1) column)
                    (unless (check-for-future-attack player row column (- row 1) column)
                        (push (concat (index-to-col column) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                    )
                )

                ;; if starting pawn, check for moving 2 spaces
                (when (equal row 7)
                    (when (and (is-square-empty 6 column) (is-square-empty 5 column))
                        (unless (check-for-future-attack player row column 5 column)
                            (push (concat (index-to-col column) (write-to-string 4)) legal-moves)
                        )
                    )
                )

                ;; check diagonals for capture
                (when (> column 1)
                    (when (equal (whose-piece (- row 1) (- column 1)) "B")
                        (unless (check-for-future-attack player row column (- row 1) (- column 1))
                            (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                        )
                    )
                )

                (when (< column 8)
                    (when (equal (whose-piece (- row 1) (+ column 1)) "B")
                        (unless (check-for-future-attack player row column (- row 1) (+ column 1))
                            (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                        )
                    )
                )
                
                ;; en passant
                (when (equal row 4)
                    (when (or (equal en-passant-col (- column 1)) (equal en-passant-col (+ column 1)))
                        (unless (check-for-future-attack player row column 3 en-passant-col)
                            (push (concat (index-to-col en-passant-col) (write-to-string (list-index-to-row 3))) legal-moves)
                        )
                    )
                )
            )
            ((equal player "B") 
                ;; check square in front
                (when (is-square-empty (+ row 1) column)
                    (unless (check-for-future-attack player row column (+ row 1) column)
                        (push (concat (index-to-col column) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                    )
                )

                ;; if starting pawn, check for moving 2 spaces
                (when (equal row 2)
                    (when (and (is-square-empty 3 column) (is-square-empty 4 column))
                        (unless (check-for-future-attack player row column 4 column)
                            (push (concat (index-to-col column) (write-to-string 5)) legal-moves)
                        )
                    )
                )

                ;; check diagonals for capture
                (when (> column 1)
                    (when (equal (whose-piece (+ row 1) (- column 1)) "W")
                        (unless (check-for-future-attack player row column (+ row 1) (- column 1))
                            (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                        )
                    )
                )

                (when (< column 8)
                    (when (equal (whose-piece (+ row 1) (+ column 1)) "W")
                        (unless (check-for-future-attack player row column (+ row 1) (+ column 1))
                            (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                        )
                    )
                )

                ;; en passant
                (when (equal row 5)
                    (when (or (equal en-passant-col (- column 1)) (equal en-passant-col (+ column 1)))
                        (unless (check-for-future-attack player row column 6 en-passant-col)
                            (push (concat (index-to-col en-passant-col) (write-to-string (list-index-to-row 6))) legal-moves)
                        )
                    )
                )
            )
        )
        legal-moves
    )
)

(defun check-legal-moves-rook (row column player)
    (let ((legal-moves '()))
        ;; up
        (unless (equal row 1)
            (do ((i (- row 1) (- i 1)))
                ((< i 1) nil)
                (if (is-square-empty i column)
                    (push (concat (index-to-col column) (write-to-string (list-index-to-row i))) legal-moves)
                
                    (if (equal (whose-piece i column) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column i column)
                                (push (concat (index-to-col column) (write-to-string (list-index-to-row i))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; down
        (unless (equal row 8)
            (do ((i (+ row 1) (+ i 1)))
                ((> i 8) nil)
                (if (is-square-empty i column)
                    (push (concat (index-to-col column) (write-to-string (list-index-to-row i))) legal-moves)
                
                    (if (equal (whose-piece i column) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column i column)
                                (push (concat (index-to-col column) (write-to-string (list-index-to-row i))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; left
        (unless (equal column 1)
            (do ((i (- column 1) (- i 1)))
                ((< i 1) nil)
                (if (is-square-empty row i)
                    (push (concat (index-to-col i) (write-to-string (list-index-to-row row))) legal-moves)
                
                    (if (equal (whose-piece row i) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column row i)
                                (push (concat (index-to-col i) (write-to-string (list-index-to-row row))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; right
        (unless (equal column 8)
            (do ((i (+ column 1) (+ i 1)))
                ((> i 8) nil)
                (if (is-square-empty row i)
                    (push (concat (index-to-col i) (write-to-string (list-index-to-row row))) legal-moves)
                
                    (if (equal (whose-piece row i) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column row i)
                                (push (concat (index-to-col i) (write-to-string (list-index-to-row row))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )
        legal-moves
    )
)

(defun check-legal-moves-knight (row column player)
    (let ((legal-moves '())) 
        ;; up
        (unless (<= row 2)
            (unless (<= column 1)
                (if (is-square-empty (- row 2) (- column 1))
                    (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (- row 2)))) legal-moves)
                    (when (equal (whose-piece (- row 2) (- column 1)) (get-opposite-color player))
                        (unless (check-for-future-attack player row column (- row 2) (- column 1))
                            (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (- row 2)))) legal-moves)
                        )
                    )
                )
            )

            (unless (>= column 8)
                (if (is-square-empty (- row 2) (+ column 1))
                    (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (- row 2)))) legal-moves)
                    (when (equal (whose-piece (- row 2) (+ column 1)) (get-opposite-color player))
                        (unless (check-for-future-attack player row column (- row 2) (+ column 1))
                            (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (- row 2)))) legal-moves)
                        )
                    )
                )
            )
        )

        ;; down
        (unless (>= row 7)
            (unless (<= column 1)
                (if (is-square-empty (+ row 2) (- column 1))
                    (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (+ row 2)))) legal-moves)
                    (when (equal (whose-piece (+ row 2) (- column 1)) (get-opposite-color player))
                        (unless (check-for-future-attack player row column (+ row 2) (- column 1))
                            (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (+ row 2)))) legal-moves)
                        )
                    )
                )
            )

            (unless (>= column 8)
                (if (is-square-empty (+ row 2) (+ column 1))
                    (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (+ row 2)))) legal-moves)
                    (when (equal (whose-piece (+ row 2) (+ column 1)) (get-opposite-color player))
                        (unless (check-for-future-attack player row column (+ row 2) (+ column 1))
                            (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (+ row 2)))) legal-moves)
                        )
                    )
                )
            )
        )

        ;; left
        (unless (<= column 2)
            (unless (<= row 1)
                (if (is-square-empty (- row 1) (- column 2))
                    (push (concat (index-to-col (- column 2)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                    (when (equal (whose-piece (- row 1) (- column 2)) (get-opposite-color player))
                        (unless (check-for-future-attack player row column (- row 1) (- column 2))
                            (push (concat (index-to-col (- column 2)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                        )
                    )
                )
            )

            (unless (>= row 8)
                (if (is-square-empty (+ row 1) (- column 2))
                    (push (concat (index-to-col (- column 2)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                    (when (equal (whose-piece (+ row 1) (- column 2)) (get-opposite-color player))
                        (unless (check-for-future-attack player row column (+ row 1) (- column 2))
                            (push (concat (index-to-col (- column 2)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                        )
                    )
                )
            )
        )

        ;; right
        (unless (>= column 7)
            (unless (<= row 1)
                (if (is-square-empty (- row 1) (+ column 2))
                    (push (concat (index-to-col (+ column 2)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                    (when (equal (whose-piece (- row 1) (+ column 2)) (get-opposite-color player))
                        (unless (check-for-future-attack player row column (- row 1) (+ column 2))
                            (push (concat (index-to-col (+ column 2)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                        )
                    )
                )
            )

            (unless (>= row 8)
                (if (is-square-empty (+ row 1) (+ column 2))
                    (push (concat (index-to-col (+ column 2)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                    (when (equal (whose-piece (+ row 1) (+ column 2)) (get-opposite-color player))
                        (unless (check-for-future-attack player row column (+ row 1) (+ column 2))
                            (push (concat (index-to-col (+ column 2)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                        )
                    )
                )
            )
        )
        legal-moves
    )
)

(defun check-legal-moves-bishop (row column player)
    (let ((legal-moves '()))
        ;; up-left
        (unless (or (equal row 1) (equal column 1))
            (do (
                    (x (- row 1) (- x 1))
                    (y (- column 1) (- y 1))
                )
                ((or (< x 1) (< y 1)) nil)
                (if (is-square-empty x y)
                    (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                
                    (if (equal (whose-piece x y) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column x y)
                                (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; up-right
        (unless (or (equal row 1) (equal column 8))
            (do (
                (x (- row 1) (- x 1))
                (y (+ column 1) (+ y 1))
                )
                ((or (< x 1) (> y 8)) nil)
                (if (is-square-empty x y)
                    (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                
                    (if (equal (whose-piece x y) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column x y)
                                (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; down-left
        (unless (or (equal row 8) (equal column 1))
            (do (
                (x (+ row 1) (+ x 1))
                (y (- column 1) (- y 1))
                )
                ((or (> x 8) (< y 1)) nil)
                (if (is-square-empty x y)
                    (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                
                    (if (equal (whose-piece x y) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column x y)
                                (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; down-right
        (unless (or (equal row 8) (equal column 8))
            (do (
                (x (+ row 1) (+ x 1))
                (y (+ column 1) (+ y 1))
                )
                ((or (> x 8) (> y 8)) nil)
                (if (is-square-empty x y)
                    (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                
                    (if (equal (whose-piece x y) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column x y)
                                (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )
        legal-moves
    )
)

(defun check-legal-moves-queen (row column player)
    (let ((legal-moves '()))
        ;; up
        (unless (equal row 1)
            (do ((i (- row 1) (- i 1)))
                ((< i 1) nil)
                (if (is-square-empty i column)
                    (push (concat (index-to-col column) (write-to-string (list-index-to-row i))) legal-moves)
                
                    (if (equal (whose-piece i column) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column i column)
                                (push (concat (index-to-col column) (write-to-string (list-index-to-row i))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; down
        (unless (equal row 8)
            (do ((i (+ row 1) (+ i 1)))
                ((> i 8) nil)
                (if (is-square-empty i column)
                    (push (concat (index-to-col column) (write-to-string (list-index-to-row i))) legal-moves)
                
                    (if (equal (whose-piece i column) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column i column)
                                (push (concat (index-to-col column) (write-to-string (list-index-to-row i))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; left
        (unless (equal column 1)
            (do ((i (- column 1) (- i 1)))
                ((< i 1) nil)
                (if (is-square-empty row i)
                    (push (concat (index-to-col i) (write-to-string (list-index-to-row row))) legal-moves)
                
                    (if (equal (whose-piece row i) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column row i)
                                (push (concat (index-to-col i) (write-to-string (list-index-to-row row))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; right
        (unless (equal column 8)
            (do ((i (+ column 1) (+ i 1)))
                ((> i 8) nil)
                (if (is-square-empty row i)
                    (push (concat (index-to-col i) (write-to-string (list-index-to-row row))) legal-moves)
                
                    (if (equal (whose-piece row i) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column row i)
                                (push (concat (index-to-col i) (write-to-string (list-index-to-row row))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; up-left
        (unless (or (equal row 1) (equal column 1))
            (do (
                (x (- row 1) (- x 1))
                (y (- column 1) (- y 1))
                )
                ((or (< x 1) (< y 1)) nil)
                (if (is-square-empty x y)
                    (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                
                    (if (equal (whose-piece x y) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column x y)
                                (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; up-right
        (unless (or (equal row 1) (equal column 8))
            (do (
                (x (- row 1) (- x 1))
                (y (+ column 1) (+ y 1))
                )
                ((or (< x 1) (> y 8)) nil)
                (if (is-square-empty x y)
                    (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                
                    (if (equal (whose-piece x y) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column x y)
                                (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; down-left
        (unless (or (equal row 8) (equal column 1))
            (do (
                (x (+ row 1) (+ x 1))
                (y (- column 1) (- y 1))
                )
                ((or (> x 8) (< y 1)) nil)
                (if (is-square-empty x y)
                    (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                
                    (if (equal (whose-piece x y) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column x y)
                                (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )

        ;; down-right
        (unless (or (equal row 8) (equal column 8))
            (do (
                (x (+ row 1) (+ x 1))
                (y (+ column 1) (+ y 1))
                )
                ((or (> x 8) (> y 8)) nil)
                (if (is-square-empty x y)
                    (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                
                    (if (equal (whose-piece x y) (get-opposite-color player))
                        (progn
                            (unless (check-for-future-attack player row column x y)
                                (push (concat (index-to-col y) (write-to-string (list-index-to-row x))) legal-moves)
                            )
                            (return)
                        )
                        (return)
                    )
                )
            ) 
        )
        legal-moves
    )
)

(defun check-legal-moves-king (row column player is-initial-move)
    (let ((legal-moves '()))
        ;; up
        (unless (equal row 1)
            (if (or 
                    (is-square-empty (- row 1) column)
                    (equal (whose-piece (- row 1) column) (get-opposite-color player))
                )
                (if is-initial-move
                    (unless (check-if-square-attacked (- row 1) column player)
                        (push (concat (index-to-col column) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                    )

                    (push (concat (index-to-col column) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                )
            )
        )

        ;; down
        (unless (equal row 8)
            (if (or 
                    (is-square-empty (+ row 1) column)
                    (equal (whose-piece (+ row 1) column) (get-opposite-color player))
                )
                (if is-initial-move
                    (unless (check-if-square-attacked (+ row 1) column player)
                        (push (concat (index-to-col column) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                    )

                    (push (concat (index-to-col column) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                )
            )
        )

        ;; left
        (unless (equal column 1)
            (if (or 
                    (is-square-empty row (- column 1))
                    (equal (whose-piece row (- column 1)) (get-opposite-color player))
                )
                (if is-initial-move
                    (unless (or (check-if-square-attacked row (- column 1) player) (check-for-future-attack player row column row (- column 1)))
                        (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row row))) legal-moves)
                    )

                    (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row row))) legal-moves)
                )
            )
        )

        ;; right
        (unless (equal column 8)
            (if (or 
                    (is-square-empty row (+ column 1))
                    (equal (whose-piece row (+ column 1)) (get-opposite-color player))
                )
                (if is-initial-move
                    (unless (or (check-if-square-attacked row (+ column 1) player) (check-for-future-attack player row column row (+ column 1)))
                        (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row row))) legal-moves)
                    )

                    (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row row))) legal-moves)
                )                
            )
        )

        ;; up-left
        (unless (or (equal row 1) (equal column 1))
            (if (or 
                    (is-square-empty (- row 1) (- column 1))
                    (equal (whose-piece (- row 1) (- column 1)) (get-opposite-color player))
                )
                (if is-initial-move
                    (unless (or (check-if-square-attacked (- row 1) (- column 1) player) (check-for-future-attack player row column (- row 1) (- column 1)))
                        (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                    )

                    (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                )                
            )
        )

        ;; up-right
        (unless (or (equal row 1) (equal column 8))
            (if (or 
                    (is-square-empty (- row 1) (+ column 1))
                    (equal (whose-piece (- row 1) (+ column 1)) (get-opposite-color player))
                )
                (if is-initial-move
                    (unless (or (check-if-square-attacked (- row 1) (+ column 1) player) (check-for-future-attack player row column (- row 1) (+ column 1)))
                        (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                    )

                    (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (- row 1)))) legal-moves)
                )                
            )
        )

        ;; down-left
        (unless (or (equal row 8) (equal column 1))
            (if (or 
                    (is-square-empty (+ row 1) (- column 1))
                    (equal (whose-piece (+ row 1) (- column 1)) (get-opposite-color player))
                )
                (if is-initial-move
                    (unless (or (check-if-square-attacked (+ row 1) (- column 1) player) (check-for-future-attack player row column (+ row 1) (- column 1)))
                        (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                    )

                    (push (concat (index-to-col (- column 1)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                )                
            )
        )

        ;; down-right
        (unless (or (equal row 8) (equal column 8))
            (if (or 
                    (is-square-empty (+ row 1) (+ column 1))
                    (equal (whose-piece (+ row 1) (+ column 1)) (get-opposite-color player))
                )
                (if is-initial-move
                    (unless (or (check-if-square-attacked (+ row 1) (+ column 1) player) (check-for-future-attack player row column (+ row 1) (+ column 1)))
                        (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                    )

                    (push (concat (index-to-col (+ column 1)) (write-to-string (list-index-to-row (+ row 1)))) legal-moves)
                )                
            )
        )

        legal-moves
    )
)           

;; generic function to "redirect" to the correct function
(defun check-legal-moves (row column piece player is-initial-move)
    (cond 
        ((or (equal piece "p") (equal piece "P")) ; pawn
            (check-legal-moves-pawn row column player)
        )

        ((or (equal piece "r") (equal piece "R")) ; rook
            (check-legal-moves-rook row column player)
        )

        ((or (equal piece "n") (equal piece "N")) ; knight
            (check-legal-moves-knight row column player)
        )

        ((or (equal piece "b") (equal piece "B")) ; bishop
            (check-legal-moves-bishop row column player)
        )

        ((or (equal piece "q") (equal piece "Q")) ; queen
            (check-legal-moves-queen row column player)
        )

        ((or (equal piece "k") (equal piece "K")) ; king
            (check-legal-moves-king row column player is-initial-move)
        )
    )
)

;; check if iten is in list
(defun is-in-list (item list)
    (cond 
        ((equal list nil) nil)
        ((equal item (car list)) t)
        (t (is-in-list item (cdr list)))
    )
)

;; convert square reference to row and column
;; used to parse user input
(defun sq-to-coords (sq)
    (let (
        (col (col-to-index (char sq 0)))
        (row (+ 1 (row-to-list-index (char-to-num (char sq 1))))))
        (values (cons row col))
    )
)

;; game loop
(loop
    (display-board)
    (when (check-for-mate current-player)
        (princ "Stalemate")
        (return)
    )
    (if (equal current-player "W")
        (write-line "White to move (uppercase)")
        (write-line "Black to move (lowercase)")
    )
    (write-line "Select a piece")
    (let* (
        (sq-from (read-line))
        (row-from (car (sq-to-coords sq-from)))
        (column-from (cdr (sq-to-coords sq-from))))
        (if (or (< row-from 1) (> row-from 8) (< column-from 1) (> column-from 8))
            (write-line "Invalid square")
            
            (progn
                (if (or ;; no legal moves if:
                        (null (check-legal-moves row-from column-from (get-piece row-from column-from) current-player T)) ;; can't move that piece for any reason
                        (equal (get-piece row-from column-from) ".") ;; square is empty
                        (equal (whose-piece row-from column-from) (get-opposite-color current-player)) ;; piece is opposite color
                    )
                    (write-line "No legal moves")
                    (progn
                        (write-line "Possible moves:")
                        (princ (check-legal-moves row-from column-from (get-piece row-from column-from) current-player T))
                        (write-line "")
                        (write-line "To where?")
                        (let* (
                            (sq-to (read-line))
                            (row-to (car (sq-to-coords sq-to)))
                            (column-to (cdr (sq-to-coords sq-to))))
                            (if (or (< row-from 1) (> row-from 8) (< column-from 1) (> column-from 8))
                                (write-line "Invalid square")

                                (progn
                                    (if (is-in-list sq-to (check-legal-moves row-from column-from (get-piece row-from column-from) current-player T))
                                        (progn
                                            ;; en passant move
                                            (when (or (equal (get-piece row-from column-from) "p") (equal (get-piece row-from column-from) "P"))
                                                (when (equal (get-piece row-to column-to) ".")
                                                    (when (equal (get-piece row-from column-to) (if (equal current-player "W") "p" "P"))
                                                        (setf (nth (- en-passant-col 1) (nth (- row-from 1) board)) ".")
                                                    )
                                                )
                                            )

                                            ;; clear en passant flag
                                            (setf en-passant-col -1)

                                            ;; move that enables en passant
                                            (when (or (equal (get-piece row-from column-from) "p") (equal (get-piece row-from column-from) "P"))
                                                (when (or (equal row-from 2) (equal row-from 7))
                                                    (when (or (equal row-to 4) (equal row-to 5))
                                                        (setf en-passant-col column-to)
                                                    )
                                                )
                                            )

                                            ;; move
                                            (setf (nth (- column-to 1) (nth (- row-to 1) board)) (get-piece row-from column-from))
                                            (setf (nth (- column-from 1) (nth (- row-from 1) board)) ".")

                                            ;; pawn promotion
                                            (when (or (equal (get-piece row-to column-to) "p") (equal (get-piece row-to column-to) "P"))
                                                (when (or (equal row-to 1) (equal row-to 8))
                                                    (write-line "Promote to (R N B Q):")
                                                    (let* (
                                                        (promote-to (read-line))
                                                        )
                                                        (setf (nth (- column-to 1) (nth (- row-to 1) board)) promote-to)
                                                    )
                                                )
                                            )
                                                

                                            (if (equal current-player "W")
                                                (setf current-player "B")
                                                (setf current-player "W")
                                            )

                                            (when (check-for-check current-player)
                                                (princ (if (equal current-player "W") "White" "Black"))
                                                (write-line " is in check")
                                                (when (check-for-mate current-player)
                                                    (princ (if (equal current-player "W") "White" "Black"))
                                                    (write-line " is in checkmate")
                                                    (display-board)
                                                    (return)
                                                )
                                            )
                                        )
                                        (progn
                                            (write-line "Illegal move")
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)