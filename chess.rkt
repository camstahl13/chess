#lang racket
(struct piece (player type moved move))
(require anaphoric)

;**In moves hash, the two elements are forward/backward and left/right**;

(define all-moves
  (hash "pawn" '((1 0) (2 0) (1 -1) (1 1))
        "knight" '((2 -1) (2 1) (-2 -1) (-2 1) (1 2) (1 -2) (-1 2) (-1 -2))
        "bishop" '((#t #t))
        "rook" '((#t 0) (0 #t))
        "queen" '((#t 0) (0 #t) (#t #t))
        "king" '((1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1) (0 -1))))

(define displayables
  (hash (cons "pawn" 0) " \u2659 "
        (cons "knight" 0) " \u2658 "
        (cons "bishop" 0) " \u2657 "
        (cons "rook" 0) " \u2656 "
        (cons "queen" 0) " \u2655 "
        (cons "king" 0) " \u2654 "
        (cons "pawn" 1) " \u265F "
        (cons "knight" 1) " \u265E "
        (cons "bishop" 1) " \u265D "
        (cons "rook" 1) " \u265C "
        (cons "queen" 1) " \u265B "
        (cons "king" 1) " \u265A "))

(define (init_board)
  (for/hash ([p (range 0 2)]
             #:when #t
             [i (if (= p 0) '(0 1) '(6 7))]
             #:when #t
             [j (range 0 8)]
             [t (if (or (= i 0) (= i 7))
                    (list "rook" "bishop" "knight" "king" "queen" "knight" "bishop" "rook")
                    (make-list 8 "pawn"))])                    
    (values (cons i j) (piece p t #f t))))

(define (member_moves? rdifference cdifference moves)
  (or (member (list rdifference cdifference) moves)
      (and (member (list #t #t) moves)
           (not (zero? rdifference))
           (not (zero? cdifference))
           (= (abs rdifference) (abs cdifference)))
      (and (not (zero? rdifference)) (zero? cdifference) (member (list #t 0) moves))
      (and (zero? rdifference) (not (zero? cdifference)) (member (list 0 #t) moves))))

(define (on_board? rdestination cdestination)
  (and (>= rdestination 0)
       (<= rdestination 7)
       (>= cdestination 0)
       (<= cdestination 7)))

(define (valid_piece? player pieces posp)
  (and-let [hr (hash-ref pieces posp #f)] (= player (piece-player hr))))

(define (valid_dest? player pieces posd)
  (let ([hr (hash-ref pieces posd #f)])
    (or (and hr (= (abs (- player 1)) (piece-player hr)))
        (and (not (valid_piece? player pieces posd))
             (>= (car posd) 0)
             (<= (car posd) 7)
             (>= (cdr posd) 0)
             (<= (cdr posd) 7)))))

(define (gen_path start finish inc)
  (let ([p-new-el (cons (+ (car start) (car inc)) (+ (cdr start) (cdr inc)))])
    (cond
      [(equal? p-new-el finish) '()]
      [else (cons p-new-el (gen_path p-new-el finish inc))])))

(define (has_path? player pieces posp posd difference type)
  (printf "\nGetting path...\n")
  (let* ([r1 (car posp)]
         [c1 (cdr posp)]
         [r2 (car posd)]
         [c2 (cdr posd)]
         [rd (car difference)]
         [cd (cdr difference)])
    (or (and (< (abs rd) 2) (< (abs cd) 2))
        (let ([gp (gen_path posp posd (cons (if (zero? rd) 0 (/ rd (abs rd))) (if (zero? cd) 0 (/ cd (abs cd)))))])
          (andmap (lambda (el)
                    (let ([r (car el)]
                          [c (cdr el)])
                      (not (and-let [hr (hash-ref pieces (cons r c) #f)]
                                    (= (piece-player hr) player)))))
                  gp)))))

(define (check_move player pieces posp posd)
  (let* ([piece (hash-ref pieces posp #f)]
         [type (if piece (piece-type piece) #f)]
         [moves (if piece (hash-ref all-moves (piece-move piece) #f) #f)]
         [r1 (car posp)]
         [c1 (cdr posp)]
         [r2 (car posd)]
         [c2 (cdr posd)]
         [rd (- r2 r1)]
         [cd (- c2 c1)])
    (and (valid_piece? player pieces posp)
         (valid_dest? player pieces posd)
         (member_moves? (if (= player 0) (- r2 r1) (- r1 r2))
                        (if (= player 0) (- c1 c2) (- c2 c1))
                        (if (equal? type "pawn")
                            (if (hash-has-key? pieces posd)
                                (rest (rest moves))
                                (if (piece-moved piece)
                                    (list (first moves))
                                    (list (first moves) (second moves))))
                            moves))
         (or (equal? type "knight")
             (has_path? player pieces posp posd (cons rd cd) type)))))

; HELPER FUNCTIONS BELOW
(define (alternate-string str sep cnt)
  (cond
    [(= cnt 0) ""]
    [(= cnt 1) str]
    [else (string-append str sep (alternate-string str sep (- cnt 1)))]))

(define (create-end length top-or-bottom)
  (let ([lstring (if top-or-bottom "\u250F" "\u2517")]
        [rstring (if top-or-bottom "\u2513" "\u251B")]
        [sstring (if top-or-bottom "\u2533" "\u253B")])
    (string-append "  " lstring (alternate-string (make-string 3 #\u2501) sstring length) rstring)))

(define (create-separator length)
  (string-append "  " "\u2523" (alternate-string (make-string 3 #\u2501) "\u254B" length) "\u252B"))

(define (create-line lis)
  (let ([ll (length lis)])
    (cond
      [(empty? lis) "\u2503"]
      [else (string-append "\u2503" (first lis) (create-line (rest lis)))])))
; HELPER FUNCTIONS ABOVE

(define (display_board pieces)
  (for ([i (in-range 8)])
    (let ([l (for/list ([j (in-range 8)])
               (if-let [hr (hash-ref pieces (cons i j) #f)]
                       (hash-ref displayables (cons (piece-type hr) (piece-player hr)) #f)
                       "   "))])
      (printf (string-append (if (= i 0)
                                 (string-append "    0   1   2   3   4   5   6   7"
                                                "\n"
                                                (create-end 8 #t))
                                 "")
                             (string-append "\n"
                                            (string (integer->char (+ i 65)))
                                            " "
                                            (create-line l)
                                            "\n")
                             (if (= i 7)
                                 (string-append (create-end 8 #f)
                                                "\n")
                                 (create-separator 8)))))))

(define (get_valid_move player pieces)
  (let ([prompt_user1 (printf "Input format: [location of piece]->[location of destination]: i.e., A7->C7\n")]      
        [prompt_user2 (printf (string-append "Input your move, Player " (if (= player 0) "One." "Two.")))]
        [raw_input (read-line)])
    (if (and (string? raw_input) (= (string-length raw_input) 6) (equal? (substring raw_input 2 4) "->"))
        (let ([rp (string-ref raw_input 0)]
              [cp (string->number (substring raw_input 1 2))]
              [rd (string-ref raw_input 4)]
              [cd (string->number (substring raw_input 5 6))])
          (if (and cp cd (char>=? rp #\A) (char<=? rp #\H) (char>=? rd #\A) (char<=? rd #\H))
              (let ([posp (cons (- (char->integer rp) 65) cp)]
                    [posd (cons (- (char->integer rd) 65) cd)])
                (if (check_move player pieces posp posd)
                    (values posp posd)
                    (get_valid_move player pieces)))
              (get_valid_move player pieces)))
        (get_valid_move player pieces))))

(define (make_move player pieces posp posd)
  (let* ([p (hash-ref pieces posp #f)]
         [v1 (hash-set pieces posd (piece (piece-player p)
                                          (piece-type p)
                                          #t
                                          (piece-move p)))]
         [v2 (hash-remove v1 posp)])
    v2))

(define (find_moves piece pieces

(define (find_escapes king paths pieces)

(define (check? pieces king player)
  (let* ([opp-player (abs (- player 1))])
        (let ([paths (foldl (lambda (piece acc)
                              (if-let (hp (check_move player
                                                      pieces
                                                      (car piece)
                                                      king))
                                      (append (let* ([r1 (caar piece)]
                                                     [c1 (cdar piece)]
                                                     [r2 (car king)]
                                                     [c2 (cdr king)]
                                                     [rd (- r2 r1)]
                                                     [cd (- c2 c1)])
                                                (if (or (equal? (piece-type (cdr piece)) "knight")
                                                        (and (< (abs rd) 2) (< (abs cd) 2)))
                                                    (cons king '())
                                                    (cons king (gen_path (car piece)
                                                                         king
                                                                         (cons (if (zero? rd) 0 (/ rd (abs rd)))
                                                                               (if (zero? cd) 0 (/ cd (abs cd))))))) acc))
                                      acc))
                            '()
                            (filter (lambda (el) (= opp-player (piece-player (cdr el)))) (hash->list pieces)))])
          (if (empty? paths)
              #f
              paths))))

(define (game_loop pieces player)
  (display_board pieces)
  (let* ([king (for/or ([k (hash-keys pieces)]
                        [v (hash-values pieces)])
                 (and (= (piece-player v) player) (equal? (piece-type v) "king") k))]
         [paths (check? pieces king player)]
         [escapes (if paths (find_escapes king paths pieces) #t)])
    (print res)
    (if (and (boolean? res) res)
        (printf (string-append "Game over! Player " (if (= player 0) "One" "Two") " wins!"))
        (let-values ([(posp posd) (get_valid_move player pieces res)])
          (let ([npieces (make_move player pieces posp posd)])
                 (game_loop npieces (abs (- player 1))))))))
  

(game_loop (init_board) 0)