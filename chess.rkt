#lang racket
; BPS: Adding the #:transparent flag allows printf to display the fields of the struct.
(struct piece (player type) #:transparent)
(require anaphoric)
;(require (prefix-in dc: data/collection))

;**In moves hash, the two elements are forward/backward and left/right**;

(define (delta pos1 pos2)
  (let ([r1 (car pos1)]
        [c1 (cdr pos1)]
        [r2 (car pos2)]
        [c2 (cdr pos2)])
    (cons (- r2 r1) (- c2 c1))))

(define all-moves
  ; BPS: Idea: What if there were 2 distinct types of pawns, and the original
  ; pawn piece were replaced with the second type after initial move? This
  ; approach would obviate need for "moved" flag and the special logic
  ; stripping off the longer moves for all subsequent moves.
  ; BPS: Not recommending a change here, as what you've got works fine; just
  ; pointing out an alternate approach... What if moves were expressed as a
  ; vector and max-steps. IOW, mostly what you have, except the #t's would be
  ; 1's and -1's, with the corresponding max-step being 8. Might make some
  ; things simpler (since vector math is easer with 1's than with #t's), but
  ; whether it would be simpler overall, I'm not sure... Again, just throwing
  ; out an idea, not saying there's anything wrong with the way you did it.
  (hash "moved pawn" (hash #(1 0) 1 #(1 -1) 1 #(1 1) 1)
        "unmoved pawn" (hash #(1 0) 2 #(1 -1) 1 #(1 1) 1)
        "knight" (vector #(2 -1) #(2 1) #(-2 -1) #(-2 1) #(1 2) #(1 -2) #(-1 2) #(-1 -2))
        "bishop" (hash #(1 1) 7 #(1 -1) 7 #(-1 1) 7 #(-1 -1) 7)
        "rook" (hash #(1 0) 7 #(0 1) 7 #(-1 0) 7 #(0 -1) 7)
        "queen" (hash #(1 0) 7 #(0 1) 7 #(-1 0) 7 #(0 -1) 7 #(1 1) 7 #(1 -1) 7 #(-1 1) 7 #(-1 -1) 7)
        "king" (hash #(1 -1) 1 #(1 0) 1 #(1 1) 1 #(0 1) 1 #(-1 1) 1 #(-1 0) 1 #(-1 -1) 1 #(0 -1) 1)))

(define displayables
  (hash (cons "moved pawn" 0) " \u2659 "
        (cons "unmoved pawn" 0) " \u2659 "
        (cons "knight" 0) " \u2658 "
        (cons "bishop" 0) " \u2657 "
        (cons "rook" 0) " \u2656 "
        (cons "queen" 0) " \u2655 "
        (cons "king" 0) " \u2654 "
        (cons "moved pawn" 1) " \u265F "
        (cons "unmoved pawn" 1) " \u265F "
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
                    (make-list 8 "unmoved pawn"))])                    
    (values (cons i j) (piece p t))))

(define (member_moves? rdiff cdiff moves type)
  (if (equal? type "knight")
      (vector-member (vector rdiff cdiff) moves)
      (let ([absmaxrcd (max (abs rdiff) (abs cdiff))])
        (and-let [hr (hash-ref moves (vector (/ rdiff absmaxrcd) (/ cdiff absmaxrcd)) #f)]
                 (<= absmaxrcd hr)))))

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
    ; BPS: cond provides no advantage over if here.
    (if (equal? p-new-el finish)
        empty
        (cons p-new-el (gen_path p-new-el finish inc)))))

(define (has_path? player pieces posp posd difference type)
  (printf "\ndifference = ~a\n" difference)
  (let* ([rd (car difference)]
         [cd (cdr difference)])
    (or (and (< (abs rd) 2) (< (abs cd) 2))
	; BPS: The "sgn" operation was done elsewhere (in check?); should probably have it one place.
        (let* ([gp (gen_path posp posd (cons (sgn rd) (sgn cd)))]
               [test (println gp)])
          (if (andmap (lambda (el)
                    (let ([r (car el)]
                          [c (cdr el)])
                      (not (hash-ref pieces (cons r c) #f))))
                  gp)
              gp
              #f)))))

(define (check_move player pieces posp posd)
  (let* ([piece (hash-ref pieces posp #f)]
         [type (if piece (piece-type piece) #f)]
         [moves (if piece (hash-ref all-moves (piece-type piece) #f) #f)]
	 ; BPS: Consider implementing function that takes 2 positions and
	 ; returns (eg) the vector between, or vector and # of steps, or
	 ; something like that. Just seems like there are a number of places
	 ; where you're extracting row/col and calculating deltas. Could
	 ; probably reduce some of the boilerplate with a well-designed helper
	 ; function or two...
         [difference (delta posp posd)]
         [rd (car difference)]
         [cd (cdr difference)])
    (and (valid_piece? player pieces posp)
         (valid_dest? player pieces posd)
         (member_moves? (if (= player 0) rd (- 0 rd))
                        (if (= player 0) (- 0 cd) cd)
                        moves
                        type)
         (or (equal? type "knight")
             (has_path? player pieces posp posd difference type)))))

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
  (let ([raw_input (read-line)])
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

(define (check? pieces king opp-player)
  ; BPS: (modulo (add1 player) 2) is a more natural way to express this.
  (let ([paths (foldl (lambda (piece acc)
                        ; BPS: anaphoric let not really used/needed here.
                        (let ([hp (check_move player
                                              pieces
                                              (car piece)
                                              king)])
                          (if (boolean? hp)
                              (if hp (cons (cons (car piece) empty) acc) acc)
                              (cons (cons (car piece) hp) acc))))
                      '()
                      (foldl (lambda (el acc)
                               (if (= opp-player (piece-player (cdr el)))
                                   (cons (car el) acc)
                                   acc))
                             '()
                             pieces))])
    (if (empty? paths)
        #f
        paths)))

(define (make_move player pieces posp posd)
  (let* ([p (hash-ref pieces posp #f)]
         [v1 (hash-set pieces posd (piece (piece-player p)
                                          (let ([pt (piece-type p)])
                                            (if (equal? pt "unmoved pawn")
                                                "moved pawn"
                                                pt))))]
         [v2 (hash-remove v1 posp)])
    v2))

(define (game_loop pieces player)
  (display_board pieces)
  (let-values ([(posp posd) (get_valid_move player pieces)])
    (let ([npieces (make_move player pieces posp posd)])
      (game_loop npieces (abs (- player 1))))))

;(printf "~a~n" (init_board))

(game_loop (init_board) 0)