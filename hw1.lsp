;;; Yijing Jiang
;;; 304843076


;;; 1.
;;; the TREE-CONTAINS takes a number N and an ordered tree
;;; and checks if N appears in the TREE
;;; returns a boolean 

;;; all numbers in the TREE are in order 
;;; L m R, L and R are ordered trees, m is a number

(defun TREE-CONTAINS(N TREE)
	(cond 
		((NULL TREE) NIL) ;if tree is null, return NIL
		((numberp TREE) (equal N TREE))	;if TREE is a number, and it equals to N, true
		((let ((m (second TREE)))	;else, let m = the second element of TREE 
				(cond
					((equal m N) t)		;if m = N, true
					((< N m) (TREE-CONTAINS N (first TREE)))	;if N < m, N must be in (first TREE) 
					((> N m) (TREE-CONTAINS N (third TREE))))))))	;if N > m, N must be in (third TREE)
 


;;; 2.
;;; TREE-MAX takes an ordered TREE
;;; and returns the maximum number appearing in TREE
;;; returns the max number

;;; REMEMBER: TREE has the same identity as in the first question!

(defun TREE-MAX(TREE)
	(if (atom TREE) TREE ;if TREE is a number or an empty list, return TREE	
		(TREE-MAX(third TREE))))	;else, call (TREE-MAX (third TREE)) recursively, since the larger number appears on the rightmost 



;;; 3.
;;; TREE-ORDER takes an ordered TREE as the only argument
;;; and returns an in-ordered list of the numbers appearing in the TREE

(defun TREE-ORDER (TREE)
	(cond 
		((null TREE) TREE)	;if TREE is an empty list, return itself
		((numberp TREE) (list TREE))	;if TREE is a number, return the number as a list
		((listp TREE)	;else, if TREE is a list of numbers, append the first, second and third element of TREE togethe as one list
			(append
				(TREE-ORDER (first TREE))	;recursively call (TREE-ORDER) since the first, second and third element could contain lists in them 
				(TREE-ORDER (second TREE))
				(TREE-ORDER (third TREE))))))




;;; 4.
;;; SUB-LIST takes a list L, two non-negative integers START and LEN
;;; START indicates the starting point of the sublist
;;; LEN indicates the length of the sublist
;;; returns a list from the indicated strating point with the indicated length
 
;;; the first element of L has position 0
  
(defun SUB-LIST(L START LEN)
	(cond 
		((equal LEN 0) nil)	;if LEN = 0, return NIL
		((> START (length L)) nil)	;if START > length of the original list, return NIL
		((> (+ LEN START) (length L)) nil)	;if the sublist exceeds the original list, return NIL
		((> START 0) (SUB-LIST(cdr L) (- START 1) LEN))	;if START > 0, call (SUB-LIST), and move START to the right one unit
		((equal START 0) (append (list (first L)) (SUB-LIST (cdr L) 0 (- LEN 1))))))	;if START = 0, append the first element (as a list) to the rest of the list, and minus one from LEN 





;;; 5.
;;; SPLIT-LIST takes a list L, and returns a list of two lists L1 and L2
;;; L is the combination of L1 and L2
;;; length of L2 minus length of L1 is 0 or 1

(defun SPLIT-LIST(L)
	(cond
		((equal (length L) 0) nil)	;if the length of L equals 0, return nil
		((equal (length L) 1) (list L))	;if the length of l equals 1, return the number as a list
		((oddp (length L)) (append (list (SUB-LIST L 0 (/ (- (length L) 1) 2))) 
									(list (SUB-LIST L (/ (- (length L) 1) 2) (/ (+ (length L) 1) 2))))) ;if the length of the list is an odd number, divide the list from half of the length - 1
		((evenp (length L)) (append (list (SUB-LIST L 0 (/ (length L) 2)))
									(list (SUB-LIST L (/(length L) 2) (/ (length L) 2))))))) ;if the length of the list is an even number, divide the list from half of the length



;;; 6.
;;; BTREE-HEIGHT takes a binary tree, and returns the height of TREE
;;; the height of TREE is the longest path from the root node to the farthest leaf node

(defun BTREE-HEIGHT (TREE)
	(cond 
		((NULL TREE) 0)	;if TREE is an empty list, return 0
		((numberp TREE) 0)	;if TREE is just a number, return 0 as well
		((listp TREE)	;if TREE is a list
			(let* ((L (+ (BTREE-HEIGHT (first TREE)) 1)) (R (+ (BTREE-HEIGHT (second TREE)) 1)))	;let L be the longest path in left, and R be the longest path in the right 
			(cond
				((equal L R) L)		
				((> L R) L)
				((< L R) R)))))) ;return the longest number from L and R
  


;;; 7. 
;;; LIST2BTREE takes a non-empty list of atoms LEAVES, and returns a binary tree such that
;;; The tree leaves are the elements of LEAVES;
;;; For any internal node in the tree, the number of leaves in its right branch minus the number of leaves in its left branch is 0 or 1.

;;; recursively call SPLIT-LIST function, until the number of leaves in its right branch minus the number of leaves in its left branch is 0 or 1.

(defun LIST2BTREE (LEAVES)
	(cond 
		((equal (length LEAVES) 1) LEAVES)	;if the length of LEAVES = 1, return LEAVES
		((equal (length LEAVES) 2) LEAVES)	;if the length of LEAVES = 2, return LEAVES
		(t 
			(append (LIST (LIST2BTREE (first (SPLIT-LIST LEAVES)))) (LIST (LIST2BTREE (second (SPLIT-LIST LEAVES))))))))	;else, recursively call SPLIT-LIST until satisfy either base case



;;; 8.
;;; BTREE2LIST takes a binary tree TREE as input, and returns a list of atoms 
;;; each node has at most 2 children

(defun BTREE2LIST (TREE)
	(cond 
		((NULL TREE) nil)	;if TREE is an empty list, return NIL
		((numberp TREE) (list TREE))	;if TREE is a number, return the number as a list 
		((and (= (length TREE) 2) (and (atom (first TREE))) (atom (second TREE))) TREE)	;if TREE has length of 2, and both elements are atoms, return TREE
		(t
			(append (BTREE2LIST(first TREE)) (BTREE2LIST(second TREE)))))) ;else, append the first and second elements




;;; 9.
;;; IS-SAME takes two LISP expressions E1 and E2 whose atoms are all numbers
;;; and checks whether the expressions are identical.
;;; returns a boolean

(defun IS-SAME(E1 E2)
	(cond 
		((and (null E1) (null E2)) t)	;if both are null, return true
		((and (null E1) (not (null E2))) nil)	;if any one of expression is null, while another one is not null, return NIL
		((and (not (null E1)) (null E2)) nil)
		((and (and (numberp E1) (numberp E2)) (= E1 E2)) t)	;if both elements are numbers, and they are equal to each other, return true
		((and (numberp (first E1)) (numberp (first E2)) (= (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))) t)	;if the first element are numbers, and equal to each other, compare the rest
		((and (listp (first E1)) (listp (first E2)) (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))) t)))	;if the first element are lists, compare the first and rest 






 