(defun negative (x) ()
  cond (x nil) (t t))


;4
(defun cont
  (arg lst)
  (cond
    ((atom lst) (eq arg lst))
    (t (or
        (equal arg lst)
        (cont arg (car lst))
        (cont arg (cdr lst))))))

(cont '(1 3) '((1 2) 4 6 (5 (1 3))))

;5
(defun rep
  (lst obj1 obj2)
  (cond
    ((equal lst obj1) obj2)
    ((atom lst) lst)
    (t(cons(rep(car lst) obj1 obj2) (rep(cdr lst)obj1 obj2)))))

(rep '((1 4) 9 8 4 1 (4 (1))) '(1 5) 3)

;6
(defun singleLvlLst
  (arg)
  (cond
    ((not arg) t)
    (t(cond
        ((atom(car arg)) (singlelvllst(cdr arg)))))))

(singleLvlLst '(1 2 (7) 3))

;7
(defun deepSum
  (arg)
  (cond
    ((atom arg)(cond
                 ((integerp arg) arg)
                 (t 0)))
    (t(+(deepSum(car arg)) (deepSum(cdr arg))))))

(deepSum '(1 ((2 3) 4 а ы) 5 а н 6))

;8
(defun firstAtom
  (arg)
  (cond
    ((atom (car arg)) (car arg))
    (t (or (cond
             ((not (atom (car arg))) (firstAtom (car arg)))
             (t nil))
           (firstAtom (cdr arg))))))
(firstAtom '(((a b))c d))

;1
(defun advcont
  (n arg lst)
  (cond (lst (or (cond
                      ((not (atom (car lst))) (or
                                                  (and (zerop n) (equal arg (car lst)))
                                                  (advcont (1- n) arg (car lst))))
                      (t nil))
                 (advcont n arg (cdr lst))))
        (t nil)))

(advcont 1 '(2 3) '((1 2) 2 4 5 1 ((2 3) ((1 2 3)))))

;2
(defun level-n
  (lst n)
  (cond ((or(atom lst) (null lst)) 0)
        ((zerop n) (cond ((listp (car lst)) (1+ (level-n (cdr lst) 0)))
                         (t (level-n (cdr lst) 0))))
        (t (+ (level-n (car lst) (1- n)) (level-n (cdr lst) n)))))

(defun count1
  (lst &optional (n 0))
  (cond ((zerop (level-n lst n)) nil)
        (t (cons (list n (level-n lst n)) (count1 lst (1+ n))))))

(count1 '((1 2) 4 ((4 ((5))) 5)))

;3
(defun contains?
  (lst x)
  (cond (lst (or (eq (car lst) x) (contains? (cdr lst) x)))
        (t nil)))

(contains? '(3 4 1 5 8 6) 2)

(defun deleteElement
  (lst x)
  (cond
    ((not lst) nil)
    ((= (car lst) x) (deleteElement (cdr lst) x))
    (t (cons (car lst) (deleteElement (cdr lst) x)))))

(deleteelement '(3 4 1 5 2 8 6) 2)

(defun deleteSet
  (lst1 lst2)
  (cond ((not lst2) lst1)
        (t (deleteSet (deleteElement lst1 (car lst2)) (cdr lst2)))))


(deleteset '(3 5 6 1 8 9 12 2) '(1 2))

(defun haveSubset?
  (lst subset)
  (cond
      (subset (and
                (haveSubset? lst (cdr subset))
                (contains? lst (car subset))))
      (t t)))
(haveSubset? '(3 5 6 1 8 9 12 2) '(1 2 34))
;1
(defun tmp
  (word target key)
  (cond ((equal (word target))(cons word key))
        (t(word))))

(defun joinWord (list o1 o2)
  (flat(mapcar (lambda (x) (cond ((equal x o1) (cons x (cons o2 nil))) (t x))) list)))
(joinword '(quick brown koshka quick brown koshka) 'koshka 'jump)


(defun flat (x)
  (cond
   ((null x) nil)
   ((atom x) (list x))
   (t(append (flat (car x)) (flat (cdr x))))))



(defun joinWord
  (str target key)
  (mapcar (function(lambda(word) (tmp word target key)strlst))))

(joinword '(quick brown koshka quick brown koshka) koshka jump)

;2
(defun shiftChar
  (char shftAmt)
  (cond
        ((= (char-code char) 32) (code-char 32))
        (t (code-char (+ (mod (+ (- (char-code char) 65) shftAmt) 26) 65)))))

(defun encrypt(str shftAmt)
  (map 'string #'(lambda (char) (shiftChar char shftAmt)) (string-downcase str)))

(setq str "The quick brown fox jumped over the lazy dog")
(setq shftAmt 1)
(setq str (encrypt str shftAmt))

;7
(setq tr '(5
            (3
                         (1 nil nil)
                         (4 nil nil))
            (7
                         (6 nil nil) nil)))
(setq tr '(5(3(2 nil nil)(4 nil nil))(7(6 nil nil)(8 nil nil))))
(setq tr1 '(51
            (3
             (1 nil nil)
             (34 nil nil))
            (7
             (6 nil nil) nil)))

(defun data
  (tree)
  (cond
    ((atom tree) nil)
    ((null tree) nil)
    (t (car tree))))

(data tr)

(defun left
  (tree)
  (cond
    ((atom tree) nil)
    ((null tree) nil)
    (t (cadr tree))))

(left tr)

(defun right
  (tree)
  (cond
    ((atom tree) nil)
    ((null tree) nil)
    (t (caddr tree))))

(right tr)

(defun findtr
  (x tree)
  (cond
    ((null tree) nil)
    ((null (data tree))nil)
    ((null x) nil)
    ((= x (data tree)) t)
    ((< x (data tree)) (findtr x (left tree)))
    ((> x (data tree)) (findtr x (right tree)))))

(findtr 7 tr)

(defun add
  (x tree)
  (cond
    ((null x) '(nil nil nil))
    ((null tree) (list x nil nil))
    ((null (data tree)) (list x nil nil))
    ((= x (data tree)) tree)
    ((< x (data tree)) (list (data tree) (add x (left tree)) (right tree)))
    ((> x (data tree)) (list (data tree) (left tree) (add x (right tree))))))

(add 7 tr)

(defun prevTree
  (x tree)
  (cond
    ((null tree) nil)
    ((<= x (data tree)) (prevTree x (left tree)))
    (t (list (data tree) (left tree) (prevTree x (right tree))))))

(prevtree 7 tr)

(defun nextTree
  (x tree)
  (cond
    ((null tree) nil)
    ((>= x (data tree)) (nextTree x (right tree)))
    (t (list (data tree) (nextTree x (left tree)) (right tree)))))

(nexttree 4 tr)

(defun join
  (p q)
  (cond
    ((null p) q)
    ((null q) p)
    (t (list(data p)
          (join (left p)
              (prevTree (data p) q))
          (join (right p)
              (nextTree (data p) q))))))
(join tr tr1)



(defun takeleft
  (tree)
  (cond
    ((null (left tree)) (data tree))
    (t (takeleft (left tree)))))

(takeright tr)

(defun del
 (x tree)
 (cond
   ((null tree) nil)
   ((= x (data tree))
    (cond
       ((null (right tree)) (left tree))
       ((null (left tree)) (right tree))
       (t (list (takeleft (right tree))
               (left tree)
               (del (takeleft (right tree)) (right tree))))))

   ((< x (data tree)) (list (data tree) (del x (left tree)) (right tree)))
   ((> x (data tree)) (list (data tree) (left tree) (del x (right tree))))))

(del 5 tr)

(defun joinWord (list o1 o2)
  (flat(mapcar (lambda (x) (cond ((equal x o1) (cons x (cons o2 nil))) (t x))) list)))
(joinword '(quick brown koshka quick brown koshka) 'koshka 'jump)


(defun flat (x)
  (cond
   ((null x) nil)
   ((atom x) (list x))
   (t(append (flat (car x)) (flat (cdr x))))))
