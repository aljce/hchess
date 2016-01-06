(set-option :auto-config false)
(set-option :model true)
(set-option :model.partial false)
(set-option :smt.mbqi false)
(define-sort Elt () Int)
(define-sort Set () (Array Elt Bool))
(define-fun smt_set_emp () Set ((as const Set) false))
(define-fun smt_set_mem ((x Elt) (s Set)) Bool (select s x))
(define-fun smt_set_add ((s Set) (x Elt)) Set (store s x true))
(define-fun smt_set_cup ((s1 Set) (s2 Set)) Set ((_ map or) s1 s2))
(define-fun smt_set_cap ((s1 Set) (s2 Set)) Set ((_ map and) s1 s2))
(define-fun smt_set_com ((s Set)) Set ((_ map not) s))
(define-fun smt_set_dif ((s1 Set) (s2 Set)) Set (smt_set_cap s1 (smt_set_com s2)))
(define-fun smt_set_sub ((s1 Set) (s2 Set)) Bool (= smt_set_emp (smt_set_dif s1 s2)))
(define-sort Map () (Array Elt Elt))
(define-fun smt_map_sel ((m Map) (k Elt)) Elt (select m k))
(define-fun smt_map_sto ((m Map) (k Elt) (v Elt)) Map (store m k v))
(declare-fun Z3_OP_MUL (Real Real) Real)
(declare-fun Z3_OP_DIV (Real Real) Real)
(declare-fun runFun (Int Int) Int)
(declare-fun GHC.Num.$42$$35$ru (Int Int) Int)
(declare-fun GHC.Tuple.$40$$44$$44$$44$$44$$44$$44$$41$$35$7e (Int Int Int Int Int Int Int) Int)
(declare-fun GHC.Base.Just$35$r1e (Int) Int)
(declare-fun addrLen (Int) Int)
(declare-fun papp5 (Int Int Int Int Int Int) Bool)
(declare-fun xsListSelector (Int) Int)
(declare-fun x_Tuple21 (Int) Int)
(declare-fun GHC.Prim.$61$$61$$35$$35$9o (Int Int) Int)
(declare-fun x_Tuple65 (Int) Int)
(declare-fun x_Tuple55 (Int) Int)
(declare-fun x_Tuple33 (Int) Int)
(declare-fun x_Tuple77 (Int) Int)
(declare-fun papp3 (Int Int Int Int) Bool)
(declare-fun x_Tuple63 (Int) Int)
(declare-fun x_Tuple41 (Int) Int)
(declare-fun Test.test$35$rjG (Int) Int)
(declare-fun papp4 (Int Int Int Int Int) Bool)
(declare-fun x_Tuple64 (Int) Int)
(declare-fun GHC.Tuple.$40$$44$$44$$44$$41$$35$78 (Int Int Int Int) Int)
(declare-fun GHC.Num.$36$fNumInt$35$rlH () Int)
(declare-fun autolen (Int) Int)
(declare-fun GHC.Base.Nothing$35$r1d () Int)
(declare-fun x_Tuple52 (Int) Int)
(declare-fun GHC.Types.EQ$35$6U () Int)
(declare-fun null (Int) Bool)
(declare-fun VV$35$167 () Int)
(declare-fun papp2 (Int Int Int) Bool)
(declare-fun x_Tuple62 (Int) Int)
(declare-fun fromJust (Int) Int)
(declare-fun papp7 (Int Int Int Int Int Int Int Int) Bool)
(declare-fun GHC.Types.$91$$93$$35$6m () Int)
(declare-fun x_Tuple53 (Int) Int)
(declare-fun lq_anf$36$$35$dvW () Int)
(declare-fun GHC.Tuple.$40$$44$$41$$35$74 (Int Int) Int)
(declare-fun x_Tuple71 (Int) Int)
(declare-fun GHC.Integer.Type.smallInteger$35$0Z (Int) Int)
(declare-fun GHC.Num.$45$$35$02B (Int Int) Int)
(declare-fun GHC.Types.$58$$35$64 (Int Int) Int)
(declare-fun GHC.Num.$43$$35$rt (Int Int) Int)
(declare-fun VV$35$F1 () Int)
(declare-fun x_Tuple74 (Int) Int)
(declare-fun GHC.Prim.$45$$35$$35$99 (Int Int) Int)
(declare-fun len (Int) Int)
(declare-fun papp6 (Int Int Int Int Int Int Int) Bool)
(declare-fun x_Tuple22 (Int) Int)
(declare-fun x_Tuple66 (Int) Int)
(declare-fun x_Tuple44 (Int) Int)
(declare-fun x$35$avK () Int)
(declare-fun xListSelector (Int) Int)
(declare-fun GHC.Tuple.$40$$44$$44$$41$$35$76 (Int Int Int) Int)
(declare-fun strLen (Int) Int)
(declare-fun x_Tuple72 (Int) Int)
(declare-fun isJust (Int) Bool)
(declare-fun Prop (Int) Bool)
(declare-fun x_Tuple31 (Int) Int)
(declare-fun x_Tuple75 (Int) Int)
(declare-fun VV$35$170 () Int)
(declare-fun papp1 (Int Int) Bool)
(declare-fun x_Tuple61 (Int) Int)
(declare-fun x_Tuple43 (Int) Int)
(declare-fun GHC.Tuple.$40$$44$$44$$44$$44$$41$$35$7a (Int Int Int Int Int) Int)
(declare-fun GHC.Prim.$62$$35$$35$9m (Int Int) Int)
(declare-fun VV$35$164 () Int)
(declare-fun GHC.Prim.$60$$61$$35$$35$9r (Int Int) Int)
(declare-fun GHC.Prim.$43$$35$$35$98 (Int Int) Int)
(declare-fun x_Tuple51 (Int) Int)
(declare-fun GHC.Types.I$35$$35$6c (Int) Int)
(declare-fun x_Tuple73 (Int) Int)
(declare-fun GHC.Tuple.$40$$44$$44$$44$$44$$44$$41$$35$7c (Int Int Int Int Int Int) Int)
(declare-fun VV$35$173 () Int)
(declare-fun x_Tuple54 (Int) Int)
(declare-fun GHC.Prim.$60$$35$$35$9q (Int Int) Int)
(declare-fun GHC.Types.GT$35$6W () Int)
(declare-fun GHC.Prim.$62$$61$$35$$35$9n (Int Int) Int)
(declare-fun x_Tuple32 (Int) Int)
(declare-fun cmp (Int) Int)
(declare-fun x_Tuple76 (Int) Int)
(declare-fun GHC.Types.LT$35$6S () Int)
(declare-fun fst (Int) Int)
(declare-fun snd (Int) Int)
(declare-fun x_Tuple42 (Int) Int)
(assert (distinct GHC.Types.LT$35$6S GHC.Types.GT$35$6W GHC.Types.EQ$35$6U))
(assert (distinct GHC.Num.$36$fNumInt$35$rlH))
(push 1)
(assert (and (= VV$35$F1 (- x$35$avK lq_anf$36$$35$dvW)) (= (cmp GHC.Types.GT$35$6W) GHC.Types.GT$35$6W) (= (cmp GHC.Types.LT$35$6S) GHC.Types.LT$35$6S) (= (cmp GHC.Types.EQ$35$6U) GHC.Types.EQ$35$6U) (>= x$35$avK 0) (= lq_anf$36$$35$dvW 1) (= VV$35$164 (- x$35$avK lq_anf$36$$35$dvW))))
(push 1)
(assert (not (>= VV$35$F1 (- 1))))
(check-sat)
; SMT Says: Unsat
(pop 1)
(pop 1)