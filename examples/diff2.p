% differentiation
% source: extension of Example 3.4 in Jouannaud & Rubio ACM Trans. Com. Log. 2015

thf(real_type,type,
    real: $tType ).

thf(zero,type,
    zero: real ).

thf(one,type,
    one: real ).

thf(sin,type,
    sin: real > real ).

thf(cos,type,
    cos: real > real ).

thf(ln,type,
    ln: real > real ).

thf(diff,type,
    diff: (real > real) > real > real ).

thf(plus,type,
    plus: (real > real) > (real > real) > real > real ).

thf(minus,type,
    minus: (real > real) > real > real ).

thf(times,type,
    times: (real > real) > (real > real) > real > real ).

thf(div,type,
    div: (real > real) > (real > real) > real > real ).

thf(diff_const,axiom,
    ! [Y: real] :
      ( diff @ (^ [X: real] : Y)
      = ^ [X: real] : zero) ).

thf(diff_id,axiom,
    ! [X: real] :
    ( diff @ (^ [X: real] : X)
    = ^ [X: real]: one ) ).

thf(diff_sin,axiom,
    ! [F: real > real] :
      ( diff @ (^ [X : real] : sin @ (F @ X))
      = times @ (^ [X : real] : cos @ (F @ X)) @ (diff @ (^ [Y : real] : F @ Y)) ) ) .

thf(diff_cos,axiom,
    ! [F: real > real] :
      ( diff @ (^ [X : real] : cos @ (F @ X))
      = times @ (minus @ (^ [X : real] : sin @ (F @ X))) @ (diff @ (^ [Y : real] : F @ Y)) ) ) .

thf(diff_plus,axiom,
    ! [F : real > real, G : real > real] :
      ( diff @ (plus @ (^ [X: real] : F @ X) @ (^ [Y: real] : G @ Y))
      = plus @ (diff @ (^ [X: real] : F @ X)) @ (diff @ (^ [Y: real] : G @ Y)) ) ) .

thf(diff_times,axiom,
    ! [F: real > real, G: real > real] :
      ( diff @ (times @ (^ [X : real] : F @ X) @ (^ [Y : real] : G @ Y))
      = plus @ (times @ (diff @ (^ [X : real] : F @ X)) @ (^ [Y : real] : G @ Y))
             @ (times @ (^ [X : real] : F @ X) @ (diff @ (^ [Y : real] : G @ Y))) ) ) .

thf(diff_ln,axiom,
    ! [F : real > real] :
      ( diff @ (^ [X: real] : ln @ (F @ X))
      = div @ (diff @ (^ [X: real] : F @ X)) @ F ) ) .
