% differentiation
% source: Example 3.4 in Jouannaud & Rubio ACM Trans. Com. Log. 2015

thf(real_type,type,
    real: $tType ).

thf(sin,type,
    sin: real > real ).

thf(cos,type,
    cos: real > real ).

thf(diff,type,
    diff: (real > real) > real > real ).

thf(plus,type,
    plus: (real > real) > (real > real) > real > real ).

thf(times,type,
    times: (real > real) > (real > real) > real > real ).

thf(diff_sin,axiom,
    ! [F: real > real] :
      ( diff @ (^ [X : real] : sin @ (F @ X))
      = times @ (^ [X : real] : cos @ (F @ X)) @ (diff @ (^ [X : real] : F @ X)) ) ) .

thf(diff_times,axiom,
    ! [F: real > real] :
      ( diff @ (times @ (^ [X : real] : F @ X) @ (^ [Y : real] : F @ Y))
      = plus @ (times @ (diff @ (^ [X : real] : F @ X)) @ (^ [Y : real] : F @ Y))
             @ (times @ (^ [X : real] : F @ X) @ (diff @ (^ [Y : real] : F @ Y))) ) ) .



