echo "--- Example 4 (extended) --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/diff2.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/diff2.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/diff2.p
echo "--- Example 5 --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/nnf.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/nnf.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/nnf.p
echo "--- Example 6 --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/map_inc.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/map_inc.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/map_inc.p
echo "--- Example 5.2 in [BJR15] --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/hofmann.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/hofmann.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/hofmann.p
echo "--- Example 7.1 in [BJR15] --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/brouwerord.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/brouwerord.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/brouwerord.p
echo "--- Example 8.19 in [BJR15] --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/applicative_treeflatten.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/applicative_treeflatten.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/applicative_treeflatten.p
echo "--- Example 7.1 in [JR15] --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/pnf.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/pnf.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/pnf.p
echo "--- Example 7.2 in [JR15] --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/sdu.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/sdu.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/sdu.p
echo "--- Example 7.3 in [JR15] --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/pad.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/pad.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/pad.p
echo "--- neutr.p --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/neutr.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/neutr.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/neutr.p
echo "--- neutrN.p --"
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/neutrN.p
echo "NHORPO:"
cabal run hrsterm -- -t nhorpo -s z3 examples/neutrN.p
echo "NHORPO+neutralization:"
cabal run hrsterm -- -t nhorpo_n -s z3 examples/neutrN.p
