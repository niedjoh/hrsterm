echo "--- Example 6 in [NM25] (extended) --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/diff2_base.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/diff2_base.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/diff2_base.afsm
echo "--- Example 7 in [NM25] --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/nnf.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/nnf.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/nnf.afsm
echo "--- Example 8 in [NM25] --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/map_inc.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/map_inc.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/map_inc.afsm
echo "--- Example 5.2 in [BJR15] --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/hofmann.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/hofmann.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/hofmann.afsm
echo "--- Example 7.1 in [BJR15] --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/brouwerord.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/brouwerord.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/brouwerord.afsm
echo "--- Example 8.19 in [BJR15] --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/applicative_treeflatten.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/applicative_treeflatten.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/applicative_treeflatten.afsm
echo "--- Example 7.1 in [JR15] --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/pnf.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/pnf.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/pnf.afsm
echo "--- Example 7.2 in [JR15] --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/sdu.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/sdu.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/sdu.afsm
echo "--- Example 7.3 in [JR15] --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/pad.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/pad.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/pad.afsm
echo "--- neutr.p --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/neutr.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/neutr.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/neutr.afsm
echo "--- neutrN.p --"
echo "NCPO-LNF:"
cabal run hrsterm -- -t ncpo_lnf -s z3 examples/neutrN.p
echo "NCPO:"
cabal run hrsterm -- -t ncpo -s z3 examples/neutrN.p
echo "WANDA:"
./wanda.exe --betafirst --silent examples/other_formats/lnf/neutrN.afsm
