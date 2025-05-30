echo "--- Example 6 (extended) --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/diff2.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/diff2.trs
echo "--- Example 7 --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/nnf.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/nnf.trs
echo "--- Example 8 --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/map_inc.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/map_inc.trs
echo "--- Example 5.2 in [BJR15] --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/hofmann.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/hofmann.trs
echo "--- Example 7.1 in [BJR15] --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/brouwerord.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/brouwerord.trs
echo "--- Example 8.19 in [BJR15] --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/applicative_treeflatten.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/applicative_treeflatten.trs
echo "--- Example 7.1 in [JR15] --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/pnf.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/pnf.trs
echo "--- Example 7.2 in [JR15] --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/sdu.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/sdu.trs
echo "--- Example 7.3 in [JR15] --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/pad.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/pad.trs
echo "--- neutr --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/neutr.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/neutr.trs
echo "--- neutrN --"
echo "WANDA:"
./wanda.exe --betafirst -d nt,poly,dp --silent examples/other_formats/neutrN.afsm
echo "CSI^ho:"
./csiho --category HO --strategy horpo -p -t examples/other_formats/neutrN.trs
