:set -i../../
:set prompt "> "
:l LogAnalysis.hs

let file = "../../../data/error.log"

testParse parse 10 file

testWhatWentWrong parse whatWentWrong file
