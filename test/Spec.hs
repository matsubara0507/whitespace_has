import Test.HUnit
import Whitespace
import Parser
import Machine
import System.IO (stderr)

{-
	[Space][Space][Space][Tab][LF]	Put a 1 on the stack
	[LF][Space][Space][Space][Tab][Space][Space] [Space][Space][Tab][Tab][LF]	Set a Label at this point
	[Space][LF][Space]	Duplicate the top stack item
	[Tab][LF][Space][Tab]	Output the current value
	[Space][Space][Space][Tab][Space][Tab][Space][LF]	Put 10 (newline) on the stack...
	[Tab][LF][Space][Space]	...and output the newline
	[Space][Space][Space][Tab][LF]	Put a 1 on the stack
	[Tab][Space][Space][Space]	Addition. This increments our current value.
	[Space][LF][Space]	Duplicate that value so we can test it
	[Space][Space][Space][Tab][Space][Tab][Tab][LF]	Push 11 onto the stack
	[Tab][Space][Space][Tab]	Subtraction. So if we've reached the end, we have a zero on the stack.
	[LF][Tab][Space][Space][Tab][Space][Space] [Space][Tab][Space][Tab][LF]	If we have a zero, jump to the end
	[LF][Space][LF][Space][Tab][Space] [Space][Space][Space][Tab][Tab][LF]	Jump to the start
	[LF][Space][Space][Space][Tab][Space] [Space][Space][Tab][Space][Tab][LF]	Set the end label
	[Space][LF][LF]	Discard our accumulator, to be tidy
	[LF][LF][LF]
-}
step0 = empty;
step1 = incPC $ addCommand [PUSH 1] step0
step2 = incPC . addCommand [MARK "STSSSSTT"] $ addLabel "STSSSSTT" step1
step3 = incPC $ addCommand [DUP] step2
step4 = incPC $ addCommand [OUTNUM] step3
step5 = incPC $ addCommand [PUSH 10] step4
step6 = incPC $ addCommand [OUTCHR] step5
step7 = incPC $ addCommand [PUSH 1] step6
step8 = incPC $ addCommand [ADD] step7
step9 = incPC $ addCommand [DUP] step8
step10 = incPC $ addCommand [PUSH 11] step9
step11 = incPC $ addCommand [SUB] step10
step12 = incPC $ addCommand [JMPZERO "STSSSTST"] step11
step13 = incPC $ addCommand [JUMP "STSSSSTT"] step12
step14 = incPC . addCommand [MARK "STSSSTST"] $ addLabel "STSSSTST" step13
step15 = incPC $ addCommand [DISC] step14
step16 = incPC $ addCommand [EXIT] step15

tests = TestList
    [ "step1" ~: parse "SSSTL" step0 ~?= step1
    , "step2" ~: parse "LSSSTSSSSTTL" step1 ~?= step2
    , "step3" ~: parse "SLS" step2 ~?= step3
    , "step4" ~: parse "TLST" step3 ~?= step4
    , "step5" ~: parse "SSSTSTSL" step4 ~?= step5
    , "step6" ~: parse "TLSS" step5 ~?= step6
    , "step7" ~: parse "SSSTL" step6 ~?= step7
    , "step8" ~: parse "TSSS" step7 ~?= step8
    , "step9" ~: parse "SLS" step8 ~?= step9
    , "step10" ~: parse "SSSTSTTL" step9 ~?= step10
    , "step11" ~: parse "TSST" step10 ~?= step11
    , "step12" ~: parse "LTSSTSSSTSTL" step11 ~?= step12
    , "step13" ~: parse "LSLSTSSSSTTL" step12 ~?= step13
    , "step14" ~: parse "LSSSTSSSTSTL" step13 ~?= step14
    , "step15" ~: parse "SLL" step14 ~?= step15
    , "step16" ~: parse "LLL" step15 ~?= step16
    ]

main :: IO ()
main = do
  runTestText (putTextToHandle stderr False) tests
  let m = step16
  _ <- runEval (resetPC m) . eval $ getCommand m
  putStrLn "FIN"
