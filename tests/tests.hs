module Main where
import Test.Tasty
import qualified BasisTests as BT
import qualified ClifTests as CT
import qualified InternalTests as IT

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "Implementation"      [IT.tests]
    , testGroup "Properties"
        [testGroup "Basis"            [BT.tests]
        ,testGroup "Clifford algebra" [CT.tests]
        ]
    ]
        

