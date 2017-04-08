import Test.Hspec

main = hspec $ do
    describe "a suite" $ do
        it "shoud pass" $ do
            2+2 `shouldBe` 4
        it "should not fail" $ do
            2*2 `shouldBe` 4
    describe "response" $ do
        it "should be a yes or a no" $ do
            response 'N' `shouldBe` False
            response 'n' `shouldBe` False
            response 'Y' `shouldBe` True
            response 'y' `shouldBe` True
    --9
    describe "label" $ do
        it "should be an english label" $ do
            label "WO" `shouldBe` "Wool"
            label "CO" `shouldBe` "Cotton"
            label "PA" `shouldBe` "Nylon"
            label "PC" `shouldBe` "Acrylic"
            label "XX" `shouldBe` "--- unknown label ---"
            label "YY" `shouldBe` "--- unknown label ---"
    describe "average" $ do
        it "should calculate the average" $ do
            average [] `shouldBe` 0
            average [2,4,12] `shouldBe` 6
    describe "compare" $ do
        it "should compare values of any type of class Ord" $ do
            compare 42 17       `shouldBe` GT
            compare 'A' 'B'     `shouldBe` LT
            compare 11.3 11.3   `shouldBe` EQ
            compare "cat" "dog" `shouldBe` LT

--pattern matching
response 'N' = False
response 'n' = False
response 'Y' = True
response 'y' = True

label "WO" = "Wool"
label "CO" = "Cotton"
label "PA" = "Nylon"
label "PC" = "Acrylic"
label _ = "--- unknown label ---"

average [] = 0
average xs = sum xs `div` length xs
