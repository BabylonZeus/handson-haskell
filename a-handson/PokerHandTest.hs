import Test.Hspec
import PokerHand
import Data.Ord

main = hspec $ do
    --15
    describe "comparing card by rank" $ do
        it "should follow the rules of poker" $ do
            comparing (rank . card) "8d" "6h" `shouldBe` GT
            comparing (rank . card) "4d" "4h" `shouldBe` EQ
            comparing (rank . card) "Td" "Jh" `shouldBe` LT
            comparing (rank . card) "9d" "Th" `shouldBe` LT
            comparing (rank . card) "Jd" "Qh" `shouldBe` LT
            comparing (rank . card) "Qd" "Kh" `shouldBe` LT
            comparing (rank . card) "Kd" "Ah" `shouldBe` LT
    describe "comparing card by suit" $ do
        it "should follow the rules of poker" $ do
            suit (Eight,Diamonds) == suit (Six, Diamonds) `shouldBe` True
            suit (Four,Diamonds) == suit (Four, Hearts) `shouldBe` False
            suit (Nine,Diamonds) == suit (Ten,Clubs) `shouldBe` False
            suit (Ten,Diamonds) == suit (Jack,Spades) `shouldBe` False
    describe "cards" $ do
        it "should collect cards from a string" $ do
            cards "8d Ah Qc" `shouldBe` [(Eight,Diamonds),(Ace,Hearts),(Queen,Clubs)]
    describe "ranks" $ do
        it "should give the sorted ranks of a hand" $ do
            ranks (cards "8d Ah Qc")  `shouldBe` [Ace, Queen, Eight]

