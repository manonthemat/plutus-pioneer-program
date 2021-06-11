{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract 100 "Alice" "Bob" "Charlie"

contract :: Integer -> Party -> Party -> Party -> Contract
contract amount partyOne partyTwo arbitrator = When
    [Case
        (Deposit
            arbitrator
            arbitrator
            ada
            (Constant (amount + amount))
        )
        (When
            [ f partyOne partyTwo
            , f partyTwo partyOne
            ]
            10 Close
        )
    ]
    5 Close

  where
  -- charlie should put down a deposit first (twice the amount)
  -- if charlie does not choose, alice and bob get half of charlie's
    f :: Party -> Party -> Case
    f firstDeposit secondDeposit =
        Case
            (Deposit
                firstDeposit
                firstDeposit
                ada
                (Constant amount)
            )
            (When
                [Case
                    (Deposit
                        secondDeposit
                        secondDeposit
                        ada
                        (Constant amount)
                    )
                    (When
                        [Case
                            (Choice
                                (ChoiceId
                                    "Winner"
                                    arbitrator
                                )
                                [Bound 1 2]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue
                                        (ChoiceId
                                            "Winner"
                                            arbitrator
                                        ))
                                    (Constant 1)
                                )
                                (Pay
                                    partyTwo
                                    (Account partyOne)
                                    ada
                                    (Constant amount)
                                    Close
                                )
                                (Pay
                                    partyOne
                                    (Account partyTwo)
                                    ada
                                    (Constant amount)
                                    Close
                                )
                            )]
                        30 (
                            Pay arbitrator (Account partyOne) ada (Constant amount) $
                            Pay arbitrator (Account partyTwo) ada (Constant amount)
                            Close
                        )
                    )]
                20 Close
            )
