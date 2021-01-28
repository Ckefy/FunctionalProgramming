module TestDays (testDays) where

import Days
import Test.Tasty
import Test.Tasty.HUnit

nextDayTests :: Assertion
nextDayTests =
        [Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, Monday]
          @?=
          map nextDay
            [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

afterDaysTests :: Assertion
afterDaysTests =
    [Tuesday, Wednesday, Saturday, Friday, Sunday, Friday]
      @?=
       map (uncurry afterDays)
        [(Monday, 1), (Monday, 2), (Tuesday, 4), (Friday, 0), (Saturday, 10^6), (Wednesday, 10^8)]

isWeekendTests :: Assertion
isWeekendTests =
    [False, False, False, False, False, True, True]
      @?=
        map isWeekend
          [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

daysToPartyTests :: Assertion
daysToPartyTests =
    [0, 1, 2, 3, 4, 5, 6]
      @?=
        map daysToParty
          [Friday, Thursday, Wednesday, Tuesday, Monday, Sunday, Saturday]

--TODO: work`s done
testDays :: TestTree
testDays =
    testGroup
    "Test module Days"
    [ testCase "nextDay function" nextDayTests
    , testCase "afterDays function" afterDaysTests
    , testCase "isWeekend function" isWeekendTests
    , testCase "daysToParty function" daysToPartyTests
    ]