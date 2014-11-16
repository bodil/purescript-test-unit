# purescript-test-unit

An asynchronous unit test runner for PureScript.

## Usage

```purescript
module Test.Main where

import Test.Unit

main = runTest do
  test "arithmetic" do
    assert "two plus two isn't four" $ (2 + 2) == 4
    assertFalse "two plus two is five" $ (2 + 2) == 5
  test "async asserts" do
    assertFn "done callback got a falseness!" \done -> done true
```

Run tests using [`pulp test`](https://github.com/bodil/pulp) or just
by compiling with `--main Test.Main`.

## License

Copyright 2014 Bodil Stokke

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program. If not, see
<http://www.gnu.org/licenses/>.

## Module Test.Unit

### Types

    type Assertion e = TestUnit e

    type Test e = TestUnit e

    type TestResult = Either String Unit

    type TestUnit e = ErrorT String (ContT Unit (Eff e)) Unit


### Values

    assert :: forall e. String -> Boolean -> Assertion e

    assertC :: forall e. String -> ContT Unit (Eff e) Boolean -> Assertion e

    assertFalse :: forall e. String -> Boolean -> Assertion e

    assertFn :: forall e. String -> ((Boolean -> Eff e Unit) -> Eff e Unit) -> Assertion e

    failure :: String -> TestResult

    runTest :: forall e. Test (testOutput :: TestOutput | e) -> Eff (testOutput :: TestOutput | e) Unit

    success :: TestResult

    test :: forall e. String -> Assertion (testOutput :: TestOutput | e) -> Test (testOutput :: TestOutput | e)

    testC :: forall e. ContT Unit (Eff e) TestResult -> Assertion e

    testFn :: forall e. ((TestResult -> Eff e Unit) -> Eff e Unit) -> Assertion e
