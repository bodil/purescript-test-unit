# purescript-test-unit

An asynchronous unit test runner for PureScript.

* [API docs on Pursuit](http://pursuit.purescript.org/packages/purescript-test-unit/)

## Usage

Test-Unit tests are simply
[Aff](https://github.com/slamdata/purescript-aff) actions, which can
either succeed (test passed) or fail (test did not pass). The type for
these tests is `Test e`, which is just an alias for `Aff e Unit`.

The `Test.Unit.Assert` module contains a number of functions for
making common assertions. The most straightforward is `assert`, which
takes a failure message and a boolean, and if the boolean is true, it
produces a `Test` which immediately succeeds. If the boolean is false,
you get a `Test` which fails with the provided error message.

Because tests are really just `Aff`s, you can perform any `Aff` inside
a do block, allowing you to easily test asynchronous code.

```purescript
module Test.Main where

import Prelude

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))

main = runTest do
  suite "sync code" do
    test "arithmetic" do
      Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
      Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
      Assert.equal (2 + 2) 4
      Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal (2 + 2) 5
  suite "async code" do
    test "with async IO" do
      fileContents <- FS.readTextFile UTF8 "file.txt"
      Assert.equal fileContents "hello here are your file contents"
    test "async operation with a timeout" do
      timeout 100 $ do
        file2Contents <- FS.readTextFile UTF8 "file2.txt"
        Assert.equal file2Contents "can we read a file in 100ms?"
```

Run tests using [`pulp test`](https://github.com/bodil/pulp) or just
by compiling with `--main Test.Main`.

## QuickCheck

[purescript-quickcheck](https://github.com/purescript/purescript-quickcheck)
tests can be run using the functions in the `Test.Unit.QuickCheck`
module. It exports two functions, `quickCheck` and `quickCheck'`,
which work like their QuickCheck counterparts, except they produce
`Test` actions so they integrate cleanly with Test-Unit.

```purescript
module Test.Main where

import Prelude

import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck (Result(), (===))

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

main = runTest do
  test "the commutative property" do
    quickCheck theCommutativeProperty
```

## Output Formats

The `Test.Unit.Main.runTest` function will default to simple output of
test results using `console.log` (the
`Test.Unit.Output.Simple.runTest` test runner). If you're running on
an ANSI colour capable terminal, it will use the
`Test.Unit.Output.Fancy.runTest` test runner, which gets a little more
colourful.

Additionally, if `Test.Unit.Main.runTest` notices the word `tap` or
`--tap` on its command line, it will pick the
`Test.Unit.Output.TAP.runTest` test runner, which outputs test results
using the [TAP](https://testanything.org/) format. A number of TAP
consumers are
[available on NPM](https://www.npmjs.com/package/tape#pretty-reporters)
to transform the test output. For instance, you could install the
[tap-spec](https://github.com/scottcorgan/tap-spec) and run your tests
like this: `pulp test tap | tap-spec`.

You can also specify your own test runner using the
`Test.Unit.Main.runTestWith` function, which takes a test runner as
its first argument. So, if you want to force the TAP test runner,
instead of `main = runTest do ...` you could use `main = runTestWith
Test.Unit.Output.TAP.runTest do ...`. You could also supply your own
custom test runner - study one of the existing test runners to learn
how.

## Licence

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
