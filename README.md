# purescript-test-unit

An asynchronous unit test runner for PureScript.

* [API docs on Pursuit](http://pursuit.purescript.org/packages/purescript-test-unit/)

## Usage

Test-Unit tests are simply
[Aff](https://github.com/slamdata/purescript-aff) actions, which can
either succeed (test passed) or fail (test did not pass). The type for
these tests is `TestUnit e`, which is just an alias for `Aff (... | e)
Unit`.

The `Test.Unit.Assert` module contains a number of functions for
making common assertions. The most straightforward is `assert`, which
takes a failure message and a boolean, and if the boolean is true, it
produces a `TestUnit` which immediately succeeds. If the boolean is
false, you get a `TestUnit` which fails with the provided error
message.

Because tests are really just `Aff`s, you can perform any `Aff` inside
a do block, allowing you to easily test asynchronous code.

```purescript
module Test.Main where

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

import Node.FS.Aff as FS

main = runTest do
  test "arithmetic" do
    Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
    Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
    Assert.equal (2 + 2) 4
    Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal (2 + 2) 5
  test "with async IO" do
    fileContents <- FS.readFile "file.txt"
    Assert.equal fileContents "hello here are your file contents"
  test "async operation with a timeout" do
    timeout 100 $ do
      file2Contents <- FS.readFile "file2.txt"
      Assert.equal file2Contents "can we read a file in 100ms?"
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
